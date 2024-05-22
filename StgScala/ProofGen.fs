module ProofGen
open ProofAst
open DAst
open FsUtils
open CommonTypes
open Language
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open AcnGenericTypes

type SizeProps =
  | ExternalField
  | BitsNullTerminated of string
  | AsciiNullTerminated of byte list

let getAccess (acc: Accessor) =
    match acc with
    | ValueAccess (sel, _, _) -> $".{sel}"
    | PointerAccess (sel, _, _) -> $".{sel}"
    | ArrayAccess (ix, _) -> $"({ix})"
let joinedSelection (sel: Selection): string =
  List.fold (fun str accessor -> $"{str}{getAccess accessor}") sel.receiverId sel.path
let getAcnDeterminantName (id : ReferenceToType) =
  match id with
  | ReferenceToType path ->
    match path with
    | (MD _) :: (TA _) :: (PRM prmName) :: [] -> ToC prmName
    | _ ->
      let longName = id.AcnAbsPath.Tail |> Seq.StrJoin "_"
      ToC (longName.Replace("#","elem"))

let fromAcnSizeProps (sizeProps: AcnStringSizeProperty): SizeProps =
  match sizeProps with
  | StrExternalField _ -> ExternalField
  | StrNullTerminated pat -> AsciiNullTerminated pat

let fromSizeableProps (sizeProps: AcnSizeableSizeProperty): SizeProps =
  match sizeProps with
  | SzExternalField _ -> ExternalField
  | SzNullTerminated pat -> BitsNullTerminated pat.Value

let stringLikeSizeExpr (sizeProps: SizeProps option) (minNbElems: bigint) (maxNbElems: bigint) (charSize: bigint) (strLength: Expr): Expr =
  // TODO: check if we need to consider the encoded size (determinant) or not
  let vleSize, nbElemsInBits =
    if minNbElems = maxNbElems then 0I, longlit (maxNbElems * charSize)
    else 0I (*GetNumberOfBitsForNonNegativeInteger(maxNbElems - minNbElems)*), Mult (longlit charSize, strLength)
  let patSize =
    match sizeProps with
    | Some ExternalField | None -> 0I
    | Some (BitsNullTerminated pat) -> (bigint pat.Length) * 8I
    | Some (AsciiNullTerminated pat) -> bigint pat.Length
  plus [longlit (vleSize + patSize); nbElemsInBits]

let intSizeExpr (int: Asn1AcnAst.Integer) (obj: Expr): Expr =
  match int.acnProperties.encodingProp, int.acnProperties.sizeProp, int.acnProperties.endiannessProp with
  | None, None, None ->
    match int.uperRange with
    | Full  ->
      plus [longlit 1I; getLengthForEncodingSigned obj]
    | NegInf _ | PosInf _ -> getBitCountUnsigned obj
    | Concrete _ ->
      assert (int.acnMinSizeInBits = int.acnMaxSizeInBits)
      assert (int.uperMinSizeInBits = int.uperMinSizeInBits)
      assert (int.acnMaxSizeInBits = int.uperMaxSizeInBits)
      longlit int.acnMaxSizeInBits
  | _ ->
    assert (int.acnMinSizeInBits = int.acnMaxSizeInBits) // TODO: Not quite true, there is ASCII encoding that is variable...
    longlit int.acnMaxSizeInBits

// TODO: Bad name (ne considère que les sequence, pas les ACN de sequence dans choice)
let rec collectAllAcnChildren (tpe: Asn1AcnAst.Asn1TypeKind): Asn1AcnAst.AcnChild list =
  match tpe.ActualType with
  | Sequence sq ->
    sq.children |> List.collect (fun c ->
      match c with
      | AcnChild c -> [c]
        // if c.inserted then [c] else []
      | Asn1Child c -> collectAllAcnChildren c.Type.Kind
    )
  | _ -> []


// TODO: ALIGN???
let acnTypeSizeExpr (acn: AcnInsertedType): Expr =
  match acn with
  | AcnInteger int->
    if int.acnMinSizeInBits <> int.acnMaxSizeInBits then failwith "TODO"
    else longlit int.acnMaxSizeInBits

  | AcnNullType nll ->
    assert (nll.acnMinSizeInBits = nll.acnMaxSizeInBits)
    longlit nll.acnMaxSizeInBits

  | AcnBoolean b ->
    assert (b.acnMinSizeInBits = b.acnMaxSizeInBits)
    longlit b.acnMaxSizeInBits

  | AcnReferenceToEnumerated e ->
    if e.enumerated.acnMinSizeInBits <> e.enumerated.acnMaxSizeInBits then failwith "TODO"
    else longlit e.enumerated.acnMaxSizeInBits

  | AcnReferenceToIA5String  s ->
    if s.str.acnMinSizeInBits <> s.str.acnMaxSizeInBits then failwith "TODO"
    else longlit s.str.acnMaxSizeInBits

let maxAlignmentOf (aligns: AcnAlignment option list): AcnAlignment option =
  assert (not aligns.IsEmpty)
  aligns |> List.maxBy (fun a -> a |> Option.map (fun a -> a.nbBits) |> Option.defaultValue 0I)

let rec maxAlignment (tp: Asn1AcnAst.Asn1Type): AcnAlignment option =
  match tp.Kind.ActualType with
  | Asn1AcnAst.Sequence sq ->
    maxAlignmentOf (tp.acnAlignment :: (sq.children |> List.map (fun c ->
      match c with
      | Asn1Child c -> maxAlignment c.Type
      | AcnChild c -> c.Type.acnAlignment
    )))
  | Choice ch ->
    maxAlignmentOf (tp.acnAlignment :: (ch.children |> List.map (fun c -> maxAlignment c.Type)))
  | SequenceOf sqf ->
    maxAlignmentOf [tp.acnAlignment; maxAlignment sqf.child]
  | _ -> tp.acnAlignment

let sizeLemmaId(align: AcnAlignment option): string =
  match align with
  | None -> "sizeLemmaAnyOffset"
  | Some NextByte -> "sizeLemmaNextByte"
  | Some NextWord -> "sizeLemmaNextWord"
  | Some NextDWord -> "sizeLemmaNextDWord"

let sizeLemmaIdForType (tp: Asn1AcnAst.Asn1TypeKind) (align: AcnAlignment option): string option =
  match tp.ActualType with
  | Sequence _ | Choice _ | SequenceOf _ -> Some (sizeLemmaId align)
  | _ -> None

let sizeLemmaCall (tp: Asn1AcnAst.Asn1TypeKind) (align: AcnAlignment option) (recv: Expr) (offset: Expr) (otherOffset: Expr): Expr option =
  sizeLemmaIdForType tp align |> Option.map (fun id -> MethodCall {recv = recv; id = id; args = [offset; otherOffset]})

let stringInvariants (minSize: bigint) (maxSize: bigint) (recv: Expr): Expr =
  let arrayLen = ArrayLength recv
  let nullCharIx = indexOfOrLength recv (IntLit (UByte, 0I))
  if minSize = maxSize then And [Leq (int32lit (maxSize + 1I), arrayLen); Equals (nullCharIx, int32lit maxSize)]
  else
    And [Leq (int32lit (maxSize + 1I), arrayLen); Leq (int32lit minSize, nullCharIx); Leq (nullCharIx, int32lit maxSize)]

let octetStringInvariants (t: Asn1AcnAst.Asn1Type) (os: Asn1AcnAst.OctetString) (recv: Expr): Expr =
  let len = ArrayLength (FieldSelect (recv, "arr"))
  if os.minSize.acn = os.maxSize.acn then Equals (len, int32lit os.maxSize.acn)
  else
    let nCount = FieldSelect (recv, "nCount")
    And [Leq (len, int32lit os.maxSize.acn); Leq (int32lit os.minSize.acn, nCount); Leq (nCount, len)]

let bitStringInvariants (t: Asn1AcnAst.Asn1Type) (bs: Asn1AcnAst.BitString) (recv: Expr): Expr =
  let len = ArrayLength (FieldSelect (recv, "arr"))
  if bs.minSize.acn = bs.maxSize.acn then Equals (len, int32lit (bigint bs.MaxOctets))
  else
    let nCount = FieldSelect (recv, "nCount")
    And [Leq (len, int32lit (bigint bs.MaxOctets)); Leq (longlit bs.minSize.acn, nCount); Leq (nCount, Mult (len, longlit 8I))] // TODO: Cast en long explicite

let sequenceInvariants (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (children: DAst.SeqChildInfo list) (recv: Expr): Expr option =
    let conds = children |> List.collect (fun child ->
      match child with
      | DAst.Asn1Child child ->
        let field = FieldSelect (recv, child._scala_name)
        let isDefined = isDefinedMutExpr field
        let opt =
          match child.Optionality with
          | Some AlwaysPresent -> [isDefined]
          | Some AlwaysAbsent -> [Not isDefined]
          | _ -> []
        // StringType is a type alias and has therefore no associated class invariant; we need to explicitly add them
        let strType =
          match child.Type.Kind.baseKind.ActualType with
          | IA5String st -> [stringInvariants st.minSize.acn st.maxSize.acn field]
          | _ -> []
        opt @ strType
      | _ -> []
    )
    if conds.IsEmpty then None
    else if conds.Tail.IsEmpty then Some conds.Head
    else Some (And conds)

let sequenceOfInvariants (sqf: Asn1AcnAst.SequenceOf) (recv: Expr): Expr =
    let len = ArrayLength (FieldSelect (recv, "arr"))
    if sqf.minSize.acn = sqf.maxSize.acn then Equals (len, int32lit sqf.maxSize.acn)
    else
      let nCount = FieldSelect (recv, "nCount")
      And [Leq (len, int32lit sqf.maxSize.acn); Leq (int32lit sqf.minSize.acn, nCount); Leq (nCount, len)]

let generateTransitiveLemmaApp (snapshots: Var list) (codec: Var): Expr =
  assert (snapshots.Length >= 2)

  let helper (start: int): Expr list =
    let s1 = snapshots.[start]
    List.rep2 ((List.skip (start + 1) snapshots) @ [codec]) |> List.map (fun (s2, s3) -> validTransitiveLemma (Var s1) (Var s2) (Var s3))

  [0 .. snapshots.Length - 2] |> List.collect helper |> mkBlock

let generateReadPrefixLemmaApp (snapshots: Var list) (children: TypeInfo list) (codec: Var) : Expr =
  assert (children.Length = snapshots.Length)

  let rec extraArgsForType (tpe: TypeEncodingKind option): Expr list =
    match tpe with
    | Some (OptionEncodingType tpe) -> extraArgsForType (Some tpe)
    | Some (Asn1IntegerEncodingType (Some encodingTpe)) ->
        match encodingTpe with
        | FullyConstrainedPositive (min, max) -> [ulonglit min; ulonglit max]
        | FullyConstrained (min, max) -> [longlit min; longlit max]
        | SemiConstrainedPositive min -> [ulonglit min]
        | SemiConstrained max -> [longlit max]
        | UnconstrainedMax max -> [longlit max]
        | Unconstrained -> []
    | _ -> [] // TODO: Rest

  let mkLemma (bs1: Var, bs2: Var, tpe: TypeInfo): Expr =
    let var = {Var.name = $"{bs2.name}_reset"; tpe = bs2.tpe}
    let rst = resetAt (Var bs2) (Var bs1)
    let tpeNoOpt =
      match tpe.typeKind with
      | Some (OptionEncodingType tpe) -> Some tpe
      | _ -> tpe.typeKind
    let lemmaPrefix, lemmaId = readPrefixLemmaIdentifier tpeNoOpt
    let varArg, codecArg =
      if lemmaPrefix = [bitStreamId] then selBitStream (Var var), selBitStream (Var codec)
      else if lemmaPrefix = [codecId] then selBase (Var var), selBase (Var codec)
      else Var var, Var codec
    let extraArgs = extraArgsForType tpeNoOpt
    let app = FunctionCall {prefix = lemmaPrefix; id = lemmaId; args = [varArg; codecArg] @ extraArgs}
    Let {bdg = var; e = rst; body = app}

  List.zip3 (List.skipLast 1 snapshots) snapshots.Tail (List.skipLast 1 children) |> List.map mkLemma |> Block

let private offsetConds (offset :Var) (maxSize: bigint) =
  And [
    Leq (longlit 0I, Var offset)
    Leq (Var offset, longlit (2I ** 63 - 1I - maxSize))
  ]

let private implyingAlignments (align: AcnAlignment option): AcnAlignment option list =
  match align with
  | None -> [None; Some NextByte; Some NextWord; Some NextDWord]
  | Some NextByte -> [Some NextByte; Some NextWord; Some NextDWord]
  | Some NextWord -> [Some NextWord; Some NextDWord]
  | Some NextDWord -> [Some NextDWord]

let private sizeLemmaTemplate (maxSize: bigint) (align: AcnAlignment option): FunDef  =
  let id = sizeLemmaId align
  let offset = {Var.name = "offset"; tpe = IntegerType Long}
  let otherOffset = {Var.name = "otherOffset"; tpe = IntegerType Long}
  let res = {name = "res"; tpe = UnitType}
  let additionalPrecond =
    match align with
    | None -> []
    | Some align ->
      let modOffset = Mod (Var offset, longlit align.nbBits)
      let modOtherOffset = Mod (Var otherOffset, longlit align.nbBits)
      [Precond (Equals (modOffset, modOtherOffset))]
  let postcond = Equals (callSize This (Var offset), callSize This (Var otherOffset))
  {
    id = id
    prms = [offset; otherOffset]
    specs = [Precond (offsetConds offset maxSize); Precond (offsetConds otherOffset maxSize)] @ additionalPrecond
    annots = [GhostAnnot; Opaque; InlineOnce]
    postcond = Some (res, postcond)
    returnTpe = UnitType
    body = UnitLit
  }


// TODO: UPER/ACN

type SizeExprRes = {
  bdgs: (Var * Expr) list
  resSize: Expr
}
type SeqSizeExprChildRes = {
  extraBdgs: (Var * Expr) list
  childVar: Var
  childSize: Expr
}
with
  member this.allBindings: (Var * Expr) list = this.extraBdgs @ [this.childVar, this.childSize]
  member this.allVariables: Var list = this.allBindings |> List.map (fun (v, _) -> v)

let renameBindings (bdgs: (Var * Expr) list) (suffix: string): (Var * Expr) list =
  let allVars = bdgs |> List.map fst
  let renamedVars = allVars |> List.map (fun v -> {v with name = $"{v.name}{suffix}"})
  let mapping = List.zip allVars (renamedVars |> List.map Var)
  let renamedVarFor (old: Var): Var =
    renamedVars.[allVars |> List.findIndex (fun v -> v = old)]
  bdgs |> List.map (fun (v, e) -> renamedVarFor v, substVars mapping e)


let renameBindingsSizeRes (res: SeqSizeExprChildRes list) (suffix: string): SeqSizeExprChildRes list =
  let allVars = res |> List.collect (fun res -> res.allVariables)
  let renamedVars = allVars |> List.map (fun v -> {v with name = $"{v.name}{suffix}"})
  let mapping = List.zip allVars (renamedVars |> List.map Var)
  let renamedVarFor (old: Var): Var =
    renamedVars.[allVars |> List.findIndex (fun v -> v = old)]
  let subst (res: SeqSizeExprChildRes): SeqSizeExprChildRes =
    {
      extraBdgs = res.extraBdgs |> List.map (fun (v, e) -> renamedVarFor v, substVars mapping e)
      childVar = renamedVarFor res.childVar
      childSize = substVars mapping res.childSize
    }
  res |> List.map subst

let rec asn1SizeExpr (align: AcnAlignment option)
                     (tp: Asn1AcnAst.Asn1TypeKind)
                     (obj: Expr)
                     (offset: Expr)
                     (nestingLevel: bigint)
                     (nestingIx: bigint): SizeExprRes =
  let aligned (res: SizeExprRes): SizeExprRes =
    {res with resSize = alignedSizeTo align res.resSize offset}

  match tp with
  | Integer int ->
    aligned {bdgs = []; resSize = intSizeExpr int obj}
  | Enumerated enm ->
    assert (enm.acnMinSizeInBits = enm.acnMaxSizeInBits)
    aligned {bdgs = []; resSize = longlit enm.acnMaxSizeInBits}
  | IA5String st ->
    let szProps = st.acnProperties.sizeProp |> Option.map fromAcnSizeProps
    let charSize = GetNumberOfBitsForNonNegativeInteger (bigint (st.uperCharSet.Length - 1))
    aligned {bdgs = []; resSize = stringLikeSizeExpr szProps st.minSize.acn st.maxSize.acn charSize (indexOfOrLength obj (IntLit (UByte, 0I)))}
  | OctetString ot ->
    let szProps = ot.acnProperties.sizeProp |> Option.map fromSizeableProps
    aligned {bdgs = []; resSize = stringLikeSizeExpr szProps ot.minSize.acn ot.maxSize.acn 8I (stringLength obj)}
  | BitString bt ->
    let szProps = bt.acnProperties.sizeProp |> Option.map fromSizeableProps
    aligned {bdgs = []; resSize = stringLikeSizeExpr szProps bt.minSize.acn bt.maxSize.acn 1I (stringLength obj)}
  | NullType nt ->
    assert (nt.acnMinSizeInBits = nt.acnMaxSizeInBits)
    aligned {bdgs = []; resSize = longlit nt.acnMaxSizeInBits}
  | Boolean bt ->
    assert (bt.acnMinSizeInBits = bt.acnMaxSizeInBits)
    aligned {bdgs = []; resSize = longlit bt.acnMaxSizeInBits}
  | Real rt ->
    assert (rt.acnMinSizeInBits = rt.acnMaxSizeInBits)
    aligned {bdgs = []; resSize = longlit rt.acnMaxSizeInBits}
  | Sequence sq ->
    // Alignment done there
    seqSizeExpr sq align obj offset (nestingLevel + 1I) nestingIx
  | Choice ch ->
    // Ditto
    choiceSizeExpr ch align obj offset (nestingLevel + 1I) nestingIx
  | SequenceOf _ ->
    // seqOfSizeRangeExpr sqf obj offset (nestingLevel + 1I) nestingIx
    // TODO: dire pk
    aligned {bdgs = []; resSize = callSize obj offset}
  | ReferenceType rt ->
    let isComposite =
      match rt.resolvedType.ActualType.Kind with
      | Sequence _ | SequenceOf _ | Choice _ -> true
      | _ -> false
    if rt.hasExtraConstrainsOrChildrenOrAcnArgs || not isComposite then
      // Alignment done there
      asn1SizeExpr align rt.resolvedType.Kind obj offset nestingLevel nestingIx
    else
      aligned {bdgs = []; resSize = callSize obj offset}
  | _ -> aligned {bdgs = []; resSize = callSize obj offset}

and seqSizeExprHelperChild (child: SeqChildInfo)
                           (ix: bigint)
                           (recv: Expr option)
                           (offset: Expr)
                           (nestingLevel: bigint)
                           (nestingIx: bigint): SizeExprRes =
  // functionArgument qui est paramétrisé (choice) indiqué par asn1Type; determinant = function-ID (dans PerformAFunction)
  match child with
  | AcnChild acn ->
    (*if acn.deps.acnDependencies.IsEmpty then
      // This should not be possible, but ACN parameters are probably validated afterwards.
      [], longlit 0I
    else*)
      // There can be multiple dependencies on an ACN field, however all must be consistent
      // (generated code checks for that, done by MultiAcnUpdate).
      // For our use case, we assume the message is consistent, we therefore pick
      // an arbitrary dependency.
      // If it is not the case, the returned value may be incorrect but we would
      // not encode the message anyway, so this incorrect size would not be used.
      // To do things properly, we should move this check of MultiAcnUpdate in the IsConstraintValid function
      // of the message and add it as a precondition to the size function.
      // TODO: variable-length size
      {bdgs = []; resSize = acnTypeSizeExpr acn.Type}
  | Asn1Child asn1 ->
    match asn1.Optionality with
    | Some _ ->
      let someBdg = {Var.name = "v"; tpe = fromAsn1TypeKind asn1.Type.Kind}
      let childRes = asn1SizeExpr asn1.Type.acnAlignment asn1.Type.Kind (Var someBdg) offset nestingLevel (nestingIx + ix)
      let resSize = optionMutMatchExpr recv.Value (Some someBdg) childRes.resSize (longlit 0I)
      {bdgs = childRes.bdgs; resSize = resSize}
    | None ->
      asn1SizeExpr asn1.Type.acnAlignment asn1.Type.Kind recv.Value offset nestingLevel (nestingIx + ix)

and seqSizeExprHelper (sq: Sequence)
                      (obj: Expr)
                      (offset: Expr)
                      (nestingLevel: bigint)
                      (nestingIx: bigint): SeqSizeExprChildRes list =
  let childSize (acc: SeqSizeExprChildRes list) (ix: int, child: SeqChildInfo): SeqSizeExprChildRes list =
    let varName =
      if nestingLevel = 0I then $"size_{nestingIx + bigint ix}"
      else $"size_{nestingLevel}_{nestingIx + bigint ix}"
    let resVar = {Var.name = varName; tpe = IntegerType Long}
    let accOffset = plus (offset :: (acc |> List.map (fun res -> Var res.childVar)))
    let recv =
      match child with
      | AcnChild _ -> None
      | Asn1Child child -> Some (FieldSelect (obj, child._scala_name))
    let childResSize = seqSizeExprHelperChild child (bigint ix) recv accOffset nestingLevel nestingIx
    acc @ [{extraBdgs = childResSize.bdgs; childVar = resVar; childSize = childResSize.resSize}]
  sq.children |> List.indexed |> (List.fold childSize [])

and seqSizeExpr (sq: Sequence)
                (align: AcnAlignment option)
                (obj: Expr)
                (offset: Expr)
                (nestingLevel: bigint)
                (nestingIx: bigint): SizeExprRes =
  if sq.children.IsEmpty then {bdgs = []; resSize = longlit 0I}
  else
    let presenceBits = sq.children |> List.sumBy (fun child ->
      match child with
        | AcnChild _ -> 0I
        | Asn1Child asn1 ->
          match asn1.Optionality with
          | Some (Optional opt) when opt.acnPresentWhen.IsNone -> 1I
          | _ -> 0I
    )
    let childrenSizes = seqSizeExprHelper sq obj offset nestingLevel nestingIx
    let allBindings = childrenSizes |> List.collect (fun s -> s.extraBdgs @ [(s.childVar, s.childSize)])
    let childrenVars = childrenSizes |> List.map (fun s -> s.childVar)
    let resultSize = plus [longlit presenceBits; childrenVars |> List.map Var |> plus]
    let resultSize = alignedSizeTo align resultSize offset
    {bdgs = allBindings; resSize = resultSize}

and choiceSizeExpr (choice: Asn1AcnAst.Choice)
                   (align: AcnAlignment option)
                   (obj: Expr)
                   (offset: Expr)
                   (nestingLevel: bigint)
                   (nestingIx: bigint): SizeExprRes =
  let cases = choice.children |> List.map (fun child ->
    let tpeId = (ToC choice.typeDef[Scala].typeName) + "." + (ToC child.present_when_name) + "_PRESENT"
    let tpe = fromAsn1TypeKind child.Type.Kind
    let binder = {Var.name = child._scala_name; tpe = tpe}
    let pat = ADTPattern {binder = None; id = tpeId; subPatterns = [Wildcard (Some binder)]}
    let res = asn1SizeExpr child.Type.acnAlignment child.Type.Kind (Var binder) offset nestingLevel nestingIx
    let resSize = alignedSizeTo align res.resSize offset
    let res = letsIn res.bdgs resSize
    {MatchCase.pattern = pat; rhs = res}
  )
  {bdgs = []; resSize = MatchExpr {scrut = obj; cases = cases}}

// TODO: incorrect si on refine un element dans la sequenceOf....
and seqOfSizeRangeExpr (sq: Asn1AcnAst.SequenceOf)
                       (align: AcnAlignment option)
                       (obj: Expr)
                       (offset: Expr)
                       (nestingLevel: bigint)
                       (nestingIx: bigint): SizeExprRes =
  let from = {name = "from"; tpe = IntegerType Int}
  let tto = {name = "to"; tpe = IntegerType Int}
  let arr = FieldSelect (obj, "arr")

  let elem = ArraySelect (arr, Var from)
  let elemSizeVar = {name = "elemSize"; tpe = IntegerType Long}
  let elemSize = asn1SizeExpr sq.child.acnAlignment sq.child.Kind elem offset nestingLevel nestingIx
  let elemSizeAssert =
    if sq.child.Kind.acnMinSizeInBits = sq.child.Kind.acnMaxSizeInBits then
      Assert (Equals (Var elemSizeVar, longlit sq.child.Kind.acnMinSizeInBits))
    else
      Assert (And [
        Leq (longlit sq.child.Kind.acnMinSizeInBits, Var elemSizeVar)
        Leq (Var elemSizeVar, longlit sq.child.Kind.acnMaxSizeInBits)
      ])
  let invAssert = Assert (sequenceOfInvariants sq obj)
  let reccall = sizeRange This (plus [offset; Var elemSizeVar]) (plus [Var from; int32lit 1I]) (Var tto)
  let resSize = alignedSizeTo align (plus [Var elemSizeVar; reccall]) offset
  let elseBody = letsIn (elemSize.bdgs @ [elemSizeVar, elemSize.resSize]) (mkBlock [elemSizeAssert; invAssert; resSize])
  let body =
    IfExpr {
      cond = Equals (Var from, Var tto)
      thn = longlit 0I
      els = elseBody
    }
  {bdgs = []; resSize = body}


let seqSizeFunDefs (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence): FunDef list =
  // TODO: Pour les int, on peut ajouter une assertion GetBitUnsignedCount(...) == resultat (ici et/ou ailleurs)
  let offset = {Var.name = "offset"; tpe = IntegerType Long}
  let res = seqSizeExpr sq t.acnAlignment This (Var offset) 0I 0I
  let finalSize = letsIn res.bdgs res.resSize
  let res = {name = "res"; tpe = IntegerType Long}
  let postcond =
    if sq.acnMinSizeInBits = sq.acnMaxSizeInBits then Equals (Var res, longlit sq.acnMaxSizeInBits)
    else And [Leq (longlit sq.acnMinSizeInBits, Var res); Leq (Var res, longlit sq.acnMaxSizeInBits)]

  let sizeLemmas (align: AcnAlignment option): FunDef =
    let template = sizeLemmaTemplate sq.acnMaxSizeInBits align
    let offset = template.prms.[0]
    let otherOffset = template.prms.[1]

    let allResWithOffset = seqSizeExprHelper sq This (Var offset) 0I 0I
    let allResWithOffset = renameBindingsSizeRes allResWithOffset "_offset"
    let allResWithOtherOffset = seqSizeExprHelper sq This (Var otherOffset) 0I 0I
    let allResWithOtherOffset = renameBindingsSizeRes allResWithOtherOffset "_otherOffset"

    let proofSubcase (ix: int, (resWithOffset: SeqSizeExprChildRes, resWithOtherOffset: SeqSizeExprChildRes, child: SeqChildInfo)) (rest: Expr): Expr =
      let withBindingsPlugged (expr: Expr option): Expr =
        let allBdgs =
          resWithOffset.extraBdgs @
          [(resWithOffset.childVar, resWithOffset.childSize)] @
          resWithOtherOffset.extraBdgs @
          [(resWithOtherOffset.childVar, resWithOtherOffset.childSize)]
        match expr with
        | Some expr -> letsIn allBdgs (mkBlock [expr; rest])
        | None -> letsIn allBdgs rest

      match child with
      | Asn1Child child ->
        let accOffset = Var offset :: (allResWithOffset |> List.take ix |> List.map (fun res -> Var res.childVar))
        let accOtherOffset = Var otherOffset :: (allResWithOtherOffset |> List.take ix |> List.map (fun res -> Var res.childVar))
        match child.Optionality with
        | Some _ ->
          let scrut = FieldSelect (This, child._scala_name)
          let someBdg = {Var.name = "v"; tpe = fromAsn1TypeKind child.Type.Kind}
          let lemmaCall = sizeLemmaCall child.Type.Kind align (Var someBdg) (plus accOffset) (plus accOtherOffset)
          let mtchExpr = lemmaCall |> Option.map (fun call -> optionMutMatchExpr scrut (Some someBdg) call UnitLit)
          withBindingsPlugged mtchExpr
        | None ->
          let lemmaCall = sizeLemmaCall child.Type.Kind align (FieldSelect (This, child._scala_name)) (plus accOffset) (plus accOtherOffset)
          withBindingsPlugged lemmaCall
      | AcnChild _ -> withBindingsPlugged None

    assert (allResWithOffset.Length = allResWithOtherOffset.Length)
    assert (allResWithOffset.Length = sq.children.Length)
    let proofBody = (List.foldBack proofSubcase ((List.zip3 allResWithOffset allResWithOtherOffset sq.children) |> List.indexed) UnitLit)

    {template with body = proofBody}

  let sizeFd = {
    id = "size"
    prms = [offset]
    specs = [Precond (offsetConds offset sq.acnMaxSizeInBits)]
    annots = []
    postcond = Some (res, postcond)
    returnTpe = IntegerType Long
    body = finalSize
  }
  let maxAlign = maxAlignment t
  let implyingAligns = implyingAlignments maxAlign
  let lemmas = implyingAligns |> List.map sizeLemmas
  sizeFd :: lemmas

let choiceSizeFunDefs (t: Asn1AcnAst.Asn1Type) (choice: Asn1AcnAst.Choice): FunDef list =
  let offset = {Var.name = "offset"; tpe = IntegerType Long}
  let sizeRes = choiceSizeExpr choice t.acnAlignment This (Var offset) 0I 0I
  assert sizeRes.bdgs.IsEmpty
  let sizeLemmas (align: AcnAlignment option): FunDef =
    let template = sizeLemmaTemplate choice.acnMaxSizeInBits align
    let offset = template.prms.[0]
    let otherOffset = template.prms.[1]
    let proofCases = choice.children |> List.map (fun child ->
      let tpeId = (ToC choice.typeDef[Scala].typeName) + "." + (ToC child.present_when_name) + "_PRESENT"
      let tpe = fromAsn1TypeKind child.Type.Kind
      let binder = {Var.name = child._scala_name; tpe = tpe}
      let pat = ADTPattern {binder = None; id = tpeId; subPatterns = [Wildcard (Some binder)]}
      let subcaseProof = sizeLemmaCall child.Type.Kind align (Var binder) (Var offset) (Var otherOffset)
      {MatchCase.pattern = pat; rhs = subcaseProof |> Option.defaultValue UnitLit}
    )
    let proof = MatchExpr  {scrut = This; cases = proofCases}
    {template with body = proof}

  let res = {name = "res"; tpe = IntegerType Long}
  let postcond =
    if choice.acnMinSizeInBits = choice.acnMaxSizeInBits then Equals (Var res, longlit choice.acnMaxSizeInBits)
    else And [Leq (longlit choice.acnMinSizeInBits, Var res); Leq (Var res, longlit choice.acnMaxSizeInBits)]
  let sizeFd = {
    id = "size"
    prms = [offset]
    specs = [Precond (offsetConds offset choice.acnMaxSizeInBits)]
    annots = []
    postcond = Some (res, postcond)
    returnTpe = IntegerType Long
    body = sizeRes.resSize
  }
  let maxAlign = maxAlignment t
  let implyingAligns = implyingAlignments maxAlign
  let lemmas = implyingAligns |> List.map sizeLemmas
  sizeFd :: lemmas

let seqOfSizeFunDefs (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.SequenceOf) (elemTpe: DAst.Asn1Type): FunDef list =
  let offset = {Var.name = "offset"; tpe = IntegerType Long}
  let res = {name = "res"; tpe = IntegerType Long}
  let count = FieldSelect (This, "nCount")
  let inv = sequenceOfInvariants sq This
  let offsetCondHelper (offset: Var) (from: Var) (tto: Var): Expr =
    let overhead = sq.acnMaxSizeInBits - sq.maxSize.acn * elemTpe.Kind.baseKind.acnMaxSizeInBits
    mkBlock [
      Assert inv
      And [
        Leq (longlit 0I, Var offset)
        Leq (Var offset, Minus (longlit (2I ** 63 - 1I - overhead), Mult (longlit elemTpe.Kind.baseKind.acnMaxSizeInBits, Minus (Var tto, Var from))))
      ]
    ]
  let rangeVarsCondHelper (from: Var) (tto: Var): Expr = And [Leq (int32lit 0I, Var from); Leq (Var from, Var tto); Leq (Var tto, count)]

  let sizeRangeFd =
    let from = {name = "from"; tpe = IntegerType Int}
    let tto = {name = "to"; tpe = IntegerType Int}
    let measure = Minus (Var tto, Var from)
    let offsetCond = offsetCondHelper offset from tto
    let rangeVarsConds = rangeVarsCondHelper from tto
    let sizeRes = seqOfSizeRangeExpr sq t.acnAlignment This (Var offset) 0I 0I
    let postcondRange =
      let nbElems = {Var.name = "nbElems"; tpe = IntegerType Int} // TODO: Add explicit cast to Long
      let sqLowerBound = Mult (longlit elemTpe.Kind.baseKind.acnMinSizeInBits, Var nbElems)
      let sqUpperBound = Mult (longlit elemTpe.Kind.baseKind.acnMaxSizeInBits, Var nbElems)
      Let {
        bdg = nbElems
        e = Minus (Var tto, Var from) // TODO: Add explicit cast to Long
        body = mkBlock [
          Assert (And [Leq (int32lit 0I, Var nbElems); Leq (Var nbElems, int32lit sq.maxSize.acn)]) // To help check against multiplication overflows
          And [
            Leq (sqLowerBound, Var res)
            Leq (Var res, sqUpperBound)
          ]
        ]
      }
    {
      id = "sizeRange"
      prms = [offset; from; tto]
      specs = [Precond rangeVarsConds; Precond offsetCond; Measure measure]
      annots = []
      postcond = Some (res, postcondRange)
      returnTpe = IntegerType Long
      body = sizeRes.resSize
    }

  let sizeLemmas (align: AcnAlignment option): FunDef =
    let elemSizeAssert (elemSizeVar: Var): Expr =
      if sq.child.Kind.acnMinSizeInBits = sq.child.Kind.acnMaxSizeInBits then
        Assert (Equals (Var elemSizeVar, longlit sq.child.Kind.acnMinSizeInBits))
      else
        Assert (And [
          Leq (longlit sq.child.Kind.acnMinSizeInBits, Var elemSizeVar)
          Leq (Var elemSizeVar, longlit sq.child.Kind.acnMaxSizeInBits)
        ])

    let template = sizeLemmaTemplate sq.acnMaxSizeInBits align
    let tmplOffset = template.prms.[0]
    let tmplOtherOffset = template.prms.[1]
    // All related to the inner recursive proof function
    let proofId = "proof"
    let offset = {Var.name = "offset"; tpe = IntegerType Long}
    let otherOffset = {Var.name = "otherOffset"; tpe = IntegerType Long}
    let from = {name = "from"; tpe = IntegerType Int}
    let tto = {name = "to"; tpe = IntegerType Int}
    let additionalPrecond =
      match align with
      | None -> []
      | Some align ->
        let modOffset = Mod (Var offset, longlit align.nbBits)
        let modOtherOffset = Mod (Var otherOffset, longlit align.nbBits)
        [Precond (Equals (modOffset, modOtherOffset))]
    let postcond =
      Equals (
        sizeRange This (Var offset) (Var from) (Var tto),
        sizeRange This (Var otherOffset) (Var from) (Var tto)
      )
    let elemSel = ArraySelect (FieldSelect (This, "arr"), Var from)
    let elemSizeOffVar = {Var.name = "elemSizeOff"; tpe = IntegerType Long}
    let elemSizeOtherOffVar = {Var.name = "elemSizeOtherOff"; tpe = IntegerType Long}
    let elemSizeOffRes = asn1SizeExpr align sq.child.Kind elemSel (Var offset) 0I 0I
    let elemSizeOtherOffRes = asn1SizeExpr align sq.child.Kind elemSel (Var otherOffset) 0I 0I
    let elemSizesBdgs =
      elemSizeOffRes.bdgs @
      [(elemSizeOffVar, elemSizeOffRes.resSize)] @
      elemSizeOtherOffRes.bdgs @
      [(elemSizeOtherOffVar, elemSizeOtherOffRes.resSize)]
    let elemLemmaCall = sizeLemmaCall sq.child.Kind align elemSel (Var offset) (Var otherOffset)
    let inductiveStep = ApplyLetRec {
      id = proofId
      args = [
        plus [Var offset; Var elemSizeOffVar]
        plus [Var otherOffset; Var elemSizeOtherOffVar]
        plus [Var from; int32lit 1I]
        Var tto
      ]
    }
    let proofElsePart = mkBlock ([
      elemSizeAssert elemSizeOffVar
      elemSizeAssert elemSizeOtherOffVar
      Assert inv
    ] @ (elemLemmaCall |> Option.toList) @ [inductiveStep])
    let proofElsePart = letsIn elemSizesBdgs proofElsePart
    let proofBody =
      IfExpr {
        cond = Equals (Var from, Var tto)
        thn = UnitLit
        els = proofElsePart
      }
    let proofSpecs =
      [
        Precond (rangeVarsCondHelper from tto)
        Precond (offsetCondHelper offset from tto)
        Precond (offsetCondHelper otherOffset from tto)
      ] @ additionalPrecond @ [Measure (Minus (Var tto, Var from))]
    let proofFd = {
      id = proofId
      prms = [offset; otherOffset; from; tto]
      annots = [GhostAnnot; Opaque; InlineOnce]
      specs = proofSpecs
      postcond = Some ({name = "_"; tpe = UnitType}, postcond)
      returnTpe = UnitType
      body = proofBody
    }
    let proofCall = ApplyLetRec {id = proofId; args = [Var tmplOffset; Var tmplOtherOffset; int32lit 0I; count]}
    {template with body = LetRec {fds = [proofFd]; body = proofCall}}

  let sizeField =
    match sq.acnEncodingClass with
    | SZ_EC_LENGTH_EMBEDDED sz -> sz
    | _ -> 0I // TODO: Pattern?
  let postcond =
    if sq.acnMinSizeInBits = sq.acnMaxSizeInBits then Equals (Var res, longlit sq.acnMaxSizeInBits)
    else And [Leq (longlit sq.acnMinSizeInBits, Var res); Leq (Var res, longlit sq.acnMaxSizeInBits)]
  let finalSize = plus [longlit sizeField; sizeRange This (Var offset) (int32lit 0I) count]
  let sizeFd = {
    id = "size"
    prms = [offset]
    specs = [Precond (offsetConds offset sq.acnMaxSizeInBits)]
    annots = []
    postcond = Some (res, postcond)
    returnTpe = IntegerType Long
    body = finalSize
  }
  let maxAlign = maxAlignment t
  let implyingAligns = implyingAlignments maxAlign
  let lemmas = implyingAligns |> List.map sizeLemmas
  sizeRangeFd :: sizeFd :: lemmas


let generateSequenceSizeDefinitions (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (children: DAst.SeqChildInfo list): string list =
  let fds = seqSizeFunDefs t sq
  fds |> List.map (fun fd -> show (FunDefTree fd))

let generateChoiceSizeDefinitions (t: Asn1AcnAst.Asn1Type) (choice: Asn1AcnAst.Choice) (children: DAst.ChChildInfo list): string list =
  let fds = choiceSizeFunDefs t choice
  fds |> List.map (fun fd -> show (FunDefTree fd))

let generateSequenceOfSizeDefinitions (t: Asn1AcnAst.Asn1Type) (sqf: Asn1AcnAst.SequenceOf) (elemTpe: DAst.Asn1Type): string list =
  let fds = seqOfSizeFunDefs t sqf elemTpe
  fds |> List.map (fun fd -> show (FunDefTree fd))

let generatePostcondExpr (t: Asn1AcnAst.Asn1Type) (pVal: Selection) (res: Var) (codec: Codec): Expr =
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = Old (Var cdc)
  let tpe = fromAsn1TypeKind t.Kind
  let lftId, rgtId, buf, szRecv =
    match codec with
    | Encode -> leftId, rightId, Equals (selBufLength oldCdc, selBufLength (Var cdc)), {Var.name = pVal.asLastOrSelf.receiverId; tpe = tpe}
    | Decode -> leftMutId, rightMutId, Equals (selBuf oldCdc, selBuf (Var cdc)), res
  let sz =
    match t.Kind with
    | Choice _ | Sequence _ | SequenceOf _ ->
      // Note that we don't have a "ReferenceType" in such cases, so we have to explicitly call `size` and not rely on asn1SizeExpr...
      {bdgs = []; resSize = callSize (Var szRecv) (bitIndex oldCdc)}
    | _ -> asn1SizeExpr t.acnAlignment t.Kind (Var szRecv) (bitIndex oldCdc) 0I 0I
  let rightBody = And [
    buf
    Equals (bitIndex (Var cdc), plus [bitIndex oldCdc; sz.resSize])
  ]
  let rightBody = letsIn sz.bdgs rightBody
  MatchExpr (eitherGenMatch lftId rgtId (Var res) None (BoolLit true) (Some res) rightBody)

let wrapAcnFuncBody (isValidFuncName: string option)
                    (t: Asn1AcnAst.Asn1Type)
                    (body: string)
                    (codec: Codec)
                    (nestingScope: NestingScope)
                    (outerSel: CallerScope)
                    (recSel: CallerScope): FunDef * Expr =
  assert recSel.arg.path.IsEmpty
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let tpe = fromAsn1TypeKind t.Kind
  let errTpe = IntegerType Int
  let recPVal = {Var.name = recSel.arg.receiverId; tpe = tpe}
  let precond = [Precond (validateOffsetBits (Var cdc) (longlit t.acnMaxSizeInBits))]
  match codec with
  | Encode ->
    let retTpe = IntegerType Int
    let outerPVal = SelectionExpr (joinedSelection outerSel.arg)
    let cstrCheck =
      isValidFuncName |> Option.map (fun validFnName ->
        let scrut = FunctionCall {prefix = []; id = validFnName; args = [Var recPVal]}
        let leftBdg = {Var.name = "l"; tpe = errTpe}
        let leftBody = Return (leftExpr errTpe retTpe (Var leftBdg))
        eitherMatchExpr scrut (Some leftBdg) leftBody None (mkBlock [])
      ) |> Option.toList

    let body = mkBlock (
      cstrCheck @
      [
        EncDec body
        ClassCtor (right errTpe retTpe (int32lit 0I))
      ]
    )

    let resPostcond = {Var.name = "res"; tpe = ClassType {id = eitherId; tps = [errTpe; IntegerType Int]}}
    let postcondExpr = generatePostcondExpr t recSel.arg resPostcond codec
    let fd = {
      id = $"encode_{outerSel.arg.asIdentifier}"
      prms = [cdc; recPVal]
      specs = precond
      annots = [Opaque; InlineOnce]
      postcond = Some (resPostcond, postcondExpr)
      returnTpe = ClassType (eitherTpe errTpe retTpe)
      body = body
    }
    let call =
      let scrut = FunctionCall {prefix = []; id = fd.id; args = [Var cdc; FreshCopy outerPVal]} // TODO: Ideally we should not be needing a freshCopy...
      let leftBdg = {Var.name = "l"; tpe = errTpe}
      let leftBody = Return (leftExpr errTpe (IntegerType Int) (Var leftBdg))
      eitherMatchExpr scrut (Some leftBdg) leftBody None UnitLit
    fd, call
  | Decode ->
    let acns = collectAllAcnChildren t.Kind
    let acnsVars = acns |> List.map (fun c -> {Var.name = getAcnDeterminantName c.id; tpe = fromAcnInsertedType c.Type})
    let acnTps = acnsVars |> List.map (fun v -> v.tpe)
    let retTpe = tupleType (tpe :: acnTps)
    let outerPVal = {Var.name = outerSel.arg.asIdentifier; tpe = tpe}
    let retInnerFd =
      let retVal = mkTuple (Var recPVal :: (acnsVars |> List.map Var))
      match isValidFuncName with
      | Some validFnName ->
        let scrut = FunctionCall {prefix = []; id = validFnName; args = [Var recPVal]}
        let leftBdg = {Var.name = "l"; tpe = errTpe}
        let leftBody = leftMutExpr errTpe retTpe (Var leftBdg)
        let rightBody = rightMutExpr errTpe retTpe retVal
        eitherMatchExpr scrut (Some leftBdg) leftBody None rightBody
      | None -> rightMutExpr errTpe retTpe retVal
    let body = mkBlock [EncDec body; retInnerFd]

    let resPostcond = {Var.name = "res"; tpe = ClassType {id = eitherMutId; tps = [errTpe; retTpe]}}
    let postcondExpr =
      if acns.IsEmpty then
        generatePostcondExpr t recSel.arg resPostcond codec
      else
        assert (match t.Kind with Sequence _ -> true | _ -> false)
        let resvalVar = {Var.name = "resVal"; tpe = tpe}
        let codecTpe = runtimeCodecTypeFor ACN
        let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
        let oldCdc = Old (Var cdc)
        let sz = callSize (Var resvalVar) (bitIndex oldCdc)
        let rightBody = And [
          Equals (selBuf oldCdc, selBuf (Var cdc))
          Equals (bitIndex (Var cdc), plus [bitIndex oldCdc; sz])
        ]
        MatchExpr {
          scrut = Var resPostcond
          cases = [
            {
              pattern = ADTPattern {binder = None; id = leftMutId; subPatterns = [Wildcard None]}
              rhs = BoolLit true
            }
            {
              pattern = ADTPattern {
                binder = None
                id = rightMutId
                subPatterns = [TuplePattern {
                  binder = None
                  subPatterns = Wildcard (Some resvalVar) :: (List.replicate acns.Length (Wildcard None))
                }]
              }
              rhs = rightBody
            }
          ]
        }

    let fd = {
      id = $"decode_{outerSel.arg.asIdentifier}"
      prms = [cdc]
      specs = precond
      annots = [Opaque; InlineOnce]
      postcond = Some (resPostcond, postcondExpr)
      returnTpe = ClassType (eitherMutTpe errTpe retTpe)
      body = body
    }
    let call =
      let scrut = FunctionCall {prefix = []; id = fd.id; args = [Var cdc]}
      let leftBdg = {Var.name = "l"; tpe = errTpe}
      // TODO: FIXME: the right type must be the outside type!!!
      let leftHACK = ClassCtor {ct = {id = leftMutId; tps = []}; args = [Var leftBdg]}
      let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
      let rightBdg = {Var.name = "v"; tpe = tpe}
      let rightBody = Var rightBdg
      eitherMutMatchExpr scrut (Some leftBdg) leftBody (Some rightBdg) rightBody
    // The rest of the backend expects a `val outerPVal = result`
    // Note: we cannot use tuple destructuring because the `acnsVars` may start with a capital letter, which is interpreted as a type
    let ret =
      if acnsVars.IsEmpty then Let {bdg = outerPVal; e = call; body = mkBlock []}
      else
        let tplVar = {Var.name = outerPVal.name + "_tuple"; tpe = retTpe}
        let bdgs = (tplVar, call) :: ((outerPVal :: acnsVars) |> List.mapi (fun i v -> v, TupleSelect (Var tplVar, i + 1)))
        letsIn bdgs (mkBlock [])
    fd, ret


let annotateSequenceChildStmt (enc: Asn1Encoding) (snapshots: Var list) (cdc: Var) (oldCdc: Var) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec) (rest: Expr): Expr =
  let nbChildren = pg.children.Length
  assert (snapshots.Length = nbChildren)
  assert (stmts.Length = nbChildren)

  let rec assertionsConditions (tpe: TypeEncodingKind option): Expr option =
    match tpe with
    | Some (OptionEncodingType tpe) -> assertionsConditions (Some tpe)
    | Some (Asn1IntegerEncodingType (Some encodingTpe)) ->
        match encodingTpe with
        | FullyConstrainedPositive (min, max) | FullyConstrained (min, max) ->
            // TODO: The RT library does not add 1, why?
            let call = getBitCountUnsigned (ulonglit (max - min))
            // TODO: Case min = max?
            let nBits = if max = min then 0I else bigint (ceil ((log (double (max - min))) / (log 2.0)))
            let cond = Equals (call, int32lit nBits)
            Some cond
        | _ -> None
      | _ -> None

  let addAssert (tpe: TypeEncodingKind option): Expr =
    assertionsConditions tpe |> Option.map (fun cond -> Assert cond)
                             |> Option.defaultValue (mkBlock [])

  let outerMaxSize = pg.outerMaxSize enc
  let thisMaxSize = pg.maxSize enc
  let fstSnap = snapshots.Head
  let isNested = pg.nestingLevel > 0I
  assert (isNested || fstSnap = oldCdc)

  //let sizeResVars = [0 .. pg.children.Length - 1] |> List.map (fun i -> {Var.name = $"size_{pg.nestingIx + bigint i}"; tpe = IntegerType Long})
  let sizeRess =
    pg.children |>
    List.indexed |>
    // TODO: if acc not needed, turn fold into map
    List.fold (fun acc (ix, c) ->
      let childVar = {Var.name = $"size_{pg.nestingIx + bigint ix}"; tpe = IntegerType Long}
      match c.info with
      | Some info ->
        //let previousSizes = acc |> List.map (fun res -> Var res.childVar)
        //let overallOffset = plus (bitIndex (Var snapshots.[ix]) :: previousSizes)
        let recv =
          match codec with
          | Encode -> SelectionExpr (joinedSelection c.sel.Value)
          | Decode -> SelectionExpr c.sel.Value.asIdentifier
        let resSize = seqSizeExprHelperChild info (bigint ix) (Some recv) (bitIndex (Var snapshots.[ix])) pg.nestingLevel pg.nestingIx
        acc @ [{extraBdgs = resSize.bdgs; childVar = childVar; childSize = resSize.resSize}]
      | None ->
        // presence bits
        acc @ [{extraBdgs = []; childVar = childVar; childSize = longlit 1I}]
    ) []

  let annotatePostPreciseSize (ix: int) (snap: Var) (child: SequenceChildProps): Expr =
    let previousSizes = sizeRess |> List.take ix |> List.map (fun res -> Var res.childVar)
    let sizeRes = sizeRess.[ix]
    let chk = Check (Equals (bitIndex (Var cdc), plus (bitIndex (Var oldCdc) :: previousSizes @ [Var sizeRes.childVar])))
    letsGhostIn sizeRes.allBindings (Ghost chk)

  let annotatePost (ix: int) (snap: Var) (child: SequenceChildProps) (stmt: string option) (offsetAcc: bigint): Expr =
    let sz = child.typeInfo.maxSize enc
    let relativeOffset = offsetAcc - (pg.maxOffset enc)
    let offsetCheckOverall = Check (Leq (bitIndex (Var cdc), plus [bitIndex (Var oldCdc); (longlit offsetAcc)]))
    let offsetCheckNested =
      if isNested then [Check (Leq (bitIndex (Var cdc), plus [bitIndex (Var fstSnap); longlit relativeOffset]))]
      else []
    let bufCheck =
      match codec with
       | Encode -> [Check ((Equals (selBufLength (Var cdc), selBufLength (Var oldCdc))))]
       | Decode -> [Check ((Equals (selBuf (Var cdc), selBuf (Var oldCdc))))]
    let offsetWidening =
      match pg.siblingMaxSize enc with
      | Some siblingMaxSize when ix = nbChildren - 1 && siblingMaxSize <> thisMaxSize ->
        let diff = siblingMaxSize - thisMaxSize
        [
          Check (Leq (bitIndex (Var cdc), plus [bitIndex (Var oldCdc); longlit (offsetAcc + diff)]))
          Check (Leq (bitIndex (Var cdc), plus [bitIndex (Var fstSnap); longlit (relativeOffset + diff)]))
        ]
      | _ -> []
    let checks = offsetCheckOverall :: offsetCheckNested @ bufCheck @ offsetWidening
    let validateOffsetLemma  =
      if stmt.IsSome && ix < nbChildren - 1 then
        [validateOffsetBitsIneqLemma (selBitStream (Var snap)) (selBitStream (Var cdc)) (longlit (outerMaxSize - offsetAcc + sz)) (longlit sz)]
      else []
    let preciseSize = annotatePostPreciseSize ix snap child
    mkBlock [Ghost (mkBlock (validateOffsetLemma @ checks)); preciseSize]

  let annotate (ix: int, (snap: Var, child: SequenceChildProps, stmt: string option)) (offsetAcc: bigint, rest: Expr): bigint * Expr =
    let sz = child.typeInfo.maxSize enc
    //assert (thisMaxSize <= (pg.siblingMaxSize enc |> Option.defaultValue thisMaxSize)) // TODO: Somehow does not always hold with UPER?
    let preAnnots =
      if stmt.IsSome then [addAssert child.typeInfo.typeKind]
      else []
    let postAnnots = annotatePost ix snap child stmt offsetAcc
    let encDec = stmt |> Option.map EncDec |> Option.toList
    let body = mkBlock (preAnnots @ encDec @ [postAnnots; rest])
    offsetAcc - sz, LetGhost {bdg = snap; e = Snapshot (Var cdc); body = body}

  let stmts = List.zip3 snapshots pg.children stmts |> List.indexed
  List.foldBack annotate stmts ((pg.maxOffset enc) + thisMaxSize, rest) |> snd

let generateSequenceChildProof (enc: Asn1Encoding) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec): string list =
  if stmts.IsEmpty then []
  else
    let codecTpe = runtimeCodecTypeFor enc
    let cdc = {Var.name = $"codec"; tpe = ClassType codecTpe}
    let oldCdc = {Var.name = $"codec_0_1"; tpe = ClassType codecTpe}
    let snapshots = [1 .. pg.children.Length] |> List.map (fun i -> {Var.name = $"codec_{pg.nestingLevel}_{pg.nestingIx + bigint i}"; tpe = ClassType codecTpe})
    let wrappedStmts = annotateSequenceChildStmt enc snapshots cdc oldCdc stmts pg codec

    let postCondLemmas =
      let cond = Leq (bitIndex (Var cdc), plus [bitIndex (Var snapshots.Head); longlit (pg.outerMaxSize enc)])
      Ghost (Check cond)
    let expr = wrappedStmts (mkBlock [postCondLemmas])
    let exprStr = show (ExprTree expr)
    [exprStr]

let generateSequenceOfLikeProof (enc: Asn1Encoding) (sqf: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): SequenceOfLikeProofGenResult option =
  None
  (*
  let lvl = max 0I (pg.nestingLevel - 1I)
  let nestingIx = pg.nestingIx + 1I

  let nbItemsMin, nbItemsMax = sqf.nbElems enc

  let accountForSize =
    match enc, sqf with
    | UPER, _ -> true
    | ACN, SqOf sqf ->
      match sqf.acnEncodingClass with
      | SZ_EC_FIXED_SIZE | SZ_EC_LENGTH_EMBEDDED _ -> not sqf.isFixedSize // TODO: Check if we can have SZ_EC_FIXED_SIZE with not sqf.isFixedSize (copying logic from DAstACN)
      | SZ_EC_ExternalField _ -> false // The external field is encoded/decoded as an ACN field, it therefore has the bitstream index offset already taken care of
      | _ -> true
    | ACN, StrType str ->
      true // TODO
    | _ -> failwith $"Unexpected encoding: {enc}"

  let sizeInBits =
    if accountForSize then GetNumberOfBitsForNonNegativeInteger (nbItemsMax - nbItemsMin)
    else 0I
  let nbItems =
    if sqf.isFixedSize then int32lit nbItemsMin
    else FieldSelect (SelectionExpr pg.sel, "nCount")
  let elemSz = sqf.maxElemSizeInBits enc
  let elemSzExpr = longlit elemSz
  let sqfMaxSizeInBits = sqf.maxSizeInBits enc
  let offset = pg.maxOffset enc
  let remainingBits = pg.outerMaxSize enc - sqfMaxSizeInBits - offset
  let remainingBitsExpr = longlit remainingBits

  let codecTpe = runtimeCodecTypeFor enc
  let cdc = {Var.name = $"codec"; tpe = ClassType codecTpe}
  // The codec snapshot before encoding/decoding the whole SequenceOf (i.e. snapshot before entering the while loop)
  let cdcSnap = {Var.name = $"codec_{lvl}_{nestingIx}"; tpe = ClassType codecTpe}
  // The codec snapshot before encoding/decoding one item (snapshot local to the loop, taken before enc/dec one item)
  let cdcLoopSnap = {Var.name = $"codecLoop_{lvl}_{nestingIx}"; tpe = ClassType codecTpe}
  let oldCdc = {Var.name = $"codec_0_1"; tpe = ClassType codecTpe}
  let ix = {name = pg.ixVariable; tpe = IntegerType Int}
  let ixPlusOne = plus [Var ix; int32lit 1I]

  let preSerde =
    LetGhost ({
      bdg = cdcLoopSnap
      e = Snapshot (Var cdc)
      body = Ghost (validateOffsetBitsWeakeningLemma (selBitStream (Var cdc)) (plus [remainingBitsExpr; Mult (elemSzExpr, Minus (nbItems, Var ix))]) elemSzExpr)
    })
  let postSerde =
    Ghost (mkBlock [
      Check (Equals (
        Mult (elemSzExpr, plus [Var ix; int32lit 1I]),
        plus [Mult (elemSzExpr, Var ix); elemSzExpr]
      ))
      Check (Leq (
        bitIndex (Var cdc),
        plus [bitIndex (Var cdcSnap); longlit sizeInBits; Mult (elemSzExpr, ixPlusOne)]
      ))
      validateOffsetBitsIneqLemma (selBitStream (Var cdcLoopSnap)) (selBitStream (Var cdc)) (plus [remainingBitsExpr; Mult (elemSzExpr, Minus (nbItems, Var ix))]) elemSzExpr
      Check (validateOffsetBits (Var cdc) (plus [remainingBitsExpr; Mult (elemSzExpr, Minus (nbItems, ixPlusOne))]))
    ])
  let invariants =
    let bufInv =
      if codec = Encode then
        Equals (selBufLength (Var cdc), selBufLength (Var cdcSnap))
      else
        Equals (selBuf (Var cdc), selBuf (Var cdcSnap))
    let cdcInv = invariant (Var cdc)
    let boundsInv =
      if sqf.isFixedSize then []
      else [And [Leq (int32lit nbItemsMin, nbItems); Leq (nbItems, int32lit nbItemsMax)]]
    let bixInv = Leq (
      bitIndex (Var cdc),
      plus [bitIndex (Var cdcSnap); longlit sizeInBits; Mult (elemSzExpr, Var ix)]
    )
    let bixInvOldCdc = Leq (
      bitIndex (Var cdc),
      plus [bitIndex (Var oldCdc); longlit (offset + sizeInBits); Mult (elemSzExpr, Var ix)]
    )
    let offsetInv = validateOffsetBits (Var cdc) (plus [remainingBitsExpr; Mult (elemSzExpr, Minus (nbItems, Var ix))])
    [bufInv; cdcInv] @ boundsInv @ [bixInv; bixInvOldCdc; offsetInv]

  let postInc =
    Ghost (mkBlock (
      Check (And [
        Leq (int32lit 0I, Var ix)
        Leq (Var ix, nbItems)
      ]) :: (invariants |> List.map Check)))

  Some {
    preSerde = show (ExprTree preSerde)
    postSerde = show (ExprTree postSerde)
    postInc = show (ExprTree postInc)
    invariant = show (ExprTree (SplitAnd invariants))
  }
  *)

// TODO: Also for strings...
let generateSequenceOfLikeAuxiliaries (enc: Asn1Encoding) (sqf: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): FunDef list * Expr =
  let sqfTpe = fromSequenceOfLike sqf
  let codecTpe = runtimeCodecTypeFor enc
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let fstCdc = {Var.name = "codec_0_1"; tpe = ClassType codecTpe}
  let cdcBeforeLoop = {Var.name = "codecBeforeLoop"; tpe = ClassType codecTpe}
  let cdcSnap1 = {Var.name = "codecSnap1"; tpe = ClassType codecTpe}
  let from = {Var.name = "from"; tpe = IntegerType Int}
  let sqfVar = {Var.name = pg.cs.arg.lastId; tpe = sqfTpe}
  let td =
    match sqf with
    | SqOf sqf -> sqf.typeDef.[Scala].typeName
    | StrType str -> str.typeDef.[Scala].typeName
  let fnid =
    let prefix = pg.nestingScope.parents |> List.tryHead |> Option.map (fun (cs, _) -> $"{cs.arg.asIdentifier}_") |> Option.defaultValue ""
    match codec with
    | Encode -> $"{ToC pg.cs.modName}_{td}_{prefix}{pg.cs.arg.lastId}_encode_loop"
    | Decode -> $"{ToC pg.cs.modName}_{td}_{prefix}{pg.cs.arg.lastId}_decode_loop"
  let nbItemsMin, nbItemsMax = sqf.nbElems enc
  let nbItems =
    if sqf.isFixedSize then int32lit nbItemsMin
    else FieldSelect (Var sqfVar, "nCount")
  let maxElemSz = sqf.maxElemSizeInBits enc

  let fromBounds = And [Leq (int32lit 0I, Var from); Leq (Var from, nbItems)]
  let nbItemsBounds =
    if sqf.isFixedSize then None
    else Some (And [Leq (int32lit nbItemsMin, Var from); Leq (Var from, int32lit nbItemsMax)])
  let validateOffset =
    validateOffsetBits (Var cdc) (Mult (longlit maxElemSz, Minus (nbItems, Var from)))
  let decreasesExpr = Minus (nbItems, Var from)

  let encDec = pg.encDec |> Option.map EncDec |> Option.toList

  let preSerde = Ghost (validateOffsetBitsWeakeningLemma (selBitStream (Var cdc)) (Mult (longlit maxElemSz, Minus (nbItems, Var from))) (longlit maxElemSz))
  let postSerde =
    Ghost (mkBlock [
      Check (Equals (
        Mult (longlit maxElemSz, plus [Var from; int32lit 1I]),
        plus [Mult (longlit maxElemSz, Var from); longlit maxElemSz]
      ))
      validateOffsetBitsIneqLemma (selBitStream (Var cdcSnap1)) (selBitStream (Var cdc)) (Mult (longlit maxElemSz, Minus (nbItems, Var from))) (longlit maxElemSz)
      Check (validateOffsetBits (Var cdc) (Mult (longlit maxElemSz, Minus (nbItems, plus [Var from; int32lit 1I]))))
    ])
  let reccall = FunctionCall {prefix = []; id = fnid; args = [Var cdc; Var sqfVar; plus [Var from; int32lit 1I]]}
  // TODO: ALIGNMENT
  let sizeLemmaCall = MethodCall {recv = Var sqfVar; id = sizeLemmaId None; args = [bitIndex (Var cdcBeforeLoop); bitIndex (Var fstCdc)]}

  match codec with
  | Encode ->
    let fnRetTpe = ClassType (eitherTpe (IntegerType Int) (IntegerType Int))
    let elseBody = LetGhost {
      bdg = cdcSnap1
      e = Snapshot (Var cdc)
      body = mkBlock (
        preSerde ::
        encDec @
        [postSerde; reccall]
      )
    }
    let body = IfExpr {
      cond = Equals (Var from, nbItems)
      thn = rightExpr (IntegerType Int) (IntegerType Int) (int32lit 0I)
      els = elseBody
    }
    let postcondRes = {Var.name = "res"; tpe = fnRetTpe}
    let postcond =
      let oldCdc = Old (Var cdc)
      let sz = sizeRange (Var sqfVar) (bitIndex oldCdc) (Var from) nbItems
      let rightBody = And [
        Equals (selBufLength oldCdc, selBufLength (Var cdc))
        Equals (bitIndex (Var cdc), plus [bitIndex oldCdc; sz])
      ]
      eitherMatchExpr (Var postcondRes) None (BoolLit true) (Some postcondRes) rightBody
    let fd = {
      FunDef.id = fnid
      prms = [cdc; sqfVar; from]
      annots = [Opaque; InlineOnce]
      specs = Precond fromBounds :: (nbItemsBounds |> Option.map Precond |> Option.toList) @ [Precond validateOffset; Measure decreasesExpr]
      postcond = Some (postcondRes, postcond)
      returnTpe = fnRetTpe
      body = body
    }

    let call =
      let scrut = FunctionCall {prefix = []; id = fnid; args = [Var cdc; Var sqfVar; int32lit 0I]}
      let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
      let leftBody = Return (leftExpr (IntegerType Int) (IntegerType Int) (Var leftBdg))
      let rightBody = Ghost (sizeLemmaCall)
      eitherMatchExpr scrut (Some leftBdg) leftBody None rightBody
    let call = letsGhostIn [cdcBeforeLoop, Snapshot (Var cdc)] call
    [fd], call
  | Decode ->
    let fnRetTpe = ClassType (eitherMutTpe (IntegerType Int) UnitType)
    let cdcSnap2 = {Var.name = "codecSnap2"; tpe = ClassType codecTpe}
    let elemTpe = fromSequenceOfLikeElemTpe sqf
    let arr1 = {Var.name = "arr1"; tpe = ArrayType {tpe = elemTpe}}
    let arr2 = {Var.name = "arr2"; tpe = ArrayType {tpe = elemTpe}}

    let sqfSelArr = FieldSelect (Var sqfVar, "arr")
    let oldSqfSelArr = FieldSelect (Old (Var sqfVar), "arr")

    let thnCase = mkBlock [
      Ghost (mkBlock [
        arrayRangesEqReflexiveLemma sqfSelArr
        arrayRangesEqSlicedLemma sqfSelArr (FieldSelect (Snapshot (Var sqfVar), "arr")) (int32lit 0I) (ArrayLength sqfSelArr) (int32lit 0I) (Var from)
      ])
      rightMutExpr (IntegerType Int) UnitType UnitLit
    ]

    let elseCase =
      let reccallRes = {Var.name = "res"; tpe = fnRetTpe}
      // TODO: Hack
      let decodedElemVar = {Var.name = $"{pg.cs.arg.asIdentifier}_arr_from_"; tpe = elemTpe}
      let updateArr =
        if encDec.IsEmpty then []
        else [ArrayUpdate (sqfSelArr, Var from, FreshCopy (Var decodedElemVar))]
      let postrecProofSuccess = mkBlock [
        arrayUpdatedAtPrefixLemma (Var arr1) (Var from) (Var decodedElemVar)
        arrayRangesEqTransitive (Var arr1) (Var arr2) sqfSelArr (int32lit 0I) (Var from) (plus [Var from; int32lit 1I])
        Check (arrayRangesEq (Var arr1) sqfSelArr (int32lit 0I) (Var from))
        arrayRangesEqImpliesEq (Var arr2) sqfSelArr (int32lit 0I) (Var from) (plus [Var from; int32lit 1I])
        // TODO: ALIGNMENT
        MethodCall {recv = Var sqfVar; id = sizeLemmaId None; args = [bitIndex (Var cdcSnap1); bitIndex (Var cdcSnap2)]}
        Check (Equals (bitIndex (Var cdc), plus [bitIndex (Var cdcSnap1); sizeRange (Var sqfVar) (bitIndex (Var cdcSnap1)) (Var from) nbItems]))
      ]
      let postrecProof = Ghost (eitherMutMatchExpr (Var reccallRes) None UnitLit None postrecProofSuccess)
      (letsGhostIn [arr1, Snapshot sqfSelArr] (
      mkBlock ((preSerde :: encDec @ updateArr) @ [
      letsGhostIn [cdcSnap2, Snapshot (Var cdc); arr2, Snapshot sqfSelArr] (
      mkBlock [
          postSerde
          letsIn [reccallRes, reccall] (mkBlock [postrecProof; Var reccallRes])
      ])])))

    let ite = IfExpr {
      cond = Equals (Var from, nbItems)
      thn = thnCase
      els = elseCase
    }
    let body = letsGhostIn [cdcSnap1, Snapshot (Var cdc)] ite

    let postcondRes = {Var.name = "res"; tpe = fnRetTpe}
    let postcond =
      let oldCdc = Old (Var cdc)
      let sz = sizeRange (Var sqfVar) (bitIndex oldCdc) (Var from) nbItems
      let ncountCond =
        if sqf.isFixedSize then []
        else [Equals (FieldSelect (Old (Var sqfVar), "nCount"), nbItems)]
      let decodeIntoArrayCond =
        match pg.elemDecodeFn with
        | None -> []
        | Some decodeFn ->
          let decodePure = TupleSelect (FunctionCall {prefix = []; id = $"{decodeFn}_pure"; args = [oldCdc]}, 2)
          [Or [
            Equals (Var from, nbItems)
            Equals (
              rightMutExpr (IntegerType Int) UnitType (ArraySelect ((Var sqfVar), Var from)),
              decodePure
            )
          ]]
      let rightBody = And ([
        Equals (selBufLength oldCdc, selBufLength (Var cdc))
        Equals (ArrayLength oldSqfSelArr, ArrayLength sqfSelArr)
      ] @ ncountCond @
        [arrayRangesEq oldSqfSelArr sqfSelArr (int32lit 0I) (Var from)] @
        decodeIntoArrayCond @
        [Equals (bitIndex (Var cdc), plus [bitIndex oldCdc; sz])])
      eitherMutMatchExpr (Var postcondRes) None (BoolLit true) None rightBody

    let fd = {
      FunDef.id = fnid
      prms = [cdc; sqfVar; from]
      annots = [Opaque; InlineOnce]
      specs = Precond fromBounds :: (nbItemsBounds |> Option.map Precond |> Option.toList) @ [Precond validateOffset; Measure decreasesExpr]
      postcond = Some (postcondRes, postcond)
      returnTpe = fnRetTpe
      body = body
    }
    let call =
      let scrut = FunctionCall {prefix = []; id = fnid; args = [Var cdc; Var sqfVar; int32lit 0I]}
      let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
      let leftBody = Return (leftMutExpr (IntegerType Int) sqfTpe (Var leftBdg))
      let rightBody = Ghost (sizeLemmaCall)
      eitherMutMatchExpr scrut (Some leftBdg) leftBody None rightBody
    let call = letsGhostIn [cdcBeforeLoop, Snapshot (Var cdc)] call
    [fd], call