module ProofGen
open ProofAst
open DAst
open FsUtils
open CommonTypes
open Language
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open AcnGenericTypes
open System.Numerics

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
    | Full ->
      plus [longlit 8I; Mult (longlit 8I, getLengthForEncodingSigned obj)]
    | NegInf _ | PosInf _ -> getBitCountUnsigned obj
    | Concrete _ ->
      assert (int.acnMinSizeInBits = int.acnMaxSizeInBits)
      assert (int.uperMinSizeInBits = int.uperMinSizeInBits)
      assert (int.acnMaxSizeInBits = int.uperMaxSizeInBits)
      longlit int.acnMaxSizeInBits
  | _ ->
    assert (int.acnMinSizeInBits = int.acnMaxSizeInBits) // TODO: Not quite true, there is ASCII encoding that is variable...
    longlit int.acnMaxSizeInBits

// TODO: Expliquer ce que cela fait et diff avec les autre
let acnChildren (tpe: Asn1AcnAst.Asn1TypeKind): Asn1AcnAst.AcnChild list =
  match tpe.ActualType with
  | Sequence sq ->
    sq.children |> List.collect (fun c ->
      match c with
      | AcnChild c -> [c]
      | Asn1Child _ -> []
    )
  | _ -> []

let rec collectNestedAcnChildren (tpe: Asn1AcnAst.Asn1TypeKind): Asn1AcnAst.AcnChild list =
  match tpe.ActualType with
  | Sequence sq ->
    sq.children |> List.collect (fun c ->
      match c with
      | AcnChild c -> [c]
      | Asn1Child c -> collectNestedAcnChildren c.Type.Kind
    )
  | _ -> []

let rec collectAllAcnChildren (tpe: Asn1AcnAst.Asn1TypeKind): Asn1AcnAst.AcnChild list =
  match tpe.ActualType with
  | Sequence sq ->
    sq.children |> List.collect (fun c ->
      match c with
      | AcnChild c -> [c]
      | Asn1Child c -> collectAllAcnChildren c.Type.Kind
    )
  | Choice ch -> ch.children |> List.collect (fun c -> collectAllAcnChildren c.Type.Kind)
  | SequenceOf sqf -> collectAllAcnChildren sqf.child.Kind
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
(*
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
*)
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
  sizeLemmaIdForType tp align |> Option.map (fun id -> MethodCall {recv = recv; id = id; args = [offset; otherOffset]; parameterless = true})

let stringInvariants (minSize: bigint) (maxSize: bigint) (recv: Expr): Expr =
  // TODO: If minSize = maxSize, we can still have '\0' before `maxSize`, right?
  // TODO: Can we have an `\0` before `minSize` as well?
  let arrayLen = ArrayLength recv
  let nullCharIx = indexOfOrLength recv (IntLit (UByte, 0I))
  And [Equals (int32lit (maxSize + 1I), arrayLen); Leq (nullCharIx, int32lit maxSize)]
  (*
  if minSize = maxSize then And [Leq (int32lit (maxSize + 1I), arrayLen); Equals (nullCharIx, int32lit maxSize)]
  else
    And [Leq (int32lit (maxSize + 1I), arrayLen); Leq (int32lit minSize, nullCharIx); Leq (nullCharIx, int32lit maxSize)]
  *)

let octetStringInvariants (minSize : SIZE) (maxSize : SIZE) (recv: Expr): Expr =
  let len = ArrayLength (FieldSelect (recv, "arr"))
  if minSize.acn = maxSize.acn then Equals (len, int32lit maxSize.acn)
  else
    let nCount = FieldSelect (recv, "nCount")
    And [Leq (len, int32lit maxSize.acn); Leq (int32lit minSize.acn, nCount); Leq (nCount, len)]

let bitStringInvariants  (minSize : SIZE) (maxSize : SIZE) (recv: Expr): Expr =
  let len = ArrayLength (FieldSelect (recv, "arr"))
  if minSize.acn = maxSize.acn then Equals (len, int32lit (bigint (getBitStringMaxOctets maxSize)))
  else
    let nCount = FieldSelect (recv, "nCount")
    And [Leq (len, int32lit (bigint (getBitStringMaxOctets maxSize))); Leq (longlit minSize.acn, nCount); Leq (nCount, Mult (len, longlit 8I))] // TODO: Cast en long explicite

let sequenceInvariantsCommon (children: (Asn1AcnAst.Asn1Child * Expr) list): Expr option =
  let conds = children |> List.collect (fun (child, field) ->
    let isDefined = isDefinedMutExpr field
    let opt =
      match child.Optionality with
      | Some AlwaysPresent -> [isDefined]
      | Some AlwaysAbsent -> [Not isDefined]
      | _ -> []
    // StringType is a type alias and has therefore no associated class invariant; we need to explicitly add them
    let strType =
      match child.Type.Kind.ActualType with
      | IA5String st -> [stringInvariants st.minSize.acn st.maxSize.acn field]
      | _ -> []
    opt @ strType
  )
  if conds.IsEmpty then None
  else if conds.Tail.IsEmpty then Some conds.Head
  else Some (And conds)

let sequenceInvariants  (children: Asn1AcnAst.Asn1Child list) (recv: Expr): Expr option =
  sequenceInvariantsCommon  (children |> List.map (fun c -> c, FieldSelect (recv, c._scala_name)))

let sequenceOfInvariants (minSize : SIZE) (maxSize : SIZE) (recv: Expr): Expr =
    let len = vecSize (FieldSelect (recv, "arr"))
    if minSize.acn = maxSize.acn then Equals (len, int32lit maxSize.acn)
    else
      let nCount = FieldSelect (recv, "nCount")
      And [Leq (len, int32lit maxSize.acn); Leq (int32lit minSize.acn, nCount); Leq (nCount, len)]

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

let countNbPresenceBits (children  : SeqChildInfo list): int =
  children |> List.sumBy (fun child ->
    match child with
    | AcnChild _ -> 0
    | Asn1Child asn1 ->
      match asn1.Optionality with
      | Some (Optional opt) when opt.acnPresentWhen.IsNone -> 1
      | _ -> 0
  )


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

let renameBindingsSizeRes (res: SizeExprRes) (suffix: string): SizeExprRes =
  let allVars = res.bdgs |> List.map fst
  let renamedVars = allVars |> List.map (fun v -> {v with name = $"{v.name}{suffix}"})
  let mapping = List.zip allVars (renamedVars |> List.map Var)
  let renamedVarFor (old: Var): Var =
    renamedVars.[allVars |> List.findIndex (fun v -> v = old)]
  let newBdgs = res.bdgs |> List.map (fun (v, e) -> renamedVarFor v, substVars mapping e)
  let newResSize = substVars mapping res.resSize
  {bdgs = newBdgs; resSize = newResSize}

let renameBindingsSeqSizeRes (res: SeqSizeExprChildRes list) (suffix: string): SeqSizeExprChildRes list =
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
    // TODO: We don't support these anyway
    // assert (rt.acnMinSizeInBits = rt.acnMaxSizeInBits)
    aligned {bdgs = []; resSize = longlit rt.acnMaxSizeInBits}
  | Sequence sq ->
    // Alignment done there
    seqSizeExpr sq.children align obj offset (nestingLevel + 1I) nestingIx
  | Choice ch ->
    // Ditto
    choiceSizeExpr ch.typeDef ch.children align obj offset (nestingLevel + 1I) nestingIx
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
      asn1SizeExpr rt.resolvedType.acnAlignment rt.resolvedType.Kind obj offset nestingLevel nestingIx
    else
      {bdgs = []; resSize = alignedSizeTo rt.resolvedType.acnAlignment (callSize obj offset) offset}
  | _ -> aligned {bdgs = []; resSize = callSize obj offset}

and seqSizeExprHelperChild (child: SeqChildInfo)
                           (ix: bigint)
                           (recv: Expr option)
                           (offset: Expr)
                           (nestingLevel: bigint)
                           (nestingIx: bigint): SizeExprRes =
  match child with
  | AcnChild acn ->
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

and seqSizeExprHelper (children : SeqChildInfo list)
                      (obj: Expr)
                      (offset: Expr)
                      (nestingLevel: bigint)
                      (nestingIx: bigint): SeqSizeExprChildRes list =
  let nbPresenceBits = countNbPresenceBits children 

  let childSize (acc: SeqSizeExprChildRes list) (ix0: int, child: SeqChildInfo): SeqSizeExprChildRes list =
    let ix = bigint (nbPresenceBits + ix0)
    let varName =
      if nestingLevel = 0I then $"size_{nestingIx + ix}"
      else $"size_{nestingLevel}_{nestingIx + ix}"
    let resVar = {Var.name = varName; tpe = IntegerType Long}
    let accOffset = plus (offset :: (acc |> List.map (fun res -> Var res.childVar)))
    let recv =
      match child with
      | AcnChild _ -> None
      | Asn1Child child -> Some (FieldSelect (obj, child._scala_name))
    let childResSize = seqSizeExprHelperChild child ix recv accOffset nestingLevel nestingIx
    acc @ [{extraBdgs = childResSize.bdgs; childVar = resVar; childSize = childResSize.resSize}]

  let presenceBitsVars = [0 .. nbPresenceBits - 1] |> List.map (fun i ->
    let varName =
      if nestingLevel = 0I then $"size_{nestingIx + bigint i}"
      else $"size_{nestingLevel}_{nestingIx + bigint i}"
    {extraBdgs = []; childVar = {name = varName; tpe = IntegerType Long}; childSize = longlit 1I}
  )
  children |> List.indexed |> (List.fold childSize presenceBitsVars)

and seqSizeExpr (children : SeqChildInfo list)
                (align: AcnAlignment option)
                (obj: Expr)
                (offset: Expr)
                (nestingLevel: bigint)
                (nestingIx: bigint): SizeExprRes =
  if children.IsEmpty then {bdgs = []; resSize = longlit 0I}
  else
    let childrenSizes = seqSizeExprHelper children obj offset nestingLevel nestingIx
    let allBindings = childrenSizes |> List.collect (fun s -> s.extraBdgs @ [(s.childVar, s.childSize)])
    let childrenVars = childrenSizes |> List.map (fun s -> s.childVar)
    let resultSize = childrenVars |> List.map Var |> plus
    let resultSize = alignedSizeTo align resultSize offset
    {bdgs = allBindings; resSize = resultSize}

and choiceSizeExpr (typeDef             : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>)
                   (children            : Asn1AcnAst.ChChildInfo list)
                   (align: AcnAlignment option)
                   (obj: Expr)
                   (offset: Expr)
                   (nestingLevel: bigint)
                   (nestingIx: bigint): SizeExprRes =
  let cases = children |> List.map (fun child ->
    let tpeId = (ToC typeDef[Scala].typeName) + "." + (ToC child.present_when_name) + "_PRESENT"
    let tpe = fromAsn1TypeKind child.Type.Kind
    let binder = {Var.name = child._scala_name; tpe = tpe}
    let pat = ADTPattern {binder = None; id = tpeId; subPatterns = [Wildcard (Some binder)]}
    let res = asn1SizeExpr child.Type.acnAlignment child.Type.Kind (Var binder) offset nestingLevel nestingIx
    let resSize = alignedSizeTo align res.resSize offset
    let res = letsIn res.bdgs resSize
    {MatchCase.pattern = pat; rhs = res}
  )
  {bdgs = []; resSize = MatchExpr {scrut = obj; cases = cases}}

let optionalSizeExpr (child: Asn1AcnAst.Asn1Child)
                     (obj: Expr)
                     (offset: Expr)
                     (nestingLevel: bigint)
                     (nestingIx: bigint): SizeExprRes =
  let sz (recv: Expr) =
    match child.Type.Kind with
    | Choice _ | Sequence _ | SequenceOf _ ->
      {bdgs = []; resSize = callSize recv offset}
    | _ -> asn1SizeExpr child.Type.acnAlignment child.Type.Kind recv offset nestingLevel nestingIx
  match child.Optionality with
    | Some AlwaysPresent -> sz (getMutExpr obj)
    | Some AlwaysAbsent -> {bdgs = []; resSize = longlit 0I}
    | Some (Optional _) ->
      let res = sz (getMutExpr obj)
      {res with resSize = IfExpr {cond = isDefinedMutExpr obj; thn = res.resSize; els = longlit 0I}}
    | None -> sz obj

    

let seqSizeFunDefs (acnAlignment : AcnAlignment option) (maxAlignment: AcnAlignment option) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (children : SeqChildInfo list) : FunDef list =
  // TODO: Pour les int, on peut ajouter une assertion GetBitUnsignedCount(...) == resultat (ici et/ou ailleurs)
  let offset = {Var.name = "offset"; tpe = IntegerType Long}
  let res = seqSizeExpr children acnAlignment This (Var offset) 0I 0I
  let finalSize = letsIn res.bdgs res.resSize
  let res = {name = "res"; tpe = IntegerType Long}
  let postcond =
    if acnMinSizeInBits = acnMaxSizeInBits then Equals (Var res, longlit acnMaxSizeInBits)
    else And [Leq (longlit 0I, Var res); Leq (Var res, longlit acnMaxSizeInBits)]

  let sizeLemmas (align: AcnAlignment option): FunDef =
    let template = sizeLemmaTemplate acnMaxSizeInBits align
    let offset = template.prms.[0]
    let otherOffset = template.prms.[1]

    let allResWithOffset = seqSizeExprHelper children This (Var offset) 0I 0I
    let allResWithOffset = renameBindingsSeqSizeRes allResWithOffset "_offset"
    let allResWithOtherOffset = seqSizeExprHelper children This (Var otherOffset) 0I 0I
    let allResWithOtherOffset = renameBindingsSeqSizeRes allResWithOtherOffset "_otherOffset"

    let proofSubcase (ix: int, (resWithOffset: SeqSizeExprChildRes, resWithOtherOffset: SeqSizeExprChildRes, child: SeqChildInfo option)) (rest: Expr): Expr =
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
      | Some (Asn1Child child) ->
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
      | _ -> withBindingsPlugged None

    let nbPresenceBits = countNbPresenceBits children
    assert (allResWithOffset.Length = allResWithOtherOffset.Length)
    assert (allResWithOffset.Length = children.Length + nbPresenceBits)
    let sqChildren = (List.replicate nbPresenceBits None) @ (children |> List.map Some)
    let proofBody = (List.foldBack proofSubcase ((List.zip3 allResWithOffset allResWithOtherOffset sqChildren) |> List.indexed) UnitLit)

    {template with body = proofBody}

  let sizeFd = {
    id = "size"
    prms = [offset]
    specs = [Precond (offsetConds offset acnMaxSizeInBits)]
    annots = []
    postcond = Some (res, postcond)
    returnTpe = IntegerType Long
    body = finalSize
  }
  let maxAlign = maxAlignment
  let implyingAligns = implyingAlignments maxAlign
  let lemmas = implyingAligns |> List.map sizeLemmas
  sizeFd :: lemmas

let choiceSizeFunDefs (acnAlignment : AcnAlignment option) 
                      (maxAlignment: AcnAlignment option)
                      (acnMinSizeInBits    : BigInteger)
                      (acnMaxSizeInBits    : BigInteger)
                      (typeDef : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) 
                      (children            : Asn1AcnAst.ChChildInfo list): FunDef list =
  let offset = {Var.name = "offset"; tpe = IntegerType Long}
  let sizeRes = choiceSizeExpr typeDef children acnAlignment This (Var offset) 0I 0I
  assert sizeRes.bdgs.IsEmpty
  let sizeLemmas (align: AcnAlignment option): FunDef =
    let template = sizeLemmaTemplate acnMaxSizeInBits align
    let offset = template.prms.[0]
    let otherOffset = template.prms.[1]
    let proofCases = children |> List.map (fun child ->
      let tpeId = (ToC typeDef[Scala].typeName) + "." + (ToC child.present_when_name) + "_PRESENT"
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
    if acnMinSizeInBits = acnMaxSizeInBits then Equals (Var res, longlit acnMaxSizeInBits)
    else And [Leq (longlit 0I, Var res); Leq (Var res, longlit acnMaxSizeInBits)]
  let sizeFd = {
    id = "size"
    prms = [offset]
    specs = [Precond (offsetConds offset acnMaxSizeInBits)]
    annots = []
    postcond = Some (res, postcond)
    returnTpe = IntegerType Long
    body = sizeRes.resSize
  }
  let maxAlign = maxAlignment 
  let implyingAligns = implyingAlignments maxAlign
  let lemmas = implyingAligns |> List.map sizeLemmas
  sizeFd :: lemmas

let seqOfSizeFunDefs (typeDef : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (maxSize : SIZE) (acnEncodingClass : SizeableAcnEncodingClass) (acnAlignment : AcnAlignment option) (maxAlignment: AcnAlignment option) (child : Asn1AcnAst.Asn1Type) : FunDef list * FunDef list =
  let td = typeDef.[Scala].typeName
  let elemTpe = fromAsn1TypeKind child.Kind
  let lsTpe = vecTpe elemTpe
  let res = {name = "res"; tpe = IntegerType Long}

  let callSizeRangeObj (ls: Expr) (offset: Expr) (from: Expr) (tto: Expr): Expr =
    FunctionCall {
      prefix = [td]
      id = "sizeRange"
      tps = []
      args = [ls; offset; from; tto]
      parameterless = true
    }

  let offsetCondHelper (offset: Var) (from: Var) (tto: Var): Expr =
    let overhead = acnMaxSizeInBits - maxSize.acn * child.Kind.acnMaxSizeInBits
    And [
      Leq (longlit 0I, Var offset)
      Leq (Var offset, Minus (longlit (2I ** 63 - 1I - overhead), Mult (longlit child.Kind.acnMaxSizeInBits, Minus (Var tto, Var from))))
    ]
  let rangeVarsCondHelper (ls: Var) (from: Var) (tto: Var): Expr = And [Leq (int32lit 0I, Var from); Leq (Var from, Var tto); Leq (Var tto, vecSize (Var ls)); Leq (vecSize (Var ls), int32lit maxSize.acn)]

  let sizeRangeObjFd =
    let ls = {name = "ls"; tpe = lsTpe}
    let offset = {Var.name = "offset"; tpe = IntegerType Long}
    let from = {name = "from"; tpe = IntegerType Int}
    let tto = {name = "to"; tpe = IntegerType Int}
    let measure = Minus (Var tto, Var from)
    let offsetCond = offsetCondHelper offset from tto
    let rangeVarsConds = rangeVarsCondHelper ls from tto
    let elem = vecApply (Var ls) (Var from)
    let elemSizeVar = {name = "elemSize"; tpe = IntegerType Long}
    let elemSize = asn1SizeExpr child.acnAlignment child.Kind elem (Var offset) 0I 0I
    let elemSizeAssert =
      if child.Kind.acnMinSizeInBits = child.Kind.acnMaxSizeInBits then
        Assert (Equals (Var elemSizeVar, longlit child.Kind.acnMinSizeInBits))
      else
        Assert (And [
          Leq (longlit 0I, Var elemSizeVar)
          Leq (Var elemSizeVar, longlit child.Kind.acnMaxSizeInBits)
        ])
    let reccall = callSizeRangeObj (Var ls) (plus [Var offset; Var elemSizeVar]) (plus [Var from; int32lit 1I]) (Var tto)
    let resSize = alignedSizeTo acnAlignment (plus [Var elemSizeVar; reccall]) (Var offset)
    let elseBody = letsIn (elemSize.bdgs @ [elemSizeVar, elemSize.resSize]) (mkBlock [elemSizeAssert; resSize])
    let body =
      IfExpr {
        cond = Equals (Var from, Var tto)
        thn = longlit 0I
        els = elseBody
      }

    let postcondRange =
      let nbElems = {Var.name = "nbElems"; tpe = IntegerType Int} // TODO: Add explicit cast to Long
      let sqUpperBound = Mult (longlit child.Kind.acnMaxSizeInBits, Var nbElems)
      Let {
        bdg = nbElems
        e = Minus (Var tto, Var from) // TODO: Add explicit cast to Long
        body = And [
          Leq (longlit 0I, Var res)
          Leq (Var res, sqUpperBound)
        ]
      }
    {
      id = "sizeRange"
      prms = [ls; offset; from; tto]
      specs = [Precond rangeVarsConds; Precond offsetCond; Measure measure]
      annots = []
      postcond = Some (res, postcondRange)
      returnTpe = IntegerType Long
      body = body
    }

  let sizeLemmas (align: AcnAlignment option): FunDef * FunDef =
    let elemSizeAssert (elemSizeVar: Var): Expr =
      if child.Kind.acnMinSizeInBits = child.Kind.acnMaxSizeInBits then
        Assert (Equals (Var elemSizeVar, longlit child.Kind.acnMinSizeInBits))
      else
        Assert (And [
          Leq (longlit 0I, Var elemSizeVar)
          Leq (Var elemSizeVar, longlit child.Kind.acnMaxSizeInBits)
        ])

    let template = sizeLemmaTemplate acnMaxSizeInBits align
    let offset = template.prms.[0]
    let otherOffset = template.prms.[1]
    let ls = {name = "ls"; tpe = lsTpe}
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
        callSizeRangeObj (Var ls) (Var offset) (Var from) (Var tto),
        callSizeRangeObj (Var ls) (Var otherOffset) (Var from) (Var tto)
      )
    let elemSel = vecApply (Var ls) (Var from)
    let elemSizeOffVar = {Var.name = "elemSizeOff"; tpe = IntegerType Long}
    let elemSizeOtherOffVar = {Var.name = "elemSizeOtherOff"; tpe = IntegerType Long}
    let elemSizeOffRes = asn1SizeExpr align child.Kind elemSel (Var offset) 0I 0I
    let elemSizeOtherOffRes = asn1SizeExpr align child.Kind elemSel (Var otherOffset) 0I 0I
    let elemSizesBdgs =
      elemSizeOffRes.bdgs @
      [(elemSizeOffVar, elemSizeOffRes.resSize)] @
      elemSizeOtherOffRes.bdgs @
      [(elemSizeOtherOffVar, elemSizeOtherOffRes.resSize)]
    let elemLemmaCall = sizeLemmaCall child.Kind align elemSel (Var offset) (Var otherOffset)
    let inductiveStep = FunctionCall {
      prefix = [td]
      id = template.id
      tps = []
      args = [
        Var ls
        plus [Var offset; Var elemSizeOffVar]
        plus [Var otherOffset; Var elemSizeOtherOffVar]
        plus [Var from; int32lit 1I]
        Var tto
      ]
      parameterless = true
    }
    let proofElsePart = mkBlock ([
      elemSizeAssert elemSizeOffVar
      elemSizeAssert elemSizeOtherOffVar
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
        Precond (rangeVarsCondHelper ls from tto)
        Precond (offsetCondHelper offset from tto)
        Precond (offsetCondHelper otherOffset from tto)
      ] @ additionalPrecond @ [Measure (Minus (Var tto, Var from))]
    let objFd = {
      id = template.id
      prms = [ls; offset; otherOffset; from; tto]
      annots = [GhostAnnot; Opaque; InlineOnce]
      specs = proofSpecs
      postcond = Some ({name = "_"; tpe = UnitType}, postcond)
      returnTpe = UnitType
      body = proofBody
    }
    let objCall = FunctionCall {prefix = [td]; id = objFd.id; tps = []; args = [FieldSelect (This, "arr"); Var offset; Var otherOffset; int32lit 0I; FieldSelect (This, "nCount")]; parameterless = true}
    let clsFd = {template with body = objCall}
    clsFd, objFd

  let sizeClsFd =
    let offset = {Var.name = "offset"; tpe = IntegerType Long}
    let sizeField =
      match acnEncodingClass with
      | SZ_EC_LENGTH_EMBEDDED sz -> sz
      | _ -> 0I // TODO: Pattern?
    let postcond =
      if acnMinSizeInBits = acnMaxSizeInBits then Equals (Var res, longlit acnMaxSizeInBits)
      else And [Leq (longlit 0I, Var res); Leq (Var res, longlit acnMaxSizeInBits)]
    let finalSize = (plus [
      longlit sizeField
      callSizeRangeObj (FieldSelect (This, "arr")) (Var offset) (int32lit 0I) (FieldSelect (This, "nCount"))
    ])
    {
      id = "size"
      prms = [offset]
      specs = [Precond (offsetConds offset acnMaxSizeInBits)]
      annots = []
      postcond = Some (res, postcond)
      returnTpe = IntegerType Long
      body = finalSize
    }

  
  let maxAlign = maxAlignment
  let implyingAligns = implyingAlignments maxAlign
  let clsLemmas, objLemmas = implyingAligns |> List.map sizeLemmas |> List.unzip
  sizeClsFd :: clsLemmas, sizeRangeObjFd :: objLemmas


let generateSequenceSizeDefinitions (acnAlignment : AcnAlignment option) (maxAlignment: AcnAlignment option) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (children : SeqChildInfo list): string list =
  let fds = seqSizeFunDefs acnAlignment maxAlignment acnMinSizeInBits  acnMaxSizeInBits children 
  fds |> List.map (fun fd -> show (FunDefTree fd))

let generateChoiceSizeDefinitions (acnAlignment : AcnAlignment option) 
                      (maxAlignment: AcnAlignment option)
                      (acnMinSizeInBits    : BigInteger)
                      (acnMaxSizeInBits    : BigInteger)
                      (typeDef : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) 
                      (children            : Asn1AcnAst.ChChildInfo list) : string list =
  let fds = choiceSizeFunDefs acnAlignment maxAlignment acnMinSizeInBits acnMaxSizeInBits typeDef children
  fds |> List.map (fun fd -> show (FunDefTree fd))

let generateSequenceOfSizeDefinitions (typeDef : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (maxSize : SIZE) (acnEncodingClass : SizeableAcnEncodingClass) (acnAlignment : AcnAlignment option) (maxAlignment: AcnAlignment option) (child : Asn1AcnAst.Asn1Type): string list * string list =
  let fdsCls, fdsObj = seqOfSizeFunDefs typeDef acnMinSizeInBits acnMaxSizeInBits maxSize acnEncodingClass acnAlignment maxAlignment child
  fdsCls |> List.map (fun fd -> show (FunDefTree fd)), fdsObj |> List.map (fun fd -> show (FunDefTree fd))

let generateSequenceSubtypeDefinitions (dealiased: string) (typeDef:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) (children: Asn1AcnAst.Asn1Child list): string list =
  let retTpe = ClassType {ClassType.prefix = []; id = typeDef[Scala].typeName; tps = []; parameterless = false} //fromAsn1TypeKind kind
  let prms = children |> List.map (fun c -> {Var.name = c.Name.Value; tpe = fromAsn1TypeKind c.Type.Kind})
  let body = ClassCtor {ct = {prefix = []; id = dealiased; tps = []; parameterless = false}; args = prms |> List.map Var}
  let reqs = sequenceInvariantsCommon (List.zip children (prms |> List.map Var))
  let fd = {
    FunDef.id = "apply"
    prms = prms
    annots = []
    specs = reqs |> Option.map Precond |> Option.toList
    postcond = None
    returnTpe = retTpe
    body = body
  }
  [show (FunDefTree fd)]


let generateEncodePostcondExprCommon (r: Asn1AcnAst.AstRoot)
                                     (tpe: Type)
                                     (maxSize: bigint)
                                     (pVal: Selection)
                                     (resPostcond: Var)
                                     (acnTps: Type list)
                                     (sz: SizeExprRes)
                                     (extraCondsPre: Expr list)
                                     (decodePureId: string)
                                     (decodeExtraArgs: Expr list): Expr =
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = Old (Var cdc)
  let szRecv = {Var.name = pVal.asLastOrSelf.receiverId; tpe = tpe}

  let acnVarsPatBdg = acnTps |> List.indexed |> List.map (fun (ix, tpe) -> {Var.name = $"acn{ix + 1}"; tpe = tpe})

  let invertibility =
    if not r.args.stainlessInvertibility then []
    else
      let prefix = isPrefixOfACN oldCdc (Var cdc)
      let r1 = {Var.name = "r1"; tpe = ClassType codecTpe}
      let lemmaCall = validateOffsetBitsContentIrrelevancyLemma (selBitStreamACN oldCdc) (selBufACN (Var cdc)) (longlit maxSize)
      let decodePureCall = FunctionCall {prefix = []; id = decodePureId; tps = []; args = (Var r1) :: decodeExtraArgs; parameterless = true}
      let r2Got = {Var.name = "r2Got"; tpe = ClassType codecTpe}
      let decodingRes = {Var.name = "decodingRes"; tpe = eitherMutTpe (IntegerType Int) tpe}
      let resGot = {Var.name = "resGot"; tpe = tpe}
      let acnVarsGotBdg = acnTps |> List.indexed |> List.map (fun (ix, tpe) -> {Var.name = $"acnGot{ix + 1}"; tpe = tpe})
      let acnEq = List.zip acnVarsGotBdg acnVarsPatBdg |> List.map (fun (acnGot, acn) -> Equals (Var acnGot, Var acn))
      let eq = And ([
        Equals (Var r2Got, Var cdc)
        Equals (Var resGot, Var szRecv)
      ] @ acnEq)
      let decodeResPatmat =
        let rightPat =
          let subpat =
            if acnTps.IsEmpty then Wildcard (Some resGot)
            else TuplePattern {binder = None; subPatterns = Wildcard (Some resGot) :: (acnVarsGotBdg |> List.map (fun v -> Wildcard (Some v)))}
          ADTPattern {
            binder = None
            id = rightMutId
            subPatterns = [subpat]
          }
        MatchExpr {
          scrut = Var decodingRes
          cases = [
            {
              pattern = ADTPattern {binder = None; id = leftMutId; subPatterns = [Wildcard None]}
              rhs = BoolLit false
            }
            {
              pattern = rightPat
              rhs = eq
            }
          ]
        }
      let boundCall =
        letsIn [r1, resetAtACN (Var cdc) oldCdc] (
          mkBlock [
            lemmaCall
            letTuple [r2Got; decodingRes] decodePureCall decodeResPatmat
          ]
        )
      [prefix; Locally boundCall]

  let rightBody = And (extraCondsPre @ [
    Equals (selBufLengthACN oldCdc, selBufLengthACN (Var cdc))
    Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz.resSize])
  ] @ invertibility)
  let rightBody = letsIn sz.bdgs rightBody
  if acnTps.IsEmpty then
    eitherMatchExpr (Var resPostcond) None (BoolLit true) None rightBody
  else
    let rightTuplePat = TuplePattern {binder = None; subPatterns = Wildcard None :: (acnVarsPatBdg |> List.map (fun v -> Wildcard (Some v)))}
    MatchExpr {
      scrut = Var resPostcond
      cases = [
        {
          pattern = ADTPattern {binder = None; id = leftId; subPatterns = [Wildcard None]}
          rhs = BoolLit true
        }
        {
          pattern = ADTPattern {binder = None; id = rightId; subPatterns = [rightTuplePat]}
          rhs = rightBody
        }
      ]
    }

let generatePrecond (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (codec: Codec): Expr =
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  validateOffsetBitsACN (Var cdc) (longlit (t.maxSizeInBits enc))

let generateDecodePostcondExprCommon (r: Asn1AcnAst.AstRoot) (resPostcond: Var) (resRightMut: Var) (sz: SizeExprRes) (extraCondsPre: Expr list) (extraCondsPost: Expr list): Expr =
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = Old (Var cdc)
  let rightBody = And (extraCondsPre @ [
    Equals (selBufACN oldCdc, selBufACN (Var cdc))
    Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz.resSize])
  ] @ extraCondsPost)
  let rightBody = letsIn sz.bdgs rightBody
  eitherMutMatchExpr (Var resPostcond) None (BoolLit true) (Some resRightMut) rightBody

let generateEncodePostcondExpr (r: Asn1AcnAst.AstRoot) (t: Asn1AcnAst.Asn1Type) (pVal: Selection) (resPostcond: Var) (decodePureId: string): Expr =
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = Old (Var cdc)
  let tpe = fromAsn1TypeKind t.Kind
  let szRecv = {Var.name = pVal.asLastOrSelf.receiverId; tpe = tpe}
  let sz =
    match t.Kind with
    | Choice _ | Sequence _ | SequenceOf _ ->
      // Note that we don't have a "ReferenceType" in such cases, so we have to explicitly call `size` and not rely on asn1SizeExpr...
      {bdgs = []; resSize = callSize (Var szRecv) (bitIndexACN oldCdc)}
    | _ -> asn1SizeExpr t.acnAlignment t.Kind (Var szRecv) (bitIndexACN oldCdc) 0I 0I
  generateEncodePostcondExprCommon r tpe t.acnMaxSizeInBits pVal resPostcond [] sz [] decodePureId []

let generateDecodePostcondExpr (r: Asn1AcnAst.AstRoot) (t: Asn1AcnAst.Asn1Type) (resPostcond: Var): Expr =
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = Old (Var cdc)
  let tpe = fromAsn1TypeKind t.Kind
  let szRecv = {Var.name = "resVal"; tpe = tpe}
  let sz =
    match t.Kind with
    | Choice _ | Sequence _ | SequenceOf _ ->
      // Note that we don't have a "ReferenceType" in such cases, so we have to explicitly call `size` and not rely on asn1SizeExpr...
      {bdgs = []; resSize = callSize (Var szRecv) (bitIndexACN oldCdc)}
    | _ -> asn1SizeExpr t.acnAlignment t.Kind (Var szRecv) (bitIndexACN oldCdc) 0I 0I
  let strSize =
    match t.ActualType.Kind with
    | IA5String str -> [Equals (vecSize (Var szRecv), int32lit (str.maxSize.acn + 1I))] // +1 for the null terminator
    | _ -> []
  let cstrIsValid =
    match t.ActualType.Kind with
    | NullType _ -> []
    | _ ->
      let isValidFuncName = $"{t.FT_TypeDefinition.[Scala].typeName}_IsConstraintValid"
      [isRightExpr (FunctionCall {prefix = []; id = isValidFuncName; tps = []; args = [Var szRecv]; parameterless = true})]
  generateDecodePostcondExprCommon r resPostcond szRecv sz [] (strSize @ cstrIsValid)

let rec tryFindFirstParentACNDependency (parents: Asn1AcnAst.Asn1Type list) (dep: RelativePath): (Asn1AcnAst.Asn1Type * Asn1AcnAst.AcnChild) option =
  match parents with
  | [] -> None
  | parent :: rest ->
    match parent.ActualType.Kind with
    | Sequence _ ->
      let directAcns = collectNestedAcnChildren parent.Kind
      assert (directAcns |> List.forall (fun acn -> List.isPrefixOf parent.id.ToScopeNodeList acn.id.ToScopeNodeList))
      directAcns |> List.tryFind (fun acn ->
        let suffix = ReferenceToType (acn.id.ToScopeNodeList |> List.skip parent.id.ToScopeNodeList.Length)
        List.endsWith suffix.fieldPath dep.asStringList
      ) |>
        Option.map (fun acn -> parent, acn) |>
        Option.orElse (tryFindFirstParentACNDependency rest dep)
    | _ -> tryFindFirstParentACNDependency rest dep

let rec firstOutermostSeqParent (parents: Asn1AcnAst.Asn1Type list): Asn1AcnAst.Asn1Type option =
  match parents with
  | [] -> None
  | parent :: rest ->
    match parent.ActualType.Kind with
    | Sequence _ -> firstOutermostSeqParent rest |> Option.orElse (Some parent)
    | _ -> None

// We must provide all ACN dependencies to auxiliary decoding functions, which can come from two sources:
// * From the current function (not the one we create but the one where we "stand") parameter list (forwarded dependency)
// * In case this is a Sequence, the corresponding decoded ACN inserted field, stored in a local variable
// In both cases, the variable names are the same, so we can (ab)use this fact and not worry from where
// we got the ACN dependency.
let acnExternDependenciesVariableDecode (t: Asn1AcnAst.Asn1Type) (parents: Asn1AcnAst.Asn1Type list): (Asn1AcnAst.Asn1Type * AcnChild * Var) list =
  t.externalDependencies |> List.map (fun dep ->
    let acnDep = tryFindFirstParentACNDependency parents dep
    assert acnDep.IsSome
    let parent, acnParam = acnDep.Value
    let nme = ToC (acnParam.id.dropModule.AcnAbsPath |> Seq.StrJoin "_")
    let tpe = fromAcnInsertedType acnParam.Type
    parent, acnParam, {Var.name = nme; tpe = tpe}
  ) |> List.distinctBy (fun (_, _, v) -> v)

// For auxiliary encoding function, we sometimes need to encode bytes that depend on the determinant
// of a field that is outside of the current encoding function. We therefore need to somehow refer to it.
// We can do so in two ways:
// * Add the dependency as a parameter and forward it as needed.
// * Refer to it from the outermost "pVal" (always in the function parameter) when possible
// The second way is preferred but not always possible (e.g. if there is a Choice in the path),
// we cannot access the field past the choice since we need to pattern match).
let acnExternDependenciesVariableEncode (t: Asn1AcnAst.Asn1Type) (nestingScope: NestingScope): Var option =
  let rec allDependenciesExcept (t: Asn1AcnAst.Asn1Type) (avoid: ReferenceToType): RelativePath list =
    if t.id = avoid then []
    else
      match t.Kind with
      | ReferenceType tp -> allDependenciesExcept tp.resolvedType avoid
      | Sequence sq ->
          sq.acnArgs @ (sq.children |> List.collect (fun c ->
              match c with
              | Asn1Child c -> allDependenciesExcept c.Type avoid
              | AcnChild _ -> []
          ))
      | Choice ch -> ch.acnArgs
      | SequenceOf sqf -> sqf.acnArgs
      | OctetString os -> os.acnArgs
      | BitString bs -> bs.acnArgs
      | _ -> []
  match firstOutermostSeqParent (nestingScope.parents |> List.map snd) with
  | None -> None
  | Some seqParent ->
    match seqParent.id.ToScopeNodeList with
    | MD _ :: TA _ :: [] ->
      // This is the outermost parent, the "pVal" that we always include in auxiliary encoding functions from `wrapAcnFuncBody`
      None
    | _ ->
      let acnChildren = collectNestedAcnChildren t.Kind |> List.map (fun acn ->
        assert List.isPrefixOf seqParent.id.fieldPath acn.id.fieldPath
        acn.id.fieldPath |> List.skip seqParent.id.fieldPath.Length
      )
      // We check whether this `t` is an external dependency to a child of the parent (other than itself, hence the "except")
      let allDeps = allDependenciesExcept seqParent t.id
      let isAnExternalDependency = allDeps |> List.exists (fun dep ->
        acnChildren |> List.exists (fun acn -> List.isPrefixOf acn dep.asStringList)
      )
      if not isAnExternalDependency then None
      else
        let tpe = fromAsn1TypeKind seqParent.Kind
        let nme = seqParent.id.lastItem
        Some {Var.name = nme; tpe = tpe}

type PrimitiveDecodeInfo = {
  prefix: string list
  tpe: Type
  decodeId: string
  decodePureId: string
  prefixLemmaId: string
  extraConstArgs: Expr list
}
type ComposedDecodeInfo = {
  decodeId: string
  decodePureId: string
  prefixLemmaId: string
}
type DecodeInfo =
| PrimitiveDecodeInfo of PrimitiveDecodeInfo
| ComposedDecodeInfo of ComposedDecodeInfo

let booleanDecodeInfo = {PrimitiveDecodeInfo.prefix = [bitStreamId]; tpe = BooleanType; decodeId = "readBit"; decodePureId = "readBitPure"; prefixLemmaId = "readBitPrefixLemma"; extraConstArgs = []}

let decodeInfo (t: Asn1AcnAst.Asn1AcnType) (id: ReferenceToType) (isOptional: bool): DecodeInfo =
  let forACNIntClass (encCls: IntEncodingClass): PrimitiveDecodeInfo =
    match encCls with
    | PositiveInteger_ConstSize_8 ->
      let baseId = "dec_Int_PositiveInteger_ConstSize_8"
      {prefix = [acnId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | PositiveInteger_ConstSize_big_endian_16 ->
      let baseId = "dec_Int_PositiveInteger_ConstSize_big_endian_16"
      {prefix = [acnId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | PositiveInteger_ConstSize_big_endian_32 ->
      let baseId = "dec_Int_PositiveInteger_ConstSize_big_endian_32"
      {prefix = [acnId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | PositiveInteger_ConstSize_big_endian_64 ->
      let baseId = "dec_Int_PositiveInteger_ConstSize_big_endian_64"
      {prefix = [acnId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | PositiveInteger_ConstSize_little_endian_16 ->
      let baseId = "dec_Int_PositiveInteger_ConstSize_little_endian_16"
      {prefix = [acnId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | PositiveInteger_ConstSize_little_endian_32 ->
      let baseId = "dec_Int_PositiveInteger_ConstSize_little_endian_32"
      {prefix = [acnId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | PositiveInteger_ConstSize_little_endian_64 ->
      let baseId = "dec_Int_PositiveInteger_ConstSize_little_endian_64"
      {prefix = [acnId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | PositiveInteger_ConstSize bits ->
      let baseId = "dec_Int_PositiveInteger_ConstSize"
      {prefix = [acnId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = [int32lit bits]}

    | TwosComplement_ConstSize_8 ->
      let baseId = "dec_Int_TwosComplement_ConstSize_8"
      {prefix = [acnId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | TwosComplement_ConstSize_big_endian_16 ->
      let baseId = "dec_Int_TwosComplement_ConstSize_big_endian_16"
      {prefix = [acnId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | TwosComplement_ConstSize_big_endian_32 ->
      let baseId = "dec_Int_TwosComplement_ConstSize_big_endian_32"
      {prefix = [acnId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | TwosComplement_ConstSize_big_endian_64 ->
      let baseId = "dec_Int_TwosComplement_ConstSize_big_endian_64"
      {prefix = [acnId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | TwosComplement_ConstSize_little_endian_16 ->
      let baseId = "dec_Int_TwosComplement_ConstSize_little_endian_16"
      {prefix = [acnId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | TwosComplement_ConstSize_little_endian_32 ->
      let baseId = "dec_Int_TwosComplement_ConstSize_little_endian_32"
      {prefix = [acnId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | TwosComplement_ConstSize_little_endian_64 ->
      let baseId = "dec_Int_TwosComplement_ConstSize_little_endian_64"
      {prefix = [acnId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | TwosComplement_ConstSize _ ->
      let baseId = "dec_Int_TwosComplement_ConstSize"
      {prefix = [acnId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}

    | Integer_uPER -> failwith "UPER encoding selected for ACN integers?"

    | _ -> failwith $"TODO: {encCls}"

  let forIntClass (intCls:Asn1AcnAst.IntegerClass) (encCls: IntEncodingClass) (range: BigIntegerUperRange): PrimitiveDecodeInfo =
    match encCls with
    | Integer_uPER ->
      match range with
      | Full ->
        let baseId = "decodeUnconstrainedWholeNumber"
        {prefix = [codecId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}Pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []}
      | PosInf min ->
        let baseId = "decodeConstrainedPosWholeNumber"
        {prefix = [codecId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}Pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = [ulonglit min]}
      | Concrete (min, max) ->
        if intCls.IsPositive then
          let baseId = "decodeConstrainedPosWholeNumber"
          {prefix = [codecId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}Pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = [ulonglit min; ulonglit max]}
        else
          let baseId = "decodeConstrainedWholeNumber"
          {prefix = [codecId]; tpe = IntegerType Long; decodeId = baseId; decodePureId = $"{baseId}Pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = [longlit min; longlit max]}
      | _ -> failwith $"TODO: {range}"
    | _ -> forACNIntClass encCls

  let octetString (ot: OctetString) =
    PrimitiveDecodeInfo {prefix = [codecId]; tpe = vecTpe (IntegerType UByte); decodeId = "decodeOctetString_no_length_vec"; decodePureId = "decodeOctetString_no_length_vec_pure"; prefixLemmaId = "decodeOctetString_no_length_vec_prefixLemma"; extraConstArgs = [int32lit (ot.maxSize.acn)]}
  let bitString (bt: BitString) =
    PrimitiveDecodeInfo {prefix = [bitStreamId]; tpe = vecTpe (IntegerType UByte); decodeId = "readBitsVec"; decodePureId = "readBitsVecPure"; prefixLemmaId = "readBitsVecPrefixLemma"; extraConstArgs = [longlit bt.maxSize.acn]}

  if isOptional then
    let baseId = $"{ToC id.dropModule.AsString}_Optional"
    ComposedDecodeInfo {decodeId = $"{baseId}_ACN_Decode"; decodePureId = $"{baseId}_ACN_Decode_pure"; prefixLemmaId = $"{baseId}_prefixLemma"}
  else
    match t with
    | Asn1 t ->
      match t.Kind with
      | Integer int -> PrimitiveDecodeInfo (forIntClass int.intClass int.acnEncodingClass int.uperRange)
      | BitString bt -> bitString bt
      | OctetString ot -> octetString ot
      | Boolean _ -> PrimitiveDecodeInfo booleanDecodeInfo
      | ReferenceType rt ->
        match rt.resolvedType.ActualType.Kind with
        | BitString bt -> bitString bt
        | IA5String str ->
          match str.acnEncodingClass with
          | Acn_Enc_String_uPER _ ->
            let baseId = t.ActualType.FT_TypeDefinition.[Scala].typeName
            ComposedDecodeInfo {decodeId = $"{baseId}_ACN_Decode"; decodePureId = $"{baseId}_ACN_Decode_pure"; prefixLemmaId = $"{baseId}_prefixLemma"}
          | _ ->
            // TODO: The second argument is the determinant but no idea where to fetch this from, therefore putting a dummy value
            let baseId = "dec_IA5String_CharIndex_External_Field_DeterminantVec"
            PrimitiveDecodeInfo {prefix = [acnId]; tpe = vecTpe (IntegerType UByte); decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = [longlit str.maxSize.acn; longlit 0I]}
        | OctetString ot -> octetString ot
        | _ ->
          let baseId =
            if rt.hasExtraConstrainsOrChildrenOrAcnArgs then ToC id.dropModule.AsString
            else t.ActualType.FT_TypeDefinition.[Scala].typeName
          ComposedDecodeInfo {decodeId = $"{baseId}_ACN_Decode"; decodePureId = $"{baseId}_ACN_Decode_pure"; prefixLemmaId = $"{baseId}_prefixLemma"}
      | Sequence _ | SequenceOf _ | Choice _ ->
          let baseId =
            if id.tasInfo.IsNone then ToC id.dropModule.AsString
            else t.ActualType.FT_TypeDefinition.[Scala].typeName
          ComposedDecodeInfo {decodeId = $"{baseId}_ACN_Decode"; decodePureId = $"{baseId}_ACN_Decode_pure"; prefixLemmaId = $"{baseId}_prefixLemma"}
      | _ ->
        let baseId = "TODO_ASN1_OTHER"
        PrimitiveDecodeInfo {prefix = [acnId]; tpe = IntegerType Int; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []} // TODO
    | Acn (AcnInteger int) ->
      PrimitiveDecodeInfo (forIntClass int.intClass int.acnEncodingClass int.uperRange)
    | Acn (AcnBoolean _) -> PrimitiveDecodeInfo booleanDecodeInfo
    | Acn (AcnReferenceToEnumerated enm) ->
      match enm.enumerated.acnEncodingClass with
      | Integer_uPER ->
        // Mimicking the logic in createEnumCommon
        let min = enm.enumerated.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.min
        let max = enm.enumerated.items |> List.map(fun x -> x.acnEncodeValue) |> Seq.max
        let baseId = "decodeConstrainedPosWholeNumber"
        PrimitiveDecodeInfo {prefix = [codecId]; tpe = IntegerType ULong; decodeId = baseId; decodePureId = $"{baseId}Pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = [ulonglit min; ulonglit max]}
      | _ -> PrimitiveDecodeInfo (forACNIntClass enm.enumerated.acnEncodingClass)
    | Acn (AcnReferenceToIA5String _) ->
      let baseId = ToC id.dropModule.AsString
      ComposedDecodeInfo {decodeId = $"{baseId}_ACN_Decode"; decodePureId = $"{baseId}_ACN_Decode_pure"; prefixLemmaId = $"{baseId}_prefixLemma"}
    | _ ->
      let baseId = "TODO_ACN_OTHER"
      PrimitiveDecodeInfo {prefix = [acnId]; tpe = IntegerType Int; decodeId = baseId; decodePureId = $"{baseId}_pure"; prefixLemmaId = $"{baseId}_prefixLemma"; extraConstArgs = []} // TODO

let selectCodecDecodeInfo (decodeInfo: DecodeInfo) (cdc: Expr): Expr =
  match decodeInfo with
  | PrimitiveDecodeInfo info ->
    if info.prefix = [bitStreamId] then selBitStreamACN cdc
    else if info.prefix = [codecId] then selBaseACN cdc
    else cdc
  | ComposedDecodeInfo _ -> cdc

type PrefixLemmaData = {
  baseId: string
  decodeId: string
  decodePureId: string
  paramsAcn: Var list
  acnTps: Type list
  c1: Var
  c2: Var
  sz: Var
  c2Reset: Var
  c1Res: Var
  decodingRes1: Var
  c2Res: Var
  decodingRes2: Var
  v1: Var
  v2: Var
  decodedAcn1: Var list
  decodedAcn2: Var list
  v1SizeExpr: SizeExprRes
  v1SizeVar: Var
  subPat1: Pattern
  subPat2: Pattern
}

let generatePrefixLemmaCommon (enc: Asn1Encoding)
                              (tpe: Type)
                              (maxSize: bigint)
                              (baseId: string)
                              (paramsAcn: Var list)
                              (acnTps: Type list)
                              (mkSizeExpr: Var -> Var -> SizeExprRes)
                              (nestingScope: NestingScope)
                              (mkProof: PrefixLemmaData -> Expr): FunDef =
  let codecTpe = runtimeCodecTypeFor enc
  let c1 = {Var.name = "c1"; tpe = ClassType codecTpe}
  let c2 = {Var.name = "c2"; tpe = ClassType codecTpe}
  let sz = {Var.name = "sz"; tpe = IntegerType Long}
  let maxSizeExpr = longlit maxSize
  let preconds = [
    Precond (Equals (selBufLengthACN (Var c1), selBufLengthACN (Var c2)))
    Precond (validateOffsetBitsACN (Var c1) maxSizeExpr)
    Precond (And [Leq (longlit 0I, Var sz); Leq (Var sz, maxSizeExpr)])
    Precond (arrayBitRangesEq
      (selBufACN (Var c1))
      (selBufACN (Var c2))
      (longlit 0I)
      (plus [bitIndexACN (Var c1); Var sz])
    )
  ]
  let decodeId = $"{baseId}_ACN_Decode"
  let decodePureId = $"{decodeId}_pure"
  let c2Reset = {Var.name = "c2Reset"; tpe = ClassType codecTpe}
  let c1Res = {Var.name = "c1Res"; tpe = ClassType codecTpe}
  let decodingRes1 = {Var.name = "decodingRes1"; tpe = tpe}
  let dec1 = {Var.name = "dec1"; tpe = TupleType [c1Res.tpe; decodingRes1.tpe]}
  let call1 = FunctionCall {prefix = []; id = decodePureId; tps = []; args = Var c1 :: (paramsAcn |> List.map Var); parameterless = true}
  let c2Res = {Var.name = "c2Res"; tpe = ClassType codecTpe}
  let decodingRes2 = {Var.name = "decodingRes2"; tpe = tpe}
  let dec2 = {Var.name = "dec2"; tpe = TupleType [c2Res.tpe; decodingRes2.tpe]}
  let call2 = FunctionCall {prefix = []; id = decodePureId; tps = []; args = Var c2Reset :: (paramsAcn |> List.map Var); parameterless = true}

  let v1 = {Var.name = "v1"; tpe = tpe}
  let v2 = {Var.name = "v2"; tpe = tpe}
  let decodedAcn1 = acnTps |> List.indexed |> List.map (fun (i, tpe) -> {Var.name = $"acn1_{i + 1}"; tpe = tpe})
  let decodedAcn2 = acnTps |> List.indexed |> List.map (fun (i, tpe) -> {Var.name = $"acn2_{i + 1}"; tpe = tpe})

  let subPat1, subPat2 =
    if acnTps.IsEmpty then
      Wildcard (Some v1), Wildcard (Some v2)
    else
      let subPat1 = TuplePattern {
        binder = None
        subPatterns = Wildcard (Some v1) :: (decodedAcn1 |> List.map (fun v -> Wildcard (Some v)))
      }
      let subPat2 = TuplePattern {
        binder = None
        subPatterns = Wildcard (Some v2) :: (decodedAcn2 |> List.map (fun v -> Wildcard (Some v)))
      }
      subPat1, subPat2

  let acnsEq = List.zip decodedAcn1 decodedAcn2 |> List.map (fun (acn1, acn2) -> Equals (Var acn1, Var acn2))
  // The size of the decoded value, in case of success
  let v1SizeExpr = mkSizeExpr v1 c1
  // We pattern match on the result and bind v1Size to the result of v1SizeExpr in case of success or to 0L otherwise
  // We also re-bind the intermediate bindings (e.g. size_1_0) since these are used by the Sequence proof
  let v1SizeVar = {Var.name = "v1Size"; tpe = IntegerType Long}
  let v1SizePatMat =
    MatchExpr {
      scrut = Var decodingRes1
      cases = [
        {
          pattern = ADTPattern {
            binder = None
            id = rightMutId
            subPatterns = [subPat1]
          }
          rhs = letsIn v1SizeExpr.bdgs (mkTuple (v1SizeExpr.resSize :: (v1SizeExpr.bdgs |> List.map fst |> List.map Var)))
        }
        {
          pattern = ADTPattern {
            binder = None
            id = leftMutId
            subPatterns = [Wildcard None]
          }
          rhs = mkTuple (List.replicate (v1SizeExpr.bdgs.Length + 1) (longlit 0I))
        }
      ]
    }

  let preSpecs =
    let sizeBdgs =
      if v1SizeExpr.bdgs.IsEmpty then [LetSpec (v1SizeVar, v1SizePatMat)]
      else
        let v1SizeTuple = {v1SizeVar with name = $"v1SizeTuple"}
        let tupleBdgs = (v1SizeVar :: (v1SizeExpr.bdgs |> List.map fst)) |> List.indexed |> List.map (fun (i, v) -> LetSpec (v, TupleSelect (Var v1SizeTuple, i + 1)))
        LetSpec (v1SizeTuple, v1SizePatMat) :: tupleBdgs
    preconds @ [
      LetSpec (c2Reset, resetAtACN (Var c2) (Var c1))
      LetSpec (dec1, call1)
      LetSpec (c1Res, TupleSelect (Var dec1, 1))
      LetSpec (decodingRes1, TupleSelect (Var dec1, 2))
      LetSpec (dec2, call2)
      LetSpec (c2Res, TupleSelect (Var dec2, 1))
      LetSpec (decodingRes2, TupleSelect (Var dec2, 2))
    ] @ sizeBdgs

  let prop =
    let prop = And ([Equals (bitIndexACN (Var c1Res), bitIndexACN (Var c2Res)); Equals (Var v1, Var v2)] @ acnsEq)
    IfExpr {
      cond = Equals (Var v1SizeVar, Var sz)
      thn = MatchExpr {
        scrut = Var decodingRes2
        cases = [
          {
            pattern = ADTPattern {
              binder = None
              id = rightMutId
              subPatterns = [subPat2]
            }
            rhs = prop
          }
          {
            pattern = ADTPattern {
              binder = None
              id = leftMutId
              subPatterns = [Wildcard None]
            }
            rhs = BoolLit false
          }
        ]
      }
      els = BoolLit true
    }

  let postcond =
    MatchExpr {
      scrut = Var decodingRes1
      cases = [
        {
          pattern = ADTPattern {
            binder = None
            id = rightMutId
            subPatterns = [subPat1]
          }
          rhs = prop
        }
        {
          pattern = ADTPattern {
            binder = None
            id = leftMutId
            subPatterns = [Wildcard None]
          }
          rhs = BoolLit true
        }
      ]
    }

  let proof = mkProof {baseId = baseId; decodeId = decodeId; decodePureId = decodePureId; paramsAcn = paramsAcn;
    acnTps = acnTps; c1 = c1; c2 = c2; sz = sz; c2Reset = c2Reset;
    c1Res = c1Res; decodingRes1 = decodingRes1; c2Res = c2Res; decodingRes2 = decodingRes2;
    v1 = v1; v2 = v2; decodedAcn1 = decodedAcn1; decodedAcn2 = decodedAcn2;
    v1SizeExpr = v1SizeExpr; v1SizeVar = v1SizeVar; subPat1 = subPat1; subPat2 = subPat2
  }

  {
    FunDef.id = $"{baseId}_prefixLemma"
    prms = [c1; c2; sz] @ paramsAcn
    annots = [GhostAnnot; Opaque; InlineOnce]
    specs = preSpecs
    postcond = Some ({Var.name = "_"; tpe = UnitType}, postcond)
    returnTpe = UnitType
    body = proof
  }


let generatePrefixLemma (enc: Asn1Encoding)
                        (t: Asn1AcnAst.Asn1Type)
                        (nestingScope: NestingScope)
                        (mkProof: PrefixLemmaData -> Expr): FunDef =
  let tpe = fromAsn1TypeKind t.Kind
  let isTopLevel = nestingScope.parents.IsEmpty
  let paramsAcn, acnTps =
    if isTopLevel then [], []
    else
      let paramsAcn = acnExternDependenciesVariableDecode t (nestingScope.parents |> List.map snd) |> List.map (fun (_, _, v) -> v)
      let acns = collectNestedAcnChildren t.Kind
      let acnTps = acns |> List.map (fun acn -> fromAcnInsertedType acn.Type)
      paramsAcn, acnTps
  let baseId =
    if isTopLevel then t.FT_TypeDefinition.[Scala].typeName
    else ToC t.id.dropModule.AsString
  let mkSizeExpr = fun v1 c1 -> asn1SizeExpr t.acnAlignment t.Kind (Var v1) (bitIndexACN (Var c1)) 0I 0I
  generatePrefixLemmaCommon enc tpe t.acnMaxSizeInBits baseId paramsAcn acnTps mkSizeExpr nestingScope mkProof

type SeqPrefixLemmaSubproofData = {
  fd: FunDef
  decInfo: DecodeInfo
  elemTpe: Type
  existArg: Expr option
  acns: AcnChild list
  paramsAcn: Var list
}
type SeqDecodeMiscData = {
  elemTpe: Type
  paramsAcn: Var list
  acns: AcnChild list
}
type SeqChildDecodeMiscData = {
  name: string
  decInfo: DecodeInfo
  existArg: Expr option
  common: SeqDecodeMiscData
}

let seqDecodeMiscData (allParents: Asn1AcnAst.Asn1Type list)
                      (t: Asn1AcnAst.Asn1Type): SeqDecodeMiscData =
  let elemTpe = fromAsn1TypeKind t.Kind
  let acns, paramsAcn =
    let acns = fun () -> collectNestedAcnChildren t.Kind
    let paramAcns = fun () -> acnExternDependenciesVariableDecode t allParents |> List.map (fun (_, _, v) -> v)
    // Top-level definitions do not return ACNs (and can't have parameter ACNs as well)
    if allParents.IsEmpty then [], []
    else
      match t.Kind with
      | ReferenceType rt ->
        if rt.hasExtraConstrainsOrChildrenOrAcnArgs then acns (), paramAcns ()
        else
          match rt.resolvedType.ActualType.Kind with
          | OctetString _ | BitString _ -> [], paramAcns ()
          | _ -> [], []
      | Sequence _ | SequenceOf _ | Choice _ -> acns (), paramAcns ()
      | OctetString _ | BitString _ -> [], paramAcns ()
      | _ -> [], []
  {elemTpe = elemTpe; paramsAcn = paramsAcn; acns = acns}

let seqChildDecodeMiscData (allParents: Asn1AcnAst.Asn1Type list)
                           (ix: int)
                           (child: Asn1AcnAst.SeqChildInfo option)
                           (seqRecv: Expr): SeqChildDecodeMiscData =
  match child with
  | None ->
    {name = $"presence_bit_{ix + 1}"; decInfo = PrimitiveDecodeInfo booleanDecodeInfo; existArg = None; common = {elemTpe = BooleanType; paramsAcn = []; acns = []}}
  | Some (Asn1Child child) ->
    let common = seqDecodeMiscData allParents child.Type
    let decInfo = decodeInfo (Asn1 child.Type) child.Type.id child.Optionality.IsSome
    let existArg =
      match child.Optionality with
      | Some (Optional _) ->
        Some (isDefinedMutExpr (FieldSelect (seqRecv, child._scala_name)))
      | _ -> None
    {name = ToC child.Name.Value; decInfo = decInfo; existArg = existArg; common = common}
  | Some (AcnChild child) ->
    let elemTpe = fromAcnInsertedType child.Type
    let decInfo = decodeInfo (Acn child.Type) child.id false
    {name = ToC child.Name.Value; decInfo = decInfo; existArg = None; common = {elemTpe = elemTpe; paramsAcn = []; acns = []}}

type DecodePureCallHelper = {
  dec: Var
  decCall: Expr
  extracted: Expr
}

let decodePureCallPrimitiveHelper (childData: SeqChildDecodeMiscData)
                                  (child: Asn1AcnAst.SeqChildInfo option)
                                  (info: PrimitiveDecodeInfo)
                                  (decodedName: string)
                                  (codec: Expr): DecodePureCallHelper =
  assert childData.common.acns.IsEmpty
  let dec = {Var.name = decodedName; tpe = childData.common.elemTpe}
  let decCall = MethodCall {
    recv = selectCodecDecodeInfo childData.decInfo codec
    id = info.decodePureId
    args = info.extraConstArgs
    parameterless = false
  }
  let extracted =
    match child with
    | Some (Asn1Child child) ->
      match child.Type.ActualType.Kind with
      | BitString _ | OctetString _ ->
        assert (childData.common.paramsAcn.Length <= 1)
        let id = child.Type.FT_TypeDefinition.[Scala].typeName
        let ncount = childData.common.paramsAcn |> List.map (fun v ->
          // The variable may have type ULong, we need to unwrap it as the constructor expects a Long
          match v.tpe with
          | IntegerType ULong -> intCast (Var v) ULong Long
          | _ -> Var v
        )
        ClassCtor {ct = {prefix = []; id = id; tps = []; parameterless = false}; args = ncount @ [Var dec]}
      | _ ->
        assert childData.common.paramsAcn.IsEmpty
        Var dec
    | Some (AcnChild _) | None ->
      assert childData.common.paramsAcn.IsEmpty
      Var dec
  {dec = dec; decCall = decCall; extracted = extracted}

let decodePureCallComposedHelper (data: SeqDecodeMiscData)
                                 (info: ComposedDecodeInfo)
                                 (existArg: Expr option)
                                 (decodedName: string)
                                 (codec: Expr)
                                 (mkLeftCase: Var -> Var list -> Expr)
                                 (mkRightCase: Var -> Var list -> Expr): DecodePureCallHelper =
  let acnTps = data.acns |> List.map (fun v -> fromAcnInsertedType v.Type)
  let decResTpe = eitherMutTpe (IntegerType Int) (tupleType (data.elemTpe :: acnTps))
  let dec = {Var.name = decodedName; tpe = decResTpe}
  let decCall = FunctionCall {
    prefix = []; id = info.decodePureId; tps = []
    args = [codec] @ (existArg |> Option.toList) @ (data.paramsAcn |> List.map Var)
    parameterless = true
  }
  let resBdg = {Var.name = "res"; tpe = data.elemTpe}
  let decodedAcnsBdgs = acnTps |> List.indexed |> List.map (fun (ix, tpe) -> {Var.name = $"acn_{ix + 1}"; tpe = tpe})
  let subPat =
    if acnTps.IsEmpty then
      Wildcard (Some resBdg)
    else
      TuplePattern {
        binder = None
        subPatterns = Wildcard (Some resBdg) :: (decodedAcnsBdgs |> List.map (fun v -> Wildcard (Some v)))
      }
  let leftCase = mkLeftCase resBdg decodedAcnsBdgs
  let rightCase = mkRightCase resBdg decodedAcnsBdgs
  let extracted =
    MatchExpr {
      scrut = Var dec
      cases = [
        {
          pattern = ADTPattern {binder = None; id = rightMutId; subPatterns = [subPat]}
          rhs = rightCase
        }
        {
          pattern = ADTPattern {binder = None; id = leftMutId; subPatterns = [Wildcard None]}
          rhs = leftCase
        }
      ]
    }
  {dec = dec; decCall = decCall; extracted = extracted}


let generatePrefixLemmaBool (enc: Asn1Encoding)
                            (t: Asn1AcnAst.Asn1Type)
                            (nestingScope: NestingScope)
                            (boolean: Boolean): FunDef =
  let mkProof (data: PrefixLemmaData): Expr =
    UnitLit // TODO
  generatePrefixLemma enc t nestingScope mkProof

let generatePrefixLemmaInteger (enc: Asn1Encoding)
                               (t: Asn1AcnAst.Asn1Type)
                               (nestingScope: NestingScope)
                               (int: Integer): FunDef =
  let mkProof (data: PrefixLemmaData): Expr =
    UnitLit // TODO
  generatePrefixLemma enc t nestingScope mkProof

let generatePrefixLemmaChoice (enc: Asn1Encoding)
                              (t: Asn1AcnAst.Asn1Type)
                              (nestingScope: NestingScope)
                              (ch: Asn1AcnAst.Choice): FunDef =
  let mkProofSubcase (data: PrefixLemmaData) (cse: ChChildInfo): Expr =
    match cse.Type.Kind with
    | NullType _ -> UnitLit
    | _ ->
      let decMiscData = seqDecodeMiscData (t ::  (nestingScope.parents |> List.map snd)) cse.Type
      let decInfo = decodeInfo (Asn1 cse.Type) cse.Type.id false
      match decInfo with
      | PrimitiveDecodeInfo info ->
        assert decMiscData.acns.IsEmpty
        FunctionCall {
          prefix = info.prefix; id = info.prefixLemmaId; tps = []
          args = [
            selectCodecDecodeInfo decInfo (Var data.c1)
            selectCodecDecodeInfo decInfo (Var data.c2)
          ] @ info.extraConstArgs
          parameterless = true
        }
      | ComposedDecodeInfo info ->
        FunctionCall {
          prefix = []; id = info.prefixLemmaId; tps = []
          args = [Var data.c1; Var data.c2; Var data.sz] @ (decMiscData.paramsAcn |> List.map Var)
          parameterless = true
        }

  let mkProof (data: PrefixLemmaData): Expr =
    let mkUnfoldCall (cdc: Var): Expr =
      Unfold (FunctionCall {prefix = []; id = data.decodeId; tps = []; args = [Snapshot (Var cdc)] @ (data.paramsAcn |> List.map Var); parameterless = true})

    let callC1 = mkUnfoldCall data.c1
    let callC2 = mkUnfoldCall data.c2Reset

    let cases = ch.children |> List.map (fun child ->
      let tpeId = (ToC ch.typeDef[Scala].typeName) + "." + (ToC child.present_when_name) + "_PRESENT"
      let tpe = fromAsn1TypeKind child.Type.Kind
      let binder = {Var.name = child._scala_name; tpe = tpe}
      let pat = ADTPattern {binder = None; id = tpeId; subPatterns = [Wildcard (Some binder)]}
      {MatchCase.pattern = pat; rhs = mkProofSubcase data child}
    )
    let proof =
      IfExpr {
        cond = Equals (Var data.v1SizeVar, Var data.sz)
        thn = MatchExpr {scrut = Var data.v1; cases = cases}
        els = UnitLit
      }
    mkBlock [
      callC1
      callC2
      MatchExpr {
        scrut = Var data.decodingRes1
        cases = [
          {
            pattern = ADTPattern {
              binder = None
              id = rightMutId
              subPatterns = [data.subPat1]
            }
            rhs = proof
          }
          {
            pattern = ADTPattern {
              binder = None
              id = leftMutId
              subPatterns = [Wildcard None]
            }
            rhs = UnitLit
          }
        ]
      }
    ]
  generatePrefixLemma enc t nestingScope mkProof

let generatePrefixLemmaNullType (enc: Asn1Encoding)
                                (t: Asn1AcnAst.Asn1Type)
                                (nestingScope: NestingScope)
                                (nt: Asn1AcnAst.NullType): FunDef =
  let mkProof (data: PrefixLemmaData): Expr =
    UnitLit // TODO
  generatePrefixLemma enc t nestingScope mkProof

let generatePrefixLemmaEnum (enc: Asn1Encoding)
                            (t: Asn1AcnAst.Asn1Type)
                            (nestingScope: NestingScope)
                            (enm: Asn1AcnAst.Enumerated): FunDef =
  let mkProof (data: PrefixLemmaData): Expr =
    UnitLit // TODO
  generatePrefixLemma enc t nestingScope mkProof

let generatePrefixLemmaSequenceOfLike (enc: Asn1Encoding)
                                      (t: Asn1TypeOrAcnRefIA5)
                                      (nestingScope: NestingScope)
                                      (sqf: SequenceOfLike): FunDef =
  let mkSqOfLikeProof (data: PrefixLemmaData): Expr =
    UnitLit // TODO
  let tpe = fromSequenceOfLike sqf
  let mkSizeExpr =
    // TODO: Alignment?
    match sqf with
    | SqOf _ -> fun v1 c1 -> {bdgs = []; resSize = callSize (Var v1) (bitIndexACN (Var c1))}
    | _ ->
      let maxElemSz = sqf.maxElemSizeInBits enc
      fun v1 _ -> {bdgs = []; resSize = Mult (longlit maxElemSz, vecSize (Var v1))}
  let baseId, paramsAcn, acnTps =
    match t with
    | Asn1TypeOrAcnRefIA5.Asn1 t ->
      let isTopLevel = nestingScope.parents.IsEmpty
      let baseId =
        if isTopLevel then t.FT_TypeDefinition.[Scala].typeName
        else ToC t.id.dropModule.AsString
      let paramsAcn = acnExternDependenciesVariableDecode t (nestingScope.parents |> List.map snd) |> List.map (fun (_, _, v) -> v)
      let acns = collectNestedAcnChildren t.Kind
      let acnTps = acns |> List.map (fun acn -> fromAcnInsertedType acn.Type)
      baseId, paramsAcn, acnTps
    | Asn1TypeOrAcnRefIA5.AcnRefIA5 (tId, _) -> ToC tId.dropModule.AsString, [], []
  generatePrefixLemmaCommon enc tpe (sqf.maxSizeInBits enc) baseId paramsAcn acnTps mkSizeExpr nestingScope mkSqOfLikeProof


let generatePrefixLemmaSequence (enc: Asn1Encoding)
                                (t: Asn1AcnAst.Asn1Type)
                                (nestingScope: NestingScope)
                                (sq: Sequence): FunDef =
  let nbPresenceBits = countNbPresenceBits sq.children
  let childrenSizes = [0..nbPresenceBits + sq.children.Length] |> List.map (fun i -> {Var.name = $"size_1_{i}"; tpe = IntegerType Long})
  let bodyWithC1Id = "bodyWithC1"
  let bodyWithC2Id = "bodyWithC2"

  // TODO: Alignment?

  let mkUnfoldedDecodeWrapper (data: PrefixLemmaData) (id: Identifier) (c: Var): FunDef =
    let cpy = {Var.name = $"{c.name}Cpy"; tpe = c.tpe}
    let call = FunctionCall {prefix = []; id = data.decodeId; tps = []; args = [Var cpy] @ (data.paramsAcn |> List.map Var); parameterless = true}
    let body = letsIn [cpy, Snapshot (Var c)] (Unfold call)
    {
      FunDef.id = id;
      prms = []
      annots = []
      specs = []
      postcond = None
      returnTpe = UnitType
      body = body
    }

  let transformAcnEnumerated (enm: AcnReferenceToEnumerated) (info: PrimitiveDecodeInfo) (decodedVar: Var): Expr =
    // For Enumerated, we need to transform the integer to a Scala enum
    let intTpe =
      match info.tpe with
      | IntegerType tp -> tp
      | _ -> failwith $"Enumerated is not an IntegerType type"
    let branches = enm.enumerated.items |> List.map (fun i ->
      let cond = Equals (Var decodedVar, IntLit (intTpe, i.acnEncodeValue))
      let branch = ClassCtor {ct = {prefix = [enm.enumerated.typeDef.[Scala].typeName]; id = i.scala_name; tps = []; parameterless = true}; args = []}
      cond, branch
    )
    // TODO: For mkFieldCodecOriginFn, what about the "???" ?
    ifElseBranches branches (mkBlock [Check (BoolLit false); TripleQMark])


  let mkFieldCodecOriginFn (data: PrefixLemmaData) (ix: int) (child: Asn1AcnAst.SeqChildInfo option) (childData: SeqChildDecodeMiscData) (prevChildrenData: (Asn1AcnAst.SeqChildInfo option * SeqChildDecodeMiscData option) list): FunDef =
    assert (ix <> 0)
    let origC1 = data.c1
    let c1 = {Var.name = $"c1_{ix + 1}"; tpe = origC1.tpe}
    let allReturnedCodecs = [0..ix] |> List.map (fun i -> {Var.name = $"c1_{i + 2}_got"; tpe = origC1.tpe})
    // The ACN parameter passed to this "origin function", which we distinguish with prefix "prm_"
    let acnParams = childData.common.paramsAcn |> List.map (fun v -> {v with name = $"prm_{v.name}"})

    let mkAcnBinding (child: AcnChild) (decInfo: DecodeInfo) (decodedVar: Var): Var * Expr =
      let v = {Var.name = getAcnDeterminantName child.id; tpe = fromAcnInsertedType child.Type}
      match child.Type with
      | AcnReferenceToEnumerated enm ->
        let primDecInfo =
          match decInfo with
          | PrimitiveDecodeInfo info -> info
          | _ -> failwith "Enumerated ACN child decoded with a generated function?"
        v, transformAcnEnumerated enm primDecInfo decodedVar
      | _ -> v, Var decodedVar

    let makeCall (ix: int) (child: Asn1AcnAst.SeqChildInfo option) (childData: SeqChildDecodeMiscData option) (rest: Expr): Expr =
      let currCodec =
        if ix = 0 then origC1
        else allReturnedCodecs.[ix - 1]
      let retCdc = allReturnedCodecs.[ix]
      match childData with
      | None ->
        // NullType, we only bind the codecs
        letsIn [retCdc, Var currCodec] rest
      | Some childData ->
        let assertion =
          if ix = 0 then []
          else
            let overallOffset = childrenSizes |> List.take ix |> List.map Var |> plus
            [Assert (Equals (bitIndexACN (Var currCodec), plus [bitIndexACN (Var origC1); overallOffset]))]
        let call =
          match childData.decInfo with
          | PrimitiveDecodeInfo info ->
            let decData = decodePureCallPrimitiveHelper childData child info $"dec_{ix + 2}_got" (Var currCodec)
            // Note: methods from BitStream/Codec return BitStream/Codec, so we need to wrap them back into an ACN codec.
            let wrapAcn (cdcId: Identifier) (recv: Expr): Expr =
              assert (cdcId = bitStreamId || cdcId = codecId)
              if cdcId = codecId then ClassCtor {ct = acnClsTpe; args = [recv]}
              else ClassCtor {ct = acnClsTpe; args = [ClassCtor {ct = codecClsTpe; args = [recv]}]}

            let acnBinding =
              match child with
              | Some (AcnChild c) -> [mkAcnBinding c childData.decInfo decData.dec]
              | Some (Asn1Child _) | None -> []

            let mkNonAcnBdg (cdcId: Identifier): Expr =
              assert (cdcId = bitStreamId || cdcId = codecId)
              let cdcTpe = ClassType {prefix = []; id = cdcId; tps = []; parameterless = false}
              let retCdcTmp = {Var.name = $"{retCdc.name}_tmp"; tpe = cdcTpe}
              letTuple [retCdcTmp; decData.dec] decData.decCall (mkBlock [
                letsIn [retCdc, wrapAcn cdcId (Var retCdcTmp)] (letsIn acnBinding rest)
              ])
            if info.prefix = [bitStreamId] then mkNonAcnBdg bitStreamId
            else if info.prefix = [codecId] then mkNonAcnBdg codecId
            else letTuple [retCdc; decData.dec] decData.decCall (letsIn acnBinding rest)
          | ComposedDecodeInfo info ->
            let mkRightCase (decodedRes: Var) (decodedAcns: Var list): Expr =
              // The ACN values returned by this function. The variable names are important here as
              // they will be picked up by later calls that depends on these ACNs.
              let acnVars = childData.common.acns |> List.map (fun c -> {Var.name = getAcnDeterminantName c.id; tpe = fromAcnInsertedType c.Type})
              assert (acnVars.Length = decodedAcns.Length)
              if decodedAcns.IsEmpty then
                match child with
                | Some (AcnChild c) ->
                  letsIn [mkAcnBinding c childData.decInfo decodedRes] rest
                | Some (Asn1Child _) | None -> rest
              else
                // Only Asn1 children (in particular, sequences) may return ACN values
                assert (
                  match child with
                  | Some (AcnChild _) | None -> false
                  | Some (Asn1Child _) -> true
                )
                let bdgs = List.zip acnVars (decodedAcns |> List.map Var)
                letsIn bdgs rest
            let helperRes = decodePureCallComposedHelper childData.common info childData.existArg $"dec_{ix + 2}_got" (Var currCodec) (fun _ _ -> BoolLit false) mkRightCase
            letTuple [retCdc; helperRes.dec] helperRes.decCall helperRes.extracted

        mkBlock (assertion @ [call])

    ///////////////

    let theCondition =
      let lastCodec = allReturnedCodecs.[ix - 1]
      let cdcEq = Equals (Var lastCodec, Var c1)
      let acnsEq = List.zip childData.common.paramsAcn acnParams |> List.map (fun (acn1, acn2) -> Equals (Var acn1, Var acn2))
      if acnsEq.IsEmpty then cdcEq
      else And (cdcEq :: acnsEq)

    let body = List.foldBack (fun (ix, (child, childData)) rest -> makeCall ix child childData rest) (prevChildrenData |> List.indexed) theCondition
    {FunDef.id = $"{childData.name}_codec_origin"; prms = c1 :: acnParams; annots = [Opaque]; specs = []; postcond = None; returnTpe = BooleanType; body = body}

  /////////////////////////////////

  let mkFieldSubproofFn (data: PrefixLemmaData) (ix: int) (child: Asn1AcnAst.SeqChildInfo option) (childData: SeqChildDecodeMiscData) (originFnId: Identifier option): FunDef =
    let origC1 = data.c1
    let origC2Reset = data.c2Reset
    let c1, c2 =
      if ix = 0 then origC1, origC2Reset
      else {Var.name = $"c1_{ix + 1}"; tpe = origC1.tpe}, {Var.name = $"c2_{ix + 1}"; tpe = origC1.tpe}

    let overallOffset = if ix = 0 then longlit 0I else childrenSizes |> List.take ix |> List.map Var |> plus
    let childSize = childrenSizes.[ix]

    // This is None for the first child, since this function only make sense for the subsequent children
    let origCodecFnCall = originFnId |> Option.map (fun id ->
      ApplyLetRec {id = id; args = (Var c1) :: (childData.common.paramsAcn |> List.map Var)}
    )

    let specs =
      if ix = 0 then []
      else (origCodecFnCall |> Option.map Precond |> Option.toList) @ [
        Precond (Equals (selBufACN (Var c1), selBufACN (Var origC1)))
        Precond (Equals (selBufACN (Var c2), selBufACN (Var origC2Reset)))
        Precond (Equals (bitIndexACN (Var c1), plus [bitIndexACN (Var origC1); overallOffset]))
        Precond (Equals (bitIndexACN (Var c1), bitIndexACN (Var c2)))
      ]

    let slicedLemmaApp = (arrayBitRangesEqSlicedLemma
      (selBufACN (Var c1))
      (selBufACN (Var c2))
      (longlit 0I)
      (Minus (plus [bitIndexACN (Var c1); Var data.v1SizeVar], overallOffset))
      (longlit 0I)
      (plus [bitIndexACN (Var c1); Var childSize]))

    let c2Moved = {Var.name = "c2Moved"; tpe = c1.tpe}
    let c2MovedValue = withMovedBitIndexACN (Var c2) (Var childSize)
    let c2MovedAssertions = [
      Assert (Equals (bitIndexACN (Var c2Moved), plus [bitIndexACN (Var c1); Var childSize]))
      Assert (arrayBitRangesEq (selBufACN (Var c1)) (selBufACN (Var c2Moved)) (longlit 0I) (plus [bitIndexACN (Var c1); Var childSize]))
      Assert (Equals (resetAtACN (Var c2Moved) (Var c1), Var c2))
    ]

    let acnTps = childData.common.acns |> List.map (fun acn -> fromAcnInsertedType acn.Type)
    let existArgList = childData.existArg |> Option.toList
    let c1Next = {Var.name = $"c1_{ix + 2}"; tpe = origC1.tpe}
    let c2Next = {Var.name = $"c2_{ix + 2}"; tpe = origC1.tpe}
    let res1 = {Var.name = $"{childData.name}_1"; tpe = childData.common.elemTpe}
    let res2 = {Var.name = $"{childData.name}_2"; tpe = childData.common.elemTpe}
    let decodedAcn1 = acnTps |> List.indexed |> List.map (fun (ix, tpe) -> {Var.name = $"acn_1_{ix + 1}"; tpe = tpe})
    let decodedAcn2 = acnTps |> List.indexed |> List.map (fun (ix, tpe) -> {Var.name = $"acn_2_{ix + 1}"; tpe = tpe})

    let prefixLemmaApp, decDataProof1, decDataProof2, decDataPostcond1, decDataPostcond2 =
      match childData.decInfo with
      | PrimitiveDecodeInfo info ->
        assert acnTps.IsEmpty
        // Note: For variable-size BitString/OctetString, `paramsAcn` is of size 1 and contains the ACN determinant for the size
        // We however do not need it for the prefix lemma application (only to build the class wrapper)
        let prefixLemmaApp = FunctionCall {
          prefix = info.prefix; id = info.prefixLemmaId; tps = []
          args = [
            selectCodecDecodeInfo childData.decInfo (Var c1)
            selectCodecDecodeInfo childData.decInfo (Var c2Moved)
          ] @ existArgList @ info.extraConstArgs
          parameterless = true
        }
        let decData1 = decodePureCallPrimitiveHelper childData child info "dec1" (Var c1)
        let decData2 = decodePureCallPrimitiveHelper childData child info "dec2" (Var c2)
        prefixLemmaApp, decData1, decData2, decData1, decData2
      | ComposedDecodeInfo info ->
        let prefixLemmaApp = FunctionCall {
          prefix = []; id = info.prefixLemmaId; tps = []
          args = [Var c1; Var c2Moved; Var childSize] @ existArgList @ (childData.common.paramsAcn |> List.map Var)
          parameterless = true
        }

        let proofLeftCase1 (_: Var) (_: Var list): Expr =
          let body =
            let callBodyWithC1 = ApplyLetRec {id = bodyWithC1Id; args = []}
            match origCodecFnCall with
            | Some origCodecFnCall ->
              mkBlock [
                Check origCodecFnCall
                Unfold origCodecFnCall
                callBodyWithC1
              ]
            | None -> callBodyWithC1
          let proofContradiction =
            {
              FunDef.id = $"proof_unreachability_{childData.name}"
              prms = []
              annots = [Pure; Opaque; InlineOnce]
              specs = []
              postcond = Some ({Var.name = "_"; tpe = UnitType}, BoolLit false)
              returnTpe = UnitType
              body = body
            }
          LetRec {fds = [proofContradiction]; body = mkBlock [ApplyLetRec {id = proofContradiction.id; args = []}; TripleQMark]}
        let mkRightCase (resBdg: Var) (decodedAcnsBdgs: Var list): Expr =
          mkTuple ((Var resBdg) :: (decodedAcnsBdgs |> List.map Var))

        let decDataProof1 = decodePureCallComposedHelper childData.common info childData.existArg "dec1" (Var c1) proofLeftCase1 mkRightCase
        let decDataProof2 = decodePureCallComposedHelper childData.common info childData.existArg "dec2" (Var c2) (fun _ _ -> mkBlock [Check (BoolLit false); TripleQMark]) mkRightCase
        let decDataPostcond1 = decodePureCallComposedHelper childData.common info childData.existArg "dec1" (Var c1) (fun _ _ -> TripleQMark) mkRightCase
        let decDataPostcond2 = decodePureCallComposedHelper childData.common info childData.existArg "dec2" (Var c2) (fun _ _ -> TripleQMark) mkRightCase
        prefixLemmaApp, decDataProof1, decDataProof2, decDataPostcond1, decDataPostcond2

    let maxSizeInBits = child |> Option.map (fun c -> c.acnMaxSizeInBits) |> Option.defaultValue 1I
    let validateOffsLemma = validateOffsetBitsContentIrrelevancyLemma (selBitStreamACN (Var c1)) (selBufACN (Var c2)) (longlit maxSizeInBits)

    // Note: The pure decoding methods on BitStream and Codec base classes return a BitStream and Codec respectively,
    // we therefore need to select the base in such cases.
    let selBuf (cdc: Expr): Expr =
      match childData.decInfo with
      | PrimitiveDecodeInfo info ->
        if info.prefix = [bitStreamId] then selBufBitStream cdc
        else if info.prefix = [codecId] then selBufCodec cdc
        else selBufACN cdc
      | ComposedDecodeInfo _ -> selBufACN cdc

    let bitIndex (cdc: Expr): Expr =
      match childData.decInfo with
      | PrimitiveDecodeInfo info ->
        if info.prefix = [bitStreamId] then bitIndexBitStream cdc
        else if info.prefix = [codecId] then bitIndexCodec cdc
        else bitIndexACN cdc
      | ComposedDecodeInfo _ -> bitIndexACN cdc

    let accOffsets = if ix = 0 then longlit 0I else childrenSizes |> List.take ix |> List.map Var |> plus
    let v1SizeVar = {Var.name = $"size_{childData.name}"; tpe = IntegerType Long}
    let v1Size =
      match child with
      | Some (Asn1Child asn1) ->
        // Note: the bindings resulting from this size for this child may conflict with the bindings in `childrenSizes`
        // We therefore suffix these
        let res = optionalSizeExpr asn1 (Var res1) (bitIndexACN (Var c1)) 0I 0I
        renameBindingsSizeRes res $"_{childData.name}"
      | Some (AcnChild child) -> {bdgs = []; resSize = acnTypeSizeExpr child.Type}
      | None ->
        // Presence bits
        {bdgs = []; resSize = longlit 1I}

    // A small lemma to prove that the size of the child decoded from the Sequence's decode function
    // is the same as the size of the child decoded by just calling the child's decode function
    // This lemma is trivial if the child is first, so it's omitted.
    // It should go before pattern matching on the success of `res2` since it also helps prove
    // that `res2` cannot fail.
    let proofSizeEq =
      match origCodecFnCall with
      | None -> []
      | Some origCodecFnCall ->
        let body = mkBlock [
          Check origCodecFnCall
          Unfold origCodecFnCall
          ApplyLetRec {id = bodyWithC1Id; args = []}
        ]
        let proofSizeEq =
          {
            FunDef.id = $"proof_size_eq_{childData.name}"
            prms = []
            annots = [Pure; Opaque; InlineOnce]
            specs = []
            postcond = Some ({Var.name = "_"; tpe = UnitType}, Equals (Var v1SizeVar, Var childSize))
            returnTpe = UnitType
            body = body
          }
        [LetRec {fds = [proofSizeEq]; body = ApplyLetRec {id = proofSizeEq.id; args = []}}]

    let isRightConds =
      match childData.decInfo with
      | PrimitiveDecodeInfo _ -> []
      | ComposedDecodeInfo _ -> [isRightExpr (Var decDataPostcond1.dec); isRightExpr (Var decDataPostcond2.dec)]

    let conds = (isRightConds @ [
      Equals (Var v1SizeVar, Var childSize)
      Equals (Var res1, Var res2)
    ] @ (List.zip decodedAcn1 decodedAcn2 |> List.map (fun (acn1, acn2) -> Equals (Var acn1, Var acn2))) @ [
      // Note: c1Next and c2Next may be Codec/BitStream instead of ACN, we therefore need to adjust as said in the comment above
      // origC1 and origC2Reset are however ACN
      Equals (selBuf (Var c1Next), selBufACN (Var origC1))
      Equals (selBuf (Var c2Next), selBufACN (Var origC2Reset))
      Equals (bitIndex (Var c1Next), plus [bitIndexACN (Var origC1); accOffsets; Var childSize])
      Equals (bitIndex (Var c1Next), bitIndex (Var c2Next))
    ])
    let proof = mkBlock [
      slicedLemmaApp
      letsIn [c2Moved, c2MovedValue] (mkBlock (
        c2MovedAssertions @ [prefixLemmaApp; validateOffsLemma] @
        [letTuple [c1Next; decDataProof1.dec] decDataProof1.decCall (
          letTuple [c2Next; decDataProof2.dec] decDataProof2.decCall (
            letTuple (res1 ::decodedAcn1) decDataProof1.extracted (
              letsIn (v1Size.bdgs @ [v1SizeVar, v1Size.resSize]) (mkBlock (
                proofSizeEq @ [
                letTuple (res2 ::decodedAcn2) decDataProof2.extracted (
                  (mkBlock (conds |> List.map Check))
                )]
              ))
            )
          )
        )]
      ))
    ]

    let postcondExpr =
      letTuple [c1Next; decDataPostcond1.dec] decDataPostcond1.decCall (mkBlock [
        letTuple [c2Next; decDataPostcond2.dec] decDataPostcond2.decCall (mkBlock [
          letTuple (res1 ::decodedAcn1) decDataPostcond1.extracted (mkBlock [
            letTuple (res2 ::decodedAcn2) decDataPostcond2.extracted (
              letsIn (v1Size.bdgs @ [v1SizeVar, v1Size.resSize]) (And conds)
            )
          ])
        ])
      ])

    {
      FunDef.id = $"proof_{ToC childData.name}"
      prms = if ix = 0 then childData.common.paramsAcn else [c1; c2] @ childData.common.paramsAcn
      annots = [Opaque; InlineOnce]
      specs = specs
      postcond = Some ({Var.name = "_"; tpe = UnitType}, postcondExpr)
      returnTpe = UnitType
      body = proof
    }

  let mkSubfieldProofCall (data: PrefixLemmaData) (ix: int) (child: Asn1AcnAst.SeqChildInfo option) (proofIdAndChildData: (Identifier * SeqChildDecodeMiscData) option) (originFnId: Identifier option): Expr =
    let origC1 = data.c1
    let origC2Reset = data.c2Reset
    let c1Prev, c2Prev =
      if ix = 0 then origC1, origC2Reset
      else {Var.name = $"c1_{ix}"; tpe = origC1.tpe}, {Var.name = $"c2_{ix}"; tpe = origC1.tpe}
    let c1, c2 = {Var.name = $"c1_{ix + 1}"; tpe = origC1.tpe}, {Var.name = $"c2_{ix + 1}"; tpe = origC1.tpe}

    let mkAcnBinding (decInfo: DecodeInfo) (dec1: Var) (c: AcnChild): Expr =
      // This ACN variable will be picked up by the later decode functions. It is important it to be bound with that specific name
      // Note that the calls with c1 and c2 yield the same values, we arbitrarily pick the value from the call with c1.
      let v = {Var.name = getAcnDeterminantName c.id; tpe = fromAcnInsertedType c.Type}
      match c.Type with
      | AcnReferenceToEnumerated enm ->
        let primDecInfo =
          match decInfo with
          | PrimitiveDecodeInfo info -> info
          | _ -> failwith "Enumerated ACN child decoded with a generated function?"
        let transformed = transformAcnEnumerated enm primDecInfo dec1
        letsIn [v, transformed] (mkBlock [])
      | _ ->
        match decInfo with
        | PrimitiveDecodeInfo _ -> letsIn [v, Var dec1] (mkBlock [])
        | ComposedDecodeInfo _ ->
          let bdg = {Var.name = "bdg"; tpe = v.tpe}
          let extracted = eitherMutMatchExpr (Var dec1) None (mkBlock [Check (BoolLit false); TripleQMark]) (Some bdg) (Var bdg)
          letsIn [v, extracted] (mkBlock [])

    match proofIdAndChildData with
    | None ->
      // NullType case: we assign the codecs to the previous ones
      // TODO: handle case where there is a bitpattern
      letsIn [(c1, Snapshot (Var c1Prev)); (c2, Snapshot (Var c2Prev))] (mkBlock [])
    | Some (proofId, childData) ->
      let codecArgs = if ix = 0 then [] else [Var c1Prev; Var c2Prev]
      let existArgList = childData.existArg |> Option.toList

      // This is None for the first child, since this function only make sense for the subsequent children
      let origCodecFnCheck = originFnId |> Option.toList |> List.collect (fun id ->
        let call = ApplyLetRec {id = id; args = (Var c1Prev) :: (childData.common.paramsAcn |> List.map Var)}
        [Unfold call; Assert call]
      )

      let callsBdgs =
        match childData.decInfo with
        | PrimitiveDecodeInfo info ->
          let dec1 = {Var.name = $"dec1_{ix + 1}"; tpe = childData.common.elemTpe}
          let dec2 = {Var.name = $"dec2_{ix + 1}"; tpe = childData.common.elemTpe}
          let dec1Call = MethodCall {recv = selectCodecDecodeInfo childData.decInfo (Var c1Prev); id = info.decodePureId; args = existArgList @ info.extraConstArgs; parameterless = false}
          let dec2Call = MethodCall {recv = selectCodecDecodeInfo childData.decInfo (Var c2Prev); id = info.decodePureId; args = existArgList @ info.extraConstArgs; parameterless = false}

          // Note: methods from BitStream/Codec return BitStream/Codec, so we need to wrap them back into an ACN codec.
          let callsBdgs =
            let wrapAcn (cdcId: Identifier) (recv: Expr): Expr =
              assert (cdcId = bitStreamId || cdcId = codecId)
              if cdcId = codecId then ClassCtor {ct = acnClsTpe; args = [recv]}
              else ClassCtor {ct = acnClsTpe; args = [ClassCtor {ct = codecClsTpe; args = [recv]}]}
            let mkNonAcnBdg (cdcId: Identifier): Expr =
              assert (cdcId = bitStreamId || cdcId = codecId)
              let cdcTpe = ClassType {prefix = []; id = cdcId; tps = []; parameterless = false}
              let c1Tmp, c2Tmp = {Var.name = $"c1_{ix + 1}_tmp"; tpe = cdcTpe}, {Var.name = $"c2_{ix + 1}_tmp"; tpe = cdcTpe}
              letTuple [c1Tmp; dec1] dec1Call (mkBlock [
                letTuple [c2Tmp; dec2] dec2Call (mkBlock [
                  letsIn [(c1, wrapAcn cdcId (Var c1Tmp)); (c2, wrapAcn cdcId (Var c2Tmp))] (mkBlock [])
                ])
              ])
            if info.prefix = [bitStreamId] then mkNonAcnBdg bitStreamId
            else if info.prefix = [codecId] then mkNonAcnBdg codecId
            else
              letTuple [c1; dec1] dec1Call (mkBlock [
                letTuple [c2; dec2] dec2Call (mkBlock [])])

          let acnBinding =
            match child with
            | Some (AcnChild c) -> mkAcnBinding childData.decInfo dec1 c
            | Some (Asn1Child _) | None -> mkBlock []

          mkBlock [
            callsBdgs
            acnBinding
          ]
        | ComposedDecodeInfo info ->
          // The ACN values returned by this function. Since the calls with c1 and c2 yield the same values, we arbitrarily bind
          // these variables to the first call. The variable name are important here as they will be picked up by later calls
          // that depends on these ACNs.
          let acnVars = childData.common.acns |> List.map (fun c -> {Var.name = getAcnDeterminantName c.id; tpe = fromAcnInsertedType c.Type})
          let acnTps = acnVars |> List.map (fun v -> v.tpe)
          let decResTpe = eitherMutTpe (IntegerType Int) (tupleType (childData.common.elemTpe :: acnTps))
          let dec1 = {Var.name = $"dec1_{ix + 1}"; tpe = decResTpe}
          let dec2 = {Var.name = $"dec2_{ix + 1}"; tpe = decResTpe}
          let dec1Call = FunctionCall {
            prefix = []; id = info.decodePureId; tps = []
            args = [Var c1Prev] @ existArgList @ (childData.common.paramsAcn |> List.map Var)
            parameterless = true
          }
          let dec2Call = FunctionCall {
            prefix = []; id = info.decodePureId; tps = []
            args = [Var c2Prev] @ existArgList @ (childData.common.paramsAcn |> List.map Var)
            parameterless = true
          }
          let acnBinding =
            if acnVars.IsEmpty then
              match child with
              | Some (AcnChild c) -> mkAcnBinding childData.decInfo dec1 c
              | Some (Asn1Child _) | None -> mkBlock []
            else
              // Only Asn1 children (in particular, sequences) may return ACN values
              assert (
                match child with
                | Some (AcnChild _) | None -> false
                | Some (Asn1Child _) -> true
              )
              let decTmp = {Var.name = $"decTmp_{ix + 1}"; tpe = tupleType (childData.common.elemTpe :: acnTps)}
              let decTmpValue = eitherMutMatchExpr (Var dec1) None (mkBlock [Check (BoolLit false); TripleQMark]) (Some decTmp) (Var decTmp)
              let acnBdgs = acnVars |> List.indexed |> List.map (fun (ix, v) -> v, TupleSelect (Var decTmp, ix + 2))
              letsIn ((decTmp, decTmpValue) :: acnBdgs) (mkBlock [])

          letTuple [c1; dec1] dec1Call (mkBlock [
            letTuple [c2; dec2] dec2Call acnBinding])

      mkBlock (origCodecFnCheck @ [
        ApplyLetRec {id = proofId; args = codecArgs @ (childData.common.paramsAcn |> List.map Var)}
        callsBdgs
      ])

  ////////////////////////////

  let mkSeqProof (data: PrefixLemmaData): Expr =
    let bodyWithC1 = mkUnfoldedDecodeWrapper data bodyWithC1Id data.c1
    let bodyWithC2 = mkUnfoldedDecodeWrapper data bodyWithC2Id data.c2Reset

    let isNullType (c: SeqChildInfo option): bool =
      match c with
      | Some (Asn1Child asn1) ->
        match asn1.Type.Kind with
        | NullType _ -> true
        | _ -> false
      | _ -> false

    let construct (previousChildData: (Asn1AcnAst.SeqChildInfo option * SeqChildDecodeMiscData option) list,
                   originFnsAcc: FunDef list,
                   subproofFnsAcc: FunDef list,
                   subproofCallsAcc: Expr list) (ix: int, child: SeqChildInfo option) =
      // `child` is None for presence bits

      if isNullType child then
        // TODO: Not quite, if the NT has an encoding pattern, there is some logic to do
        // This only does the codec bindings
        let subproofCall = mkSubfieldProofCall data ix child None None
        previousChildData @ [child, None], originFnsAcc, subproofFnsAcc, subproofCallsAcc @ [subproofCall]
      else
        let childData = seqChildDecodeMiscData (t :: (nestingScope.parents |> List.map snd)) ix child (Var data.v1)
        let originFn =
          if ix = 0 then None
          else Some (mkFieldCodecOriginFn data ix child childData previousChildData)
        let originFnId = originFn |> Option.map (fun fd -> fd.id)
        let subproofFn = mkFieldSubproofFn data ix child childData originFnId
        let subproofCall = mkSubfieldProofCall data ix child (Some (subproofFn.id, childData)) originFnId
        previousChildData @ [child, Some childData], originFnsAcc @ (originFn |> Option.toList), subproofFnsAcc @ [subproofFn], subproofCallsAcc @ [subproofCall]

    let _, originFns, subproofFns, subproofCalls = (
      (List.replicate nbPresenceBits (None: SeqChildInfo option) @ (sq.children |> List.map Some)) |>
      List.indexed |>
      List.fold (construct) ([], [], [], []))

    let finalCheck =
      let ixChecks = Check (Equals (bitIndexACN (Var data.c1Res), bitIndexACN (Var data.c2Res)))
      let decodedChecks = Check (Equals (Var data.v1, Var data.v2))
      let acnChecks = List.zip data.decodedAcn1 data.decodedAcn2 |> List.map (fun (acn1, acn2) -> Check (Equals (Var acn1, Var acn2)))
      MatchExpr {
        scrut = Var data.decodingRes2
        cases = [
          {
            pattern = ADTPattern {
              binder = None
              id = rightMutId
              subPatterns = [data.subPat2]
            }
            rhs = mkBlock (ixChecks :: decodedChecks :: acnChecks)
          }
          {
            pattern = ADTPattern {
              binder = None
              id = leftMutId
              subPatterns = [Wildcard None]
            }
            rhs = Check (BoolLit false)
          }
        ]
      }
    let body = mkBlock ([
      ApplyLetRec {id = bodyWithC1.id; args = []}
    ] @ subproofCalls @ [
      ApplyLetRec {id = bodyWithC2.id; args = []}
      finalCheck
    ])
    let proof = LetRec {fds = originFns @ subproofFns; body = body}

    let rightMutCase =
      IfExpr {
        cond = Equals (Var data.v1SizeVar, Var data.sz)
        thn = proof
        els = UnitLit
      }
    LetRec {
      fds = [bodyWithC1; bodyWithC2]
      body =  MatchExpr {
        scrut = Var data.decodingRes1
        cases = [
          {
            pattern = ADTPattern {
              binder = None
              id = rightMutId
              subPatterns = [data.subPat1]
            }
            rhs = rightMutCase
          }
          {
            pattern = ADTPattern {
              binder = None
              id = leftMutId
              subPatterns = [Wildcard None]
            }
            rhs = UnitLit
          }
        ]
      }
    }

  generatePrefixLemma enc t nestingScope mkSeqProof


let wrapAcnFuncBody (r: Asn1AcnAst.AstRoot)
                    (deps: Asn1AcnAst.AcnInsertedFieldDependencies)
                    (t: Asn1AcnAst.Asn1Type)
                    (body: string)
                    (codec: Codec)
                    (nestingScope: NestingScope)
                    (outerSel: CallerScope)
                    (recSel: CallerScope): FunDef list * Expr =
  assert recSel.arg.path.IsEmpty
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = {Var.name = "oldCdc"; tpe = ClassType codecTpe}
  let tpe = fromAsn1TypeKind t.Kind
  let errTpe = IntegerType Int
  let recPVal = {Var.name = recSel.arg.receiverId; tpe = tpe}
  let precond = [Precond (validateOffsetBitsACN (Var cdc) (longlit t.acnMaxSizeInBits))]
  let isValidFuncName = $"{t.FT_TypeDefinition.[Scala].typeName}_IsConstraintValid"
  let baseId = ToC t.id.dropModule.AsString
  // Computing external ACN dependencies for decoding
  // We also pass them to the encoding function because its postcondition
  // refers to the decoding function, which needs them
  let paramsAcnInfo = acnExternDependenciesVariableDecode t (nestingScope.parents |> List.map snd)
  let paramsAcn = paramsAcnInfo |> List.map (fun (_, _, v) -> v)
  // All ACN fields present in this SEQUENCE, including nested ones
  // Encoding functions will return them so that we can refer to them in the postcondition when calling the decoding function
  let acns = collectNestedAcnChildren t.Kind
  let acnsVars = acns |> List.map (fun c -> {Var.name = getAcnDeterminantName c.id; tpe = fromAcnInsertedType c.Type})
  let acnTps = acnsVars |> List.map (fun v -> v.tpe)

  match codec with
  | Encode ->
    let retTpe = tupleType (IntegerType Int :: acnTps)
    let outerPVal = SelectionExpr (joinedSelection outerSel.arg)
    let cstrCheck =
      let scrut = FunctionCall {prefix = []; id = isValidFuncName; tps = []; args = [Var recPVal]; parameterless = true}
      let leftBdg = {Var.name = "l"; tpe = errTpe}
      let leftBody = Return (leftExpr errTpe retTpe (Var leftBdg))
      eitherMatchExpr scrut (Some leftBdg) leftBody None (mkBlock [])

    let body = letsGhostIn [oldCdc, Snapshot (Var cdc)] (mkBlock ([
      cstrCheck
      EncDec body
      ClassCtor (right errTpe retTpe (mkTuple (int32lit 0I :: (acnsVars |> List.map Var))))
    ]))

    let outermostPVal = {Var.name = "pVal"; tpe = fromAsn1TypeKind (nestingScope.parents |> List.last |> snd).Kind}
    let acnExtVars = acnExternDependenciesVariableEncode t nestingScope |> Option.toList
    let resPostcond = {Var.name = "res"; tpe = eitherTpe errTpe retTpe}
    let decodePureId = $"{baseId}_ACN_Decode_pure"
    let szRecv = {Var.name = recSel.arg.asLastOrSelf.receiverId; tpe = tpe}
    let sz =
      match t.Kind with
      | Choice _ | SequenceOf _ -> {bdgs = []; resSize = callSize (Var szRecv) (bitIndexACN (Old (Var cdc)))}
      | _ ->
        // TODO: For Sequence, we don't know whether we have extra ACN fields or not.
        // If we do, we must "inline" the size definition which will contain the size of these extra ACN fields and if not, we can just call size
        // We always inline here since it is ok even if we don't have extra ACN fields
        asn1SizeExpr t.acnAlignment t.Kind (Var szRecv) (bitIndexACN (Old (Var cdc))) 0I 0I
    let postcondExpr = generateEncodePostcondExprCommon r tpe t.acnMaxSizeInBits recSel.arg resPostcond acnTps sz [] decodePureId (paramsAcn |> List.map Var)
    let fd = {
      id = $"{baseId}_ACN_Encode"
      prms = [cdc; outermostPVal] @ acnExtVars @ paramsAcn @ [recPVal]
      specs = precond
      annots = [Opaque; InlineOnce]
      postcond = Some (resPostcond, postcondExpr)
      returnTpe = eitherTpe errTpe retTpe
      body = body
    }

    let call =
      let scrut = FunctionCall {prefix = []; id = fd.id; tps = []; args = [Var cdc; Var outermostPVal] @ ((acnExtVars @ paramsAcn) |> List.map Var) @ [outerPVal]; parameterless = true}
      let leftBdg = {Var.name = "l"; tpe = errTpe}
      // TODO: FIXME: the right type must be the outside type!!!
      let leftHACK = ClassCtor {ct = {prefix = []; id = leftId; tps = []; parameterless = false}; args = [Var leftBdg]}
      let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
      if acnsVars.IsEmpty then
        eitherMatchExpr scrut (Some leftBdg) leftBody None UnitLit
      else
        // Since the ACN vars name may be capitalized (which can conflict with pattern matching), we use dummy var names in the binding
        let acnVarsPatBdg = acnTps |> List.indexed |> List.map (fun (ix, tpe) -> {Var.name = $"v{ix + 1}"; tpe = tpe})
        let rightTuplePat = TuplePattern {binder = None; subPatterns = Wildcard None :: (acnVarsPatBdg |> List.map (fun v -> Wildcard (Some v)))}
        let rightBody = mkTuple (acnVarsPatBdg |> List.map Var)
        let call = MatchExpr {
          scrut = scrut
          cases = [
            {
              pattern = ADTPattern {binder = None; id = leftId; subPatterns = [Wildcard (Some leftBdg)]}
              rhs = leftBody
            }
            {
              pattern = ADTPattern {binder = None; id = rightId; subPatterns = [rightTuplePat]}
              rhs = rightBody
            }
          ]
        }
        let resVar = {Var.name = $"res_{outerSel.arg.asIdentifier}"; tpe = retTpe}
        let acnVarsBdg =
          if acnsVars.Tail.IsEmpty then [(acnsVars.Head, Var resVar)]
          else acnsVars |> List.indexed |> List.map (fun (ix, v) -> (v, TupleSelect (Var resVar, ix + 1)))
        letsIn ((resVar, call) :: acnVarsBdg) (mkBlock [])

    [fd], call
  | Decode ->
    let retTpe = tupleType (tpe :: acnTps)
    let fnRetTpe = eitherMutTpe errTpe retTpe
    let outerPVal = {Var.name = outerSel.arg.asIdentifier; tpe = tpe}
    let retInnerFd =
      let retVal = mkTuple (Var recPVal :: (acnsVars |> List.map Var))
      let scrut = FunctionCall {prefix = []; id = isValidFuncName; tps = []; args = [Var recPVal]; parameterless = true}
      let leftBdg = {Var.name = "l"; tpe = errTpe}
      let leftBody = leftMutExpr errTpe retTpe (Var leftBdg)
      let rightBody = rightMutExpr errTpe retTpe retVal
      eitherMatchExpr scrut (Some leftBdg) leftBody None rightBody
    let body = letsGhostIn [oldCdc, Snapshot (Var cdc)] (mkBlock [EncDec body; retInnerFd])

    let resPostcond = {Var.name = "res"; tpe = fnRetTpe}
    let szRecv = {Var.name = "resVal"; tpe = tpe}
    let sz =
      match t.Kind with
      | Choice _ | SequenceOf _ -> {bdgs = []; resSize = callSize (Var szRecv) (bitIndexACN (Old (Var cdc)))}
      | _ ->
        // TODO: For Sequence, we don't know whether we have extra ACN fields or not.
        // If we do, we must "inline" the size definition which will contain the size of these extra ACN fields and if not, we can just call size
        // We always inline here since it is ok even if we don't have extra ACN fields
        asn1SizeExpr t.acnAlignment t.Kind (Var szRecv) (bitIndexACN (Old (Var cdc))) 0I 0I
    let cstrIsValid = isRightExpr (FunctionCall {prefix = []; id = isValidFuncName; tps = []; args = [Var szRecv]; parameterless = true})
    let postcondExpr =
      if acns.IsEmpty then
        generateDecodePostcondExprCommon r resPostcond szRecv sz [] [cstrIsValid]
      else
        assert (match t.Kind with Sequence _ -> true | _ -> false)
        let oldCdc = Old (Var cdc)
        let rightBody = letsIn sz.bdgs (And [
          Equals (selBufACN oldCdc, selBufACN (Var cdc))
          Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz.resSize])
          cstrIsValid
        ])
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
                  subPatterns = Wildcard (Some szRecv) :: (List.replicate acns.Length (Wildcard None))
                }]
              }
              rhs = rightBody
            }
          ]
        }

    let fd = {
      id = $"{baseId}_ACN_Decode"
      prms = [cdc] @ paramsAcn
      specs = precond
      annots = [Opaque; InlineOnce]
      postcond = Some (resPostcond, postcondExpr)
      returnTpe = fnRetTpe
      body = body
    }

    let call =
      let scrut = FunctionCall {prefix = []; id = fd.id; tps = []; args = [Var cdc] @ (paramsAcn |> List.map Var); parameterless = true}
      let leftBdg = {Var.name = "l"; tpe = errTpe}
      // TODO: FIXME: the right type must be the outside type!!!
      let leftHACK = ClassCtor {ct = {prefix = []; id = leftMutId; tps = []; parameterless = false}; args = [Var leftBdg]}
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

    let fdPure =
      let varCpy = {Var.name = "cpy"; tpe = ClassType codecTpe}
      let varRes = {Var.name = "res"; tpe = fnRetTpe}
      let pureBody = (letsIn
        [varCpy, Snapshot (Var cdc);
        varRes, FunctionCall {prefix = []; id = fd.id; tps = []; args = [Var varCpy] @ (paramsAcn |> List.map Var); parameterless = true}]
        (mkTuple [Var varCpy; Var varRes]))
      {
        FunDef.id = $"{baseId}_ACN_Decode_pure"
        prms = [cdc] @ paramsAcn
        annots = [GhostAnnot; Pure]
        specs = precond
        postcond = None
        returnTpe = tupleType [ClassType codecTpe; fnRetTpe]
        body = pureBody
      }

    [fd; fdPure], ret

let annotateSequenceChildStmt (enc: Asn1Encoding) (snapshots: Var list) (cdc: Var) (oldCdc: Var) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec) (rest: Expr): Expr =
  let nbPresenceBits = countNbPresenceBits pg.sq.children
  let nbTotalChildren = nbPresenceBits + pg.sq.children.Length
  assert (snapshots.Length = nbTotalChildren)
  assert (stmts.Length = nbTotalChildren)
  assert (pg.children.Length = pg.sq.children.Length)
  // Apparently, not needed?
  (*
  let rec assertionsConditions (info: Asn1AcnAst.SeqChildInfo): Expr option =
    let intRangeAssertion (int: BigIntegerUperRange): Expr option =
      match int with
      | Concrete (min, max) ->
        // TODO: The RT library does not add 1, why?
        let call = getBitCountUnsigned (ulonglit (max - min))
        // TODO: Case min = max?
        let nBits = if max = min then 0I else bigint (ceil ((log (double (max - min))) / (log 2.0)))
        let cond = Equals (call, int32lit nBits)
        Some cond
      | _ -> None
    match info with
    | Asn1Child child ->
      match child.Type.Kind with
      | Integer int -> intRangeAssertion int.uperRange
      | _ -> None
    | AcnChild child ->
      match child.Type with
      | AcnInteger int -> intRangeAssertion int.uperRange
      | _ -> None

  let addAssert (tpe: Asn1AcnAst.SeqChildInfo): Expr =
    assertionsConditions tpe |> Option.map (fun cond -> Assert cond)
                             |> Option.defaultValue (mkBlock [])
  *)
  let outerMaxSize = pg.outerMaxSize enc
  let thisMaxSize = (bigint nbPresenceBits) + (pg.sq.children |> List.sumBy (fun c -> c.maxSizeInBits enc))
  let fstSnap = snapshots.Head
  let isNested = pg.nestingLevel > 0I
  let childrenWithPresenceBits = (List.replicate nbPresenceBits (None: SequenceChildProps option)) @ (pg.children |> List.map Some)
  let sizeRess =
    childrenWithPresenceBits |>
    List.indexed |>
    List.map (fun (ix, c) ->
      let childVar = {Var.name = $"size_{pg.nestingIx + bigint ix}"; tpe = IntegerType Long}
      match c with
      | Some info ->
        let recv =
          match codec with
          | Encode -> SelectionExpr (joinedSelection info.sel)
          | Decode -> SelectionExpr info.sel.asIdentifier
        let resSize = seqSizeExprHelperChild info.info (bigint ix) (Some recv) (bitIndexACN (Var snapshots.[ix])) pg.nestingLevel pg.nestingIx
        {extraBdgs = resSize.bdgs; childVar = childVar; childSize = resSize.resSize}
      | None ->
        // presence bits
        {extraBdgs = []; childVar = childVar; childSize = longlit 1I}
    )

  let annotatePostPreciseSize (ix: int) (snap: Var) (child: SequenceChildProps option): Expr =
    let previousSizes = sizeRess |> List.take ix |> List.map (fun res -> Var res.childVar)
    let sizeRes = sizeRess.[ix]
    let chk = Check (Equals (bitIndexACN (Var cdc), plus (bitIndexACN (Var oldCdc) :: previousSizes @ [Var sizeRes.childVar])))
    letsGhostIn sizeRes.allBindings (Ghost chk)

  let annotatePost (ix: int) (snap: Var) (child: SequenceChildProps option) (stmt: string option) (offsetAcc: bigint): Expr =
    let sz = child |> Option.map (fun c -> c.info.maxSizeInBits enc) |> Option.defaultValue 1I
    let relativeOffset = offsetAcc - (pg.maxOffset enc)
    let offsetCheckOverall = Check (Leq (bitIndexACN (Var cdc), plus [bitIndexACN (Var oldCdc); (longlit offsetAcc)]))
    let offsetCheckNested =
      if isNested then [Check (Leq (bitIndexACN (Var cdc), plus [bitIndexACN (Var fstSnap); longlit relativeOffset]))]
      else []
    let bufCheck =
      match codec with
       | Encode -> [Check ((Equals (selBufLengthACN (Var cdc), selBufLengthACN (Var oldCdc))))]
       | Decode -> [Check ((Equals (selBufACN (Var cdc), selBufACN (Var oldCdc))))]
    let offsetWidening =
      match pg.siblingMaxSize enc with
      | Some siblingMaxSize when ix = nbTotalChildren - 1 && siblingMaxSize <> thisMaxSize ->
        let diff = siblingMaxSize - thisMaxSize
        [
          Check (Leq (bitIndexACN (Var cdc), plus [bitIndexACN (Var oldCdc); longlit (offsetAcc + diff)]))
          Check (Leq (bitIndexACN (Var cdc), plus [bitIndexACN (Var fstSnap); longlit (relativeOffset + diff)]))
        ]
      | _ -> []
    let checks = offsetCheckOverall :: offsetCheckNested @ bufCheck @ offsetWidening
    let validateOffsetLemma =
      if stmt.IsSome && ix < nbTotalChildren - 1 then
        [validateOffsetBitsIneqLemma (selBitStreamACN (Var snap)) (selBitStreamACN (Var cdc)) (longlit (outerMaxSize - offsetAcc + sz)) (longlit sz)]
      else []
    let preciseSize = annotatePostPreciseSize ix snap child
    mkBlock [Ghost (mkBlock (validateOffsetLemma @ checks)); preciseSize]

  let annotate (ix: int, (snap: Var, child: SequenceChildProps option, stmt: string option)) (offsetAcc: bigint, rest: Expr): bigint * Expr =
    let sz = child |> Option.map (fun c -> c.info.maxSizeInBits enc) |> Option.defaultValue 1I
    //assert (thisMaxSize <= (pg.siblingMaxSize enc |> Option.defaultValue thisMaxSize)) // TODO: Somehow does not always hold with UPER?
    // let preAnnots =
    //   match stmt, child with
    //   | Some _, Some c -> [addAssert c.info]
    //   | _ -> []
    let postAnnots = annotatePost ix snap child stmt offsetAcc
    let encDec = stmt |> Option.map EncDec |> Option.toList
    let body = mkBlock (encDec @ [postAnnots; rest])
    offsetAcc - sz, LetGhost {bdg = snap; e = Snapshot (Var cdc); body = body}

  let stmts = List.zip3 snapshots childrenWithPresenceBits stmts |> List.indexed
  List.foldBack annotate stmts ((pg.maxOffset enc) + thisMaxSize, rest) |> snd

let generateSequenceChildProof (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec): string list =
  if stmts.IsEmpty then stmts |> List.choose id
  else
    let codecTpe = runtimeCodecTypeFor enc
    let cdc = {Var.name = $"codec"; tpe = ClassType codecTpe}
    let oldCdc = {Var.name = $"oldCdc"; tpe = ClassType codecTpe}
    if enc = ACN then
      let nbPresenceBits = countNbPresenceBits pg.sq.children
      let snapshots = [1 .. nbPresenceBits + pg.sq.children.Length] |> List.map (fun i -> {Var.name = $"codec_{pg.nestingLevel}_{pg.nestingIx + bigint i}"; tpe = ClassType codecTpe})
      let wrappedStmts = annotateSequenceChildStmt enc snapshots cdc oldCdc stmts pg codec
      let postCondLemmas =
        let cond = Leq (bitIndexACN (Var cdc), plus [bitIndexACN (Var snapshots.Head); longlit (pg.outerMaxSize enc)])
        Ghost (Check cond)
      let expr = wrappedStmts (mkBlock [postCondLemmas])
      let exprStr = show (ExprTree expr)
      [exprStr]
    else
      let expr = mkBlock (stmts |> List.choose id |> List.map EncDec)
      let exprStr = show (ExprTree expr)
      [exprStr]

let generateSequenceProof (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): Expr option =
  if sq.children.IsEmpty then None
  else
    let codecTpe = runtimeCodecTypeFor enc
    let oldCdc = {Var.name = "oldCdc"; tpe = ClassType codecTpe}
    let recv =
      match codec with
      | Encode -> SelectionExpr (joinedSelection sel)
      | Decode -> SelectionExpr sel.asIdentifier

    let nbPresenceBits = countNbPresenceBits sq.children
    let childrenSizes = [0 .. nbPresenceBits + sq.children.Length - 1] |> List.map (fun i -> {name = $"size_{i}"; tpe = IntegerType Long})
    // For "nested sequences", we always inline the size definition instead of calling the corresponding `size` function
    // since we do not know whether we have extra ACN fields or not. See the TODO in `wrapAcnFuncBody`
    // Therefore, in such case, we should not assert that the size of the current Sequence is equal to the sum of the size of its children
    let sizeCheck =
      if not nestingScope.parents.IsEmpty then None
      else
        let recvSz = callSize recv (bitIndexACN (Var oldCdc))
        Some (Ghost (Check (Equals (recvSz, plus (childrenSizes |> List.map Var )))))

    if codec = Decode || (not r.args.stainlessInvertibility) then sizeCheck
    else
      assert sel.path.IsEmpty
      let codecTpe = runtimeCodecTypeFor enc
      let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
      let oldCdc = {Var.name = "oldCdc"; tpe = ClassType codecTpe}
      let seqTpe = fromAsn1TypeKind t.Kind
      let selVar = {Var.name = sel.receiverId; tpe = seqTpe}
      let snapshots = [1 .. nbPresenceBits + sq.children.Length] |> List.map (fun i -> {Var.name = $"codec_0_{i}"; tpe = ClassType codecTpe})
      let fstSnap = snapshots.Head
      let transitiveLemmas =
        if snapshots.Length < 2 then []
        else List.rep2 snapshots |> List.map (fun (s1, s2) -> lemmaIsPrefixTransitive (Var s1) (Var s2) (Var cdc)) |> List.rev

      let childrenWithPresenceBits = sq.children |> List.choose (fun child ->
        match child with
        | AcnChild _ -> None
        | Asn1Child asn1 ->
          match asn1.Optionality with
          | Some (Optional opt) when opt.acnPresentWhen.IsNone -> Some child
          | _ -> None
      )
      assert (childrenWithPresenceBits.Length = nbPresenceBits)

      let mkFieldSubproof (ix: int) (child: Asn1AcnAst.SeqChildInfo option): Expr =
        let snap = snapshots.[ix]
        let nextSnap = if ix = nbPresenceBits + sq.children.Length - 1 then cdc else snapshots.[ix + 1]
        let childSize = childrenSizes.[ix]
        // The codec returned by the pure decoding function for ix > 0. For ix = 0, this is bound to codec.resetAt(codec_0_1)
        let c = {Var.name = $"c{ix + 1}"; tpe = ClassType codecTpe}
        let cTmp = {Var.name = $"c{ix + 2}Tmp"; tpe = ClassType codecTpe}
        let nextC = {Var.name = $"c{ix + 2}"; tpe = ClassType codecTpe}

        let childData = seqChildDecodeMiscData (t :: (nestingScope.parents |> List.map snd)) ix child (Var selVar)
        let res = {Var.name = $"res_{childData.name}"; tpe = childData.common.elemTpe}
        let childAcns = childData.common.acns |> List.map (fun acn -> {Var.name = getAcnDeterminantName acn.id; tpe = fromAcnInsertedType acn.Type})
        let decodedAcn = childAcns |> List.map (fun v -> {v with name = $"childDec_{v.name}"})

        let maxSizeInBits = child |> Option.map (fun c -> c.acnMaxSizeInBits) |> Option.defaultValue 1I

        let prefixLemmaApp, decData, cNextWrapped =
          match childData.decInfo with
          | PrimitiveDecodeInfo info ->
            let prefixLemmaApp = FunctionCall {
              prefix = info.prefix; id = info.prefixLemmaId; tps = []
              args = [
                selectCodecDecodeInfo childData.decInfo (resetAtACN (Var nextSnap) (Var snap))
                selectCodecDecodeInfo childData.decInfo (Var cdc)
              ] @ (childData.existArg |> Option.toList) @ info.extraConstArgs
              parameterless = true
            }
            let decData = decodePureCallPrimitiveHelper childData child info $"dec_{ix + 1}" (Var c)
            // The pure decoding methods on BitStream and Codec base classes return a BitStream and Codec
            // so we need to wrap it in an ACN.
            let cNextWrapped =
              if info.prefix = [bitStreamId] then acnWrapperBitstream (Var cTmp)
              else if info.prefix = [codecId] then acnWrapperCodec (Var cTmp)
              else Var cTmp
            prefixLemmaApp, decData, cNextWrapped
          | ComposedDecodeInfo info ->
            let prefixLemmaApp = FunctionCall {
              prefix = []; id = info.prefixLemmaId; tps = []
              args = [
                selectCodecDecodeInfo childData.decInfo (resetAtACN (Var nextSnap) (Var snap))
                selectCodecDecodeInfo childData.decInfo (Var cdc)
              ] @ [Var childSize] @ (childData.existArg |> Option.toList) @ (childData.common.paramsAcn |> List.map Var)
              parameterless = true
            }
            let mkRightCase (resBdg: Var) (decodedAcnsBdgs: Var list): Expr =
              mkTuple ((Var resBdg) :: (decodedAcnsBdgs |> List.map Var))
            let decData = decodePureCallComposedHelper childData.common info childData.existArg $"dec_{ix + 1}" (Var c) (fun _ _ -> mkBlock [Check (BoolLit false); TripleQMark]) mkRightCase
            prefixLemmaApp, decData, Var cTmp

        let encodedValue =
          match child with
          | Some (Asn1Child child) -> FieldSelect (Var selVar, ToC child.Name.Value)
          | Some (AcnChild child) -> Var {name = getAcnDeterminantName child.id; tpe = fromAcnInsertedType child.Type}
          | None ->
            // Presence bit (presence bits start at ix = 0), so we need to fetch the ix^th optional child with implicit presence bit
            let child = childrenWithPresenceBits.[ix]
            isDefinedMutExpr (FieldSelect (Var selVar, ToC child.Name.Value))

        let cdcAssertions =
          if ix = nbPresenceBits + sq.children.Length - 1 then [Assert (Equals (Var nextC, Var cdc))]
          else [
            resetAtEqLemma (Var nextC) (Var cdc) (Var nextSnap)
            Assert (Equals (Var nextC, resetAtACN (Var cdc) (Var nextSnap)))
          ]
        let assertions = mkBlock (
          cdcAssertions @
          (Assert (Equals (Var res, encodedValue)) ::
          (List.zip decodedAcn childAcns |> List.map (fun (acn1, acn2) -> Assert (Equals (Var acn1, Var acn2)))))
        )
        // The prefix lemma is not needed for the last child
        let prefixLemmaApp =
          if ix = nbPresenceBits + sq.children.Length - 1 then []
          else [prefixLemmaApp]
        let result = mkBlock ([
          validateOffsetBitsContentIrrelevancyLemma (selBitStreamACN (Var snap)) (selBufACN (Var nextSnap)) (longlit maxSizeInBits)
        ] @ prefixLemmaApp @ [
          letTuple [cTmp; decData.dec] decData.decCall (
            letsIn [nextC, cNextWrapped] (
              letTuple (res :: decodedAcn) decData.extracted assertions
            )
          )
        ])
        if ix = 0 then letsIn [c, resetAtACN (Var cdc) (Var fstSnap)] result
        else result

      //////////

      let subproofs = (
        (List.replicate nbPresenceBits (None: SeqChildInfo option) @ (sq.children |> List.map Some)) |>
        List.indexed |>
        List.map (fun (i, c) ->
          match c with
          | Some (Asn1Child asn1) ->
            match asn1.Type.Kind with
            | NullType _ ->
                // For NullType, we only bind a new codec
                // TODO: Not quite, if the NT has an encoding pattern, there is some logic to do
                let c = {Var.name = $"c{i + 1}"; tpe = ClassType codecTpe}
                let nextC = {Var.name = $"c{i + 2}"; tpe = ClassType codecTpe}
                if i = 0 then
                  // `c1` must be bound to codec.resetAt(codec_0_1)
                  letsIn [(c, resetAtACN (Var cdc) (Var fstSnap)); (nextC, Var c)] (mkBlock [])
                else letsIn [nextC, Var c] (mkBlock [])
            | _ -> mkFieldSubproof i c
          | Some (AcnChild _) | None -> mkFieldSubproof i c
        ))

      let proof =
        let cpy = {Var.name = "cpy"; tpe = ClassType codecTpe}
        let c1 = {Var.name = "c1"; tpe = ClassType codecTpe} // Bound to codec.resetAt(codec_0_1) by mkFieldSubproof
        let r2Got = {Var.name = "r2Got"; tpe = ClassType codecTpe}
        let decValue = {Var.name = "decValue"; tpe = seqTpe}
        let decInfo =
          match decodeInfo (Asn1 t) t.id false with
          | ComposedDecodeInfo info -> info
          | PrimitiveDecodeInfo _ -> failwith "Cannot be the case"
        let decMiscData = seqDecodeMiscData (nestingScope.parents |> List.map snd) t
        let unfoldedCall = FunctionCall {
          prefix = []; id = decInfo.decodeId; tps = []
          args = Var cpy :: (decMiscData.paramsAcn |> List.map Var)
          parameterless = true
        }
        let retAcns = decMiscData.acns |> List.map (fun acn -> {Var.name = getAcnDeterminantName acn.id; tpe = fromAcnInsertedType acn.Type})
        let decRetAcns = retAcns |> List.map (fun v -> {v with name = $"dec_{v.name}"})
        let mkRightCase (resBdg: Var) (decodedAcnsBdgs: Var list): Expr =
          mkTuple ((Var resBdg) :: (decodedAcnsBdgs |> List.map Var))
        let pureCall = decodePureCallComposedHelper decMiscData decInfo None "decRes" (Var c1) (fun _ _ -> mkBlock [Check (BoolLit false); TripleQMark]) mkRightCase
        letsIn [cpy, Snapshot (Var c1)] (
          mkBlock [
            validateOffsetBitsContentIrrelevancyLemma (selBitStreamACN (Var fstSnap)) (selBufACN (Var cdc)) (longlit t.acnMaxSizeInBits)
            Unfold unfoldedCall
            letTuple [r2Got; pureCall.dec] pureCall.decCall (
              letTuple (decValue :: decRetAcns) pureCall.extracted (mkBlock ([
                Check (Equals (Var r2Got, Var cdc))
                Check (Equals (Var decValue, Var selVar))
              ] @ (List.zip decRetAcns retAcns |> List.map (fun (acn1, acn2) -> Check (Equals (Var acn1, Var acn2))))))
            )
          ])
      Some (Ghost (mkBlock ((sizeCheck |> Option.toList) @ transitiveLemmas @ subproofs @ [proof])))


let generateSequenceAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): FunDef list =
  if r.args.stainlessInvertibility && enc = ACN && codec = Decode then [generatePrefixLemmaSequence enc t nestingScope sq]
  else []

let generateIntegerAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (int: Asn1AcnAst.Integer) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): FunDef list =
  // If `tasInfo` is Some, then there is a pair of encode/decode functions that are generated wrapping a call to encode/decode the integer
  // In such cases, we generate a "read prefix" lemma that is just an application of the appropriate ACN integer lemma
  if r.args.stainlessInvertibility && enc = ACN && codec = Decode && t.id.tasInfo.IsSome then [generatePrefixLemmaInteger enc t nestingScope int]
  else []

let generateBooleanAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (boolean: Asn1AcnAst.Boolean) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): FunDef list =
  if r.args.stainlessInvertibility && enc = ACN && codec = Decode && t.id.tasInfo.IsSome then [generatePrefixLemmaBool enc t nestingScope boolean]
  else []

let generateChoiceAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (ch: Asn1AcnAst.Choice) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): FunDef list =
  if r.args.stainlessInvertibility && enc = ACN && codec = Decode then [generatePrefixLemmaChoice enc t nestingScope ch]
  else []

let generateNullTypeAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (nt: Asn1AcnAst.NullType) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): FunDef list =
  if r.args.stainlessInvertibility && enc = ACN && codec = Decode && t.id.tasInfo.IsSome then [generatePrefixLemmaNullType enc t nestingScope nt]
  else []

let generateEnumAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (enm: Asn1AcnAst.Enumerated) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): FunDef list =
  if r.args.stainlessInvertibility && enc = ACN && codec = Decode && t.id.tasInfo.IsSome then [generatePrefixLemmaEnum enc t nestingScope enm]
  else []

let generateSequenceOfLikeProof (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (sqf: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): SequenceOfLikeProofGenResult option =
  None

let generateSequenceOfLikeAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (sqf: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): FunDef list * Expr =
  let sqfTpe = fromSequenceOfLike sqf
  let elemTpe = fromSequenceOfLikeElemTpe sqf
  let codecTpe = runtimeCodecTypeFor enc
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = {Var.name = "oldCdc"; tpe = ClassType codecTpe}
  let cdcBeforeLoop = {Var.name = $"codecBeforeLoop_{pg.nestingIx}"; tpe = ClassType codecTpe}
  let cdcSnap1 = {Var.name = "codecSnap1"; tpe = ClassType codecTpe}
  let cdcSnap2 = {Var.name = "codecSnap2"; tpe = ClassType codecTpe}
  let from = {Var.name = pg.ixVariable; tpe = IntegerType Int}
  let sqfVar = {Var.name = pg.cs.arg.asIdentifier; tpe = sqfTpe}
  let count = {Var.name = "nCount"; tpe = IntegerType Int}
  let outerSqf =
    if enc = ACN || codec = Decode then Var sqfVar
    else SelectionExpr (joinedSelection pg.cs.arg)
  let collTpe = vecTpe elemTpe
  let td =
    match sqf with
    | SqOf sqf -> sqf.typeDef.[Scala].typeName
    | StrType str -> str.typeDef.[Scala].typeName

  let callSizeRangeObj (ls: Expr) (offset: Expr) (from: Expr) (tto: Expr): Expr =
    FunctionCall {
      prefix = [td]
      id = "sizeRange"
      tps = []
      args = [ls; offset; from; tto]
      parameterless = true
    }

  let fnid =
    let baseId =
      match pg.t with
      | Asn1TypeOrAcnRefIA5.Asn1 t -> $"{ToC t.id.dropModule.AsString}"
      | Asn1TypeOrAcnRefIA5.AcnRefIA5 (tId, _) -> $"{ToC tId.dropModule.AsString}"
    match codec with
    | Encode -> $"{baseId}_Encode_loop"
    | Decode -> $"{baseId}_Decode_loop"
  let nbItemsMin, nbItemsMax = sqf.nbElems enc
  let nbItems =
    if sqf.isFixedSize then int32lit nbItemsMin
    else
      if codec = Encode then
        match sqf with
        | SqOf _ -> FieldSelect (Var sqfVar, "nCount")
        | StrType _ -> Var count
      else Var count
  let maxElemSz = sqf.maxElemSizeInBits enc

  let fromBounds = And [Leq (int32lit 0I, Var from); Leq (Var from, nbItems)]
  let validateOffset =
    validateOffsetBitsACN (Var cdc) (Mult (longlit maxElemSz, Minus (nbItems, Var from)))
  let decreasesExpr = Minus (nbItems, Var from)

  let encDec = pg.encDec |> Option.map EncDec |> Option.toList

  let preSerde = Ghost (validateOffsetBitsWeakeningLemma (selBitStreamACN (Var cdc)) (Mult (longlit maxElemSz, Minus (nbItems, Var from))) (longlit maxElemSz))
  let postSerde =
    Ghost (mkBlock [
      Check (Equals (
        Mult (longlit maxElemSz, plus [Var from; int32lit 1I]),
        plus [Mult (longlit maxElemSz, Var from); longlit maxElemSz]
      ))
      validateOffsetBitsIneqLemma (selBitStreamACN (Var cdcSnap1)) (selBitStreamACN (Var cdc)) (Mult (longlit maxElemSz, Minus (nbItems, Var from))) (longlit maxElemSz)
      Check (validateOffsetBitsACN (Var cdc) (Mult (longlit maxElemSz, Minus (nbItems, plus [Var from; int32lit 1I]))))
    ])
  // TODO: ALIGNMENT
  let sizeLemmaCall =
    match sqf with
    | SqOf _ -> Some (MethodCall {recv = outerSqf; id = sizeLemmaId None; args = [bitIndexACN (Var cdcBeforeLoop); bitIndexACN (Var oldCdc)]; parameterless = true})
    | StrType _ -> None

  ////////////////////////////

  // Creates the recursive function and returns the corresponding call
  let mkEncodeRecursiveFn (): FunDef * Expr =
    let countParam =
      match sqf with
      | StrType _ when not sqf.isFixedSize -> [count]
      | _ -> []
    let fnRetTpe = eitherTpe (IntegerType Int) (IntegerType Int)
    let reccall = FunctionCall {prefix = []; id = fnid; tps = []; args = [Var cdc] @ (countParam |> List.map Var) @ [Var sqfVar; plus [Var from; int32lit 1I]]; parameterless = true}
    let checkRange =
      match sqf with
      | StrType _ ->
        let elem = vecApply (Var sqfVar) (Var from)
        [
          IfExpr {
            cond = Not (And [Leq (ubytelit 0I, elem); Leq (elem, ubytelit 127I)])
            thn = Return (leftExpr (IntegerType Int) (IntegerType Int) (int32lit 1I))
            els = UnitLit
          }
        ]
      | SqOf _ -> []
    let elseBody =
      let reccallRes = {Var.name = "res"; tpe = fnRetTpe}
      let sizeRangeProof =
        match sqf with
        | StrType _ -> []
        | SqOf sq ->
          let selArr = FieldSelect (Var sqfVar, "arr")
          let cIx = bitIndexACN (Var cdc)
          let c1Ix = bitIndexACN (Var cdcSnap1)
          let c2Ix = bitIndexACN (Var cdcSnap2)
          let elemSz = asn1SizeExpr sq.child.acnAlignment sq.child.Kind (vecApply selArr (Var from)) c1Ix 0I 0I
          let szRangeRec = callSizeRangeObj selArr c2Ix (plus [Var from; int32lit 1I]) nbItems
          let szRangePost = callSizeRangeObj selArr c1Ix (Var from) nbItems
          let proof =
            letsIn elemSz.bdgs (mkBlock [
              Assert (Equals (cIx, plus [c2Ix; szRangeRec]))
              Assert (Equals (c2Ix, plus [c1Ix; elemSz.resSize]))
              Assert (Equals (szRangePost, plus [elemSz.resSize; szRangeRec]))
              Check (Equals (cIx, plus [c1Ix; szRangePost]))
            ])
          [Ghost (eitherMatchExpr (Var reccallRes) None UnitLit None proof)]
      letsGhostIn [cdcSnap1, Snapshot (Var cdc)] (
      mkBlock (
        checkRange @
        preSerde ::
        encDec @
        [letsGhostIn [cdcSnap2, Snapshot (Var cdc)] (
        mkBlock [
          postSerde
          letsIn [reccallRes, reccall] (mkBlock (
            sizeRangeProof @ [Var reccallRes]
          ))
        ])]
      ))
    let body = IfExpr {
      cond = Equals (Var from, nbItems)
      thn = rightExpr (IntegerType Int) (IntegerType Int) (int32lit 0I)
      els = elseBody
    }
    let postcondRes = {Var.name = "res"; tpe = fnRetTpe}
    let postcond =
      let oldCdc = Old (Var cdc)
      let sz =
        match sqf with
        | SqOf _ -> callSizeRangeObj (FieldSelect (Var sqfVar, "arr")) (bitIndexACN oldCdc) (Var from) nbItems
        | StrType _ -> Mult (longlit maxElemSz, Minus (nbItems, Var from))
      let rightBody = And [
        Equals (selBufLengthACN oldCdc, selBufLengthACN (Var cdc))
        Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz])
      ]
      eitherMatchExpr (Var postcondRes) None (BoolLit true) (Some postcondRes) rightBody
    let invPrecond =
      match sqf with
      | SqOf sq when not sqf.isFixedSize ->
        // These preconds are trivial since they come from the class invariant, it however helps the solver since it does not need to unfold the class invariant
        let selArrSize = vecSize (FieldSelect (Var sqfVar, "arr"))
        [Precond (And [Leq (int32lit sq.minSize.acn, nbItems); Leq (nbItems, selArrSize); Leq (selArrSize, int32lit sq.maxSize.acn)])]
      | _ -> []
    let sizePrecond =
      match sqf with
      | StrType _ -> [Precond (Equals (vecSize (Var sqfVar), plus [nbItems; int32lit 1I]))] // +1 for the null terminator
      | SqOf _ -> []
    let fd = {
      FunDef.id = fnid
      prms = [cdc] @ countParam @ [sqfVar; from]
      annots = [Opaque; InlineOnce]
      specs = if enc = ACN then [Precond fromBounds] @ invPrecond @ sizePrecond @ [Precond validateOffset; Measure decreasesExpr] else []
      postcond = if enc = ACN then Some (postcondRes, postcond) else None
      returnTpe = fnRetTpe
      body = body
    }

    let call =
      let count =
        match sqf with
        | StrType _ when not sqf.isFixedSize -> [Var {Var.name = pg.cs.arg.asIdentifier + "_nCount"; tpe = IntegerType Int}]
        | _ -> []
      let scrut = FunctionCall {prefix = []; id = fnid; tps = []; args = [Var cdc] @ count @ [outerSqf; int32lit 0I]; parameterless = true}
      let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
      let leftBody = Return (leftExpr (IntegerType Int) (IntegerType Int) (Var leftBdg))
      let rightBody = mkBlock ((sizeLemmaCall |> Option.map Ghost |> Option.toList) @ [UnitLit])
      eitherMatchExpr scrut (Some leftBdg) leftBody None rightBody
    let call = letsGhostIn [cdcBeforeLoop, Snapshot (Var cdc)] call
    fd, call

  ////////////////////////////

  let mkDecodeRecursiveFn (): FunDef * Expr =
    let countParam = if sqf.isFixedSize then [] else [count]
    let fnRetTpe = eitherMutTpe (IntegerType Int) collTpe
    let sqfVecVar = {Var.name = pg.cs.arg.asIdentifier; tpe = collTpe}
    let thnCase =
      let ret =
        match sqf with
        | SqOf _ -> Var sqfVecVar
        | StrType _ -> vecAppend (Var sqfVecVar) (IntLit (UByte, 0I))
      mkBlock [
        Ghost (mkBlock [
          vecRangesEqReflexiveLemma ret
          vecRangesEqSlicedLemma ret ret (int32lit 0I) (vecSize ret) (int32lit 0I) (Var from)
        ])
        rightMutExpr (IntegerType Int) collTpe ret
      ]
    let elseCase =
      let reccallRes = {Var.name = "res"; tpe = fnRetTpe}
      let newVec = {Var.name = "newVec"; tpe = collTpe}
      let decodedElemVar = {Var.name = $"{pg.cs.arg.asIdentifier}_arr_{pg.ixVariable}_"; tpe = elemTpe}
      let appended = vecAppend (Var sqfVecVar) (Var decodedElemVar)
      let postrecProofSuccess = mkBlock ([
        vecRangesAppendDropEq (Var sqfVecVar) (Var newVec) (Var decodedElemVar) (int32lit 0I) (Var from)
        vecRangesEqImpliesEq appended (Var newVec) (int32lit 0I) (Var from) (plus [Var from; int32lit 1I])
        isnocIndex (vecList (Var sqfVecVar)) (Var decodedElemVar) (Var from)
        listApplyEqVecApply appended (Var from)
        Check (Equals (Var decodedElemVar, vecApply (Var newVec) (Var from)))
      ])
      let reccall = FunctionCall {prefix = []; id = fnid; tps = []; args = [Var cdc] @ (countParam |> List.map Var) @ [appended; plus [Var from; int32lit 1I]]; parameterless = true}
      let postrecProof = Ghost (eitherMutMatchExpr (Var reccallRes) None UnitLit (Some newVec) postrecProofSuccess)
      mkBlock ((preSerde :: encDec) @ [
      letsGhostIn [cdcSnap2, Snapshot (Var cdc)] (
      mkBlock [
          postSerde
          letsIn [reccallRes, reccall] (mkBlock [postrecProof; Var reccallRes])
      ])])
    let ite = IfExpr {
      cond = Equals (Var from, nbItems)
      thn = thnCase
      els = elseCase
    }
    let body = letsGhostIn [cdcSnap1, Snapshot (Var cdc)] ite
    let postcondRes = {Var.name = "res"; tpe = fnRetTpe}
    let postcond =
      let newVec = {Var.name = "newVec"; tpe = collTpe}
      let oldCdc = Old (Var cdc)
      let sz, nbEffectiveElems =
        match sqf with
        | SqOf _ -> callSizeRangeObj (Var newVec) (bitIndexACN oldCdc) (Var from) nbItems, nbItems
        | StrType _ -> Mult (longlit maxElemSz, Minus (nbItems, Var from)), plus [nbItems; int32lit 1I] // +1 for the null terminator
      let rightBody = And ([
        Equals (selBufACN oldCdc, selBufACN (Var cdc))
        Equals (vecSize (Var newVec), nbEffectiveElems)
        vecRangesEq (Var sqfVecVar) (Var newVec) (int32lit 0I) (Var from)
        Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz])
      ])
      eitherMutMatchExpr (Var postcondRes) None (BoolLit true) (Some newVec) rightBody
    let countPrecond =
      match sqf with
      | SqOf sq when not sqf.isFixedSize -> [Precond (And [Leq (int32lit sq.minSize.acn, Var count); Leq (Var count, int32lit sq.maxSize.acn)])]
      | _ -> []
    let fd = {
      FunDef.id = fnid
      prms = [cdc] @ countParam @ [sqfVecVar; from]
      annots = [Opaque; InlineOnce]
      specs = if enc = ACN then countPrecond @ [Precond fromBounds; Precond (Equals (vecSize (Var sqfVecVar), (Var from))); Precond validateOffset; Measure decreasesExpr] else []
      postcond = if enc = ACN then Some (postcondRes, postcond) else None
      returnTpe = fnRetTpe
      body = body
    }
    let call =
      let count =
        if sqf.isFixedSize then []
        else [Var {Var.name = pg.cs.arg.asIdentifier + "_nCount"; tpe = IntegerType Int}]
      let scrut = FunctionCall {prefix = []; id = fnid; tps = []; args = [Var cdc] @ count @ [vecEmpty elemTpe; int32lit 0I]; parameterless = true}
      let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
      // TODO: FIXME: the right type must be the outside type!!!
      let leftHACK = ClassCtor {ct = {prefix = []; id = leftMutId; tps = []; parameterless = false}; args = [Var leftBdg]}
      let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
      let rightBdg = {Var.name = "bdg"; tpe = collTpe}
      let rightBody =
        match sqf with
        | SqOf _ ->
          let ctor = ClassCtor {ct = {prefix = []; id = td; tps = []; parameterless = false}; args = count @ [Var rightBdg]}
          letsIn [sqfVar, ctor] (mkBlock ((sizeLemmaCall |> Option.map Ghost |> Option.toList) @ [Var sqfVar]))
        | StrType _ -> mkBlock ((sizeLemmaCall |> Option.map Ghost |> Option.toList) @ [Var rightBdg])
      letsIn [sqfVar, eitherMutMatchExpr scrut (Some leftBdg) leftBody (Some rightBdg) rightBody] (mkBlock [])
    let call = letsGhostIn [cdcBeforeLoop, Snapshot (Var cdc)] call
    fd, call

  ////////////////////////////

  match codec with
  | Encode ->
    // ASN1 SequenceOf alike have a wrapper function generated by wrapAcnFuncBody that calls the recursive function
    // However this is not the case for ACN strings, so we need to create here this "wrapper" that calls the recursive function
    // and return the call to that wrapper function
    match pg.t with
    | Asn1TypeOrAcnRefIA5.Asn1 _ ->
      let fd, reccall = mkEncodeRecursiveFn ()
      [fd], reccall
    | Asn1TypeOrAcnRefIA5.AcnRefIA5 (tId, _) ->
      let fd, _ = mkEncodeRecursiveFn ()
      let fdWrapperId = $"{ToC tId.dropModule.AsString}_ACN_Encode"
      let strType =
        match sqf with
        | StrType str -> str
        | _ -> failwith "ACN reference to string but not a StrType?"
      let countParam, countAcnVar =
        if not strType.isFixedSize then [count], [Var {Var.name = pg.cs.arg.asIdentifier + "_nCount"; tpe = IntegerType Int}]
        else [], []
      let fromBounds =
        if sqf.isFixedSize then []
        else [Precond (Leq (int32lit 0I, nbItems))]
      let sizePrecond = Precond (Equals (vecSize (Var sqfVar), plus [nbItems; int32lit 1I]))
      let validateOffset = Precond (validateOffsetBitsACN (Var cdc) (Mult (longlit maxElemSz, nbItems)))
      // TODO: Should be doing invertibility condition here as well
      let postcondRes = {Var.name = "res"; tpe = fd.returnTpe}
      let postcond =
        let oldCdc = Old (Var cdc)
        let sz = Mult (longlit maxElemSz, nbItems)
        let rightBody = And [
          Equals (selBufLengthACN oldCdc, selBufLengthACN (Var cdc))
          Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz])
        ]
        eitherMatchExpr (Var postcondRes) None (BoolLit true) (Some postcondRes) rightBody
      let fdWrapper = {
        FunDef.id = fdWrapperId
        prms = [cdc] @ countParam @ [sqfVar]
        annots = [Opaque; InlineOnce]
        specs = if enc = ACN then fromBounds @ [sizePrecond; validateOffset] else []
        postcond = if enc = ACN then Some (postcondRes, postcond) else None
        returnTpe = fd.returnTpe
        body = FunctionCall {prefix = []; id = fnid; tps = []; args = [Var cdc] @ (countParam |> List.map Var) @ [outerSqf; int32lit 0I]; parameterless = true}
      }
      let fdWrapperCall =
        let scrut = FunctionCall {prefix = []; id = fdWrapper.id; tps = []; args = [Var cdc] @ countAcnVar @ [outerSqf]; parameterless = true}
        let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
        let leftBody = Return (leftExpr (IntegerType Int) (IntegerType Int) (Var leftBdg))
        let rightBody = mkBlock ((sizeLemmaCall |> Option.map Ghost |> Option.toList) @ [UnitLit])
        eitherMatchExpr scrut (Some leftBdg) leftBody None rightBody

      [fd; fdWrapper], fdWrapperCall
  | Decode ->
    // Ditto here
    let fd, reccall = mkDecodeRecursiveFn ()
    let returnedFds, auxCall =
      match pg.t with
      | Asn1TypeOrAcnRefIA5.Asn1 _ ->
        [fd], reccall
      | Asn1TypeOrAcnRefIA5.AcnRefIA5 (tId, _) ->
        let fdWrapperId = $"{ToC tId.dropModule.AsString}_ACN_Decode"
        let strType =
          match sqf with
          | StrType str -> str
          | _ -> failwith "ACN reference to string but not a StrType?"
        let countParam = if sqf.isFixedSize then [] else [count]
        let fnRetTpe = eitherMutTpe (IntegerType Int) collTpe
        let fromBounds =
          if sqf.isFixedSize then []
          else [Precond (Leq (int32lit 0I, nbItems))]
        let validateOffset = Precond (validateOffsetBitsACN (Var cdc) (Mult (longlit maxElemSz, nbItems)))
        let postcondRes = {Var.name = "res"; tpe = fnRetTpe}
        let postcond =
          let resVec = {Var.name = "resVec"; tpe = collTpe}
          let oldCdc = Old (Var cdc)
          let sz = Mult (longlit maxElemSz, nbItems)
          let nbEffectiveElems = plus [nbItems; int32lit 1I] // +1 for the null terminator
          let rightBody = And ([
            Equals (selBufACN oldCdc, selBufACN (Var cdc))
            Equals (vecSize (Var resVec), nbEffectiveElems)
            Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz])
          ])
          eitherMutMatchExpr (Var postcondRes) None (BoolLit true) (Some resVec) rightBody
        let fdWrapper = {
          FunDef.id = fdWrapperId
          prms = [cdc] @ countParam
          annots = [Opaque; InlineOnce]
          specs = if enc = ACN then fromBounds @ [validateOffset] else []
          postcond = if enc = ACN then Some (postcondRes, postcond) else None
          returnTpe = fnRetTpe
          body = FunctionCall {prefix = []; id = fnid; tps = []; args = [Var cdc] @ (countParam |> List.map Var) @ [vecEmpty elemTpe; int32lit 0I]; parameterless = true}
        }
        let countAcnVar =
          if not strType.isFixedSize then [Var {Var.name = pg.cs.arg.asIdentifier + "_nCount"; tpe = IntegerType Int}]
          else []
        let fdWrapperCall =
          let scrut = FunctionCall {prefix = []; id = fdWrapper.id; tps = []; args = [Var cdc] @ countAcnVar; parameterless = true}
          let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
          // TODO: FIXME: the right type must be the outside type!!!
          let leftHACK = ClassCtor {ct = {prefix = []; id = leftMutId; tps = []; parameterless = false}; args = [Var leftBdg]}
          let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
          let rightBdg = {Var.name = "bdg"; tpe = collTpe}
          let rightBody = mkBlock ((sizeLemmaCall |> Option.map Ghost |> Option.toList) @ [Var rightBdg])
          letsIn [sqfVar, eitherMutMatchExpr scrut (Some leftBdg) leftBody (Some rightBdg) rightBody] (mkBlock [])
        // We also need to generate a "pure" version of the wrapper decode
        let fdWrapperPure =
          let varCpy = {Var.name = "cpy"; tpe = ClassType codecTpe}
          let varRes = {Var.name = "res"; tpe = fnRetTpe}
          let pureBody = (letsIn
            [varCpy, Snapshot (Var cdc);
            varRes, FunctionCall {prefix = []; id = fdWrapper.id; tps = []; args = [Var varCpy] @ countAcnVar; parameterless = true}]
            (mkTuple [Var varCpy; Var varRes]))
          {
            FunDef.id = $"{fdWrapperId}_pure"
            prms = fdWrapper.prms
            annots = [GhostAnnot; Pure]
            specs = fdWrapper.specs
            postcond = None
            returnTpe = tupleType [ClassType codecTpe; fnRetTpe]
            body = pureBody
          }
        [fd; fdWrapper; fdWrapperPure], fdWrapperCall
    let prefixLemma =
      if enc = ACN && r.args.stainlessInvertibility then
        [generatePrefixLemmaSequenceOfLike enc pg.t pg.nestingScope sqf]
      else []
    returnedFds @ prefixLemma, auxCall

let generateOptionalPrefixLemma (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (soc: SequenceOptionalChild): FunDef =
  // The `existVar` does not exist for always present/absent
  let existVar = soc.existVar |> Option.map (fun v -> {Var.name = v; tpe = BooleanType})
  let baseId = $"{ToC soc.child.Type.id.dropModule.AsString}_Optional"
  // TODO: paramAcn comme wrapAcnFuncBody
  let paramsAcn = acnExternDependenciesVariableDecode soc.child.toAsn1AcnAst.Type (soc.nestingScope.parents |> List.map snd) |> List.map (fun (_, _, v) -> v)
  let mkSizeExpr = fun v1 c1 -> optionalSizeExpr soc.child.toAsn1AcnAst (Var v1) (bitIndexACN (Var c1)) 0I 0I
  let elemTpe = fromAsn1TypeKind soc.child.Type.Kind.baseKind
  let tpe = optionMutTpe elemTpe

  let mkProof (data: PrefixLemmaData): Expr =
    // Note: data.paramsAcn = existVar @ paramsAcn
    // since we concatenate these in generatePrefixLemmaCommon
    let unfoldC1, unfoldC2 =
      let mkCall (cdc: Expr): Expr =
        FunctionCall {
          prefix = []; id = data.decodeId; tps = []
          args = [cdc] @ (data.paramsAcn |> List.map Var)
          parameterless = true
        }
      let unfoldC1 = Unfold (mkCall (Snapshot (Var data.c1)))
      let unfoldC2 = Unfold (mkCall (Snapshot (Var data.c2Reset)))
      unfoldC1, unfoldC2

    match soc.child.Type.Kind with
    | DAst.NullType _ -> mkBlock [unfoldC1; unfoldC2]
    | _ ->
      let decInfo = decodeInfo (Asn1 soc.child.Type.toAsn1AcnAst) soc.child.Type.id false

      let c1Aligned, unalignedSize, slicedLemmaApp =
        match soc.child.Type.acnAlignment with
        | Some align ->
          let c1Aligned = withAlignedToACN align (Var data.c1)
          let unalignedSize =
            match data.v1SizeExpr.resSize with
            | FunctionCall f when [alignedSizeToByteId; alignedSizeToWordId; alignedSizeToDWordId] |> List.contains f.id ->
              assert (f.args.Length = 2)
              f.args.Head // The first argument is the unaligned size
            | other -> failwith $"Size is not aligned, got {other}"
          let slicedLemmaApp = (arrayBitRangesEqSlicedLemma
            (selBufACN (Var data.c1)) (selBufACN (Var data.c2))
            (longlit 0I) (plus [bitIndexACN (Var data.c1); Var data.sz])
            (longlit 0I) (plus [bitIndexACN c1Aligned; unalignedSize]))
          c1Aligned, unalignedSize, Some slicedLemmaApp
        | None ->
          Var data.c1, Var data.sz, None

      let prefixLemmaApp =
        match decInfo with
        | PrimitiveDecodeInfo info ->
          let prefixLemmaApp = FunctionCall {
            prefix = info.prefix; id = info.prefixLemmaId; tps = []
            args = [
              selectCodecDecodeInfo decInfo c1Aligned
              selectCodecDecodeInfo decInfo (Var data.c2)
            ] @ info.extraConstArgs
            parameterless = true
          }
          prefixLemmaApp
        | ComposedDecodeInfo info ->
          let prefixLemmaApp = FunctionCall {
            prefix = []; id = info.prefixLemmaId; tps = []
            args = [c1Aligned; Var data.c2; unalignedSize] @ (paramsAcn |> List.map Var)
            parameterless = true
          }
          prefixLemmaApp

      let proofRightMutCase =
        let ifCond =
          let szEq = Equals (Var data.v1SizeVar, Var data.sz)
          match existVar with
          | Some existVar -> And [szEq; Var existVar]
          | None -> szEq
        IfExpr {
          cond = ifCond
          thn = mkBlock ((slicedLemmaApp |> Option.toList) @ [prefixLemmaApp])
          els = UnitLit
        }
      mkBlock [
        unfoldC1
        unfoldC2
        MatchExpr {
          scrut = Var data.decodingRes1
          cases = [
            {
              pattern = ADTPattern {
                binder = None
                id = rightMutId
                subPatterns = [data.subPat1]
              }
              rhs = proofRightMutCase
            }
            {
              pattern = ADTPattern {
                binder = None
                id = leftMutId
                subPatterns = [Wildcard None]
              }
              rhs = UnitLit
            }
          ]
        }
      ]

  generatePrefixLemmaCommon enc tpe soc.child.Type.toAsn1AcnAst.acnMaxSizeInBits baseId ((existVar |> Option.toList) @ paramsAcn) [] mkSizeExpr soc.nestingScope mkProof


let generateOptionalAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (soc: SequenceOptionalChild) (codec: Codec): FunDef list * Expr =
  if soc.child.Optionality.IsNone then [], EncDec (soc.childBody soc.p soc.existVar)
  else
    let codecTpe = runtimeCodecTypeFor enc
    let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
    let oldCdc = {Var.name = $"oldCdc"; tpe = ClassType codecTpe}
    let childAsn1Tpe = soc.child.Type.toAsn1AcnAst
    let childTpe = fromAsn1TypeKind soc.child.Type.Kind.baseKind
    let optChildTpe = optionMutTpe childTpe
    let fnid, fnIdPure =
      let fnId =
        match codec with
        | Encode -> $"{ToC soc.child.Type.id.dropModule.AsString}_Optional_ACN_Encode"
        | Decode -> $"{ToC soc.child.Type.id.dropModule.AsString}_Optional_ACN_Decode"
      fnId, $"{ToC soc.child.Type.id.dropModule.AsString}_Optional_ACN_Decode_pure"
    let errTpe = IntegerType Int
    let validateOffsetBitCond = [Precond (validateOffsetBitsACN (Var cdc) (longlit childAsn1Tpe.acnMaxSizeInBits))]
    let isValidFuncName = soc.child.Type.Kind.isValidFunction |> Option.bind (fun vf -> vf.funcName)

    let sizeExprOf (recv: Expr): SizeExprRes =
      optionalSizeExpr soc.child.toAsn1AcnAst recv (bitIndexACN (Old (Var cdc))) 0I 0I

    // Computing external ACN dependencies for decoding
    // We also pass them to the encoding function because its postcondition
    // refers to the decoding function, which needs them
    let paramsAcn = acnExternDependenciesVariableDecode soc.child.Type.toAsn1AcnAst (soc.nestingScope.parents |> List.map snd) |> List.map (fun (_, _, v) -> v)

    match codec with
    | Encode ->
      let rightTpe = IntegerType Int
      let fnRetTpe = eitherTpe errTpe rightTpe
      let childVar = {Var.name = soc.p.arg.lastId; tpe = optChildTpe}
      let cstrCheck =
        isValidFuncName |> Option.map (fun validFnName ->
          let bdg = {Var.name = "v"; tpe = childTpe}
          let validCall =
            let scrut = FunctionCall {prefix = []; id = validFnName; tps = []; args = [Var bdg]; parameterless = true}
            let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
            let leftBody = Return (leftExpr errTpe rightTpe (Var leftBdg))
            eitherMatchExpr scrut (Some leftBdg) leftBody None (mkBlock [])
          optionMutMatchExpr (Var childVar) (Some bdg) validCall UnitLit
        ) |> Option.toList
      let encDec = EncDec (soc.childBody {soc.p with arg = soc.p.arg.asLastOrSelf} None)
      let resPostcond = {Var.name = "res"; tpe = fnRetTpe}

      let outermostPVal = {Var.name = "pVal"; tpe = fromAsn1TypeKind (soc.nestingScope.parents |> List.last |> snd).Kind}
      let acnExtVars = acnExternDependenciesVariableEncode soc.child.Type.toAsn1AcnAst soc.nestingScope |> Option.toList
      let outerPVal = SelectionExpr (joinedSelection soc.p.arg)
      let sz = sizeExprOf (Var childVar)
      let isDefined =
        match soc.child.Optionality with
        | Some (AlwaysPresent | AlwaysAbsent) -> []
        | _ -> [isDefinedMutExpr (Var childVar)]
      let postcondExpr = generateEncodePostcondExprCommon r optChildTpe childAsn1Tpe.acnMaxSizeInBits soc.p.arg resPostcond [] sz [] fnIdPure (isDefined @ (paramsAcn |> List.map Var))
      let retRes = rightExpr errTpe rightTpe (int32lit 0I)
      let body = letsGhostIn [(oldCdc, Snapshot (Var cdc))] (mkBlock (cstrCheck @ [encDec; retRes]))
      let fd = {
        FunDef.id = fnid
        prms = [cdc; outermostPVal] @ acnExtVars @ paramsAcn @ [childVar]
        annots = [Opaque; InlineOnce]
        specs = validateOffsetBitCond
        postcond = Some (resPostcond, postcondExpr)
        returnTpe = fnRetTpe
        body = body
      }
      let call =
        let scrut = FunctionCall {prefix = []; id = fd.id; tps = []; args = [Var cdc; Var outermostPVal] @ ((acnExtVars @ paramsAcn) |> List.map Var) @ [outerPVal]; parameterless = true}
        let leftBdg = {Var.name = "l"; tpe = errTpe}
        // TODO: FIXME: the right type must be the outside type!!!
        let leftHACK = ClassCtor {ct = {prefix = []; id = leftId; tps = []; parameterless = false}; args = [Var leftBdg]}
        let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
        eitherMatchExpr scrut (Some leftBdg) leftBody None UnitLit

      [fd], call
    | Decode ->
      // The `existVar` does not exist for always present/absent
      let existVar = soc.existVar |> Option.map (fun v -> {Var.name = v; tpe = BooleanType})
      let rightTpe = optChildTpe
      let outerPVal = {Var.name = soc.p.arg.asIdentifier; tpe = rightTpe}
      let encDec = EncDec (soc.childBody {soc.p with arg = soc.p.arg.asLastOrSelf} soc.existVar)
      let fnRetTpe = eitherMutTpe errTpe rightTpe
      let retVal = {Var.name = soc.p.arg.lastId; tpe = childTpe}
      let retInnerFd =
        let rightRet = rightMutExpr errTpe rightTpe (Var retVal)
        match isValidFuncName with
        | Some validFnName ->
          let someBdg = {Var.name = "v"; tpe = childTpe}
          let eitherPatmat =
            let scrut = FunctionCall {prefix = []; id = validFnName; tps = []; args = [Var someBdg]; parameterless = true}
            let leftBdg = {Var.name = "l"; tpe = errTpe}
            let leftBody = leftMutExpr errTpe rightTpe (Var leftBdg)
            eitherMatchExpr scrut (Some leftBdg) leftBody None rightRet
          optionMutMatchExpr (Var retVal) (Some someBdg) eitherPatmat rightRet
        | None -> rightRet

      let resPostcond = {Var.name = "res"; tpe = fnRetTpe}
      let resvalVar = {Var.name = "resVal"; tpe = childTpe}
      let alwaysAbsentOrPresent =
        match soc.child.Optionality with
        | Some AlwaysPresent -> [isDefinedMutExpr (Var resvalVar)]
        | Some AlwaysAbsent -> [Not (isDefinedMutExpr (Var resvalVar))]
        | _ -> []
      let sz = sizeExprOf (Var resvalVar)
      let cstrIsValid = isValidFuncName |> Option.map (fun isValid ->
        let someBdg = {Var.name = "v"; tpe = childTpe}
        let isRight = isRightExpr (FunctionCall {prefix = []; id = isValid; tps = []; args = [Var someBdg]; parameterless = true})
        optionMutMatchExpr (Var resvalVar) (Some someBdg) isRight (BoolLit true)) |> Option.toList
      let postcondExpr = generateDecodePostcondExprCommon r resPostcond resvalVar sz alwaysAbsentOrPresent cstrIsValid
      let body = letsGhostIn [(oldCdc, Snapshot (Var cdc))] (mkBlock [encDec; retInnerFd])

      let fd = {
        FunDef.id = fnid
        prms = [cdc] @ (existVar |> Option.toList) @ paramsAcn
        annots = [Opaque; InlineOnce]
        specs = validateOffsetBitCond
        postcond = Some (resPostcond, postcondExpr)
        returnTpe = fnRetTpe
        body = body
      }

      let call =
        let scrut = FunctionCall {prefix = []; id = fd.id; tps = []; args = [Var cdc] @ (existVar |> Option.map Var |> Option.toList) @ (paramsAcn |> List.map Var); parameterless = true}
        let leftBdg = {Var.name = "l"; tpe = errTpe}
        // TODO: FIXME: the right type must be the outside type!!!
        let leftHACK = ClassCtor {ct = {prefix = []; id = leftMutId; tps = []; parameterless = false}; args = [Var leftBdg]}
        let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
        let rightBdg = {Var.name = "v"; tpe = childTpe}
        let rightBody = Var rightBdg
        eitherMutMatchExpr scrut (Some leftBdg) leftBody (Some rightBdg) rightBody

      // The rest of the backend expects a `val outerPVal = result`
      let ret = Let {bdg = outerPVal; e = call; body = mkBlock []}

      let fdPure =
        let varCpy = {Var.name = "cpy"; tpe = ClassType codecTpe}
        let varRes = {Var.name = "res"; tpe = fnRetTpe}
        let pureBody = (letsIn
          [varCpy, Snapshot (Var cdc);
          varRes, FunctionCall {prefix = []; id = fd.id; tps = []; args = [Var varCpy] @ (existVar |> Option.map Var |> Option.toList) @ (paramsAcn |> List.map Var); parameterless = true}]
          (mkTuple [Var varCpy; Var varRes]))
        {
          FunDef.id = fnIdPure
          prms = [cdc] @ (existVar |> Option.toList) @ paramsAcn
          annots = [GhostAnnot; Pure]
          specs = validateOffsetBitCond
          postcond = None
          returnTpe = tupleType [ClassType codecTpe; fnRetTpe]
          body = pureBody
        }
      let prefixLemma =
        if enc = ACN && r.args.stainlessInvertibility then
          [generateOptionalPrefixLemma r enc soc]
        else []
      [fd; fdPure] @ prefixLemma, ret
