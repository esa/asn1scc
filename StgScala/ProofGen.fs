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
  // TODO: If minSize = maxSize, we can still have '\0' before `maxSize`, right?
  // TODO: Can we have an `\0` before `minSize` as well?
  let arrayLen = ArrayLength recv
  let nullCharIx = indexOfOrLength recv (IntLit (UByte, 0I))
  And [Leq (int32lit (maxSize + 1I), arrayLen); Leq (nullCharIx, int32lit maxSize)]
  (*
  if minSize = maxSize then And [Leq (int32lit (maxSize + 1I), arrayLen); Equals (nullCharIx, int32lit maxSize)]
  else
    And [Leq (int32lit (maxSize + 1I), arrayLen); Leq (int32lit minSize, nullCharIx); Leq (nullCharIx, int32lit maxSize)]
  *)

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
    let len = isize (FieldSelect (recv, "arr"))
    if sqf.minSize.acn = sqf.maxSize.acn then Equals (len, int32lit sqf.maxSize.acn)
    else
      let nCount = FieldSelect (recv, "nCount")
      And [Leq (len, int32lit sqf.maxSize.acn); Leq (int32lit sqf.minSize.acn, nCount); Leq (nCount, len)]

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
    // TODO: We don't support these anyway
    // assert (rt.acnMinSizeInBits = rt.acnMaxSizeInBits)
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

let seqOfSizeFunDefs (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.SequenceOf): FunDef list * FunDef list =
  let td = sq.typeDef.[Scala].typeName
  let elemTpe = fromAsn1TypeKind sq.child.Kind
  let lsTpe = ClassType (listTpe elemTpe)
  let res = {name = "res"; tpe = IntegerType Long}

  let callSizeRangeObj (ls: Expr) (offset: Expr) (from: Expr) (tto: Expr): Expr =
    FunctionCall {
      prefix = [td]
      id = "sizeRange"
      args = [ls; offset; from; tto]
    }

  let offsetCondHelper (offset: Var) (from: Var) (tto: Var): Expr =
    let overhead = sq.acnMaxSizeInBits - sq.maxSize.acn * sq.child.Kind.acnMaxSizeInBits
    And [
      Leq (longlit 0I, Var offset)
      Leq (Var offset, Minus (longlit (2I ** 63 - 1I - overhead), Mult (longlit sq.child.Kind.acnMaxSizeInBits, Minus (Var tto, Var from))))
    ]
  let rangeVarsCondHelper (ls: Var) (from: Var) (tto: Var): Expr = And [Leq (int32lit 0I, Var from); Leq (Var from, Var tto); Leq (Var tto, isize (Var ls)); Leq (isize (Var ls), int32lit sq.maxSize.acn)]

  let sizeRangeObjFd =
    let ls = {name = "ls"; tpe = lsTpe}
    let offset = {Var.name = "offset"; tpe = IntegerType Long}
    let from = {name = "from"; tpe = IntegerType Int}
    let tto = {name = "to"; tpe = IntegerType Int}
    let measure = Minus (Var tto, Var from)
    let offsetCond = offsetCondHelper offset from tto
    let rangeVarsConds = rangeVarsCondHelper ls from tto
    let elem = iapply (Var ls) (Var from)
    let elemSizeVar = {name = "elemSize"; tpe = IntegerType Long}
    let elemSize = asn1SizeExpr sq.child.acnAlignment sq.child.Kind elem (Var offset) 0I 0I
    let elemSizeAssert =
      if sq.child.Kind.acnMinSizeInBits = sq.child.Kind.acnMaxSizeInBits then
        Assert (Equals (Var elemSizeVar, longlit sq.child.Kind.acnMinSizeInBits))
      else
        Assert (And [
          Leq (longlit sq.child.Kind.acnMinSizeInBits, Var elemSizeVar)
          Leq (Var elemSizeVar, longlit sq.child.Kind.acnMaxSizeInBits)
        ])
    let reccall = callSizeRangeObj (Var ls) (plus [Var offset; Var elemSizeVar]) (plus [Var from; int32lit 1I]) (Var tto)
    let resSize = alignedSizeTo t.acnAlignment (plus [Var elemSizeVar; reccall]) (Var offset)
    let elseBody = letsIn (elemSize.bdgs @ [elemSizeVar, elemSize.resSize]) (mkBlock [elemSizeAssert; resSize])
    let body =
      IfExpr {
        cond = Equals (Var from, Var tto)
        thn = longlit 0I
        els = elseBody
      }

    let postcondRange =
      let nbElems = {Var.name = "nbElems"; tpe = IntegerType Int} // TODO: Add explicit cast to Long
      let sqUpperBound = Mult (longlit sq.child.Kind.acnMaxSizeInBits, Var nbElems)
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
      if sq.child.Kind.acnMinSizeInBits = sq.child.Kind.acnMaxSizeInBits then
        Assert (Equals (Var elemSizeVar, longlit sq.child.Kind.acnMinSizeInBits))
      else
        Assert (And [
          Leq (longlit sq.child.Kind.acnMinSizeInBits, Var elemSizeVar)
          Leq (Var elemSizeVar, longlit sq.child.Kind.acnMaxSizeInBits)
        ])

    let template = sizeLemmaTemplate sq.acnMaxSizeInBits align
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
    let elemSel = iapply (Var ls) (Var from)
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
    let inductiveStep = FunctionCall {
      prefix = [td]
      id = template.id
      args = [
        Var ls
        plus [Var offset; Var elemSizeOffVar]
        plus [Var otherOffset; Var elemSizeOtherOffVar]
        plus [Var from; int32lit 1I]
        Var tto
      ]
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
    let objCall = FunctionCall {prefix = [td]; id = objFd.id; args = [FieldSelect (This, "arr"); Var offset; Var otherOffset; int32lit 0I; FieldSelect (This, "nCount")]}
    let clsFd = {template with body = objCall}
    clsFd, objFd

  let sizeClsFd =
    let offset = {Var.name = "offset"; tpe = IntegerType Long}
    let sizeField =
      match sq.acnEncodingClass with
      | SZ_EC_LENGTH_EMBEDDED sz -> sz
      | _ -> 0I // TODO: Pattern?
    let postcond =
      if sq.acnMinSizeInBits = sq.acnMaxSizeInBits then Equals (Var res, longlit sq.acnMaxSizeInBits)
      else And [Leq (longlit 0I, Var res); Leq (Var res, longlit sq.acnMaxSizeInBits)]
    let finalSize = (plus [
      longlit sizeField
      callSizeRangeObj (FieldSelect (This, "arr")) (Var offset) (int32lit 0I) (FieldSelect (This, "nCount"))
    ])
    {
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
  let clsLemmas, objLemmas = implyingAligns |> List.map sizeLemmas |> List.unzip
  sizeClsFd :: clsLemmas, sizeRangeObjFd :: objLemmas


let generateSequenceSizeDefinitions (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (children: DAst.SeqChildInfo list): string list =
  let fds = seqSizeFunDefs t sq
  fds |> List.map (fun fd -> show (FunDefTree fd))

let generateChoiceSizeDefinitions (t: Asn1AcnAst.Asn1Type) (choice: Asn1AcnAst.Choice) (children: DAst.ChChildInfo list): string list =
  let fds = choiceSizeFunDefs t choice
  fds |> List.map (fun fd -> show (FunDefTree fd))

let generateSequenceOfSizeDefinitions (t: Asn1AcnAst.Asn1Type) (sqf: Asn1AcnAst.SequenceOf) (elemTpe: DAst.Asn1Type): string list * string list =
  let fdsCls, fdsObj = seqOfSizeFunDefs t sqf
  fdsCls |> List.map (fun fd -> show (FunDefTree fd)), fdsObj |> List.map (fun fd -> show (FunDefTree fd))

let generateEncodePostcondExprCommon (tpe: Type)
                                     (maxSize: bigint)
                                     (pVal: Selection)
                                     (resPostcond: Var)
                                     (sz: SizeExprRes)
                                     (extraCondsPre: Expr list)
                                     (decodePureId: string)
                                     (decodeExtraArgs: Expr list): Expr =
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = Old (Var cdc)
  let szRecv = {Var.name = pVal.asLastOrSelf.receiverId; tpe = tpe}
  // TODO: Invertibility for ACN parameters as well
  let invertibility =
    let prefix = isPrefixOfACN oldCdc (Var cdc)
    let r1 = resetAtACN (Var cdc) oldCdc
    let lemmaCall = validateOffsetBitsContentIrrelevancyLemma (selBitStream oldCdc) (selBuf (Var cdc)) (longlit maxSize)
    let r2Got = {Var.name = "r2Got"; tpe = ClassType codecTpe}
    let resGot = {Var.name = "resGot"; tpe = tpe}
    let decodePure = FunctionCall {prefix = []; id = decodePureId; args = r1 :: decodeExtraArgs}
    let eq = And [Equals (Var resGot, rightMutExpr (IntegerType Int) tpe (Var szRecv)); Equals (Var r2Got, Var cdc)]
    let block = Locally (mkBlock [
      lemmaCall
      LetTuple {
        bdgs = [r2Got; resGot]
        e = decodePure
        body = eq
      }
    ])
    [prefix; block]

  // TODO: Put back invertibility
  let rightBody = And (extraCondsPre @ [
    Equals (selBufLength oldCdc, selBufLength (Var cdc))
    Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz.resSize])
  ] (*@ invertibility*))
  let rightBody = letsIn sz.bdgs rightBody
  eitherMatchExpr (Var resPostcond) None (BoolLit true) None rightBody

let generateDecodePostcondExprCommon (resPostcond: Var) (resRightMut: Var) (sz: SizeExprRes) (extraCondsPre: Expr list) (extraCondsPost: Expr list): Expr =
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = Old (Var cdc)
  let rightBody = And (extraCondsPre @ [
    Equals (selBuf oldCdc, selBuf (Var cdc))
    Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz.resSize])
  ] @ extraCondsPost)
  let rightBody = letsIn sz.bdgs rightBody
  eitherMutMatchExpr (Var resPostcond) None (BoolLit true) (Some resRightMut) rightBody

let generateEncodePostcondExpr (t: Asn1AcnAst.Asn1Type) (pVal: Selection) (resPostcond: Var) (decodePureId: string): Expr =
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
  generateEncodePostcondExprCommon tpe t.acnMaxSizeInBits pVal resPostcond sz [] decodePureId []

let generateDecodePostcondExpr (t: Asn1AcnAst.Asn1Type) (resPostcond: Var): Expr =
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
  let cstrIsValid =
    match t.ActualType.Kind with
    | NullType _ -> []
    | _ ->
      let isValidFuncName = $"{t.FT_TypeDefinition.[Scala].typeName}_IsConstraintValid"
      [isRightExpr (FunctionCall {prefix = []; id = isValidFuncName; args = [Var szRecv]})]
  generateDecodePostcondExprCommon resPostcond szRecv sz [] cstrIsValid

let rec tryFindFirstParentACNDependency (parents: Asn1AcnAst.Asn1Type list) (dep: RelativePath): (Asn1AcnAst.Asn1Type * Asn1AcnAst.AcnChild) option =
  match parents with
  | [] -> None
  | parent :: rest ->
    match parent.ActualType.Kind with
    | Sequence _ ->
      let directAcns = collectNestedAcnChildren parent.Kind
      directAcns |> List.tryFind (fun acn -> List.endsWith acn.id.fieldPath dep.asStringList) |>
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
let acnExternDependenciesVariableDecode (t: Asn1AcnAst.Asn1Type) (nestingScope: NestingScope): Var list =
  t.externalDependencies |> List.map (fun dep ->
    let acnDep = tryFindFirstParentACNDependency (nestingScope.parents |> List.map snd) dep
    assert acnDep.IsSome
    let _, acnParam = acnDep.Value
    let nme = ToC (acnParam.id.dropModule.AcnAbsPath.StrJoin "_")
    let tpe = fromAcnInsertedType acnParam.Type
    {Var.name = nme; tpe = tpe}
  )

// For auxiliary encoding function, we sometimes need to encode bytes that depend on the determinant
// of a field that is outside of the current encoding function. We therefore need to somehow refer to it.
// We can do so in two ways:
// * Add the dependency as a parameter and forward it as needed.
// * Refer to it from the outermost "pVal" (always in the function parameter) when possible
// The second way is preferred but not always possible (e.g. if there is a Choice in the path),
// we cannot access the field pass the choice since we need to pattern match).
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

let wrapAcnFuncBody (t: Asn1AcnAst.Asn1Type)
                    (body: string)
                    (codec: Codec)
                    (nestingScope: NestingScope)
                    (outerSel: CallerScope)
                    (recSel: CallerScope): FunDef * Expr =
  assert recSel.arg.path.IsEmpty
  let codecTpe = runtimeCodecTypeFor ACN
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = {Var.name = "oldCdc"; tpe = ClassType codecTpe}
  let tpe = fromAsn1TypeKind t.Kind
  let errTpe = IntegerType Int
  let recPVal = {Var.name = recSel.arg.receiverId; tpe = tpe}
  let precond = [Precond (validateOffsetBitsACN (Var cdc) (longlit t.acnMaxSizeInBits))]
  let isValidFuncName = $"{t.FT_TypeDefinition.[Scala].typeName}_IsConstraintValid"

  match codec with
  | Encode ->
    let retTpe = IntegerType Int
    let outerPVal = SelectionExpr (joinedSelection outerSel.arg)
    let cstrCheck =
      let scrut = FunctionCall {prefix = []; id = isValidFuncName; args = [Var recPVal]}
      let leftBdg = {Var.name = "l"; tpe = errTpe}
      let leftBody = Return (leftExpr errTpe retTpe (Var leftBdg))
      eitherMatchExpr scrut (Some leftBdg) leftBody None (mkBlock [])

    let body = letsGhostIn [oldCdc, Snapshot (Var cdc)] (mkBlock ([
      cstrCheck
      EncDec body
      ClassCtor (right errTpe retTpe (int32lit 0I))
    ]))

    let outermostPVal = {Var.name = "pVal"; tpe = fromAsn1TypeKind (nestingScope.parents |> List.last |> snd).Kind}
    let acnVars = acnExternDependenciesVariableEncode t nestingScope |> Option.toList
    let resPostcond = {Var.name = "res"; tpe = ClassType {id = eitherId; tps = [errTpe; IntegerType Int]}}
    let decodePureId = $"{t.FT_TypeDefinition.[Scala].typeName}_ACN_Decode_pure"
    let szRecv = {Var.name = recSel.arg.asLastOrSelf.receiverId; tpe = tpe}
    let sz =
      match t.Kind with
      | Choice _ | SequenceOf _ -> {bdgs = []; resSize = callSize (Var szRecv) (bitIndexACN (Old (Var cdc)))}
      | _ ->
        // TODO: For Sequence, we don't know whether we have extra ACN fields or not.
        // If we do, we must "inline" the size definition which will contain the size of these extra ACN fields and if not, we can just call size
        // We always inline here since it is ok even if we don't have extra ACN fields
        asn1SizeExpr t.acnAlignment t.Kind (Var szRecv) (bitIndexACN (Old (Var cdc))) 0I 0I
    let postcondExpr = generateEncodePostcondExprCommon tpe t.acnMaxSizeInBits recSel.arg resPostcond sz [] decodePureId []
    let fd = {
      id = $"{ToC t.id.dropModule.AsString}_ACN_Encode"
      prms = [cdc; outermostPVal] @ acnVars @ [recPVal]
      specs = precond
      annots = [Opaque; InlineOnce]
      postcond = Some (resPostcond, postcondExpr)
      returnTpe = ClassType (eitherTpe errTpe retTpe)
      body = body
    }

    let call =
      let scrut = FunctionCall {prefix = []; id = fd.id; args = [Var cdc; Var outermostPVal] @ (acnVars |> List.map (fun v -> Var v)) @ [FreshCopy outerPVal]} // TODO: Ideally we should not be needing a freshCopy...
      let leftBdg = {Var.name = "l"; tpe = errTpe}
      let leftBody = Return (leftExpr errTpe (IntegerType Int) (Var leftBdg))
      eitherMatchExpr scrut (Some leftBdg) leftBody None UnitLit

    fd, call
  | Decode ->
    // Computing external ACN dependencies
    let paramsAcn = acnExternDependenciesVariableDecode t nestingScope

    // All ACN fields present in this SEQUENCE, including nested ones
    let acns = collectNestedAcnChildren t.Kind
    let acnsVars = acns |> List.map (fun c -> {Var.name = getAcnDeterminantName c.id; tpe = fromAcnInsertedType c.Type})
    let acnTps = acnsVars |> List.map (fun v -> v.tpe)
    let retTpe = tupleType (tpe :: acnTps)
    let outerPVal = {Var.name = outerSel.arg.asIdentifier; tpe = tpe}
    let retInnerFd =
      let retVal = mkTuple (Var recPVal :: (acnsVars |> List.map Var))
      let scrut = FunctionCall {prefix = []; id = isValidFuncName; args = [Var recPVal]}
      let leftBdg = {Var.name = "l"; tpe = errTpe}
      let leftBody = leftMutExpr errTpe retTpe (Var leftBdg)
      let rightBody = rightMutExpr errTpe retTpe retVal
      eitherMatchExpr scrut (Some leftBdg) leftBody None rightBody
    let body = letsGhostIn [oldCdc, Snapshot (Var cdc)] (mkBlock [EncDec body; retInnerFd])

    let resPostcond = {Var.name = "res"; tpe = ClassType {id = eitherMutId; tps = [errTpe; retTpe]}}
    let szRecv = {Var.name = "resVal"; tpe = tpe}
    let sz =
      match t.Kind with
      | Choice _ | SequenceOf _ -> {bdgs = []; resSize = callSize (Var szRecv) (bitIndexACN (Old (Var cdc)))}
      | _ ->
        // TODO: For Sequence, we don't know whether we have extra ACN fields or not.
        // If we do, we must "inline" the size definition which will contain the size of these extra ACN fields and if not, we can just call size
        // We always inline here since it is ok even if we don't have extra ACN fields
        asn1SizeExpr t.acnAlignment t.Kind (Var szRecv) (bitIndexACN (Old (Var cdc))) 0I 0I
    let cstrIsValid = isRightExpr (FunctionCall {prefix = []; id = isValidFuncName; args = [Var szRecv]})
    let postcondExpr =
      if acns.IsEmpty then
        generateDecodePostcondExprCommon resPostcond szRecv sz [] [cstrIsValid]
      else
        assert (match t.Kind with Sequence _ -> true | _ -> false)
        let codecTpe = runtimeCodecTypeFor ACN
        let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
        let oldCdc = Old (Var cdc)
        let rightBody = letsIn sz.bdgs (And [
          Equals (selBuf oldCdc, selBuf (Var cdc))
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
      id = $"{ToC t.id.dropModule.AsString}_ACN_Decode"
      prms = [cdc] @ paramsAcn
      specs = precond
      annots = [Opaque; InlineOnce]
      postcond = Some (resPostcond, postcondExpr)
      returnTpe = ClassType (eitherMutTpe errTpe retTpe)
      body = body
    }

    let call =
      let scrut = FunctionCall {prefix = []; id = fd.id; args = [Var cdc] @ (paramsAcn |> List.map Var)}
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

  let sizeRess =
    pg.children |>
    List.indexed |>
    // TODO: if acc not needed, turn fold into map
    List.fold (fun acc (ix, c) ->
      let childVar = {Var.name = $"size_{pg.nestingIx + bigint ix}"; tpe = IntegerType Long}
      match c.info with
      | Some info ->
        let recv =
          match codec with
          | Encode -> SelectionExpr (joinedSelection c.sel.Value)
          | Decode -> SelectionExpr c.sel.Value.asIdentifier
        let resSize = seqSizeExprHelperChild info (bigint ix) (Some recv) (bitIndexACN (Var snapshots.[ix])) pg.nestingLevel pg.nestingIx
        acc @ [{extraBdgs = resSize.bdgs; childVar = childVar; childSize = resSize.resSize}]
      | None ->
        // presence bits
        acc @ [{extraBdgs = []; childVar = childVar; childSize = longlit 1I}]
    ) []

  let annotatePostPreciseSize (ix: int) (snap: Var) (child: SequenceChildProps): Expr =
    let previousSizes = sizeRess |> List.take ix |> List.map (fun res -> Var res.childVar)
    let sizeRes = sizeRess.[ix]
    let chk = Check (Equals (bitIndexACN (Var cdc), plus (bitIndexACN (Var oldCdc) :: previousSizes @ [Var sizeRes.childVar])))
    letsGhostIn sizeRes.allBindings (Ghost chk)

  let annotatePost (ix: int) (snap: Var) (child: SequenceChildProps) (stmt: string option) (offsetAcc: bigint): Expr =
    let sz = child.typeInfo.maxSize enc
    let relativeOffset = offsetAcc - (pg.maxOffset enc)
    let offsetCheckOverall = Check (Leq (bitIndexACN (Var cdc), plus [bitIndexACN (Var oldCdc); (longlit offsetAcc)]))
    let offsetCheckNested =
      if isNested then [Check (Leq (bitIndexACN (Var cdc), plus [bitIndexACN (Var fstSnap); longlit relativeOffset]))]
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
          Check (Leq (bitIndexACN (Var cdc), plus [bitIndexACN (Var oldCdc); longlit (offsetAcc + diff)]))
          Check (Leq (bitIndexACN (Var cdc), plus [bitIndexACN (Var fstSnap); longlit (relativeOffset + diff)]))
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
  if stmts.IsEmpty then stmts |> List.choose id
  else
    let codecTpe = runtimeCodecTypeFor enc
    let cdc = {Var.name = $"codec"; tpe = ClassType codecTpe}
    let oldCdc = {Var.name = $"oldCdc"; tpe = ClassType codecTpe}
    if enc = ACN then
      let snapshots = [1 .. pg.children.Length] |> List.map (fun i -> {Var.name = $"codec_{pg.nestingLevel}_{pg.nestingIx + bigint i}"; tpe = ClassType codecTpe})
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


type PrefixLemmaInfo = {
  prefix: string list
  id: string
  extraArgs: Expr list
}
let readPrefixLemmaIdentifier (t: Asn1AcnAst.Asn1AcnTypeKind) (id: ReferenceToType) (isOptional: bool): PrefixLemmaInfo =
  let forIntClass (intCls:Asn1AcnAst.IntegerClass) (encCls: IntEncodingClass) (range: BigIntegerUperRange): PrefixLemmaInfo =
    match encCls with
    | PositiveInteger_ConstSize_8 -> {prefix = [acnId]; id = "dec_Int_PositiveInteger_ConstSize_8_prefixLemma"; extraArgs = []}
    | PositiveInteger_ConstSize_big_endian_16 -> {prefix = [acnId]; id = "dec_Int_PositiveInteger_ConstSize_big_endian_16_prefixLemma"; extraArgs = []}
    | PositiveInteger_ConstSize_big_endian_32 -> {prefix = [acnId]; id = "dec_Int_PositiveInteger_ConstSize_big_endian_32_prefixLemma"; extraArgs = []}
    | PositiveInteger_ConstSize_big_endian_64 -> {prefix = [acnId]; id = "dec_Int_PositiveInteger_ConstSize_big_endian_64_prefixLemma"; extraArgs = []}
    | PositiveInteger_ConstSize_little_endian_16 -> {prefix = [acnId]; id = "dec_Int_PositiveInteger_ConstSize_little_endian_16_prefixLemma"; extraArgs = []}
    | PositiveInteger_ConstSize_little_endian_32 -> {prefix = [acnId]; id = "dec_Int_PositiveInteger_ConstSize_little_endian_32_prefixLemma"; extraArgs = []}
    | PositiveInteger_ConstSize_little_endian_64 -> {prefix = [acnId]; id = "dec_Int_PositiveInteger_ConstSize_little_endian_64_prefixLemma"; extraArgs = []}
    | PositiveInteger_ConstSize _ -> {prefix = [acnId]; id = "dec_Int_PositiveInteger_ConstSize_prefixLemma"; extraArgs = []}
    | TwosComplement_ConstSize_8 -> {prefix = [acnId]; id = "dec_Int_TwosComplement_ConstSize_8_prefixLemma"; extraArgs = []}
    | TwosComplement_ConstSize_big_endian_16 -> {prefix = [acnId]; id = "dec_Int_TwosComplement_ConstSize_big_endian_16_prefixLemma"; extraArgs = []}
    | TwosComplement_ConstSize_big_endian_32 -> {prefix = [acnId]; id = "dec_Int_TwosComplement_ConstSize_big_endian_32_prefixLemma"; extraArgs = []}
    | TwosComplement_ConstSize_big_endian_64 -> {prefix = [acnId]; id = "dec_Int_TwosComplement_ConstSize_big_endian_64_prefixLemma"; extraArgs = []}
    | TwosComplement_ConstSize_little_endian_16 -> {prefix = [acnId]; id = "dec_Int_TwosComplement_ConstSize_little_endian_16_prefixLemma"; extraArgs = []}
    | TwosComplement_ConstSize_little_endian_32 -> {prefix = [acnId]; id = "dec_Int_TwosComplement_ConstSize_little_endian_32_prefixLemma"; extraArgs = []}
    | TwosComplement_ConstSize_little_endian_64 -> {prefix = [acnId]; id = "dec_Int_TwosComplement_ConstSize_little_endian_64_prefixLemma"; extraArgs = []}
    | TwosComplement_ConstSize _ -> {prefix = [acnId]; id = "dec_Int_TwosComplement_ConstSize_prefixLemma"; extraArgs = []}
    | Integer_uPER ->
      match range with
      | Full -> {prefix = [codecId]; id = "decodeUnconstrainedWholeNumber_prefixLemma"; extraArgs = []}
      | PosInf min -> {prefix = [codecId]; id = "decodeConstrainedPosWholeNumber_prefixLemma"; extraArgs = [ulonglit min]}
      | Concrete (min, max) ->
        if intCls.IsPositive then {prefix = [codecId]; id = "decodeConstrainedPosWholeNumber_prefixLemma"; extraArgs = [ulonglit min; ulonglit max]}
        else {prefix = [codecId]; id = "decodeConstrainedWholeNumber_prefixLemma"; extraArgs = [longlit min; longlit max]}
      | _ -> failwith $"TODO: {range}"
    | _ -> failwith $"TODO: {encCls}"

  if isOptional then
    {prefix = []; id = $"{ToC id.dropModule.AsString}_prefixLemma"; extraArgs = []}
  else
    match t with
    | Asn1 (Integer int) -> forIntClass int.intClass int.acnEncodingClass int.uperRange
    | Acn (AcnInteger int) -> forIntClass int.intClass int.acnEncodingClass int.uperRange
    | Asn1 (Boolean _) | Acn (AcnBoolean _) -> {prefix = [bitStreamId]; id = "readBitPrefixLemma"; extraArgs = []}
    | _ ->
      {prefix = [acnId]; id = "readPrefixLemma_TODO"; extraArgs = []} // TODO

let selectCodecReadPrefixLemma (prefixLemmaInfo: PrefixLemmaInfo) (cdcSnap: Expr) (cdc: Expr): Expr * Expr =
  if prefixLemmaInfo.prefix = [bitStreamId] then selBitStream cdcSnap, selBitStream cdc
  else if prefixLemmaInfo.prefix = [codecId] then selBase cdcSnap, selBase cdc
  else cdcSnap, cdc

let generateSequencePrefixLemma (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence): FunDef =
  let codecTpe = runtimeCodecTypeFor enc
  let c1 = {Var.name = "c1"; tpe = ClassType codecTpe}
  let c2 = {Var.name = "c2"; tpe = ClassType codecTpe}
  let tpe = fromAsn1TypeKind t.Kind
  let sizeExpr = longlit t.Kind.acnMaxSizeInBits
  let preconds = [
    Precond (Equals (selBufLength (Var c1), selBufLength (Var c2)))
    Precond (validateOffsetBitsACN (Var c1) sizeExpr)
    Precond (arrayBitRangesEq
      (selBuf (Var c1))
      (selBuf (Var c2))
      (longlit 0I)
      (plus [bitIndexACN (Var c1); sizeExpr])
    )
  ]

  let decodeId = $"{ToC t.id.dropModule.AsString}_ACN_Decode"
  let decodePureId = $"{decodeId}_pure"
  let c2Reset = {Var.name = "c2Reset"; tpe = ClassType codecTpe}
  let c1Res = {Var.name = "c1Res"; tpe = ClassType codecTpe}
  let v1 = {Var.name = "v1"; tpe = tpe}
  let dec1 = {Var.name = "dec1"; tpe = TupleType [c1Res.tpe; v1.tpe]}
  let call1 = FunctionCall {prefix = []; id = decodePureId; args = [Var c1]}
  let c2Res = {Var.name = "c2Res"; tpe = ClassType codecTpe}
  let v2 = {Var.name = "v2"; tpe = tpe}
  let dec2 = {Var.name = "dec2"; tpe = TupleType [c2Res.tpe; v2.tpe]}
  let call2 = FunctionCall {prefix = []; id = decodePureId; args = [Var c2Reset]}

  let preSpecs =
    preconds @ [
      LetSpec (c2Reset, resetAtACN (Var c2) (Var c1))
      LetSpec (dec1, call1)
      LetSpec (c1Res, TupleSelect (Var dec1, 1))
      LetSpec (v1, TupleSelect (Var dec1, 2))
      LetSpec (dec2, call2)
      LetSpec (c2Res, TupleSelect (Var dec2, 1))
      LetSpec (v2, TupleSelect (Var dec2, 2))
    ]
  let postcond = And [Equals (bitIndexACN (Var c1Res), bitIndexACN (Var c2Res)); Equals (Var v1, Var v2)]

  failwith "TODO"

let generateSequenceProof (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (sel: Selection) (codec: Codec): Expr option =
  None
  (*
  if codec = Decode || sq.children.IsEmpty then None
  else
    assert sel.path.IsEmpty
    let codecTpe = runtimeCodecTypeFor enc
    let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
    let oldCdc = {Var.name = "oldCdc"; tpe = ClassType codecTpe}
    let seqTpe = fromAsn1TypeKind t.Kind
    let selVar = {Var.name = sel.receiverId; tpe = seqTpe}
    let nbPresenceBits = sq.children |> List.sumBy (fun child ->
      match child with
      | AcnChild _ -> 0
      | Asn1Child asn1 ->
        match asn1.Optionality with
        | Some (Optional opt) when opt.acnPresentWhen.IsNone -> 1
        | _ -> 0
    )
    let snapshots = [1 .. nbPresenceBits + sq.children.Length] |> List.map (fun i -> {Var.name = $"codec_0_{i}"; tpe = ClassType codecTpe})

    let transitiveLemmas =
      if snapshots.Length < 2 then []
      else List.rep2 snapshots |> List.map (fun (s1, s2) -> validTransitiveLemma (Var s1) (Var s2) (Var cdc)) |> List.rev

    // let optionalReflexiveLemmaApp (ix0: int, child: Asn1AcnAst.SeqChildInfo): Expr option =
    //   let ix = ix0 + nbPresenceBits
    //   match child with
    //   | AcnChild _ -> None
    //   | Asn1Child asn1 ->
    //     if asn1.Optionality.IsNone then None
    //     else
    //       let theCdc = if ix = snapshots.Length - 1 then cdc else snapshots.[ix + 1]
    //       Some (validReflexiveLemma (Var theCdc))

    let readPrefixLemmaApp (ix0: int, child: Asn1AcnAst.SeqChildInfo): Expr =
      let ix = ix0 + nbPresenceBits
      let cdcSnapReset = resetAtACN (Var snapshots.[ix + 1]) (Var snapshots.[ix])
      let asn1Tpe, id, isOpt, existArg =
        match child with
        | Asn1Child child ->
          let existArg =
            match child.Optionality with
            | Some (Optional _) ->
              [isDefinedMutExpr (FieldSelect (Var selVar, child._scala_name))]
            | _ -> []
          Asn1 child.Type.Kind, child.Type.id, child.Optionality.IsSome, existArg
        | AcnChild child -> Acn child.Type, child.id, false, []
      let prefixLemmaInfo = readPrefixLemmaIdentifier asn1Tpe id isOpt
      let cdcSnapRecv, cdcRecv = selectCodecReadPrefixLemma prefixLemmaInfo cdcSnapReset (Var cdc)
      FunctionCall {prefix = prefixLemmaInfo.prefix; id = prefixLemmaInfo.id; args = [cdcSnapRecv; cdcRecv] @ existArg @ prefixLemmaInfo.extraArgs}

    // let optionals = sq.children |> List.indexed |> List.choose optionalReflexiveLemmaApp
    let presenceBitsPrefixLemmaApps = [0 .. nbPresenceBits - 1] |> List.map (fun ix ->
      let cdcSnapReset = resetAtACN (Var snapshots.[ix + 1]) (Var snapshots.[ix])
      FunctionCall {prefix = [bitStreamId]; id = "readBitPrefixLemma"; args = [selBitStream cdcSnapReset; selBitStream (Var cdc)]}
    )
    let childrenPrefixLemmaApps = sq.children |> List.indexed |> List.initial |> List.map readPrefixLemmaApp

    let proof =
      let cpy = {Var.name = "cpy"; tpe = ClassType codecTpe}
      let decodeId = $"{t.FT_TypeDefinition.[Scala].typeName}_ACN_Decode"
      let decodeIdPure = $"{decodeId}_pure"
      let r1 = {Var.name = "r1"; tpe = ClassType codecTpe}
      let r2Got = {Var.name = "r2Got"; tpe = ClassType codecTpe}
      let resGot = {Var.name = "resGot"; tpe = ClassType (eitherMutTpe (IntegerType Int) seqTpe)}
      letsIn [cpy, Snapshot (resetAtACN (Var cdc) (Var oldCdc))] (
        mkBlock [
          Unfold (FunctionCall {prefix = []; id = decodeId; args = [Var cpy]})
          letsIn [r1, resetAtACN (Var cdc) (Var oldCdc)] (mkBlock [
            letTuple [r2Got; resGot] (FunctionCall {prefix = []; id = decodeIdPure; args = [Var r1]}) (mkBlock [
              Check (Equals (Var resGot, rightMutExpr (IntegerType Int) seqTpe (Var selVar)))
              Check (Equals (Var r2Got, Var cdc))
            ])
          ])
        ])
    Some (Ghost (mkBlock (transitiveLemmas @ presenceBitsPrefixLemmaApps @ childrenPrefixLemmaApps @ [proof])))
  *)
let generateSequenceOfLikeProof (enc: Asn1Encoding) (sqf: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): SequenceOfLikeProofGenResult option =
  None


let generateSequenceOfLikeAuxiliaries (enc: Asn1Encoding) (sqf: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): FunDef list * Expr =
  let sqfTpe = fromSequenceOfLike sqf
  let codecTpe = runtimeCodecTypeFor enc
  let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
  let oldCdc = {Var.name = "oldCdc"; tpe = ClassType codecTpe}
  let cdcBeforeLoop = {Var.name = $"codecBeforeLoop_{pg.nestingIx}"; tpe = ClassType codecTpe}
  let cdcSnap1 = {Var.name = "codecSnap1"; tpe = ClassType codecTpe}
  let from = {Var.name = pg.ixVariable; tpe = IntegerType Int}
  let sqfVar = {Var.name = pg.cs.arg.asIdentifier; tpe = sqfTpe}
  let count = {Var.name = "nCount"; tpe = IntegerType Int}
  let outerSqf =
    if enc = ACN || codec = Decode then Var sqfVar
    else SelectionExpr (joinedSelection pg.cs.arg)
  let td =
    match sqf with
    | SqOf sqf -> sqf.typeDef.[Scala].typeName
    | StrType str -> str.typeDef.[Scala].typeName

  let callSizeRangeObj (ls: Expr) (offset: Expr) (from: Expr) (tto: Expr): Expr =
    FunctionCall {
      prefix = [td]
      id = "sizeRange"
      args = [ls; offset; from; tto]
    }

  let fnid =
    let prefix = pg.nestingScope.parents |> List.tryHead |> Option.map (fun (cs, _) -> $"{cs.arg.asIdentifier}_") |> Option.defaultValue ""
    match codec with
    | Encode -> $"{ToC pg.cs.modName}_{td}_{prefix}{pg.cs.arg.lastIdOrArr}_Encode_loop"
    | Decode -> $"{ToC pg.cs.modName}_{td}_{prefix}{pg.cs.arg.lastIdOrArr}_Decode_loop"
  let nbItemsMin, nbItemsMax = sqf.nbElems enc
  let nbItems =
    if sqf.isFixedSize then int32lit nbItemsMin
    else
      match sqf with
      | StrType _ when enc = UPER -> ArrayLength (Var sqfVar)
      | _ -> if codec = Encode then FieldSelect (Var sqfVar, "nCount") else Var count
  let maxElemSz = sqf.maxElemSizeInBits enc

  let fromBounds = And [Leq (int32lit 0I, Var from); Leq (Var from, nbItems)]
  let validateOffset =
    validateOffsetBitsACN (Var cdc) (Mult (longlit maxElemSz, Minus (nbItems, Var from)))
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
      Check (validateOffsetBitsACN (Var cdc) (Mult (longlit maxElemSz, Minus (nbItems, plus [Var from; int32lit 1I]))))
    ])
  // TODO: ALIGNMENT
  let sizeLemmaCall =
    match sqf with
    | SqOf _ -> Some (MethodCall {recv = outerSqf; id = sizeLemmaId None; args = [bitIndexACN (Var cdcBeforeLoop); bitIndexACN (Var oldCdc)]})
    | StrType _ -> None

  let getLength (arr: Expr): Expr =
    match sqf with
    | SqOf _ -> isize arr
    | StrType _ -> ArrayLength arr

  let select (arr: Expr) (ix: Expr): Expr =
    match sqf with
    | SqOf _ -> iapply arr ix
    | StrType _ -> ArraySelect (arr, ix)

  let rangesEq =
    match sqf with
    | SqOf _ -> listRangesEq
    | StrType _ -> arrayRangesEq

  let rangesEqReflexiveLemma =
    match sqf with
    | SqOf _ -> listRangesEqReflexiveLemma
    | StrType _ -> arrayRangesEqReflexiveLemma

  let rangesEqSlicedLemma =
    match sqf with
    | SqOf _ -> listRangesEqSlicedLemma
    | StrType _ -> arrayRangesEqSlicedLemma

  let updatedAtPrefixLemma =
    match sqf with
    | SqOf _ -> listUpdatedAtPrefixLemma
    | StrType _ -> arrayUpdatedAtPrefixLemma

  let rangesEqTransitive =
    match sqf with
    | SqOf _ -> listRangesEqTransitive
    | StrType _ -> arrayRangesEqTransitive

  let rangesEqImpliesEq =
    match sqf with
    | SqOf _ -> listRangesEqImpliesEq
    | StrType _ -> arrayRangesEqImpliesEq

  match codec with
  | Encode ->
    let fnRetTpe = ClassType (eitherTpe (IntegerType Int) (IntegerType Int))
    let reccall = FunctionCall {prefix = []; id = fnid; args = [Var cdc; Var sqfVar; plus [Var from; int32lit 1I]]}
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
      let sz =
        match sqf with
        | SqOf _ -> callSizeRangeObj (FieldSelect (Var sqfVar, "arr")) (bitIndexACN oldCdc) (Var from) nbItems
        | StrType _ -> Mult (longlit maxElemSz, Var from)
      let rightBody = And [
        Equals (selBufLength oldCdc, selBufLength (Var cdc))
        Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz])
      ]
      eitherMatchExpr (Var postcondRes) None (BoolLit true) (Some postcondRes) rightBody
    let fd = {
      FunDef.id = fnid
      prms = [cdc; sqfVar; from]
      annots = [Opaque; InlineOnce]
      specs = if enc = ACN then [Precond fromBounds; Precond validateOffset; Measure decreasesExpr] else []
      postcond = if enc = ACN then Some (postcondRes, postcond) else None
      returnTpe = fnRetTpe
      body = body
    }

    let call =
      let scrut = FunctionCall {prefix = []; id = fnid; args = [Var cdc; outerSqf; int32lit 0I]}
      let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
      let leftBody = Return (leftExpr (IntegerType Int) (IntegerType Int) (Var leftBdg))
      let rightBody = sizeLemmaCall |> Option.map Ghost |> Option.defaultValue UnitLit
      eitherMatchExpr scrut (Some leftBdg) leftBody None rightBody
    let call = letsGhostIn [cdcBeforeLoop, Snapshot (Var cdc)] call
    [fd], call
  | Decode ->
    let elemTpe = fromSequenceOfLikeElemTpe sqf
    let cdcSnap2 = {Var.name = "codecSnap2"; tpe = ClassType codecTpe}
    match sqf with
    | SqOf sq ->
      let countParam = if sqf.isFixedSize then [] else [count]
      let collTpe = ClassType (listTpe elemTpe)
      let fnRetTpe = ClassType (eitherMutTpe (IntegerType Int) collTpe)
      let sqfListVar = {Var.name = pg.cs.arg.asIdentifier + "_list"; tpe = collTpe}
      let reversed = reverse (Var sqfListVar)
      let thnCase =
        mkBlock [
          Ghost (mkBlock [
            rangesEqReflexiveLemma reversed
            rangesEqSlicedLemma reversed reversed (int32lit 0I) (getLength reversed) (int32lit 0I) (Var from)
          ])
          rightMutExpr (IntegerType Int) collTpe reversed
        ]
      let elseCase =
        let reccallRes = {Var.name = "res"; tpe = fnRetTpe}
        let newList = {Var.name = "newList"; tpe = collTpe}
        let decodedElemVar = {Var.name = $"{pg.cs.arg.asIdentifier}_arr_iapply_{pg.ixVariable}_"; tpe = elemTpe}
        let postrecProofSuccess = mkBlock ([
          listRangesAppendDropEq reversed (Var newList) (Var decodedElemVar) (int32lit 0I) (Var from)
          listRangesEqImpliesEq (reverse (consExpr elemTpe (Var decodedElemVar) (Var sqfListVar))) (Var newList) (int32lit 0I) (Var from) (plus [Var from; int32lit 1I])
          isnocIndex reversed (Var decodedElemVar) (Var from)
        ])
        let updated = consExpr elemTpe (Var decodedElemVar) (Var sqfListVar)
        let reccall = FunctionCall {prefix = []; id = fnid; args = [Var cdc] @ (countParam |> List.map Var) @ [updated; plus [Var from; int32lit 1I]]}
        let postrecProof = Ghost (eitherMutMatchExpr (Var reccallRes) None UnitLit (Some newList) postrecProofSuccess)
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
        let newList = {Var.name = "newList"; tpe = collTpe}
        let oldCdc = Old (Var cdc)
        let sz = callSizeRangeObj (Var newList) (bitIndexACN oldCdc) (Var from) nbItems
        // let decodeIntoArrayCond =
        //   match pg.elemDecodeFn with
        //   | None -> []
        //   | Some decodeFn ->
        //     let decodePure = TupleSelect (FunctionCall {prefix = []; id = $"{decodeFn}_pure"; args = [oldCdc]}, 2)
        //     [Or [
        //       Equals (Var from, nbItems)
        //       Equals (
        //         rightMutExpr (IntegerType Int) UnitType (select (Var sqfVar) (Var from)),
        //         decodePure
        //       )
        //     ]]
        let rightBody = And ([
          Equals (selBuf oldCdc, selBuf (Var cdc))
          Equals (isize (Var newList), nbItems)
          listRangesEq reversed (Var newList) (int32lit 0I) (Var from)
          Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz])
        ])
        eitherMutMatchExpr (Var postcondRes) None (BoolLit true) (Some newList) rightBody
      let countPrecond =
        if sqf.isFixedSize then []
        else [Precond (And [Leq (int32lit sq.minSize.acn, Var count); Leq (Var count, int32lit sq.maxSize.acn)])]
      let fd = {
        FunDef.id = fnid
        prms = [cdc] @ countParam @ [sqfListVar; from]
        annots = [Opaque; InlineOnce]
        specs = if enc = ACN then countPrecond @ [Precond fromBounds; Precond (Equals (isize (Var sqfListVar), (Var from))); Precond validateOffset; Measure decreasesExpr] else []
        postcond = if enc = ACN then Some (postcondRes, postcond) else None
        returnTpe = fnRetTpe
        body = body
      }
      let call =
        let count =
          if sqf.isFixedSize then []
          else [Var {Var.name = pg.cs.arg.asIdentifier + "_nCount"; tpe = IntegerType Int}]
        let scrut = FunctionCall {prefix = []; id = fnid; args = [Var cdc] @ count @ [nilExpr elemTpe; int32lit 0I]}
        let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
        // TODO: FIXME: the right type must be the outside type!!!
        let leftHACK = ClassCtor {ct = {id = leftMutId; tps = []}; args = [Var leftBdg]}
        let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
        let rightBody =
          let ctor = ClassCtor {ct = {id = td; tps = []}; args = count @ [Var sqfListVar]}
          letsIn [sqfVar, ctor] (mkBlock ((sizeLemmaCall |> Option.map Ghost |> Option.toList) @ [Var sqfVar]))
        letsIn [sqfVar, eitherMutMatchExpr scrut (Some leftBdg) leftBody (Some sqfListVar) rightBody] (mkBlock [])
      let call = letsGhostIn [cdcBeforeLoop, Snapshot (Var cdc)] call
      [fd], call
    | StrType _ ->
    let fnRetTpe = ClassType (eitherMutTpe (IntegerType Int) UnitType)
    let cdcSnap2 = {Var.name = "codecSnap2"; tpe = ClassType codecTpe}
    let elemTpe = fromSequenceOfLikeElemTpe sqf
    let arr1 = {Var.name = "arr1"; tpe = ArrayType {tpe = elemTpe}}
    let arr2 = {Var.name = "arr2"; tpe = ArrayType {tpe = elemTpe}}
    let sqfSelArr, oldSqfSelArr, sqfSelSnap = Var sqfVar, Old (Var sqfVar), Snapshot (Var sqfVar)
    let thnCase = mkBlock [
      Ghost (mkBlock [
        rangesEqReflexiveLemma sqfSelArr
        rangesEqSlicedLemma sqfSelArr sqfSelSnap (int32lit 0I) (getLength sqfSelArr) (int32lit 0I) (Var from)
      ])
      rightMutExpr (IntegerType Int) UnitType UnitLit
    ]

    let elseCase =
      let reccallRes = {Var.name = "res"; tpe = fnRetTpe}
      let decodedElemVar = {Var.name = $"{pg.cs.arg.asIdentifier}_arr_iapply_{pg.ixVariable}_"; tpe = elemTpe}
      let sizeConds = [Check (Equals (bitIndexACN (Var cdc), plus [bitIndexACN (Var cdcSnap1); Mult (longlit maxElemSz, Var from)]))]
      let postrecProofSuccess = mkBlock ([
        updatedAtPrefixLemma (Var arr1) (Var from) (Var decodedElemVar)
        rangesEqTransitive (Var arr1) (Var arr2) sqfSelArr (int32lit 0I) (Var from) (plus [Var from; int32lit 1I])
        Check (rangesEq (Var arr1) sqfSelArr (int32lit 0I) (Var from))
        rangesEqImpliesEq (Var arr2) sqfSelArr (int32lit 0I) (Var from) (plus [Var from; int32lit 1I])
      ] @ sizeConds)
      let reccall = FunctionCall {prefix = []; id = fnid; args = [Var cdc; Var sqfVar; plus [Var from; int32lit 1I]]}
      let postrecProof = Ghost (eitherMutMatchExpr (Var reccallRes) None UnitLit None postrecProofSuccess)
      (letsGhostIn [arr1, Snapshot sqfSelArr] (
      mkBlock ((preSerde :: encDec) @ [
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
      let sz =
        match sqf with
        | SqOf _ -> callSizeRangeObj (FieldSelect (Var sqfVar, "arr")) (bitIndexACN oldCdc) (Var from) nbItems
        | StrType _ -> Mult (longlit maxElemSz, Var from)
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
              rightMutExpr (IntegerType Int) UnitType (select (Var sqfVar) (Var from)),
              decodePure
            )
          ]]
      let rightBody = And ([
        Equals (selBuf oldCdc, selBuf (Var cdc))
        Equals (getLength oldSqfSelArr, getLength sqfSelArr)
      ] @ ncountCond @
        [rangesEq oldSqfSelArr sqfSelArr (int32lit 0I) (Var from)] @
        decodeIntoArrayCond @
        [Equals (bitIndexACN (Var cdc), plus [bitIndexACN oldCdc; sz])])
      eitherMutMatchExpr (Var postcondRes) None (BoolLit true) None rightBody

    let fd = {
      FunDef.id = fnid
      prms = [cdc; sqfVar; from]
      annots = [Opaque; InlineOnce]
      specs = if enc = ACN then [Precond fromBounds; Precond validateOffset; Measure decreasesExpr] else []
      postcond = if enc = ACN then Some (postcondRes, postcond) else None
      returnTpe = fnRetTpe
      body = body
    }
    let call =
      let scrut = FunctionCall {prefix = []; id = fnid; args = [Var cdc; Var sqfVar; int32lit 0I]}
      let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
      // TODO: FIXME: the right type must be the outside type!!!
      let leftHACK = ClassCtor {ct = {id = leftMutId; tps = []}; args = [Var leftBdg]}
      let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
      let rightBody = sizeLemmaCall |> Option.map Ghost |> Option.defaultValue UnitLit
      eitherMutMatchExpr scrut (Some leftBdg) leftBody None rightBody
    let call = letsGhostIn [cdcBeforeLoop, Snapshot (Var cdc)] call
    [fd], call

let generateOptionalPrefixLemma (enc: Asn1Encoding) (soc: SequenceOptionalChild): FunDef =
  let codecTpe = runtimeCodecTypeFor enc
  let c1 = {Var.name = "c1"; tpe = ClassType codecTpe}
  let c2 = {Var.name = "c2"; tpe = ClassType codecTpe}
  // The `existVar` does not exist for always present/absent
  let existVar = soc.existVar |> Option.map (fun v -> {Var.name = v; tpe = BooleanType})
  let sizeExpr = longlit soc.child.Type.Kind.baseKind.acnMaxSizeInBits
  let preconds = [
    Precond (Equals (selBufLength (Var c1), selBufLength (Var c2)))
    Precond (validateOffsetBitsACN (Var c1) sizeExpr)
    Precond (arrayBitRangesEq
      (selBuf (Var c1))
      (selBuf (Var c2))
      (longlit 0I)
      (plus [
        bitIndexACN (Var c1)
        existVar |>
          Option.map (fun exist -> IfExpr {cond = Var exist; thn = sizeExpr; els = longlit 0I}) |>
          Option.defaultValue sizeExpr
      ])
    )
  ]
  let elemTpe = fromAsn1TypeKind soc.child.Type.Kind.baseKind
  let existExprArg = existVar |> Option.map Var |> Option.toList
  let decodeId = $"{ToC soc.child.Type.id.dropModule.AsString}_Optional_ACN_Decode"
  let decodePureId = $"{decodeId}_pure"
  let c2Reset = {Var.name = "c2Reset"; tpe = ClassType codecTpe}
  let c1Res = {Var.name = "c1Res"; tpe = ClassType codecTpe}
  let v1 = {Var.name = "v1"; tpe = elemTpe}
  let dec1 = {Var.name = "dec1"; tpe = TupleType [c1Res.tpe; v1.tpe]}
  let call1 = FunctionCall {prefix = []; id = decodePureId; args = [Var c1] @ existExprArg}
  let c2Res = {Var.name = "c2Res"; tpe = ClassType codecTpe}
  let v2 = {Var.name = "v2"; tpe = elemTpe}
  let dec2 = {Var.name = "dec2"; tpe = TupleType [c2Res.tpe; v2.tpe]}
  let call2 = FunctionCall {prefix = []; id = decodePureId; args = [Var c2Reset] @ existExprArg}

  let preSpecs =
    preconds @ [
      LetSpec (c2Reset, resetAtACN (Var c2) (Var c1))
      LetSpec (dec1, call1)
      LetSpec (c1Res, TupleSelect (Var dec1, 1))
      LetSpec (v1, TupleSelect (Var dec1, 2))
      LetSpec (dec2, call2)
      LetSpec (c2Res, TupleSelect (Var dec2, 1))
      LetSpec (v2, TupleSelect (Var dec2, 2))
    ]
  let postcond = And [Equals (bitIndexACN (Var c1Res), bitIndexACN (Var c2Res)); Equals (Var v1, Var v2)]

  let c1Cpy = {Var.name = "c1Cpy"; tpe = ClassType codecTpe}
  let c2ResetCpy = {Var.name = "c2ResetCpy"; tpe = ClassType codecTpe}
  let underlyingPrefixLemma = readPrefixLemmaIdentifier (Asn1 soc.child.Type.Kind.baseKind) soc.child.Type.id false
  let c1Recv, c2Recv = selectCodecReadPrefixLemma underlyingPrefixLemma (Var c1) (Var c2)
  let underlyingPrefixLemmaCall = FunctionCall {prefix = underlyingPrefixLemma.prefix; id = underlyingPrefixLemma.id; args = [c1Recv; c2Recv] @ underlyingPrefixLemma.extraArgs}
  let body = (letsIn [
    (c1Cpy, Snapshot (Var c1))
    (c2ResetCpy, Snapshot (Var c2Reset))
  ] (mkBlock [
    Unfold (FunctionCall {prefix = []; id = decodeId; args = [Var c1Cpy] @ existExprArg})
    Unfold (FunctionCall {prefix = []; id = decodeId; args = [Var c2ResetCpy] @ existExprArg})
    existVar |>
      Option.map (fun exist -> IfExpr {cond = Var exist; thn = underlyingPrefixLemmaCall; els = UnitLit}) |>
      Option.defaultValue underlyingPrefixLemmaCall
  ]))

  {
    FunDef.id = $"{ToC soc.child.Type.id.dropModule.AsString}_prefixLemma"
    prms = [c1; c2] @ (existVar |> Option.toList)
    annots = [GhostAnnot; Pure; Opaque; InlineOnce]
    specs = preSpecs
    postcond = Some ({Var.name = "_"; tpe = UnitType}, postcond)
    returnTpe = UnitType
    body = body
  }

let generateOptionalAuxiliaries (enc: Asn1Encoding) (soc: SequenceOptionalChild) (codec: Codec): FunDef list * Expr =
  if soc.child.Optionality.IsNone then [], EncDec (soc.childBody soc.p soc.existVar)
  else
    //assert (codec = Encode || soc.existVar.IsSome)
    let codecTpe = runtimeCodecTypeFor enc
    let cdc = {Var.name = "codec"; tpe = ClassType codecTpe}
    let oldCdc = {Var.name = $"oldCdc"; tpe = ClassType codecTpe}
    let childAsn1Tpe = soc.child.Type.toAsn1AcnAst
    let childTpe = fromAsn1TypeKind soc.child.Type.Kind.baseKind
    let optChildTpe = ClassType (optionMutTpe childTpe)
    let fnid, fnIdPure =
      //let td = soc.sq.typeDef.[Scala].typeName
      //let prefix = soc.nestingScope.parents |> List.tryHead |> Option.map (fun (cs, _) -> $"{cs.arg.asIdentifier}_") |> Option.defaultValue ""
      let fnId =
        match codec with
        | Encode -> $"{ToC soc.child.Type.id.dropModule.AsString}_Optional_ACN_Encode"
        | Decode -> $"{ToC soc.child.Type.id.dropModule.AsString}_Optional_ACN_Decode"
      fnId, $"{ToC soc.child.Type.id.dropModule.AsString}_Optional_ACN_Decode_pure"
    let errTpe = IntegerType Int
    let validateOffsetBitCond = [Precond (validateOffsetBitsACN (Var cdc) (longlit childAsn1Tpe.acnMaxSizeInBits))]
    let isValidFuncName = soc.child.Type.Kind.isValidFunction |> Option.bind (fun vf -> vf.funcName)

    let sizeExprOf (recv: Expr): SizeExprRes =
      let sz =
        match childAsn1Tpe.Kind with
        | Choice _ | Sequence _ | SequenceOf _ ->
          {bdgs = []; resSize = callSize (getMutExpr recv) (bitIndexACN (Old (Var cdc)))}
        | _ -> asn1SizeExpr childAsn1Tpe.acnAlignment childAsn1Tpe.Kind (getMutExpr recv) (bitIndexACN (Old (Var cdc))) 0I 0I
      match soc.child.Optionality with
        | Some AlwaysPresent -> sz
        | Some AlwaysAbsent -> {sz with resSize = longlit 0I}
        | _ -> {sz with resSize = IfExpr {cond = isDefinedMutExpr recv; thn = sz.resSize; els = longlit 0I}}

    match codec with
    | Encode ->
      let rightTpe = IntegerType Int
      let fnRetTpe = ClassType (eitherTpe errTpe rightTpe)
      let childVar = {Var.name = soc.p.arg.lastId; tpe = optChildTpe}
      let cstrCheck =
        isValidFuncName |> Option.map (fun validFnName ->
          let bdg = {Var.name = "v"; tpe = childTpe}
          let validCall =
            let scrut = FunctionCall {prefix = []; id = validFnName; args = [Var bdg]}
            let leftBdg = {Var.name = "l"; tpe = IntegerType Int}
            let leftBody = Return (leftExpr errTpe rightTpe (Var leftBdg))
            eitherMatchExpr scrut (Some leftBdg) leftBody None (mkBlock [])
          optionMutMatchExpr (Var childVar) (Some bdg) validCall UnitLit
        ) |> Option.toList
      let encDec = EncDec (soc.childBody {soc.p with arg = soc.p.arg.asLastOrSelf} None)
      let resPostcond = {Var.name = "res"; tpe = fnRetTpe}

      let outermostPVal = {Var.name = "pVal"; tpe = fromAsn1TypeKind (soc.nestingScope.parents |> List.last |> snd).Kind}
      let outerPVal = SelectionExpr (joinedSelection soc.p.arg)
      let sz = sizeExprOf (Var childVar)
      let isDefined, alwaysAbsentOrPresent =
        match soc.child.Optionality with
        | Some AlwaysPresent -> [], [isDefinedMutExpr (Var childVar)]
        | Some AlwaysAbsent -> [], [Not (isDefinedMutExpr (Var childVar))]
        | _ -> [isDefinedMutExpr (Var childVar)], []
      let postcondExpr = generateEncodePostcondExprCommon optChildTpe childAsn1Tpe.acnMaxSizeInBits soc.p.arg resPostcond sz alwaysAbsentOrPresent fnIdPure isDefined
      let body = letsGhostIn [(oldCdc, Snapshot (Var cdc))] (mkBlock (cstrCheck @ [encDec; rightExpr errTpe rightTpe (int32lit 0I)]))
      let fd = {
        FunDef.id = fnid
        prms = [cdc; outermostPVal; childVar]
        annots = [Opaque; InlineOnce]
        specs = validateOffsetBitCond
        postcond = Some (resPostcond, postcondExpr)
        returnTpe = fnRetTpe
        body = body
      }
      let call =
        let scrut = FunctionCall {prefix = []; id = fd.id; args = [Var cdc; Var outermostPVal; outerPVal]}
        let leftBdg = {Var.name = "l"; tpe = errTpe}
        // TODO: FIXME: the right type must be the outside type!!!
        let leftHACK = ClassCtor {ct = {id = leftId; tps = []}; args = [Var leftBdg]}
        let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
        eitherMatchExpr scrut (Some leftBdg) leftBody None UnitLit
      [fd], call
    | Decode ->
      // The `existVar` does not exist for always present/absent
      let existVar = soc.existVar |> Option.map (fun v -> {Var.name = v; tpe = BooleanType})
      let rightTpe = optChildTpe
      let outerPVal = {Var.name = soc.p.arg.asIdentifier; tpe = rightTpe}
      let encDec = EncDec (soc.childBody {soc.p with arg = soc.p.arg.asLastOrSelf} soc.existVar)
      let fnRetTpe = ClassType (eitherMutTpe errTpe rightTpe)
      let retVal = {Var.name = soc.p.arg.lastId; tpe = childTpe}
      let retInnerFd =
        let rightRet = rightMutExpr errTpe rightTpe (Var retVal)
        match isValidFuncName with
        | Some validFnName ->
          let someBdg = {Var.name = "v"; tpe = childTpe}
          let eitherPatmat =
            let scrut = FunctionCall {prefix = []; id = validFnName; args = [Var someBdg]}
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
        let isRight = isRightExpr (FunctionCall {prefix = []; id = isValid; args = [Var someBdg]})
        optionMutMatchExpr (Var resvalVar) (Some someBdg) isRight (BoolLit true)) |> Option.toList
      let postcondExpr = generateDecodePostcondExprCommon resPostcond resvalVar sz alwaysAbsentOrPresent cstrIsValid
      let body = letsGhostIn [(oldCdc, Snapshot (Var cdc))] (mkBlock [encDec; retInnerFd])
      let acnParams = acnExternDependenciesVariableDecode soc.child.Type.toAsn1AcnAst soc.nestingScope

      let fd = {
        FunDef.id = fnid
        prms = [cdc] @ (existVar |> Option.toList) @ acnParams
        annots = [Opaque; InlineOnce]
        specs = validateOffsetBitCond
        postcond = Some (resPostcond, postcondExpr)
        returnTpe = fnRetTpe
        body = body
      }

      let call =
        let scrut = FunctionCall {prefix = []; id = fd.id; args = [Var cdc] @ (existVar |> Option.map Var |> Option.toList) @ (acnParams |> List.map Var)}
        let leftBdg = {Var.name = "l"; tpe = errTpe}
        // TODO: FIXME: the right type must be the outside type!!!
        let leftHACK = ClassCtor {ct = {id = leftMutId; tps = []}; args = [Var leftBdg]}
        let leftBody = Return leftHACK // (leftMutExpr errTpe tpe (Var leftBdg)) // TODO: Wrong tpe, it's the one outside!!!
        let rightBdg = {Var.name = "v"; tpe = childTpe}
        let rightBody = Var rightBdg
        eitherMutMatchExpr scrut (Some leftBdg) leftBody (Some rightBdg) rightBody
      let ret = letsIn [(outerPVal, call)] (mkBlock [])

      let fdPure =
        let varCpy = {Var.name = "cpy"; tpe = ClassType codecTpe}
        let varRes = {Var.name = "res"; tpe = fnRetTpe}
        let pureBody = (letsIn
          [varCpy, Snapshot (Var cdc);
          varRes, FunctionCall {prefix = []; id = fd.id; args = [Var varCpy] @ (existVar |> Option.map Var |> Option.toList) @ (acnParams |> List.map Var)}]
          (mkTuple [Var varCpy; Var varRes]))
        {
          FunDef.id = fnIdPure
          prms = [cdc] @ (existVar |> Option.toList) @ acnParams
          annots = [GhostAnnot; Pure]
          specs = validateOffsetBitCond
          postcond = None
          returnTpe = tupleType [ClassType codecTpe; fnRetTpe]
          body = pureBody
        }
      let prefixLemma = generateOptionalPrefixLemma enc soc
      [fd; fdPure], ret
