module ProofGen
open ProofAst
open DAst
open FsUtils
open CommonTypes
open Language
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open AcnGenericTypes

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

let wrapEncDecStmts (enc: Asn1Encoding) (snapshots: Var list) (cdc: Var) (oldCdc: Var) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec) (rest: Expr): Expr =
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

  let wrap (ix: int, (snap: Var, child: SequenceChildProps, stmt: string option)) (offsetAcc: bigint, rest: Expr): bigint * Expr =
    let sz = child.typeInfo.maxSize enc
    //assert (thisMaxSize <= (pg.siblingMaxSize enc |> Option.defaultValue thisMaxSize)) // TODO: Somehow does not always hold with UPER?
    let relativeOffset = offsetAcc - (pg.maxOffset enc)
    let offsetCheckOverall = Check (Leq (bitIndex (Var cdc), plus [bitIndex (Var oldCdc); (longlit offsetAcc)]))
    let offsetCheckNested =
      if isNested then [Check (Leq (bitIndex (Var cdc), plus [bitIndex (Var fstSnap); longlit relativeOffset]))]
      else []
    let bufCheck =
      match codec with
       | Encode -> []
       | Decode -> [Check ((Equals (selBuf (Var cdc), selBuf (Var oldCdc))))]
    let offsetWidening =
      match pg.siblingMaxSize enc with
      | Some siblingMaxSize when ix = nbChildren - 1 && siblingMaxSize <> thisMaxSize ->
        let diff = siblingMaxSize - thisMaxSize
        [
          Check (Leq (bitIndex (Var cdc), plus [bitIndex (Var oldCdc); longlit (offsetAcc + diff)]));
          Check (Leq (bitIndex (Var cdc), plus [bitIndex (Var fstSnap); longlit (relativeOffset + diff)]));
        ]
      | _ -> []
    let checks = offsetCheckOverall :: offsetCheckNested @ bufCheck @ offsetWidening
    let body =
      match stmt with
      | Some stmt when true || ix < nbChildren - 1 ->
        let lemma = validateOffsetBitsIneqLemma (selBitStream (Var snap)) (selBitStream (Var cdc)) (longlit (outerMaxSize - offsetAcc + sz)) (longlit sz)
        mkBlock ((addAssert child.typeInfo.typeKind) :: [EncDec stmt; Ghost (mkBlock (lemma :: checks)); rest])

      | Some stmt ->
        mkBlock ((addAssert child.typeInfo.typeKind) :: ([EncDec stmt; Ghost (mkBlock checks); rest]))

      | _ -> mkBlock [Ghost (mkBlock checks); rest]

    (offsetAcc - sz, LetGhost {bdg = snap; e = Snapshot (Var cdc); body = body})

  let stmts = List.zip3 snapshots pg.children stmts |> List.indexed
  List.foldBack wrap stmts ((pg.maxOffset enc) + thisMaxSize, rest) |> snd

let generateSequenceChildProof (enc: Asn1Encoding) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec): string list =
  if stmts.IsEmpty then []
  else
    let codecTpe = runtimeCodecTypeFor enc
    let cdc = {Var.name = $"codec"; tpe = RuntimeType (CodecClass codecTpe)}
    let oldCdc = {Var.name = $"codec_0_1"; tpe = RuntimeType (CodecClass codecTpe)}
    let snapshots = [1 .. pg.children.Length] |> List.map (fun i -> {Var.name = $"codec_{pg.nestingLevel}_{pg.nestingIx + bigint i}"; tpe = RuntimeType (CodecClass codecTpe)})

    let wrappedStmts = wrapEncDecStmts enc snapshots cdc oldCdc stmts pg codec

    let postCondLemmas =
      let cond = Leq (bitIndex (Var cdc), plus [bitIndex (Var snapshots.Head); longlit (pg.outerMaxSize enc)])
      Ghost (Check cond)
    let expr = wrappedStmts (mkBlock [postCondLemmas])
    let exprStr = show (ExprTree expr)
    [exprStr]

let generateSequenceOfLikeProof (enc: Asn1Encoding) (sqf: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): SequenceOfLikeProofGenResult option =
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
    else SelectionExpr $"{pg.sel}.nCount" // TODO: Not ideal...
  let elemSz = sqf.maxElemSizeInBits enc
  let elemSzExpr = longlit elemSz
  let sqfMaxSizeInBits = sqf.maxSizeInBits enc
  let offset = pg.maxOffset enc
  let remainingBits = pg.outerMaxSize enc - sqfMaxSizeInBits - offset
  let remainingBitsExpr = longlit remainingBits

  let codecTpe = runtimeCodecTypeFor enc
  let cdc = {Var.name = $"codec"; tpe = RuntimeType (CodecClass codecTpe)}
  // The codec snapshot before encoding/decoding the whole SequenceOf (i.e. snapshot before entering the while loop)
  let cdcSnap = {Var.name = $"codec_{lvl}_{nestingIx}"; tpe = RuntimeType (CodecClass codecTpe)}
  // The codec snapshot before encoding/decoding one item (snapshot local to the loop, taken before enc/dec one item)
  let cdcLoopSnap = {Var.name = $"codecLoop_{lvl}_{nestingIx}"; tpe = RuntimeType (CodecClass codecTpe)}
  let oldCdc = {Var.name = $"codec_0_1"; tpe = RuntimeType (CodecClass codecTpe)}
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

type SizeProps =
  | ExternalField
  | BitsNullTerminated of string
  | AsciiNullTerminated of byte list

let fromAcnSizeProps (sizeProps: AcnStringSizeProperty): SizeProps =
  match sizeProps with
  | StrExternalField _ -> ExternalField
  | StrNullTerminated pat -> AsciiNullTerminated pat

let fromSizeableProps (sizeProps: AcnSizeableSizeProperty): SizeProps =
  match sizeProps with
  | SzExternalField _ -> ExternalField
  | SzNullTerminated pat -> BitsNullTerminated pat.Value

let stringLikeSizeExpr (sizeProps: SizeProps option) (minNbElems: bigint) (maxNbElems: bigint) (charSize: bigint) (obj: Expr): Expr =
  let vleSize, nbElemsInBits =
    if minNbElems = maxNbElems then 0I, longlit (maxNbElems * charSize)
    else GetNumberOfBitsForNonNegativeInteger(maxNbElems - minNbElems), Mult (longlit charSize, stringLength obj)
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

let asn1SizeExpr (tp: DAst.Asn1TypeKind) (obj: Expr): Expr =
  match tp with
  | DAst.Integer int -> intSizeExpr int.baseInfo obj
  | DAst.Enumerated enm ->
    assert (enm.baseInfo.acnMinSizeInBits = enm.baseInfo.acnMaxSizeInBits)
    longlit enm.baseInfo.acnMaxSizeInBits
  | DAst.IA5String st ->
    let szProps = st.baseInfo.acnProperties.sizeProp |> Option.map fromAcnSizeProps
    let charSize = GetNumberOfBitsForNonNegativeInteger (bigint (st.baseInfo.uperCharSet.Length - 1))
    stringLikeSizeExpr szProps st.baseInfo.minSize.acn st.baseInfo.maxSize.acn charSize obj
  | DAst.OctetString ot ->
    let szProps = ot.baseInfo.acnProperties.sizeProp |> Option.map fromSizeableProps
    stringLikeSizeExpr szProps ot.baseInfo.minSize.acn ot.baseInfo.maxSize.acn 8I obj
  | DAst.BitString bt ->
    let szProps = bt.baseInfo.acnProperties.sizeProp |> Option.map fromSizeableProps
    stringLikeSizeExpr szProps bt.baseInfo.minSize.acn bt.baseInfo.maxSize.acn 1I obj
  | DAst.NullType nt ->
    assert (nt.baseInfo.acnMinSizeInBits = nt.baseInfo.acnMaxSizeInBits)
    longlit nt.baseInfo.acnMaxSizeInBits
  | _ -> callSize obj

// TODO: Alignment???
// TODO: UPER/ACN
let seqSizeExpr (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (children: DAst.SeqChildInfo list): FunDef =
  // TODO: Alignment???
  // TODO: Pour les int, on peut ajouter une assertion GetBitUnsignedCount(...) == resultat (ici et/ou ailleurs)
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

  if sq.acnMinSizeInBits = sq.acnMaxSizeInBits then
    {
      id = "size"
      prms = []
      specs = []
      postcond = None
      returnTpe = IntegerType Long
      body = longlit sq.acnMaxSizeInBits
    }
  else
    // TODO: +Option/presence bit...
    let sizes =
      children |> List.map (fun child ->
        // functionArgument qui est paramétrisé (choice) indiqué par asn1Type; determinant = function-ID (dans PerformAFunction)
        match child with
        | DAst.AcnChild acn ->
          if acn.deps.acnDependencies.IsEmpty then
            // This should not be possible, but ACN parameters are probably validated afterwards.
            longlit 0I
          else
            // There can be multiple dependencies on an ACN field, however all must be consistent
            // (generated code checks for that, done by MultiAcnUpdate).
            // For our use case, we assume the message is consistent, we therefore pick
            // an arbitrary dependency.
            // If it is not the case, the returned value may be incorrect but we would
            // not encode the message anyway, so this incorrect size would not be used.
            // To do things properly, we should move this check of MultiAcnUpdate in the IsConstraintValid function
            // of the message and add it as a precondition to the size function.
            // TODO: variable-length size
            acnTypeSizeExpr acn.Type
        | DAst.Asn1Child asn1 ->
          asn1SizeExpr asn1.Type.Kind (FieldSelect (This, asn1._scala_name))
      )
    let res = {name = "res"; tpe = IntegerType Long}
    let postcond = And [Leq (longlit sq.acnMinSizeInBits, Var res); Leq (Var res, longlit sq.acnMaxSizeInBits)]
    {
      id = "size"
      prms = []
      specs = []
      postcond = Some (res, postcond)
      returnTpe = IntegerType Long
      body = plus sizes
    }

let choiceSizeExpr (t: Asn1AcnAst.Asn1Type) (choice: Asn1AcnAst.Choice) (children: DAst.ChChildInfo list): FunDef =
  if choice.acnMinSizeInBits = choice.acnMaxSizeInBits then
    {
      id = "size"
      prms = []
      specs = []
      postcond = None
      returnTpe = IntegerType Long
      body = longlit choice.acnMaxSizeInBits
    }
  else
    let cases = children |> List.map (fun child ->
      let tpeId = (ToC child._present_when_name_private) + "_PRESENT"
      let tpe = TypeInfo {
        typeKind = Some (ReferenceEncodingType tpeId)
        uperMaxSizeBits = child.chType.Kind.baseKind.uperMaxSizeInBits
        acnMaxSizeBits = child.chType.Kind.baseKind.acnMaxSizeInBits
      }
      let binder = {Var.name = child._scala_name; tpe = tpe}
      let pat = ADTPattern {binder = None; id = tpeId; subPatterns = [Wildcard (Some binder)]}
      let rhs = asn1SizeExpr child.chType.Kind (Var binder)
      {MatchCase.pattern = pat; rhs = rhs}
    )
    let res = {name = "res"; tpe = IntegerType Long}
    let postcond = And [Leq (longlit choice.acnMinSizeInBits, Var res); Leq (Var res, longlit choice.acnMaxSizeInBits)]
    {
      id = "size"
      prms = []
      specs = []
      postcond = Some (res, postcond)
      returnTpe = IntegerType Long
      body = MatchExpr {scrut = This; cases = cases}
    }

let seqOfSizeExpr (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.SequenceOf) (elemTpe: DAst.Asn1TypeKind): FunDef option * FunDef =
  if sq.acnMinSizeInBits = sq.acnMaxSizeInBits then
    let fd2 =
      {
        id = "size"
        prms = []
        specs = []
        postcond = None
        returnTpe = IntegerType Long
        body = longlit sq.acnMaxSizeInBits
      }
    None, fd2
  else
    let res = {name = "res"; tpe = IntegerType Long}
    let postcond = And [Leq (longlit sq.acnMinSizeInBits, Var res); Leq (Var res, longlit sq.acnMaxSizeInBits)]
    let count = FieldSelect (This, "nCount")

    let fd1 =
      let from = {name = "from"; tpe = IntegerType Int}
      let tto = {name = "to"; tpe = IntegerType Int}
      let arr = FieldSelect (This, "arr")
      let require = And [Leq (int32lit 0I, Var from); Leq (Var from, Var tto); Leq (Var tto, count)]

      let elem = ArraySelect (arr, Var from)
      let reccall = MethodCall {recv = This; id = "sizeRange"; args = [plus [Var from; int32lit 1I]; Var tto]}
      let body =
        IfExpr {
          cond = Equals (Var from, Var tto)
          thn = longlit 0I
          els = plus [asn1SizeExpr elemTpe elem; reccall]
        }
      {
        id = "sizeRange"
        prms = [from; tto]
        specs = [Precond require]
        postcond = Some (res, postcond)
        returnTpe = IntegerType Long
        body = body
      }
    let fd2 =
      {
        id = "size"
        prms = []
        specs = []
        postcond = Some (res, postcond)
        returnTpe = IntegerType Long
        body = MethodCall {recv = This; id = fd1.id; args = [int32lit 0I; count]}
      }
    Some fd1, fd2

// TODO: Postcond avec les size
let generateSequenceSizeDefinitions (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (children: DAst.SeqChildInfo list): string list =
  let fd = seqSizeExpr t sq children
  [show (FunDefTree fd)]

let generateChoiceSizeDefinitions (t: Asn1AcnAst.Asn1Type) (choice: Asn1AcnAst.Choice) (children: DAst.ChChildInfo list): string list =
  let fd = choiceSizeExpr t choice children
  [show (FunDefTree fd)]

let generateSequenceOfSizeDefinitions (t: Asn1AcnAst.Asn1Type) (sqf: Asn1AcnAst.SequenceOf) (elemTpe: DAst.Asn1TypeKind): string list =
  let fd1, fd2 = seqOfSizeExpr t sqf elemTpe
  let fd1 = fd1 |> Option.map (fun fd -> [show (FunDefTree fd)]) |> Option.defaultValue []
  fd1 @ [show (FunDefTree fd2)]
