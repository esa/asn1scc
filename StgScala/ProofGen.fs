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

  let mkLemma (s1: Var) (s2: Var, s3: Var): Expr =
    AppliedLemma {lemma = ValidTransitiveLemma; args = [selBitStream (Var s1); selBitStream (Var s2); selBitStream (Var s3)]}

  let helper (start: int): Expr list =
    let s1 = snapshots.[start]
    List.rep2 ((List.skip (start + 1) snapshots) @ [codec]) |> List.map (mkLemma s1)

  [0 .. snapshots.Length - 2] |> List.collect helper |> mkBlock

let generateReadPrefixLemmaApp (snapshots: Var list) (children: TypeInfo list) (codec: Var) : Expr =
  assert (children.Length = snapshots.Length)

  let rec extraArgsForType (tpe: TypeEncodingKind option): Expr list =
    match tpe with
    | Some (OptionEncodingType tpe) -> extraArgsForType (Some tpe)
    | Some (Asn1IntegerEncodingType (Some encodingTpe)) ->
        match encodingTpe with
        | FullyConstrainedPositive (min, max) -> [IntLit (ULong, min); IntLit (ULong, max)]
        | FullyConstrained (min, max) -> [IntLit (Long, min); IntLit (Long, max)]
        | SemiConstrainedPositive min -> [IntLit (ULong, min)]
        | SemiConstrained max -> [IntLit (Long, max)]
        | UnconstrainedMax max -> [IntLit (Long, max)]
        | Unconstrained -> []
    | _ -> [] // TODO: Rest

  let mkLemma (bs1: Var, bs2: Var, tpe: TypeInfo): Expr =
    let var = {Var.name = $"{bs2.name}_reset"; tpe = bs2.tpe}
    let rst = resetAt (Var bs2) (Var bs1)
    let tpeNoOpt =
      match tpe.typeKind with
      | Some (OptionEncodingType tpe) -> Some tpe
      | _ -> tpe.typeKind
    let varArg, codecArg =
      match lemmaOwner (ReadPrefixLemma tpeNoOpt) with
      | Some (CodecClass BaseCodec) -> selBase (Var var), selBase (Var codec)
      | Some BitStream -> selBitStream (Var var), selBitStream (Var codec)
      | _ -> Var var, Var codec
    let extraArgs = extraArgsForType tpeNoOpt
    let app = AppliedLemma {lemma = ReadPrefixLemma tpeNoOpt; args = [varArg; codecArg] @ extraArgs}
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
            let call = getBitCountUnsigned (IntLit (ULong, max - min))
            // TODO: Case min = max?
            let nBits = if max = min then 0I else bigint (ceil ((log (double (max - min))) / (log 2.0)))
            let cond = Equals (call, IntLit (Int, nBits))
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
    let offsetCheckOverall = Check (Leq (bitIndex (Var cdc), Plus ((bitIndex (Var oldCdc)), (IntLit (Long, offsetAcc)))))
    let offsetCheckNested =
      if isNested then [Check (Leq (bitIndex (Var cdc), Plus ((bitIndex (Var fstSnap)), (IntLit (Long, relativeOffset)))))]
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
          Check (Leq (bitIndex (Var cdc), Plus ((bitIndex (Var oldCdc)), (IntLit (Long, offsetAcc + diff)))));
          Check (Leq (bitIndex (Var cdc), Plus ((bitIndex (Var fstSnap)), (IntLit (Long, relativeOffset + diff)))));
        ]
      | _ -> []
    let checks = offsetCheckOverall :: offsetCheckNested @ bufCheck @ offsetWidening
    let body =
      match stmt with
      | Some stmt when true || ix < nbChildren - 1 ->
        let lemma = AppliedLemma {
          lemma = ValidateOffsetBitsIneqLemma;
          args = [selBitStream (Var snap); selBitStream (Var cdc); IntLit (Long, outerMaxSize - offsetAcc + sz); IntLit (Long, sz)] }
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
      let cond = Leq (bitIndex (Var cdc), Plus ((bitIndex (Var snapshots.Head)), (IntLit (Long, pg.outerMaxSize enc))))
      Ghost (Check cond)
    let expr = wrappedStmts (mkBlock [postCondLemmas])
    let exprStr = show expr
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
    if sqf.isFixedSize then IntLit (Int, nbItemsMin)
    else SelectionExpr $"{pg.sel}.nCount" // TODO: Not ideal...
  let elemSz = sqf.maxElemSizeInBits enc
  let elemSzExpr = IntLit (Long, elemSz)
  let sqfMaxSizeInBits = sqf.maxSizeInBits enc
  let offset = pg.maxOffset enc
  let remainingBits = pg.outerMaxSize enc - sqfMaxSizeInBits - offset
  let remainingBitsExpr = IntLit (Long, remainingBits)

  let codecTpe = runtimeCodecTypeFor enc
  let cdc = {Var.name = $"codec"; tpe = RuntimeType (CodecClass codecTpe)}
  // The codec snapshot before encoding/decoding the whole SequenceOf (i.e. snapshot before entering the while loop)
  let cdcSnap = {Var.name = $"codec_{lvl}_{nestingIx}"; tpe = RuntimeType (CodecClass codecTpe)}
  // The codec snapshot before encoding/decoding one item (snapshot local to the loop, taken before enc/dec one item)
  let cdcLoopSnap = {Var.name = $"codecLoop_{lvl}_{nestingIx}"; tpe = RuntimeType (CodecClass codecTpe)}
  let oldCdc = {Var.name = $"codec_0_1"; tpe = RuntimeType (CodecClass codecTpe)}
  let ix = {name = pg.ixVariable; tpe = IntegerType Int}
  let ixPlusOne = Plus (Var ix, IntLit (Int, 1I))

  let preSerde =
    LetGhost ({
      bdg = cdcLoopSnap
      e = Snapshot (Var cdc)
      body = Ghost (AppliedLemma {
        lemma = ValidateOffsetBitsWeakeningLemma
        args = [
          selBitStream (Var cdc);
          Plus (remainingBitsExpr, Mult (elemSzExpr, Minus (nbItems, Var ix)))
          elemSzExpr
        ]
      })
    })
  let postSerde =
    Ghost (mkBlock [
      Check (Equals (
        Mult (elemSzExpr, Plus (Var ix, IntLit (Int, 1I))),
        Plus (Mult (elemSzExpr, Var ix), elemSzExpr)
      ))
      Check (Leq (
        bitIndex (Var cdc),
        Plus (bitIndex (Var cdcSnap), Plus (IntLit (Long, sizeInBits), Mult (elemSzExpr, ixPlusOne)))
      ))
      AppliedLemma {
        lemma = ValidateOffsetBitsIneqLemma
        args = [
          selBitStream (Var cdcLoopSnap)
          selBitStream (Var cdc)
          Plus (remainingBitsExpr, Mult (elemSzExpr, Minus (nbItems, Var ix)))
          elemSzExpr
        ]
      }
      Check (validateOffsetBits (Var cdc) (Plus (remainingBitsExpr, Mult (elemSzExpr, Minus (nbItems, ixPlusOne)))))
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
      else [And [Leq (IntLit (Int, nbItemsMin), nbItems); Leq (nbItems, (IntLit (Int, nbItemsMax)))]]
    let bixInv = Leq (
      bitIndex (Var cdc),
      Plus (bitIndex (Var cdcSnap), Plus (IntLit (Long, sizeInBits), Mult (elemSzExpr, Var ix)))
    )
    let bixInvOldCdc = Leq (
      bitIndex (Var cdc),
      Plus (bitIndex (Var oldCdc), Plus (IntLit (Long, offset + sizeInBits), Mult (elemSzExpr, Var ix)))
    )
    let offsetInv = validateOffsetBits (Var cdc) (Plus (remainingBitsExpr, Mult (elemSzExpr, Minus (nbItems, Var ix))))
    [bufInv; cdcInv] @ boundsInv @ [bixInv; bixInvOldCdc; offsetInv]

  let postInc =
    Ghost (mkBlock (
      Check (And [
        Leq (IntLit (Int, 0I), Var ix)
        Leq (Var ix, nbItems)
      ]) :: (invariants |> List.map Check)))

  Some {
    preSerde = show preSerde
    postSerde = show postSerde
    postInc = show postInc
    invariant = show (SplitAnd invariants)
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
    if minNbElems = maxNbElems then 0I, IntLit (Long, maxNbElems * charSize)
    else GetNumberOfBitsForNonNegativeInteger(maxNbElems - minNbElems), Mult (IntLit (Long, charSize), stringLength obj)
  let patSize =
    match sizeProps with
    | Some ExternalField | None -> 0I
    | Some (BitsNullTerminated pat) -> (bigint pat.Length) * 8I
    | Some (AsciiNullTerminated pat) -> bigint pat.Length
  Plus (IntLit (Long, vleSize + patSize), nbElemsInBits)

// UPER?
let stringSizeExpr (str: Asn1AcnAst.StringType) (obj: Expr): Expr =

  failwith "TODO"
  (*
  let len = stringLength obj
  let charSize = IntLit (Long, GetNumberOfBitsForNonNegativeInteger (bigint (str.uperCharSet.Length - 1)))
  // TODO: Pas tout à fait
  // The size to encode the length of the string
  let vleSize (minSize: bigint) (maxSize: bigint): Expr =
    let sz =
      if minSize = maxSize then 0I
      else GetNumberOfBitsForNonNegativeInteger(maxSize - minSize)
    IntLit (Long, sz)

  let uperVleSize = vleSize str.minSize.uper str.maxSize.uper
  let acnVleSize = vleSize str.minSize.acn str.maxSize.acn

  // TODO: ACN incomplete, check AcnEncodingClasses.GetStringEncodingClass
  // Plus (uperVleSize, Mult (len, charSize)),
  Plus (acnVleSize, Mult (len, charSize))
  *)

let intSizeExpr (int: Asn1AcnAst.Integer) (obj: Expr): Expr =
  match int.acnProperties.encodingProp, int.acnProperties.sizeProp, int.acnProperties.endiannessProp with
  | None, None, None ->
    match int.uperRange with
    | Full  ->
      Plus (IntLit (Long, 1I), getLengthForEncodingSigned obj)
    | NegInf _ | PosInf _ -> getBitCountUnsigned obj
    | Concrete _ ->
      assert (int.acnMinSizeInBits = int.acnMaxSizeInBits)
      assert (int.uperMinSizeInBits = int.uperMinSizeInBits)
      assert (int.acnMaxSizeInBits = int.uperMaxSizeInBits)
      IntLit (Long, int.acnMaxSizeInBits)
  | _ ->
    assert (int.acnMinSizeInBits = int.acnMaxSizeInBits) // TODO: Not quite true, there is ASCII encoding that is variable...
    IntLit (Long, int.acnMaxSizeInBits)

let asn1SizeExpr (tp: DAst.Asn1TypeKind) (obj: Expr): Expr =
  match tp with
  | DAst.Integer int -> intSizeExpr int.baseInfo obj
  | DAst.Enumerated enm ->
    assert (enm.baseInfo.acnMinSizeInBits = enm.baseInfo.acnMaxSizeInBits)
    IntLit (Long, enm.baseInfo.acnMaxSizeInBits)
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
  | _ -> callSize obj

// TODO: Alignment???
// TODO: UPER/ACN
let seqSizeExpr (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (children: DAst.SeqChildInfo list): Expr =
  // TODO: Alignment???
  // TODO: Pour les int, on peut ajouter une assertion GetBitUnsignedCount(...) == resultat (ici et/ou ailleurs)
  let acnTypeSizeExpr (acn: AcnInsertedType): Expr =
    match acn with
    | AcnInteger int->
      if int.acnMinSizeInBits <> int.acnMaxSizeInBits then failwith "TODO"
      else IntLit (Long, int.acnMaxSizeInBits)

    | AcnNullType nll ->
      assert (nll.acnMinSizeInBits = nll.acnMaxSizeInBits)
      IntLit (Long, nll.acnMaxSizeInBits)

    | AcnBoolean b ->
      assert (b.acnMinSizeInBits = b.acnMaxSizeInBits)
      IntLit (Long, b.acnMaxSizeInBits)

    | AcnReferenceToEnumerated e ->
      if e.enumerated.acnMinSizeInBits <> e.enumerated.acnMaxSizeInBits then failwith "TODO"
      else IntLit (Long, e.enumerated.acnMaxSizeInBits)

    | AcnReferenceToIA5String  s ->
      if s.str.acnMinSizeInBits <> s.str.acnMaxSizeInBits then failwith "TODO"
      else IntLit (Long, s.str.acnMaxSizeInBits)


  // TODO: +Option/presence bit...
  children |> List.fold (fun (acc: Expr) child ->
    // functionArgument qui est paramétrisé (choice) indiqué par asn1Type; determinant = function-ID (dans PerformAFunction)
    let childSz =
      match child with
      | DAst.AcnChild acn ->
        if acn.deps.acnDependencies.IsEmpty then
          // This should not be possible, but ACN parameters are probably validated afterwards.
          IntLit (Long, 0I)
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
    Plus (acc, childSz)
  ) (IntLit (Long, 0I))

let choiceSizeExpr (t: Asn1AcnAst.Asn1Type) (choice: Asn1AcnAst.Choice) (children: DAst.ChChildInfo list): Expr =
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
  MatchExpr {scrut = This; cases = cases}

let seqOfSizeExpr (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.SequenceOf) (elemTpe: DAst.Asn1TypeKind): Expr =
  let from = {name = "from"; tpe = IntegerType Int}
  let tto = {name = "to"; tpe = IntegerType Int}
  let arr = FieldSelect (This, "arr")
  let count = FieldSelect (This, "nCount")
  let require = Require (And [Leq (IntLit (Int, 0I), Var from); Leq (Var from, Var tto); Leq (Var tto, count)])

  let elem = ArraySelect (arr, Var from)
  let reccall = MethodCall {recv = This; id = "size"; args = [Plus (Var from, IntLit (Int, 1I)); Var tto]}
  (mkBlock [
    require
    IfExpr {
      cond = Equals (Var from, Var tto)
      thn = IntLit (Long, 0I)
      els = Plus (asn1SizeExpr elemTpe elem, reccall)
    }
  ])

// TODO: Postcond avec les size
let generateSequenceSizeDefinitions (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (children: DAst.SeqChildInfo list): string list =
  let e = seqSizeExpr t sq children
  // TODO: Function as AST
  let fn = ["def size: Long = {"] @ ((showLines e) |> List.map (fun s -> "    " + s)) @ ["}"]
  [fn.StrJoin "\n"]

let generateChoiceSizeDefinitions (t: Asn1AcnAst.Asn1Type) (choice: Asn1AcnAst.Choice) (children: DAst.ChChildInfo list): string list =
  let e = choiceSizeExpr t choice children
  // TODO: Function as AST
  let fn = ["def size: Long = {"] @ ((showLines e) |> List.map (fun s -> "    " + s)) @ ["}"]
  [fn.StrJoin "\n"]

let generateSequenceOfSizeDefinitions (t: Asn1AcnAst.Asn1Type) (sqf: Asn1AcnAst.SequenceOf) (elemTpe: DAst.Asn1TypeKind): string list =
  let e = seqOfSizeExpr t sqf elemTpe
  // TODO: Function as AST
  let fn1 = ["def size(from: Int, to: Int): Long = {"] @ ((showLines e) |> List.map (fun s -> "    " + s)) @ ["}"]
  let fn2 = "def size: Long = size(0, nCount)"
  [fn1.StrJoin "\n"; fn2]
