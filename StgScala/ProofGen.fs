module ProofGen
open ProofAst
open DAst
open FsUtils
open CommonTypes
open Language
open Asn1AcnAst
open Asn1AcnAstUtilFunctions

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
    let rst = BitStreamMethodCall {method = ResetAt; recv = Var bs2; args = [Var bs1]}
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
            let call = RTFunctionCall {fn = GetBitCountUnsigned; args = [IntLit (ULong, max - min)]}
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
  let isNested = pg.nestingLevel > 0
  assert (isNested || fstSnap = oldCdc)

  let wrap (ix: int, (snap: Var, child: SequenceChildProps, stmt: string option)) (offsetAcc: bigint, rest: Expr): bigint * Expr =
    let sz = child.typeInfo.maxSize enc
    //assert (thisMaxSize <= (pg.siblingMaxSize enc |> Option.defaultValue thisMaxSize)) // TODO: Somehow does not always hold with UPER?
    let relativeOffset = offsetAcc - (pg.maxOffset enc)
    let offsetCheckOverall = Check (Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var oldCdc)), (IntLit (Long, offsetAcc)))))
    let offsetCheckNested =
      if isNested then [Check (Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var fstSnap)), (IntLit (Long, relativeOffset)))))]
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
          Check (Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var oldCdc)), (IntLit (Long, offsetAcc + diff)))));
          Check (Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var fstSnap)), (IntLit (Long, relativeOffset + diff)))));
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
    let snapshots = [1 .. pg.children.Length] |> List.map (fun i -> {Var.name = $"codec_{pg.nestingLevel}_{pg.nestingIx + i}"; tpe = RuntimeType (CodecClass codecTpe)})

    let wrappedStmts = wrapEncDecStmts enc snapshots cdc oldCdc stmts pg codec

    let postCondLemmas =
      let cond = Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var snapshots.Head)), (IntLit (Long, pg.outerMaxSize enc))))
      Ghost (Check cond)
    let expr = wrappedStmts (mkBlock [postCondLemmas])
    let exprStr = show expr
    [exprStr]

let generateSequenceOfLikeProof (enc: Asn1Encoding) (sqf: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): SequenceOfLikeProofGenResult option =
  let nbItemsMin, nbItemsMax = sqf.nbElems enc

  let isSizeExternal =
    match enc, sqf with
    | UPER, _ -> true
    | ACN, SqOf sqf ->
      match sqf.acnEncodingClass with
      | SizeableAcnEncodingClass.SZ_EC_FIXED_SIZE | SizeableAcnEncodingClass.SZ_EC_LENGTH_EMBEDDED _ -> not sqf.isFixedSize // TODO: Check if we can have SZ_EC_FIXED_SIZE with not sqf.isFixedSize (copying logic from DAstACN)
      | _ -> true
    | ACN, StrType str ->
      true // TODO
    | _ -> failwith $"Unexpected encoding: {enc}"

  let externSizeInBits =
    if isSizeExternal then GetNumberOfBitsForNonNegativeInteger (nbItemsMax - nbItemsMin)
    else 0I
  let nbItems =
    if sqf.isFixedSize then IntLit (Int, nbItemsMin)
    else SelectionExpr $"{pg.sel}.nCount" // TODO: Not ideal...
  let elemSz = sqf.maxElemSizeInBits enc
  let elemSzExpr = IntLit (Long, elemSz)
  let sqfMaxSizeInBits = sqf.maxSizeInBits enc
  let remainingBits = pg.outerMaxSize enc - sqfMaxSizeInBits - pg.maxOffset enc
  let remainingBitsExpr = IntLit (Long, remainingBits)

  let codecTpe = runtimeCodecTypeFor enc
  let cdc = {Var.name = $"codec"; tpe = RuntimeType (CodecClass codecTpe)}
  // The codec snapshot before encoding/decoding the whole SequenceOf (i.e. snapshot before entering the while loop)
  let lvl = max 0 (pg.nestingLevel - 1)
  let cdcSnap = {Var.name = $"codec_{lvl}_{pg.nestingIx + 1}"; tpe = RuntimeType (CodecClass codecTpe)}
  // The codec snapshot before encoding/decoding one item (snapshot local to the loop, taken before enc/dec one item)
  let cdcLoopSnap = {Var.name = $"codecLoop_{lvl}_{pg.nestingIx + 1}"; tpe = RuntimeType (CodecClass codecTpe)}
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
        callBitIndex (Var cdc),
        Plus (callBitIndex (Var cdcSnap), Plus (IntLit (Long, externSizeInBits), Mult (elemSzExpr, ixPlusOne)))
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
      Check (callValidateOffsetBits (Var cdc) (Plus (remainingBitsExpr, Mult (elemSzExpr, Minus (nbItems, ixPlusOne)))))
    ])
  let invariants =
    let bufInv =
      if codec = Encode then
        Equals (selBufLength (Var cdc), selBufLength (Var cdcSnap))
      else
        Equals (selBuf (Var cdc), selBuf (Var cdcSnap))
    let cdcInv = callInvariant (Var cdc)
    let boundsInv =
      if sqf.isFixedSize then []
      else [And [Leq (IntLit (Int, nbItemsMin), nbItems); Leq (nbItems, (IntLit (Int, nbItemsMax)))]]
    let bixInv = Leq (
      callBitIndex (Var cdc),
      Plus (callBitIndex (Var cdcSnap), Plus (IntLit (Long, externSizeInBits), Mult (elemSzExpr, Var ix)))
    )
    let offsetInv = callValidateOffsetBits (Var cdc) (Plus (remainingBitsExpr, Mult (elemSzExpr, Minus (nbItems, Var ix))))
    [bufInv; cdcInv] @ boundsInv @ [bixInv; offsetInv]

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