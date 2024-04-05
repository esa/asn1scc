module ProofGen
open ProofAst
open DAst
open FsUtils
open CommonTypes
open Language

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
        | FullyConstrainedPositive (min, max) -> [ToRawULong (IntLit min); ToRawULong (IntLit max)]
        | FullyConstrained (min, max) -> [IntLit min; IntLit max]
        | SemiConstrainedPositive min -> [ToRawULong (IntLit min)]
        | SemiConstrained max -> [IntLit max]
        | UnconstrainedMax max -> [IntLit max]
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
            let call = RTFunctionCall {fn = GetBitCountUnsigned; args = [ToRawULong (IntLit (max - min))]}
            // TODO: Cas min = max?
            let nBits = if max = min then 0I else bigint (ceil ((log (double (max - min))) / (log 2.0)))
            let cond = Equals (call, IntLit nBits)
            Some cond
        | _ -> None
      | _ -> None

  let addAssert (tpe: TypeEncodingKind option) (exprs: Expr list): Expr =
    assertionsConditions tpe |> Option.map (fun cond -> Assert (cond, mkBlock exprs))
                             |> Option.defaultValue (mkBlock exprs)

  let outerMaxSize = pg.outerMaxSize enc
  let thisMaxSize = pg.maxSize enc
  let fstSnap = snapshots.Head
  let isNested = pg.nestingLevel > 0
  assert (isNested || fstSnap = oldCdc)

  let wrap (ix: int, (snap: Var, child: SequenceChildProps, stmt: string option)) (offsetAcc: bigint, rest: Expr): bigint * Expr =
    let sz = child.typeInfo.maxSize enc
    assert (thisMaxSize <= (pg.siblingMaxSize enc |> Option.defaultValue thisMaxSize))
    let relativeOffset = offsetAcc - (pg.maxOffset enc)
    let offsetCheckOverall = Check (Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var oldCdc)), (IntLit offsetAcc))), Block [])
    let offsetCheckNested =
      if isNested then [Check (Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var fstSnap)), (IntLit relativeOffset))), Block [])]
      else []
    let bufCheck =
      match codec with
       | Encode -> []
       | Decode -> [Check ((Equals (selBuf (Var cdc), selBuf (Var oldCdc))), Block [])]
    let offsetWidening =
      match pg.siblingMaxSize enc with
      | Some siblingMaxSize when ix = nbChildren - 1 && siblingMaxSize <> thisMaxSize ->
        let diff = siblingMaxSize - thisMaxSize
        [
          Check (Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var oldCdc)), (IntLit (offsetAcc + diff)))), Block []);
          Check (Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var fstSnap)), (IntLit (relativeOffset + diff)))), Block []);
        ]
      | _ -> []
    let checks = offsetCheckOverall :: offsetCheckNested @ bufCheck @ offsetWidening
    let body =
      match stmt with
      | Some stmt when true || ix < nbChildren - 1 ->
        let lemma = AppliedLemma {
          lemma = ValidateOffsetBitsIneqLemma;
          args = [selBitStream (Var snap); selBitStream (Var cdc); IntLit (outerMaxSize - offsetAcc + sz); IntLit sz] }
        addAssert child.typeInfo.typeKind [EncDec stmt; Ghost (mkBlock (lemma :: checks)); rest]

      | Some stmt ->
        addAssert child.typeInfo.typeKind ([EncDec stmt; Ghost (mkBlock checks); rest])

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
    let snapshots = [1 .. pg.children.Length] |> List.map (fun i -> {Var.name = $"codec_{pg.nestingLevel}_{i}"; tpe = RuntimeType (CodecClass codecTpe)})

    let wrappedStmts = wrapEncDecStmts enc snapshots cdc oldCdc stmts pg codec

    let postCondLemmas =
      let cond = Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var snapshots.Head)), (IntLit (pg.outerMaxSize enc))))
      Ghost (Check (cond, mkBlock []))
    let expr = wrappedStmts (mkBlock [postCondLemmas])
    let exprStr = show expr
    [exprStr]
