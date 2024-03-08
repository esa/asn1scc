module ProofGen
open ProofAst
open DAst
open FsUtils
open CommonTypes
open Language


// TODO: Gag, est si l'un des codec manque?

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

let wrapEncDecStmts (enc: Asn1Encoding) (snapshots: Var list) (cdc: Var) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec) (rest: Expr): Expr =
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

  let extraLemmas (sel: string option) (snap: Var) (tpe: TypeEncodingKind option) (offset: bigint) (sz: bigint): Expr list =
    match tpe with
    | Some (OptionEncodingType _) when codec = Encode ->
      assert sel.IsSome
      let scrut = SelectionExpr sel.Value
      let somePat = ADTPattern {binder = None; id = "SomeMut"; subPatterns = [Wildcard None]}
      let someCase = {pattern = somePat; rhs = mkBlock []}
      let nonePat = ADTPattern {binder = None; id = "NoneMut"; subPatterns = []}
      // let buf = selBuf (Var snap)
      let noneRhs = [
        // AppliedLemma {lemma = ValidReflexiveLemma; args = [selBitStream (Var snap)]};
        Check (Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var snapshots.Head)), (IntLit offset))), Block [])
        // AppliedLemma {lemma = ArrayBitRangesEqReflexiveLemma; args = [buf]};
        // Snapshot buf needed for AntiAliasing
        // AppliedLemma {lemma = ArrayBitRangesEqSlicedLemma; args = [buf; Snapshot buf; IntLit 0I; Mult (ToLong (ArrayLength buf), IntLit 8I); IntLit 0I; IntLit offset]};
      ]
      let noneCase  = {pattern = nonePat; rhs = mkBlock noneRhs}
      [MatchExpr {scrut = scrut; cases = [someCase; noneCase]}]
    | _ -> []
  let maxSize = pg.maxSize enc
  let wrap (ix: int, (snap: Var, child: SequenceChildProps, stmt: string option)) (offsetAcc: bigint, rest: Expr): bigint * Expr =
    let sz = child.typeInfo.maxSize enc
    let body =
      match stmt with
      | Some stmt when ix < nbChildren - 1 ->
        let lemma = AppliedLemma {
          lemma = ValidateOffsetBitsIneqLemma;
          args = [selBitStream (Var snap); selBitStream (Var cdc); IntLit (maxSize - offsetAcc + sz); IntLit sz]}
        let extra = extraLemmas child.sel snap child.typeInfo.typeKind offsetAcc sz
        addAssert child.typeInfo.typeKind [EncDec stmt; Ghost (mkBlock (lemma :: extra)); rest]
      | Some stmt ->
        let extra = extraLemmas child.sel snap child.typeInfo.typeKind offsetAcc sz
        let ghostBlock =
          if extra.IsEmpty then []
          else [Ghost (mkBlock extra)]
        addAssert child.typeInfo.typeKind ([EncDec stmt] @ ghostBlock @ [rest])
      | _ -> mkBlock [rest]
    (offsetAcc - sz, LetGhost {bdg = snap; e = Snapshot (Var cdc); body = body})

  let stmts = List.zip3 snapshots pg.children stmts |> List.indexed
  List.foldBack wrap stmts (maxSize, rest) |> snd

let generateSequenceChildProof (enc: Asn1Encoding) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec): string list =
  if stmts.IsEmpty then []
  else
    let codecTpe = runtimeCodecTypeFor enc
    let cdc = {Var.name = $"codec"; tpe = RuntimeType (CodecClass codecTpe)}
    let snapshots = [1 .. pg.children.Length] |> List.map (fun i -> {Var.name = $"codec{i}"; tpe = RuntimeType (CodecClass codecTpe)})

    let wrappedStmts = wrapEncDecStmts enc snapshots cdc stmts pg codec
    (*
    let postCondLemmas =
      match codec with
      | Decode -> []
      | Encode ->
        [generateTransitiveLemmaApp snapshots cdc] @
        [generateReadPrefixLemmaApp snapshots (pg.children |> List.map (fun c -> c.typeInfo)) cdc]
    *)
    let postCondLemmas =
      let cond = Leq (callBitIndex (Var cdc), Plus ((callBitIndex (Var snapshots.Head)), (IntLit (pg.maxSize enc))))
      Ghost (Check (cond, mkBlock []))
    let expr = wrappedStmts (mkBlock [postCondLemmas])
    let exprStr = show expr
    [exprStr]
