module ProofAst

open FsUtils
open Language
open DAst
open CommonTypes

type CodecClass =
  | BaseCodec
  | AcnCodec
  | UperCodec
with
  member this.companionObjectName =
    match this with
    | BaseCodec -> "Codec"
    | AcnCodec -> "ACN"
    | UperCodec -> "UPER"

type RuntimeType =
  | BitStream
  | CodecClass of CodecClass
with
  member this.companionObjectName =
    match this with
    | BitStream -> "BitStream"
    | CodecClass cc -> cc.companionObjectName

type Type =
  | RuntimeType of RuntimeType
  | TypeInfo of TypeInfo


type Lemma =
  | ValidTransitiveLemma
  | ValidReflexiveLemma
  | ArrayBitRangesEqReflexiveLemma
  | ArrayBitRangesEqSlicedLemma
  | ValidateOffsetBitsIneqLemma
  | ReadPrefixLemma of TypeEncodingKind option

type BitStreamMethod =
  | ResetAt

type BitStreamFunction =
  | BitIndex

type RTFunction =
  | GetBitCountUnsigned

type Var = {
  name: string
  tpe: Type
}

type Pattern =
  | Wildcard of Var option
  | ADTPattern of ADTPattern

and ADTPattern = {
  binder: Var option
  id: string // TODO: Have something better
  subPatterns: Pattern list
}

type Expr =
  | Var of Var
  | Block of Expr list
  | Ghost of Expr
  | Locally of Expr
  | AppliedLemma of AppliedLemma
  | Snapshot of Expr
  | Let of Let
  | LetGhost of Let
  | Assert of Expr * Expr
  | Check of Expr * Expr
  | BitStreamMethodCall of BitStreamMethodCall
  | BitStreamFunctionCall of BitStreamFunctionCall
  | RTFunctionCall of RTFunctionCall
  | TupleSelect of Expr * int
  | FieldSelect of Expr * string
  | ArraySelect of Expr * Expr
  | ArrayLength of Expr
  | MatchExpr of MatchExpr
  | Equals of Expr * Expr
  | Mult of Expr * Expr
  | Plus of Expr * Expr
  | Leq of Expr * Expr
  | IntLit of bigint // TODO: Add the ranges as well
  | ToRawULong of Expr // TODO: Find something better and that makes all necessary "jumps", so no Int -> ULong but rather Int -> Long -> ULong
  | ToLong of Expr // TODO: Find something better and that makes all necessary "jumps", so no Int -> ULong but rather Int -> Long -> ULong
  | EncDec of string
  | SelectionExpr of string // TODO: Not ideal

and AppliedLemma = {
  lemma: Lemma
  args: Expr list
}

and Let = {
  bdg: Var
  e: Expr
  body: Expr
}

and BitStreamMethodCall = {
  method: BitStreamMethod
  recv: Expr
  args: Expr list
}
and BitStreamFunctionCall = {
  fn: BitStreamFunction
  args: Expr list
}
and RTFunctionCall = {
  fn: RTFunction
  args: Expr list
}
and MatchExpr = {
  scrut: Expr
  cases: MatchCase list
}
and MatchCase = {
  pattern: Pattern
  rhs: Expr
}

let mkBlock (exprs: Expr list): Expr =
  if exprs.Length = 1 then exprs.Head
  else
    exprs |> List.collect (fun e -> match e with Block exprs -> exprs | _ -> [e])
          |> Block

let selBase (recv: Expr): Expr = FieldSelect (recv, "base")

let selBitStream (recv: Expr): Expr = FieldSelect (selBase recv, "bitStream")
let selBuf (recv: Expr): Expr = FieldSelect (selBase recv, "buf")
let selBufLength (recv: Expr): Expr =  ArrayLength (selBuf recv)
let selCurrentByte (recv: Expr): Expr =  FieldSelect (selBitStream recv, "currentByte")
let selCurrentBit (recv: Expr): Expr =  FieldSelect (selBitStream recv, "currentBit")
let callBitIndex (recv: Expr): Expr = BitStreamFunctionCall { fn = BitIndex; args = [selBufLength recv; selCurrentByte recv; selCurrentBit recv] }


//////////////////////////////////////////////////////////

let runtimeCodecTypeFor (enc: Asn1Encoding): CodecClass =
  match enc with
  | UPER -> UperCodec
  | ACN -> AcnCodec
  | _ -> failwith $"Unsupported: {enc}"
let lemmaOwner (lemma: Lemma): RuntimeType option =
  match lemma with
  | ValidateOffsetBitsIneqLemma
  | ValidTransitiveLemma
  | ValidReflexiveLemma -> Some BitStream

  | ArrayBitRangesEqReflexiveLemma
  | ArrayBitRangesEqSlicedLemma -> None

  | ReadPrefixLemma t ->
    match t with
    | Some (AcnIntegerEncodingType int) -> Some (CodecClass AcnCodec)
    | Some (Asn1IntegerEncodingType _) -> Some (CodecClass BaseCodec)
    | Some (AcnBooleanEncodingType None) -> Some BitStream // TODO: Check this
    | None -> failwith "TODO: Implement me"
    | _ ->
      None // TODO: Rest

let lemmaStr (lemma: Lemma): string =
  let name =
    match lemma with
    | ValidTransitiveLemma -> "validTransitiveLemma"
    | ValidReflexiveLemma -> "validReflexiveLemma"
    | ValidateOffsetBitsIneqLemma -> "validateOffsetBitsIneqLemma"
    | ArrayBitRangesEqReflexiveLemma -> "arrayBitRangesEqReflexiveLemma"
    | ArrayBitRangesEqSlicedLemma -> "arrayBitRangesEqSlicedLemma"
    | ReadPrefixLemma t ->
      match t with
      | None -> failwith "TODO: Implement me"
      | Some (AcnBooleanEncodingType None) -> "readBitPrefixLemma" // TODO: Check this
      | Some (AcnIntegerEncodingType int) ->
        let sign =
          match int.signedness with
          | Positive -> "PositiveInteger"
          | TwosComplement -> "TwosComplement"
        let endian, sz =
          match int.endianness with
          | Byte -> None, Some "8"
          | Unbounded -> None, None
          | LittleEndian sz -> Some "little_endian", Some (sz.bitSize.ToString())
          | BigEndian sz -> Some "big_endian", Some (sz.bitSize.ToString())
        ([Some "dec"; Some "Int"; Some sign; Some "ConstSize"; endian; sz; Some "prefixLemma"] |> List.choose id).StrJoin "_"
      | Some (Asn1IntegerEncodingType (Some Unconstrained)) ->
        "decodeUnconstrainedWholeNumber_prefixLemma"
      | Some (Asn1IntegerEncodingType (Some (FullyConstrainedPositive _))) ->
        "decodeConstrainedPosWholeNumber_prefixLemma"
      | _ ->
        "ACN.readPrefixLemma_TODO" // TODO
  let owner = lemmaOwner lemma
  ((owner |> Option.map (fun o -> o.companionObjectName) |> Option.toList) @ [name]).StrJoin "."

let bsMethodCallStr (meth: BitStreamMethod): string =
  match meth with
  | ResetAt -> "resetAt"

let rtFnCall (fn: RTFunction): string =
  match fn with
  | GetBitCountUnsigned -> "GetBitCountUnsigned"

let bsFnCall (fn: BitStreamFunction): string =
  match fn with
  | BitIndex -> "BitStream.bitIndex"

//////////////////////////////////////////////////////////

type PrintCtx = {
  curr: Expr
  parents: Expr list
  lvl: int
} with
  member this.inc: PrintCtx = {this with lvl = this.lvl + 1}

  member this.parent = List.tryHead this.parents

  member this.nest (e: Expr): PrintCtx = {this with curr = e; parents = this.curr :: this.parents}

type Line = {
  txt: string
  lvl: int
} with
  member this.inc: Line = {this with lvl = this.lvl + 1}

let isSimpleExpr (e: Expr): bool =
  match e with
  | Let _ | LetGhost _ | Block _ | Assert _ -> false
  | _ -> true

// TODO: Match case?
let noBracesSub (e: Expr): Expr list =
  match e with
  | Let l -> [l.body]
  | LetGhost l -> [l.body]
  | Ghost e -> [e]
  | Locally e -> [e]
  | Assert (_, body) -> [body]
  | _ -> []

let requiresBraces (e: Expr) (within: Expr option): bool =
  match within with
  | _ when isSimpleExpr e -> false
  | Some(Ghost _ | Locally _) -> false
  | Some(within) when List.contains e (noBracesSub within) -> false
  | Some(_) ->
    // TODO
    false
  | _ -> false

let joined (ctx: PrintCtx) (lines: Line list) (sep: string): Line =
  if lines.IsEmpty then {lvl = ctx.lvl; txt = ""}
  else {lvl = lines.Head.lvl; txt = lines.StrJoin sep}

let prepend (ctx: PrintCtx) (prefix: string) (lines: Line list): Line list =
  if lines.IsEmpty then [{lvl = ctx.lvl; txt = prefix}]
  else {lvl = lines.Head.lvl; txt = $"{prefix}{lines.Head.txt}"} :: lines.Tail

let append (ctx: PrintCtx) (suffix: string) (lines: Line list): Line list =
  if lines.IsEmpty then [{lvl = ctx.lvl; txt = suffix}]
  else
    let lst = List.last lines
    (List.initial lines) @ [{lvl = lst.lvl; txt = $"{lst.txt}{suffix}"}]

let join (ctx: PrintCtx) (sep: string) (lhs: Line list) (rhs: Line list): Line list =
  if lhs.IsEmpty && rhs.IsEmpty then [{lvl = ctx.lvl; txt = sep}]
  else if lhs.IsEmpty then prepend ctx sep rhs
  else if rhs.IsEmpty then append ctx sep lhs
  else
    let lst = List.last lhs
    let middle = {lvl = lst.lvl; txt = $"{lst.txt}{sep}{rhs.Head.txt}"}
    (List.skipLast 1 lhs) @ [middle] @ rhs.Tail


let rec joinN (ctx: PrintCtx) (sep: string) (liness: Line list list): Line list =
  match liness with
  | [] -> []
  | lines :: [] -> lines
  | fst :: rest ->
    let rest = joinN ctx sep rest
    join ctx sep fst rest

// TODO: Fix this
// TODO: Parenthesis
let rec pp (ctx: PrintCtx) (e: Expr): Line list =
  if requiresBraces e ctx.parent then
    [{txt = "{"; lvl = ctx.lvl}] @ ppBody ctx.inc e @  [{txt = "}"; lvl = ctx.lvl}]
  else ppBody ctx e

and joinCallLike (ctx: PrintCtx) (prefix: Line list) (argss: Line list list): Line list =
  assert (not prefix.IsEmpty)
  if argss |> List.exists (fun args -> args.Length > 1) then
    let args = (((List.initial argss) |> List.collect (fun args ->
      if args.IsEmpty then []
      else
        let last = List.last args
        (List.initial args) @ [{last with txt = last.txt + ", "}]
    )) @ (List.last argss)) |> List.map (fun l -> l.inc)
    (join ctx "(" prefix args) @ [{lvl = ctx.lvl; txt = ")"}]
  else
    join ctx "(" prefix [{lvl = ctx.lvl; txt = ((List.concat argss) |> List.map (fun l -> l.txt)).StrJoin ", " + ")"}]

and ppLet (ctx: PrintCtx) (theLet: Expr) (lt: Let) (annot: string list): Line list =
  let e2 = pp (ctx.nest theLet) lt.e
  let body = pp (ctx.nest theLet) lt.body
  let annot = if annot.IsEmpty then "" else (annot.StrJoin " ") + " "
  let prepended = (prepend ctx $"{annot}val {lt.bdg.name} = " e2)
  prepended @ body

and ppMatchExpr (ctx: PrintCtx) (mexpr: MatchExpr): Line list =
  let rec ppPattern (pat: Pattern): string =
    match pat with
    | Wildcard None -> "_"
    | Wildcard (Some var) -> var.name
    | ADTPattern pat ->
      let bdg = pat.binder |> Option.map (fun bdg -> $"${bdg.name} @ ") |> Option.defaultValue ""
      let subpats = (pat.subPatterns |> List.map ppPattern).StrJoin ", "
      $"{bdg}{pat.id}({subpats})"

  let ppMatchCase (ctx: PrintCtx) (cse: MatchCase): Line list =
    let pat = {txt = $"case {ppPattern cse.pattern} =>"; lvl = ctx.lvl}
    pat :: pp (ctx.inc) cse.rhs

  let ctxNested = ctx.nest (MatchExpr mexpr)
  let cases = mexpr.cases |> List.collect (ppMatchCase ctxNested.inc)
  let scrut = pp ctxNested mexpr.scrut
  (append ctx " match {" scrut) @ cases @ [{txt = "}"; lvl = ctx.lvl}]

and ppBody (ctx: PrintCtx) (e: Expr): Line list =
  let line (str: string): Line = {txt = str; lvl = ctx.lvl}

  match e with
  | Var v -> [line v.name]
  | Block exprs ->
    List.collect (pp (ctx.nest e)) exprs

  | Ghost e2 ->
    [line "ghostExpr {"] @ (pp (ctx.inc.nest e) e2) @ [line "}"]

  | Locally e2 ->
    [line "locally {"] @ (pp (ctx.inc.nest e) e2) @ [line "}"]

  | AppliedLemma app ->
    let args = app.args |> List.map (pp (ctx.nest e))
    joinCallLike ctx [line (lemmaStr app.lemma)] args

  | Snapshot e2 ->
    joinCallLike ctx [line "snapshot"] [pp (ctx.nest e) e2]

  | Let lt -> ppLet ctx e lt []

  | LetGhost lt -> ppLet ctx e lt ["@ghost"]

  | Assert (pred, body) ->
    let pred = pp (ctx.nest e) pred
    let body = pp (ctx.nest e) body
    (joinCallLike ctx [line "assert"] [pred]) @ body

  | Check (pred, body) ->
    let pred = pp (ctx.nest e) pred
    let body = pp (ctx.nest e) body
    (joinCallLike ctx [line "check"] [pred]) @ body

  | BitStreamMethodCall call ->
    let recv = pp (ctx.nest e) call.recv
    let meth = bsMethodCallStr call.method
    let args = call.args |> List.map (pp (ctx.nest e))
    joinCallLike ctx (append ctx $".{meth}" recv) args

  | BitStreamFunctionCall call ->
    let meth = bsFnCall call.fn
    let args = call.args |> List.map (pp (ctx.nest e))
    joinCallLike ctx [line meth] args

  | RTFunctionCall call ->
    let meth = rtFnCall call.fn
    let args = call.args |> List.map (pp (ctx.nest e))
    joinCallLike ctx [line meth] args

  | TupleSelect (recv, ix) ->
    let recv = pp (ctx.nest e) recv
    append ctx $"._{ix}" recv

  | FieldSelect (recv, sel) ->
    let recv = pp (ctx.nest e) recv
    append ctx $".{sel}" recv

  | ArraySelect (arr, ix) ->
    let recv = pp (ctx.nest e) arr
    let ix = pp (ctx.nest e) ix
    joinCallLike ctx recv [ix]

  | ArrayLength arr ->
    let arr = pp (ctx.nest e) arr
    append ctx $".length" arr

  | IntLit i -> [line (i.ToString())]

  | Equals (lhs, rhs) ->
    let lhs = pp (ctx.nest e) lhs
    let rhs = pp (ctx.nest e) rhs
    join ctx " == " lhs rhs

  | Leq (lhs, rhs) ->
    let lhs = pp (ctx.nest e) lhs
    let rhs = pp (ctx.nest e) rhs
    join ctx " <= " lhs rhs

  | Plus (lhs, rhs) ->
    let lhs = pp (ctx.nest e) lhs
    let rhs = pp (ctx.nest e) rhs
    join ctx " + " lhs rhs

  | Mult (lhs, rhs) ->
    let lhs = pp (ctx.nest e) lhs
    let rhs = pp (ctx.nest e) rhs
    join ctx " * " lhs rhs

  | MatchExpr mexpr -> ppMatchExpr ctx mexpr

  | ToRawULong e2 ->
    joinCallLike ctx [line "ULong.fromRaw"] [pp (ctx.nest e) e2]

  | ToLong e2 ->
    let e2 = pp (ctx.nest e) e2
    append ctx $".toLong" e2

  | SelectionExpr sel -> [line sel]

  | EncDec stmt ->
    (stmt.Split [|'\n'|]) |> Array.toList |> List.map line

let show (e: Expr): string =
  (pp {curr = e; parents = []; lvl = 0} e |> List.map (fun line -> (String.replicate line.lvl "    ") + line.txt)).StrJoin "\n"
