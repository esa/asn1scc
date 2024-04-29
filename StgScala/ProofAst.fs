module ProofAst

open FsUtils
open Language
open DAst
open CommonTypes

type Identifier = string // TODO: Find something better

type CodecClass =
  | BaseCodec
  | AcnCodec
  | UperCodec

type RuntimeType =
  | BitStream
  | CodecClass of CodecClass

type IntegerType =
  | Byte
  | Short
  | Int
  | Long
  | UByte
  | UShort
  | UInt
  | ULong

type Type =
  | IntegerType of IntegerType
  | RuntimeType of RuntimeType // TODO: Merge with TypeInfo
  | TypeInfo of TypeInfo // TODO: Remove encoding info and only e,g, classes.

type Var = {
  name: Identifier
  tpe: Type
}

type Pattern =
  | Wildcard of Var option
  | ADTPattern of ADTPattern

and ADTPattern = {
  binder: Var option
  id: Identifier // TODO: Have something better
  subPatterns: Pattern list
}
// TODO: Have "Tree" as well

type Tree =
  | ExprTree of Expr
  | FunDefTree of FunDef

and Expr =
  | Var of Var
  | Block of Expr list
  | Ghost of Expr
  | Locally of Expr
  | Snapshot of Expr
  | Let of Let
  | LetGhost of Let
  | Assert of Expr
  | Check of Expr
  | FunctionCall of FunctionCall
  | MethodCall of MethodCall
  | TupleSelect of Expr * int
  | FieldSelect of Expr * Identifier
  | ArraySelect of Expr * Expr
  | ArrayLength of Expr
  | IfExpr of IfExpr
  | MatchExpr of MatchExpr
  | And of Expr list
  | SplitAnd of Expr list
  | Or of Expr list
  | Not of Expr
  | Equals of Expr * Expr
  | Mult of Expr * Expr
  | Plus of Expr list
  | Minus of Expr * Expr
  | Leq of Expr * Expr
  | IntLit of IntegerType * bigint
  | EncDec of string
  | This // TODO: Add type
  | SelectionExpr of string // TODO: Not ideal



and Let = {
  bdg: Var
  e: Expr
  body: Expr
}
and FunctionCall = {
  prefix: Identifier list
  id: Identifier
  args: Expr list
}
and MethodCall = {
  recv: Expr
  id: Identifier
  args: Expr list
}
and IfExpr = {
  cond: Expr
  thn: Expr
  els: Expr
}
and MatchExpr = {
  scrut: Expr
  cases: MatchCase list
}
and MatchCase = {
  pattern: Pattern
  rhs: Expr
}
and PreSpec =
  | LetSpec of Var * Expr
  | Precond of Expr
  | Measure of Expr

and FunDef = {
  id: Identifier // TODO: Quid name clash???
  prms: Var list
  specs: PreSpec list
  postcond: (Var * Expr) option
  returnTpe: Type
  body: Expr
}


let mkBlock (exprs: Expr list): Expr =
  if exprs.Length = 1 then exprs.Head
  else
    exprs |> List.collect (fun e -> match e with Block exprs -> exprs | _ -> [e])
          |> Block

let bitStreamId: Identifier = "BitStream"
let codecId: Identifier = "Codec"
let acnId: Identifier = "ACN"

let int32lit (l: bigint): Expr = IntLit (Int, l)

let longlit (l: bigint): Expr = IntLit (Long, l)

let ulonglit (l: bigint): Expr = IntLit (ULong, l)

let plus (terms: Expr list): Expr =
  assert (not terms.IsEmpty)
  let litTpe = terms |> List.tryFindMap (fun e ->
    match e with
    | IntLit (tpe, _) -> Some tpe
    | _ -> None
  )
  let cst, newTerms =
    terms |> List.fold (fun (acc, newTerms) e ->
      match e with
      | IntLit (tpe, lit) ->
        assert (Some tpe = litTpe)
        let sz, unsigned =
          match tpe with
          | Byte -> 8, false
          | Short -> 16, false
          | Int -> 32, false
          | Long -> 64, false
          | UByte -> 8, true
          | UShort -> 16, true
          | UInt -> 32, true
          | ULong -> 64, true
        let min, max =
          if unsigned then 0I, 2I ** sz
          else -2I ** (sz - 1), 2I ** (sz - 1) - 1I
        let nbits = max - min + 1I
        let sum = acc + lit
        let newAcc =
          if unsigned then sum % nbits
          else if min <= sum && sum <= max then sum
          else if max < sum then -nbits + sum
          else nbits + sum
        newAcc, newTerms
      | _ ->
        acc, e :: newTerms
    ) (0I, [])
  let newTerms = List.rev newTerms
  if cst = 0I then Plus newTerms
  else Plus (newTerms @ [IntLit (litTpe.Value, cst)])

let selBase (recv: Expr): Expr = FieldSelect (recv, "base")

let selBitStream (recv: Expr): Expr = FieldSelect (selBase recv, "bitStream")

let selBuf (recv: Expr): Expr = FieldSelect (selBase recv, "buf")

let selBufLength (recv: Expr): Expr =  ArrayLength (selBuf recv)

let selCurrentByte (recv: Expr): Expr =  FieldSelect (selBitStream recv, "currentByte")

let selCurrentBit (recv: Expr): Expr =  FieldSelect (selBitStream recv, "currentBit")

let bitIndex (recv: Expr): Expr = MethodCall { id = "bitIndex"; recv = selBitStream recv; args = [] }

let resetAt (recv: Expr) (arg: Expr): Expr = MethodCall { id = "resetAt"; recv = selBitStream recv; args = [arg] }

let invariant (recv: Expr): Expr = FunctionCall { prefix = [bitStreamId]; id = "invariant"; args = [selCurrentBit recv; selCurrentByte recv; selBufLength recv] }

let getBitCountUnsigned (arg: Expr): Expr = FunctionCall { prefix = []; id = "GetBitCountUnsigned"; args = [arg] }

let validateOffsetBits (recv: Expr) (offset: Expr): Expr = MethodCall { id = "validate_offset_bits"; recv = selBitStream recv; args = [offset] }

let callSize (recv: Expr): Expr = MethodCall { id = "size"; recv = recv; args = [] }

let getLengthForEncodingSigned (arg: Expr): Expr = FunctionCall { prefix = []; id = "GetLengthForEncodingSigned"; args = [arg] }

let stringLength (recv: Expr): Expr = FieldSelect (recv, "nCount")

let stringCapacity (recv: Expr): Expr = ArrayLength (FieldSelect (recv, "arr"))


let validTransitiveLemma (b1: Expr) (b2: Expr) (b3: Expr): Expr =
  FunctionCall { prefix = [bitStreamId]; id = "validTransitiveLemma"; args = [b1; b2; b3] }

let validateOffsetBitsIneqLemma (b1: Expr) (b2: Expr) (b1ValidateOffsetBits: Expr) (advancedAtMostBits: Expr): Expr =
  FunctionCall { prefix = [bitStreamId]; id = "validateOffsetBitsIneqLemma"; args = [b1; b2; b1ValidateOffsetBits; advancedAtMostBits] }

let validateOffsetBitsWeakeningLemma (b: Expr) (origOffset: Expr) (newOffset: Expr): Expr =
  FunctionCall { prefix = [bitStreamId]; id = "validateOffsetBitsWeakeningLemma"; args = [b; origOffset; newOffset] }

// TODO: Pas terrible, trouver une meilleure solution
let readPrefixLemmaIdentifier (t: TypeEncodingKind option): string list * string =
  match t with
  | None -> failwith "TODO: Implement me"
  | Some (AcnBooleanEncodingType None) -> [bitStreamId], "readBitPrefixLemma" // TODO: Check this
  | Some (AcnIntegerEncodingType int) ->
    let sign =
      match int.signedness with
      | Positive -> "PositiveInteger"
      | TwosComplement -> "TwosComplement"
    let endian, sz =
      match int.endianness with
      | IntegerEndianness.Byte -> None, Some "8"
      | Unbounded -> None, None
      | LittleEndian sz -> Some "little_endian", Some (sz.bitSize.ToString())
      | BigEndian sz -> Some "big_endian", Some (sz.bitSize.ToString())
    [acnId], ([Some "dec"; Some "Int"; Some sign; Some "ConstSize"; endian; sz; Some "prefixLemma"] |> List.choose id).StrJoin "_"
  | Some (Asn1IntegerEncodingType (Some Unconstrained)) ->
    [codecId], "decodeUnconstrainedWholeNumber_prefixLemma"
  | Some (Asn1IntegerEncodingType (Some (FullyConstrainedPositive _))) ->
    [codecId], "decodeConstrainedPosWholeNumber_prefixLemma"
  | _ ->
    [acnId], "readPrefixLemma_TODO" // TODO

let runtimeCodecTypeFor (enc: Asn1Encoding): CodecClass =
  match enc with
  | UPER -> UperCodec
  | ACN -> AcnCodec
  | _ -> failwith $"Unsupported: {enc}"

//////////////////////////////////////////////////////////

type PrintCtx = {
  curr: Tree
  parents: Tree list
  lvl: int
} with
  member this.inc: PrintCtx = {this with lvl = this.lvl + 1}

  member this.parent = List.tryHead this.parents

  member this.nest (t: Tree): PrintCtx = {this with curr = t; parents = this.curr :: this.parents}

  member this.nestExpr (e: Expr): PrintCtx = this.nest (ExprTree e)

type Line = {
  txt: string
  lvl: int
} with
  member this.inc: Line = {this with lvl = this.lvl + 1}

let isSimpleExpr (e: Tree): bool =
  match e with
  | ExprTree (Let _ | LetGhost _ | Block _ | Assert _) -> false
  | _ -> true

// TODO: Match case?
let noBracesSub (e: Tree): Tree list =
  match e with
  | ExprTree (Let l) -> [ExprTree l.body]
  | ExprTree (LetGhost l) -> [ExprTree l.body]
  | ExprTree (Ghost e) -> [ExprTree e]
  | ExprTree (Locally e) -> [ExprTree e]
  | _ -> []

let requiresBraces (e: Tree) (within: Tree option): bool =
  match within with
  | _ when isSimpleExpr e -> false
  | Some (ExprTree (Ghost _ | Locally _)) -> false
  | Some within when List.contains e (noBracesSub within) -> false
  | Some _ ->
    // TODO
    false
  | _ -> false

let precedence (e: Expr): int =
  match e with
  | Or _ -> 1
  | And _ | SplitAnd _ -> 3
  | Leq _ -> 4
  | Equals _ | Not _ -> 5
  | Plus _ | Minus _ -> 7
  | Mult _ -> 8
  | _ -> 9

let requiresParentheses (curr: Tree) (parent: Tree option): bool =
  match curr, parent with
  | (_, None) -> false
  | (_, Some (ExprTree (Let _ | FunctionCall _ | Assert _ | Check _ | IfExpr _ | MatchExpr _))) -> false
  | (_, Some (ExprTree (MethodCall call))) -> not (List.contains curr (call.args |> List.map ExprTree))
  | (ExprTree e1, Some (ExprTree e2)) when precedence e1 > precedence e2 -> false
  | _ -> true

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

let lined (liness: Line list list): Line list list =
  liness |> List.collect id |> List.map (fun l -> [l])

let rec joinN (ctx: PrintCtx) (sep: string) (liness: Line list list): Line list =
  match liness with
  | [] -> []
  | lines :: [] -> lines
  | fst :: rest ->
    let rest = joinN ctx sep rest
    join ctx sep fst rest

let ppType (tpe: Type): string =
  match tpe with
  | IntegerType int -> int.ToString()
  | _ -> failwith "TODO"

// TODO: Maybe have ctx.nest here already?
let rec pp (ctx: PrintCtx) (t: Tree): Line list =
  if requiresBraces t ctx.parent && ctx.parent <> Some t then
    [{txt = "{"; lvl = ctx.lvl}] @ ppBody ctx.inc t @  [{txt = "}"; lvl = ctx.lvl}]
  else ppBody ctx t

and ppExpr (ctx: PrintCtx) (e: Expr): Line list = pp ctx (ExprTree e)

and joinCallLike (ctx: PrintCtx) (prefix: Line list) (argss: Line list list) (parameterless: bool): Line list =
  assert (not prefix.IsEmpty)
  if argss.IsEmpty && parameterless then
      prefix
  else
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
  let e2 = ppExpr (ctx.nestExpr theLet) lt.e
  let body = ppExpr (ctx.nestExpr theLet) lt.body
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
    pat :: ppExpr ctx.inc cse.rhs

  let ctxNested = ctx.nestExpr (MatchExpr mexpr)
  let cases = mexpr.cases |> List.collect (ppMatchCase ctxNested.inc)
  let scrut = ppExpr ctxNested mexpr.scrut
  (append ctx " match {" scrut) @ cases @ [{txt = "}"; lvl = ctx.lvl}]

and ppIfExpr (ctx: PrintCtx) (ifexpr: IfExpr): Line list =
  let ctxNested = ctx.nestExpr (IfExpr ifexpr)
  let cond = ppExpr ctxNested ifexpr.cond
  let thn = ppExpr ctxNested.inc ifexpr.thn
  let els = ppExpr ctxNested.inc ifexpr.els
  (append ctx ") {" (prepend ctx "if (" cond)) @ thn @ [{txt = "} else {"; lvl = ctx.lvl}] @ els @ [{txt = "}"; lvl = ctx.lvl}]

and ppFunDef (ctx: PrintCtx) (fd: FunDef): Line list =
  // TODO: What about "nestExpr" ???
  let prms =
    if fd.prms.IsEmpty then ""
    else
      let prms = (fd.prms |> List.map (fun v -> $"{v.name}: {ppType v.tpe}")).StrJoin ", "
      $"({prms})"
  let header = [{txt = $"def {fd.id}{prms}: {ppType fd.returnTpe} = {{"; lvl = ctx.lvl}]
  let preSpecs = fd.specs |> List.collect (fun s ->
    match s with
    | Precond e -> joinCallLike ctx.inc [{txt = "require"; lvl = ctx.lvl + 1}] [ppExpr ctx.inc e] false
    | Measure e -> joinCallLike ctx.inc [{txt = "decreases"; lvl = ctx.lvl + 1}] [ppExpr ctx.inc e] false
    | LetSpec (v, e) -> (prepend ctx.inc $"val {v.name} = " (ppExpr ctx.inc e))
  )
  let hasBdgInSpec = fd.specs |> List.exists (fun s -> match s with LetSpec _ -> true | _ -> false)

  match fd.postcond, hasBdgInSpec with
  | Some (resVar, postcond), true ->
    let body = ppExpr ctx.inc.inc fd.body
    let postcond = ppExpr ctx.inc.inc postcond
    [{txt = "{"; lvl = ctx.lvl + 1}] @
    preSpecs @
    [] @
    body @
    [{txt = $"}}.ensuring {{ {resVar.name} => "; lvl = ctx.lvl + 1}] @
    postcond @
    [{txt = "}"; lvl = ctx.lvl + 1}; {txt = "}"; lvl = ctx.lvl}]
  | Some (resVar, postcond), false ->
    let body = ppExpr ctx.inc fd.body
    let postcond = ppExpr ctx.inc postcond
    header @
    preSpecs @
    body @
    [{txt = $"}}.ensuring {{ {resVar.name} => "; lvl = ctx.lvl}] @
    postcond @
    [{txt = "}"; lvl = ctx.lvl}]
  | None, _ ->
    let body = ppExpr ctx.inc fd.body
    header @ preSpecs @ body @ [{txt = "}"; lvl = ctx.lvl}]

and optP (ctx: PrintCtx) (ls: Line list): Line list =
  if requiresParentheses ctx.curr ctx.parent then
    prepend ctx "(" (append ctx ")" ls)
  else ls

and ppBody (ctx: PrintCtx) (t: Tree): Line list =
  match t with
  | ExprTree e -> ppExprBody ctx e
  | FunDefTree fd -> ppFunDef ctx fd

and ppExprBody (ctx: PrintCtx) (e: Expr): Line list =
  let line (str: string): Line = {txt = str; lvl = ctx.lvl}

  match e with
  | Var v -> [line v.name]
  | Block exprs ->
    List.collect (fun e2 -> ppExpr (ctx.nestExpr e2) e2) exprs

  | Ghost e2 ->
    [line "ghostExpr {"] @ (ppExpr (ctx.inc.nestExpr e2) e2) @ [line "}"]

  | Locally e2 ->
    [line "locally {"] @ (ppExpr (ctx.inc.nestExpr e2) e2) @ [line "}"]

  | Snapshot e2 ->
    joinCallLike ctx [line "snapshot"] [ppExpr (ctx.nestExpr e2) e2] false

  | Let lt -> ppLet ctx e lt []

  | LetGhost lt -> ppLet ctx e lt ["@ghost"]

  | Assert pred ->
    let pred = ppExpr (ctx.nestExpr pred) pred
    joinCallLike ctx [line "assert"] [pred] false

  | Check pred ->
    let pred = ppExpr (ctx.nestExpr pred) pred
    joinCallLike ctx [line "check"] [pred] false

  | MethodCall call ->
    let recv = ppExpr (ctx.nestExpr call.recv) call.recv
    let args = call.args |> List.map (fun a -> ppExpr (ctx.nestExpr a) a)
    joinCallLike ctx (append ctx $".{call.id}" recv) args true

  | FunctionCall call ->
    let id = if call.prefix.IsEmpty then call.id else (call.prefix.StrJoin ".") + "." + call.id
    let args = call.args |> List.map (fun a -> ppExpr (ctx.nestExpr a) a)
    joinCallLike ctx [line id] args true

  | TupleSelect (recv, ix) ->
    let recv = ppExpr (ctx.nestExpr recv) recv
    append ctx $"._{ix}" recv

  | FieldSelect (recv, sel) ->
    let recv = ppExpr (ctx.nestExpr recv) recv
    append ctx $".{sel}" recv

  | ArraySelect (arr, ix) ->
    let recv = ppExpr (ctx.nestExpr arr) arr
    let ix = ppExpr (ctx.nestExpr ix) ix
    joinCallLike ctx recv [ix] false

  | ArrayLength arr ->
    let arr = ppExpr (ctx.nestExpr arr) arr
    append ctx $".length" arr

  | IntLit (tpe, i) ->
    let i = i.ToString()
    let str =
      match tpe with
      | Byte -> $"{i}.toByte"
      | Short -> $"{i}.toShort"
      | Int -> i
      | Long -> $"{i}L"
      | UByte -> $"UByte.fromRaw({i}.toByte)"
      | UShort -> $"UShort.fromRaw({i}.toShort)"
      | UInt -> $"UInt.fromRaw({i})"
      | ULong -> $"ULong.fromRaw({i}L)"
    [line str]

  | Equals (lhs, rhs) ->
    let lhs = ppExpr (ctx.nestExpr lhs) lhs
    let rhs = ppExpr (ctx.nestExpr rhs) rhs
    optP ctx (join ctx " == " lhs rhs)

  | Leq (lhs, rhs) ->
    let lhs = ppExpr (ctx.nestExpr lhs) lhs
    let rhs = ppExpr (ctx.nestExpr rhs) rhs
    optP ctx (join ctx " <= " lhs rhs)

  | And conjs ->
    let conjs = conjs |> List.map (fun c -> ppExpr (ctx.nestExpr c) c)
    optP ctx (joinN ctx " && " conjs)

  | SplitAnd conjs ->
    let conjs = conjs |> List.map (fun c -> ppExpr (ctx.nestExpr c) c)
    optP ctx (joinN ctx " &&& " conjs)

  | Or disjs ->
    let disjs = disjs |> List.map (fun d -> ppExpr (ctx.nestExpr d) d)
    optP ctx (joinN ctx " || " disjs)

  | Not e2 ->
    let e2 = ppExpr (ctx.nestExpr e2) e2
    optP ctx (prepend ctx "!" e2)

  | Plus terms ->
    let terms = terms |> List.map (fun c -> ppExpr (ctx.nestExpr c) c)
    optP ctx (joinN ctx " + " terms)

  | Minus (lhs, rhs) ->
    let lhs = ppExpr (ctx.nestExpr lhs) lhs
    let rhs = ppExpr (ctx.nestExpr rhs) rhs
    optP ctx (join ctx " - " lhs rhs)

  | Mult (lhs, rhs) ->
    let lhs = ppExpr (ctx.nestExpr lhs) lhs
    let rhs = ppExpr (ctx.nestExpr rhs) rhs
    optP ctx (join ctx " * " lhs rhs)

  | IfExpr ifexpr -> ppIfExpr ctx ifexpr

  | MatchExpr mexpr -> ppMatchExpr ctx mexpr

  | SelectionExpr sel -> [line sel]

  | This -> [line "this"]

  | EncDec stmt ->
    (stmt.Split [|'\n'|]) |> Array.toList |> List.map line


let showLines (t: Tree): string list =
  pp {curr = t; parents = []; lvl = 0} t |> List.map (fun line -> (String.replicate line.lvl "    ") + line.txt)

let show (t: Tree): string =
  (showLines t).StrJoin "\n"
