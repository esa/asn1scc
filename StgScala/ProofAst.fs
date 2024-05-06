module ProofAst

open FsUtils
open Language
open DAst
open CommonTypes
open Asn1AcnAstUtilFunctions

type Identifier = string // TODO: Find something better

type IntegerType =
  | Byte
  | Short
  | Int
  | Long
  | UByte
  | UShort
  | UInt
  | ULong

type Annot =
  | Opaque
  | InlineOnce
  | GhostAnnot

type Type =
  | IntegerType of IntegerType
  | BooleanType
  | DoubleType
  | ArrayType of ArrayType
  | ClassType of ClassType
and ClassType = {
  id: Identifier
  tps: Type list
}
and ArrayType = {
  tpe: Type
}

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
  | FreshCopy of Expr
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
  | ClassCtor of ClassCtor
  | Old of Expr
  | Return of Expr
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
  | BoolLit of bool
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
and ClassCtor = {
  ct: ClassType
  args: Expr list
}
and PreSpec =
  | LetSpec of Var * Expr
  | Precond of Expr
  | Measure of Expr

and FunDef = {
  id: Identifier // TODO: Quid name clash???
  prms: Var list
  annots: Annot list
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
let uperId: Identifier = "UPER"
let acnId: Identifier = "ACN"

let optionId: Identifier = "Option"
let someId: Identifier = "Some"
let noneId: Identifier = "None"

let optionMutId: Identifier = "OptionMut"
let someMutId: Identifier = "SomeMut"
let noneMutId: Identifier = "NoneMut"

let eitherId: Identifier = "Either"
let leftId: Identifier = "Left"
let rightId: Identifier = "Right"

let eitherMutId: Identifier = "EitherMut"
let leftMutId: Identifier = "LeftMut"
let rightMutId: Identifier = "RightMut"

let bitstreamClsTpe = {ClassType.id = bitStreamId; tps = []}
let codecClsTpe = {ClassType.id = codecId; tps = []}
let uperClsTpe = {ClassType.id = uperId; tps = []}
let acnClsTpe = {ClassType.id = acnId; tps = []}

let optionTpe (tpe: Type): ClassType = {ClassType.id = optionId; tps = [tpe]}
let someTpe (tpe: Type): ClassType = {ClassType.id = someId; tps = [tpe]}
let noneTpe (tpe: Type): ClassType = {ClassType.id = noneId; tps = [tpe]}
let some (tpe: Type) (e: Expr): ClassCtor = {ct = someTpe tpe; args = [e]}
let someExpr (tpe: Type) (e: Expr): Expr = ClassCtor (some tpe e)
let none (tpe: Type): ClassCtor = {ct = noneTpe tpe; args = []}
let noneExpr (tpe: Type): Expr = ClassCtor (none tpe)

let optionMutTpe (tpe: Type): ClassType = {ClassType.id = optionMutId; tps = [tpe]}
let someMutTpe (tpe: Type): ClassType = {ClassType.id = someMutId; tps = [tpe]}
let noneMutTpe (tpe: Type): ClassType = {ClassType.id = noneMutId; tps = [tpe]}
let someMut (tpe: Type) (e: Expr): ClassCtor = {ct = someMutTpe tpe; args = [e]}
let someMutExpr (tpe: Type) (e: Expr): Expr = ClassCtor (someMut tpe e)
let noneMut (tpe: Type): ClassCtor = {ct = noneMutTpe tpe; args = []}
let noneMutExpr (tpe: Type): Expr = ClassCtor (noneMut tpe)

let isDefinedExpr (recv: Expr): Expr = MethodCall {recv = recv; id = "isDefined"; args = []}
let isDefinedMutExpr (recv: Expr): Expr = isDefinedExpr recv // TODO: We can't distinguish symbols right now


let eitherTpe (l: Type) (r: Type): ClassType = {ClassType.id = eitherId; tps = [l; r]}
let leftTpe (l: Type) (r: Type): ClassType = {ClassType.id = leftId; tps = [l; r]}
let rightTpe (l: Type) (r: Type): ClassType = {ClassType.id = rightId; tps = [l; r]}
let left (l: Type) (r: Type) (e: Expr): ClassCtor = {ct = leftTpe l r; args = [e]}
let leftExpr (l: Type) (r: Type) (e: Expr): Expr = ClassCtor (left l r e)
let right (l: Type) (r: Type) (e: Expr): ClassCtor = {ct = rightTpe l r; args = [e]}
let rightExpr (l: Type) (r: Type) (e: Expr): Expr = ClassCtor (right l r e)

let eitherMutTpe (l: Type) (r: Type): ClassType = {ClassType.id = eitherMutId; tps = [l; r]}
let leftMutTpe (l: Type) (r: Type): ClassType = {ClassType.id = leftMutId; tps = [l; r]}
let rightMutTpe (l: Type) (r: Type): ClassType = {ClassType.id = rightMutId; tps = [l; r]}
let leftMut (l: Type) (r: Type) (e: Expr): ClassCtor = {ct = leftMutTpe l r; args = [e]}
let leftMutExpr (l: Type) (r: Type) (e: Expr): Expr = ClassCtor (leftMut l r e)
let rightMut (l: Type) (r: Type) (e: Expr): ClassCtor = {ct = rightMutTpe l r; args = [e]}
let rightMutExpr (l: Type) (r: Type) (e: Expr): Expr = ClassCtor (rightMut l r e)

let optionGenMatch (someId: Identifier) (noneId: Identifier)
                   (scrut: Expr)
                   (someBdg: Var option) (someBody: Expr)
                   (noneBody: Expr): MatchExpr =
  {
    scrut = scrut
    cases = [
      {
        pattern = ADTPattern {binder = None; id = someId; subPatterns = [Wildcard someBdg]}
        rhs = someBody
      }
      {
        pattern = ADTPattern {binder = None; id = noneId; subPatterns = []}
        rhs = noneBody
      }
    ]
  }
let optionMatch (scrut: Expr)
                (someBdg: Var option) (someBody: Expr)
                (noneBody: Expr): MatchExpr =
  optionGenMatch someId noneId scrut someBdg someBody noneBody
let optionMatchExpr (scrut: Expr)
                    (someBdg: Var option) (someBody: Expr)
                    (noneBody: Expr): Expr =
  MatchExpr (optionMatch scrut someBdg someBody noneBody)

let optionMutMatch (scrut: Expr)
                (someBdg: Var option) (someBody: Expr)
                (noneBody: Expr): MatchExpr =
  optionGenMatch someMutId noneMutId scrut someBdg someBody noneBody
let optionMutMatchExpr (scrut: Expr)
                    (someBdg: Var option) (someBody: Expr)
                    (noneBody: Expr): Expr =
  MatchExpr (optionMutMatch scrut someBdg someBody noneBody)

let eitherGenMatch (leftId: Identifier) (rightId: Identifier)
                   (scrut: Expr)
                   (leftBdg: Var option) (leftBody: Expr)
                   (rightBdg: Var option) (rightBody: Expr): MatchExpr =
  {
    scrut = scrut
    cases = [
      {
        pattern = ADTPattern {binder = None; id = leftId; subPatterns = [Wildcard leftBdg]}
        rhs = leftBody
      }
      {
        pattern = ADTPattern {binder = None; id = rightId; subPatterns = [Wildcard rightBdg]}
        rhs = rightBody
      }
    ]
  }

let eitherMatch (scrut: Expr)
                (leftBdg: Var option) (leftBody: Expr)
                (rightBdg: Var option) (rightBody: Expr): MatchExpr =
  eitherGenMatch leftId rightId scrut leftBdg leftBody rightBdg rightBody
let eitherMatchExpr (scrut: Expr)
                    (leftBdg: Var option) (leftBody: Expr)
                    (rightBdg: Var option) (rightBody: Expr): Expr =
  MatchExpr (eitherMatch scrut leftBdg leftBody rightBdg rightBody)

let eitherMutMatch (scrut: Expr)
                   (leftBdg: Var option) (leftBody: Expr)
                   (rightBdg: Var option) (rightBody: Expr): MatchExpr =
  eitherGenMatch leftMutId rightMutId scrut leftBdg leftBody rightBdg rightBody
let eitherMutMatchExpr (scrut: Expr)
                       (leftBdg: Var option) (leftBody: Expr)
                       (rightBdg: Var option) (rightBody: Expr): Expr =
  MatchExpr (eitherMutMatch scrut leftBdg leftBody rightBdg rightBody)



let int32lit (l: bigint): Expr = IntLit (Int, l)

let longlit (l: bigint): Expr = IntLit (Long, l)

let ulonglit (l: bigint): Expr = IntLit (ULong, l)

let plus (terms: Expr list): Expr =
  assert (not terms.IsEmpty)

  let rec flattenAdd (e: Expr): Expr list =
    match e with
    | Plus terms -> terms |> List.collect flattenAdd
    | _ -> [e]

  let terms = terms |> List.collect flattenAdd
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
  if cst = 0I then
    if newTerms.IsEmpty then IntLit (litTpe.Value, 0I)
    else Plus newTerms
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

let indexOfOrLength (recv: Expr) (elem: Expr): Expr = MethodCall {recv = recv; id = "indexOfOrLength"; args = [elem]}

let stringCapacity (recv: Expr): Expr = ArrayLength (FieldSelect (recv, "arr"))

let alignedToByte (bits: Expr): Expr = FunctionCall {prefix = []; id = "alignedToByte"; args = [bits]}

let alignedToWord (bits: Expr): Expr = FunctionCall {prefix = []; id = "alignedToWord"; args = [bits]}

let alignedToDWord (bits: Expr): Expr = FunctionCall {prefix = []; id = "alignedToDWord"; args = [bits]}

let alignedTo (alignment: AcnGenericTypes.AcnAlignment option) (bits: Expr): Expr =
  match alignment with
  | None -> bits
  | Some AcnGenericTypes.NextByte -> alignedToByte bits
  | Some AcnGenericTypes.NextWord -> alignedToWord bits
  | Some AcnGenericTypes.NextDWord -> alignedToDWord bits

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

let rec fromAsn1TypeKind (t: Asn1AcnAst.Asn1TypeKind): Type =
  match t.ActualType with
  | Asn1AcnAst.Sequence sq -> ClassType {id = sq.typeDef[Scala].typeName; tps = []}
  | Asn1AcnAst.SequenceOf sqf -> ClassType {id = sqf.typeDef[Scala].typeName; tps = []}
  | Asn1AcnAst.Choice ch -> ClassType {id = ch.typeDef[Scala].typeName; tps = []}
  | Asn1AcnAst.Enumerated enm -> ClassType {id = enm.typeDef[Scala].typeName; tps = []}
  | Asn1AcnAst.Integer int ->
    match int.intClass with
    | Asn1AcnAst.ASN1SCC_Int8 _ -> IntegerType Byte
    | Asn1AcnAst.ASN1SCC_Int16 _ -> IntegerType Short
    | Asn1AcnAst.ASN1SCC_Int32 _ -> IntegerType Int
    | Asn1AcnAst.ASN1SCC_Int64 _ | Asn1AcnAst.ASN1SCC_Int _ -> IntegerType Long
    | Asn1AcnAst.ASN1SCC_UInt8 _ -> IntegerType UByte
    | Asn1AcnAst.ASN1SCC_UInt16 _ -> IntegerType UShort
    | Asn1AcnAst.ASN1SCC_UInt32 _ -> IntegerType UInt
    | Asn1AcnAst.ASN1SCC_UInt64 _ | Asn1AcnAst.ASN1SCC_UInt _ -> IntegerType ULong
  | Asn1AcnAst.Boolean _ -> BooleanType
  | Asn1AcnAst.NullType _ -> IntegerType Byte
  | Asn1AcnAst.BitString bt -> ClassType {id = bt.typeDef[Scala].typeName; tps = []}
  | Asn1AcnAst.OctetString ot -> ClassType {id = ot.typeDef[Scala].typeName; tps = []}
  | Asn1AcnAst.IA5String bt -> ArrayType {tpe = IntegerType UByte}
  | Asn1AcnAst.Real _ -> DoubleType
  | t -> failwith $"TODO {t}"

let fromAcnInsertedType (t: Asn1AcnAst.AcnInsertedType): Type = failwith "TODO"

let fromAsn1AcnTypeKind (t: Asn1AcnAst.Asn1AcnTypeKind): Type =
  match t with
  | Asn1AcnAst.Asn1AcnTypeKind.Acn t -> fromAcnInsertedType t
  | Asn1AcnAst.Asn1AcnTypeKind.Asn1 t -> fromAsn1TypeKind t

let runtimeCodecTypeFor (enc: Asn1Encoding): ClassType =
  match enc with
  | UPER -> uperClsTpe
  | ACN -> acnClsTpe
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

let rec ppType (tpe: Type): string =
  match tpe with
  | IntegerType int -> int.ToString()
  | BooleanType -> "Boolean"
  | DoubleType -> "Double"
  | ArrayType at -> $"Array[{ppType at.tpe}]"
  | ClassType ct -> ppClassType ct
and ppClassType (ct: ClassType): string =
  let tps =
    if ct.tps.IsEmpty then ""
    else "[" + ((ct.tps |> List.map ppType).StrJoin ", ") + "]"
  ct.id + tps

let ppAnnot (annot: Annot): string =
  match annot with
  | Opaque -> "@opaque"
  | InlineOnce -> "@inlineOnce"
  | GhostAnnot -> "@ghost"

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
  let annots =
    if fd.annots.IsEmpty then []
    else [{txt = (fd.annots |> List.map ppAnnot).StrJoin " "; lvl = ctx.lvl}]
  let header = annots @ [{txt = $"def {fd.id}{prms}: {ppType fd.returnTpe} = {{"; lvl = ctx.lvl}]
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
    [{txt = ""; lvl = ctx.lvl}] @ // for Scala to avoid defining an anonymous class with bindings from above
    body @
    // We type-annotate the result to avoid inference failure which may occur from time to time
    [{txt = $"}}.ensuring {{ ({resVar.name}: {ppType resVar.tpe}) => "; lvl = ctx.lvl + 1}] @
    postcond @
    [{txt = "}"; lvl = ctx.lvl + 1}; {txt = "}"; lvl = ctx.lvl}]
  | Some (resVar, postcond), false ->
    let body = ppExpr ctx.inc fd.body
    let postcond = ppExpr ctx.inc postcond
    header @
    preSpecs @
    body @
    [{txt = $"}}.ensuring {{ ({resVar.name}: {ppType resVar.tpe}) => "; lvl = ctx.lvl}] @
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

  | FreshCopy e2 ->
    joinCallLike ctx [line "freshCopy"] [ppExpr (ctx.nestExpr e2) e2] false

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

  | ClassCtor cc ->
    let ct = ppClassType cc.ct
    let args = cc.args |> List.map (fun a -> ppExpr (ctx.nestExpr a) a)
    joinCallLike ctx [line ct] args true

  | Old e2 ->
    let e2 = ppExpr (ctx.nestExpr e2) e2
    joinCallLike ctx [line "old"] [e2] false

  | ArrayLength arr ->
    let arr = ppExpr (ctx.nestExpr arr) arr
    append ctx $".length" arr

  | Return ret ->
    let ret = ppExpr (ctx.nestExpr ret) ret
    prepend ctx $"return " ret

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

  | BoolLit b -> [line (if b then "true" else "false")]

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

  | IfExpr ifexpr -> optP ctx (ppIfExpr ctx ifexpr)

  | MatchExpr mexpr -> optP ctx (ppMatchExpr ctx mexpr)

  | SelectionExpr sel -> [line sel]

  | This -> [line "this"]

  | EncDec stmt ->
    (stmt.Split [|'\n'|]) |> Array.toList |> List.map line


let showLines (t: Tree): string list =
  pp {curr = t; parents = []; lvl = 0} t |> List.map (fun line -> (String.replicate line.lvl "    ") + line.txt)

let show (t: Tree): string =
  (showLines t).StrJoin "\n"
