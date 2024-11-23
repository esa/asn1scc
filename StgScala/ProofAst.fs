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
  | Pure

type Type =
  | IntegerType of IntegerType
  | BooleanType
  | UnitType
  | DoubleType
  | ArrayType of ArrayType
  | ClassType of ClassType
  | TupleType of Type list
and ClassType = {
  prefix: string list
  id: Identifier
  tps: Type list
  parameterless: bool // For constructor arguments
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
  | TuplePattern of TuplePattern
with
  member this.allBindings: Var list =
    match this with
    | Wildcard bdg -> bdg |> Option.toList
    | ADTPattern pat ->
      (pat.binder |> Option.toList) @ (pat.subPatterns |> List.collect (fun subpat -> subpat.allBindings))
    | TuplePattern pat ->
      (pat.binder |> Option.toList) @ (pat.subPatterns |> List.collect (fun subpat -> subpat.allBindings))

and ADTPattern = {
  binder: Var option
  id: Identifier // TODO: Have something better
  subPatterns: Pattern list
}
and TuplePattern = {
  binder: Var option
  subPatterns: Pattern list
}
// TODO: Have "Tree" as well

type Tree =
  | ExprTree of Expr
  | FunDefTree of FunDef
  | LocalFunDefTree of LocalFunDef

and Expr =
  | Var of Var
  | Block of Expr list
  | Ghost of Expr
  | Locally of Expr
  | Snapshot of Expr
  | FreshCopy of Expr
  | Unfold of Expr
  | Let of Let
  | LetGhost of Let
  | LetTuple of LetTuple
  | LetRec of LetRec
  | Assert of Expr
  | Check of Expr
  | FunctionCall of FunctionCall
  | ApplyLetRec of ApplyLetRec
  | MethodCall of MethodCall
  | Tuple of Expr list
  | TupleSelect of Expr * int
  | FieldSelect of Expr * Identifier
  | ArraySelect of Expr * Expr
  | ArrayUpdate of Expr * Expr * Expr
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
  | Mod of Expr * Expr
  | Plus of Expr list
  | Minus of Expr * Expr
  | Leq of Expr * Expr
  | UnitLit
  | BoolLit of bool
  | IntLit of IntegerType * bigint
  | EncDec of string
  | IntCast of Expr * IntegerType * IntegerType
  | This // TODO: Add type
  | TripleQMark // TODO: Add type
  | SelectionExpr of string // TODO: Not ideal



and Let = {
  bdg: Var
  e: Expr
  body: Expr
}
and LetTuple = {
  bdgs: Var list
  e: Expr
  body: Expr
}
and LetRec = {
  fds: LocalFunDef list
  body: Expr
}
and FunctionCall = {
  prefix: Identifier list
  id: Identifier
  tps: Type list
  args: Expr list
  parameterless: bool
}
and ApplyLetRec = {
  id: Identifier
  args: Expr list
}
and MethodCall = {
  recv: Expr
  id: Identifier
  args: Expr list
  parameterless: bool
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

and FunDefLike = {
  id: Identifier // TODO: Quid name clash???
  prms: Var list
  annots: Annot list
  specs: PreSpec list
  postcond: (Var * Expr) option
  returnTpe: Type
  body: Expr
}
and FunDef = FunDefLike
and LocalFunDef = FunDefLike


let mkBlock (exprs: Expr list): Expr =
  if exprs.Length = 1 then exprs.Head
  else
    exprs |> List.collect (fun e -> match e with Block exprs -> exprs | _ -> [e])
          |> Block

let intCast (e: Expr) (from: IntegerType) (tto: IntegerType): Expr =
  if from = tto then e
  else IntCast (e, from, tto)

let mkTuple (exprs: Expr list): Expr =
  assert (not exprs.IsEmpty)
  if exprs.Length = 1 then exprs.Head
  else Tuple exprs

let tupleType (tps: Type list): Type =
  assert (not tps.IsEmpty)
  if tps.Length = 1 then tps.Head
  else TupleType tps

let rec substVars (vs: (Var * Expr) list) (inExpr: Expr): Expr =
  let rec loop (inExpr: Expr): Expr =
    let substInLetGeneric (bdgs: Var list) (e: Expr) (body: Expr): Expr * Expr =
      let newE = loop e
      let newVs = vs |> List.filter (fun (v, _) -> not (bdgs |> List.contains v))
      let newBody = substVars newVs body
      (newE, newBody)

    let substInLet (lt: Let): Let =
      let newE, newBody = substInLetGeneric [lt.bdg] lt.e lt.body
      {lt with e = newE; body = newBody}

    let substFd (fd: FunDefLike): FunDefLike =
      let newVs = vs |> List.filter (fun (v, _) -> not (fd.prms |> List.contains v))
      {fd with body = substVars newVs fd.body}

    match inExpr with
    | Var v2 ->
      vs |> List.tryFind (fun (v, _) -> v = v2)
         |> Option.map (fun (_, e) -> e)
         |> Option.defaultValue inExpr
    | Block stmts ->
      mkBlock (stmts |> List.map loop)
    | Ghost inExpr -> Ghost (loop inExpr)
    | Locally inExpr -> Ghost (loop inExpr)
    | Snapshot inExpr -> Ghost (loop inExpr)
    | FreshCopy inExpr -> Ghost (loop inExpr)
    | Unfold inExpr -> Ghost (loop inExpr)
    | Let lt -> Let (substInLet lt)
    | LetGhost lt -> LetGhost (substInLet lt)
    | LetTuple lt ->
      let newE, newBody = substInLetGeneric lt.bdgs lt.e lt.body
      LetTuple {lt with e = newE; body = newBody}
    | LetRec lrec ->
      LetRec {fds = lrec.fds |> List.map substFd; body = loop lrec.body}
    | Assert inExpr -> Assert (loop inExpr)
    | Check inExpr -> Check (loop inExpr)
    | FunctionCall call ->
      FunctionCall {call with args = call.args |> List.map loop}
    | ApplyLetRec call ->
      ApplyLetRec {call with args = call.args |> List.map loop}
    | MethodCall call ->
      MethodCall {call with recv = loop call.recv; args = call.args |> List.map loop}
    | Tuple tpls -> Tuple (tpls |> List.map loop)
    | TupleSelect (recv, ix) -> TupleSelect (loop recv, ix)
    | FieldSelect (recv, id) -> FieldSelect (loop recv, id)
    | ArraySelect (arr, ix) -> ArraySelect (loop arr, loop ix)
    | ArrayUpdate (arr, ix, newVal) -> ArrayUpdate (loop arr, loop ix, loop newVal)
    | ArrayLength arr -> ArrayLength (loop arr)
    | ClassCtor ct -> ClassCtor {ct with args = ct.args |> List.map loop}
    | Old inExpr -> Old (loop inExpr)
    | Return inExpr -> Return (loop inExpr)
    | IfExpr ifExpr -> IfExpr {cond = loop ifExpr.cond; thn = loop ifExpr.thn; els = loop ifExpr.els}
    | MatchExpr mtch ->
      let cases = mtch.cases |> List.map (fun cse ->
        let allBdgs = cse.pattern.allBindings
        let newVs = vs |> List.filter (fun (v, _) -> not (allBdgs |> List.contains v))
        {cse with rhs = substVars newVs cse.rhs}
      )
      MatchExpr {scrut = loop mtch.scrut; cases = cases}
    | And conjs -> And (conjs |> List.map loop)
    | SplitAnd conjs -> SplitAnd (conjs |> List.map loop)
    | Or disjs -> Or (disjs |> List.map loop)
    | Not inExpr -> Not (loop inExpr)
    | Equals (lhs, rhs) -> Equals (loop lhs, loop rhs)
    | Mult (lhs, rhs) -> Mult (loop lhs, loop rhs)
    | Mod (lhs, rhs) -> Mod (loop lhs, loop rhs)
    | Plus terms -> Plus (terms |> List.map loop)
    | Minus (lhs, rhs) -> Minus (loop lhs, loop rhs)
    | Leq (lhs, rhs) -> Leq (loop lhs, loop rhs)
    | IntCast (e, from, tto) -> IntCast (loop e, from, tto)
    | BoolLit _ | UnitLit | IntLit _ | EncDec _ | This | TripleQMark | SelectionExpr _ -> inExpr
  if vs.IsEmpty then inExpr else loop inExpr

let bitStreamId: Identifier = "BitStream"
let codecId: Identifier = "Codec"
let uperId: Identifier = "UPER"
let acnId: Identifier = "ACN"

let listId: Identifier = "List"
let consId: Identifier = "Cons"
let nilId: Identifier = "Nil"

let vecId: Identifier = "Vector"

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

let bitstreamClsTpe = {ClassType.prefix = []; id = bitStreamId; tps = []; parameterless = false}
let codecClsTpe = {ClassType.prefix = []; id = codecId; tps = []; parameterless = false}
let uperClsTpe = {ClassType.prefix = []; id = uperId; tps = []; parameterless = false}
let acnClsTpe = {ClassType.prefix = []; id = acnId; tps = []; parameterless = false}

let listTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = listId; tps = [tpe]; parameterless = false}
let consTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = consId; tps = [tpe]; parameterless = false}
let nilTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = nilId; tps = [tpe]; parameterless = false}
let cons (tpe: Type) (head: Expr) (tail: Expr): ClassCtor = {ct = consTpe tpe; args = [head; tail]}
let consExpr (tpe: Type) (head: Expr) (tail: Expr): Expr = ClassCtor (cons tpe head tail)
let nil (tpe: Type): ClassCtor = {ct = nilTpe tpe; args = []}
let nilExpr (tpe: Type): Expr = ClassCtor (nil tpe)
let reverse (list: Expr): Expr = MethodCall {recv = list; id = "reverse"; args = []; parameterless = true}
let isize (list: Expr): Expr = MethodCall {recv = list; id = "isize"; args = []; parameterless = true}
let iupdated (list: Expr) (ix: Expr) (v: Expr): Expr = MethodCall {recv = list; id = "iupdated"; args = [ix; v]; parameterless = true}

let iapply (list: Expr) (ix: Expr): Expr = MethodCall {recv = list; id = "iapply"; args = [ix]; parameterless = true}

let vecClsTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = vecId; tps = [tpe]; parameterless = false}
let vecTpe (tpe: Type): Type = ClassType (vecClsTpe tpe)

let vecApply (vec: Expr) (ix: Expr): Expr = MethodCall {recv = vec; id = "apply"; args = [ix]; parameterless = true}
let vecSize (vec: Expr): Expr = MethodCall {recv = vec; id = "size"; args = []; parameterless = true}
let vecList (vec: Expr): Expr = MethodCall {recv = vec; id = "list"; args = []; parameterless = true}
let vecAppend (vec: Expr) (v: Expr): Expr = MethodCall {recv = vec; id = "append"; args = [v]; parameterless = true}
let vecEmpty (tpe: Type): Expr = FunctionCall {prefix = [vecId]; id = "empty"; tps = [tpe]; args = []; parameterless = true}

let optionClsTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = optionId; tps = [tpe]; parameterless = false}
let optionTpe (tpe: Type): Type = ClassType (optionClsTpe tpe)

let someClsTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = someId; tps = [tpe]; parameterless = false}
let someTpe (tpe: Type): Type = ClassType (someClsTpe tpe)

let noneClsTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = noneId; tps = [tpe]; parameterless = false}
let noneTpe (tpe: Type): Type = ClassType (noneClsTpe tpe)

let some (tpe: Type) (e: Expr): ClassCtor = {ct = someClsTpe tpe; args = [e]}
let someExpr (tpe: Type) (e: Expr): Expr = ClassCtor (some tpe e)

let none (tpe: Type): ClassCtor = {ct = noneClsTpe tpe; args = []}
let noneExpr (tpe: Type): Expr = ClassCtor (none tpe)

let optionMutClsTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = optionMutId; tps = [tpe]; parameterless = false}
let optionMutTpe (tpe: Type): Type = ClassType (optionMutClsTpe tpe)

let someMutClsTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = someMutId; tps = [tpe]; parameterless = false}
let someMutTpe (tpe: Type): Type = ClassType (someMutClsTpe tpe)

let noneMutClsTpe (tpe: Type): ClassType = {ClassType.prefix = []; id = noneMutId; tps = [tpe]; parameterless = false}
let noneMutTpe (tpe: Type): Type = ClassType (noneMutClsTpe tpe)

let someMut (tpe: Type) (e: Expr): ClassCtor = {ct = someMutClsTpe tpe; args = [e]}
let someMutExpr (tpe: Type) (e: Expr): Expr = ClassCtor (someMut tpe e)

let noneMut (tpe: Type): ClassCtor = {ct = noneMutClsTpe tpe; args = []}
let noneMutExpr (tpe: Type): Expr = ClassCtor (noneMut tpe)

let isDefinedExpr (recv: Expr): Expr = MethodCall {recv = recv; id = "isDefined"; args = []; parameterless = true}
let isDefinedMutExpr (recv: Expr): Expr = isDefinedExpr recv // TODO: We can't distinguish symbols right now

let getMutExpr (recv: Expr): Expr = MethodCall {recv = recv; id = "get"; args = []; parameterless = true}
let getExpr (recv: Expr): Expr = getMutExpr recv // TODO: We can't distinguish symbols right now


let eitherClsTpe (l: Type) (r: Type): ClassType = {ClassType.prefix = []; id = eitherId; tps = [l; r]; parameterless = false}
let eitherTpe (l: Type) (r: Type): Type = ClassType (eitherClsTpe l r)

let leftClsTpe (l: Type) (r: Type): ClassType = {ClassType.prefix = []; id = leftId; tps = [l; r]; parameterless = false}
let leftTpe (l: Type) (r: Type): Type = ClassType (leftClsTpe l r)

let rightClsTpe (l: Type) (r: Type): ClassType = {ClassType.prefix = []; id = rightId; tps = [l; r]; parameterless = false}
let rightTpe (l: Type) (r: Type): Type = ClassType (rightClsTpe l r)

let left (l: Type) (r: Type) (e: Expr): ClassCtor = {ct = leftClsTpe l r; args = [e]}
let leftExpr (l: Type) (r: Type) (e: Expr): Expr = ClassCtor (left l r e)

let right (l: Type) (r: Type) (e: Expr): ClassCtor = {ct = rightClsTpe l r; args = [e]}
let rightExpr (l: Type) (r: Type) (e: Expr): Expr = ClassCtor (right l r e)

let isRightExpr (recv: Expr): Expr = MethodCall {recv = recv; id = "isRight"; args = []; parameterless = true}
let isRightMutExpr (recv: Expr): Expr = isRightExpr recv // TODO: We can't distinguish symbols right now

let eitherMutClsTpe (l: Type) (r: Type): ClassType = {ClassType.prefix = []; id = eitherMutId; tps = [l; r]; parameterless = false}
let eitherMutTpe (l: Type) (r: Type): Type = ClassType (eitherMutClsTpe l r)

let leftMutClsTpe (l: Type) (r: Type): ClassType = {ClassType.prefix = []; id = leftMutId; tps = [l; r]; parameterless = false}
let leftMutTpe (l: Type) (r: Type): Type = ClassType (leftMutClsTpe l r)

let rightMutClsTpe (l: Type) (r: Type): ClassType = {ClassType.prefix = []; id = rightMutId; tps = [l; r]; parameterless = false}
let rightMutTpe (l: Type) (r: Type): Type = ClassType (rightMutClsTpe l r)

let leftMut (l: Type) (r: Type) (e: Expr): ClassCtor = {ct = leftMutClsTpe l r; args = [e]}
let leftMutExpr (l: Type) (r: Type) (e: Expr): Expr = ClassCtor (leftMut l r e)

let rightMut (l: Type) (r: Type) (e: Expr): ClassCtor = {ct = rightMutClsTpe l r; args = [e]}
let rightMutExpr (l: Type) (r: Type) (e: Expr): Expr = ClassCtor (rightMut l r e)

let listMatch (scrut: Expr)
              (hdBdg: Var option) (tailBdg: Var option) (consBody: Expr)
              (nilBody: Expr): MatchExpr =
  {
    scrut = scrut
    cases = [
      {
        pattern = ADTPattern {binder = None; id = consId; subPatterns = [Wildcard hdBdg; Wildcard tailBdg]}
        rhs = consBody
      }
      {
        pattern = ADTPattern {binder = None; id = nilId; subPatterns = []}
        rhs = nilBody
      }
    ]
  }

let listMatchExpr (scrut: Expr)
                  (hdBdg: Var option) (tailBdg: Var option) (consBody: Expr)
                  (nilBody: Expr): Expr =
  MatchExpr (listMatch scrut hdBdg tailBdg consBody nilBody)
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



let ubytelit (l: bigint): Expr = IntLit (UByte, l)

let int32lit (l: bigint): Expr = IntLit (Int, l)

let longlit (l: bigint): Expr = IntLit (Long, l)

let ulonglit (l: bigint): Expr = IntLit (ULong, l)

let plus (terms: Expr list): Expr =
  assert (not terms.IsEmpty) // We don't know what the type of the "0" is (if we were to return that in case there are no terms), so we enforce at least one element

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

let letTuple (bdgs: Var list) (e: Expr) (body: Expr): Expr =
  assert (not bdgs.IsEmpty)
  if bdgs.Length = 1 then Let {bdg = bdgs.Head; e = e; body = body}
  else LetTuple {bdgs = bdgs; e = e; body = body}

let letsIn (bdgs: (Var * Expr) list) (body: Expr): Expr =
  List.foldBack (fun (v, e) body -> Let {bdg = v; e = e; body = body}) bdgs body

let letsGhostIn (bdgs: (Var * Expr) list) (body: Expr): Expr =
  List.foldBack (fun (v, e) body -> LetGhost {bdg = v; e = e; body = body}) bdgs body

let ifElseBranches (branches: (Expr * Expr) list) (els: Expr): Expr =
  List.foldBack (fun (cond, thn) els -> IfExpr {cond = cond; thn = thn; els = els}) branches els

let selBaseACN (recv: Expr): Expr = FieldSelect (recv, "base")

let selBitStreamCodec (recv: Expr): Expr = FieldSelect (recv, "bitStream")

let selBitStreamACN (recv: Expr): Expr = FieldSelect (selBaseACN recv, "bitStream")

let selBufBitStream (recv: Expr): Expr = FieldSelect (recv, "buf")

let selBufCodec (recv: Expr): Expr = FieldSelect (selBitStreamCodec recv, "buf")

let selBufACN (recv: Expr): Expr = FieldSelect (selBaseACN recv, "buf")

let selBufLengthBitStream (recv: Expr): Expr =  ArrayLength recv

let selBufLengthCodec (recv: Expr): Expr =  ArrayLength (selBufCodec recv)

let selBufLengthACN (recv: Expr): Expr =  ArrayLength (selBufACN recv)

let selCurrentByteACN (recv: Expr): Expr =  FieldSelect (selBitStreamACN recv, "currentByte")

let selCurrentBitACN (recv: Expr): Expr =  FieldSelect (selBitStreamACN recv, "currentBit")

let bitIndexBitStream (recv: Expr): Expr = MethodCall { id = "bitIndex"; recv = recv; args = []; parameterless = true }

let bitIndexCodec (recv: Expr): Expr = MethodCall { id = "bitIndex"; recv = selBitStreamCodec recv; args = []; parameterless = true }

let bitIndexACN (recv: Expr): Expr = MethodCall { id = "bitIndex"; recv = selBitStreamACN recv; args = []; parameterless = true }

let resetAtACN (recv: Expr) (arg: Expr): Expr = MethodCall { id = "resetAt"; recv = recv; args = [arg]; parameterless = true }

let withMovedBitIndexACN (recv: Expr) (diff: Expr): Expr = MethodCall { id = "withMovedBitIndex"; recv = recv; args = [diff]; parameterless = true }

let withAlignedToByteACN (recv: Expr): Expr = MethodCall { id = "withAlignedToByte"; recv = recv; args = []; parameterless = false }

let withAlignedToShortACN (recv: Expr): Expr = MethodCall { id = "withAlignedToShort"; recv = recv; args = []; parameterless = false }

let withAlignedToIntACN (recv: Expr): Expr = MethodCall { id = "withAlignedToInt"; recv = recv; args = []; parameterless = false }

let withAlignedToACN (align: AcnGenericTypes.AcnAlignment) (recv: Expr): Expr =
  match align with
  | AcnGenericTypes.AcnAlignment.NextByte -> withAlignedToByteACN recv
  | AcnGenericTypes.AcnAlignment.NextWord -> withAlignedToShortACN recv
  | AcnGenericTypes.AcnAlignment.NextDWord -> withAlignedToIntACN recv

let invariant (recv: Expr): Expr = FunctionCall { prefix = [bitStreamId]; id = "invariant"; tps = []; args = [selCurrentBitACN recv; selCurrentByteACN recv; selBufLengthACN recv]; parameterless = true }

let getBitCountUnsigned (arg: Expr): Expr = FunctionCall { prefix = []; id = "GetBitCountUnsigned"; tps = []; args = [arg]; parameterless = true }

let validateOffsetBitsACN (recv: Expr) (offset: Expr): Expr = MethodCall { id = "validate_offset_bits"; recv = selBitStreamACN recv; args = [offset]; parameterless = true }

let isPrefixOfACN (recv: Expr) (other: Expr): Expr = MethodCall { id = "isPrefixOf"; recv = selBitStreamACN recv; args = [selBitStreamACN other]; parameterless = true }

let callSize (recv: Expr) (offset: Expr): Expr = MethodCall { id = "size"; recv = recv; args = [offset]; parameterless = true }

let getLengthForEncodingSigned (arg: Expr): Expr = FunctionCall { prefix = []; id = "GetLengthForEncodingSigned"; tps = []; args = [arg]; parameterless = true }

let acnReader (oldCdc: Expr) (cdc: Expr): Expr = FunctionCall { prefix = [acnId]; id = "reader"; tps = []; args = [oldCdc; cdc]; parameterless = true }

let stringLength (recv: Expr): Expr = FieldSelect (recv, "nCount")

let indexOfOrLength (recv: Expr) (elem: Expr): Expr = MethodCall {recv = recv; id = "indexOfOrLength"; args = [elem]; parameterless = true}

let stringCapacity (recv: Expr): Expr = ArrayLength (FieldSelect (recv, "arr"))

let alignedToByte (bits: Expr): Expr = FunctionCall {prefix = []; id = "alignedToByte"; tps = []; args = [bits]; parameterless = true}

let alignedToWord (bits: Expr): Expr = FunctionCall {prefix = []; id = "alignedToWord"; tps = []; args = [bits]; parameterless = true}

let alignedToDWord (bits: Expr): Expr = FunctionCall {prefix = []; id = "alignedToDWord"; tps = []; args = [bits]; parameterless = true}

let codecWrapper (bitstream: Expr): Expr = ClassCtor {ct = codecClsTpe; args = [bitstream]}

let acnWrapperBitstream (bitstream: Expr): Expr = ClassCtor {ct = acnClsTpe; args = [codecWrapper bitstream]}

let acnWrapperCodec (codec: Expr): Expr = ClassCtor {ct = acnClsTpe; args = [codec]}


let alignedTo (alignment: AcnGenericTypes.AcnAlignment option) (bits: Expr): Expr =
  match alignment with
  | None -> bits
  | Some AcnGenericTypes.NextByte -> alignedToByte bits
  | Some AcnGenericTypes.NextWord -> alignedToWord bits
  | Some AcnGenericTypes.NextDWord -> alignedToDWord bits

let alignedSizeToByteId: Identifier = "alignedSizeToByte"

let alignedSizeToWordId: Identifier = "alignedSizeToWord"

let alignedSizeToDWordId: Identifier = "alignedSizeToDWord"

let alignedSizeToByte (bits: Expr) (offset: Expr): Expr = FunctionCall {prefix = []; id = alignedSizeToByteId; tps = []; args = [bits; offset]; parameterless = true}

let alignedSizeToWord (bits: Expr) (offset: Expr): Expr = FunctionCall {prefix = []; id = alignedSizeToWordId; tps = []; args = [bits; offset]; parameterless = true}

let alignedSizeToDWord (bits: Expr) (offset: Expr): Expr = FunctionCall {prefix = []; id = alignedSizeToDWordId; tps = []; args = [bits; offset]; parameterless = true}

let alignedSizeTo (alignment: AcnGenericTypes.AcnAlignment option) (bits: Expr) (offset: Expr): Expr =
  match alignment with
  | None -> bits
  | Some AcnGenericTypes.NextByte -> alignedSizeToByte bits offset
  | Some AcnGenericTypes.NextWord -> alignedSizeToWord bits offset
  | Some AcnGenericTypes.NextDWord -> alignedSizeToDWord bits offset

let resetAtEqLemma (b1: Expr) (b2: Expr) (b3: Expr): Expr =
  FunctionCall { prefix = [bitStreamId]; id = "resetAtEqLemma"; tps = []; args = [selBitStreamACN b1; selBitStreamACN b2; selBitStreamACN b3]; parameterless = true }

let lemmaIsPrefixRefl (b: Expr): Expr =
  FunctionCall { prefix = [bitStreamId]; id = "lemmaIsPrefixRefl"; tps = []; args = [selBitStreamACN b]; parameterless = true }

let lemmaIsPrefixTransitive (b1: Expr) (b2: Expr) (b3: Expr): Expr =
  FunctionCall { prefix = [bitStreamId]; id = "lemmaIsPrefixTransitive"; tps = []; args = [selBitStreamACN b1; selBitStreamACN b2; selBitStreamACN b3]; parameterless = true }

let validateOffsetBitsIneqLemma (b1: Expr) (b2: Expr) (b1ValidateOffsetBits: Expr) (advancedAtMostBits: Expr): Expr =
  FunctionCall { prefix = [bitStreamId]; id = "validateOffsetBitsIneqLemma"; tps = []; args = [b1; b2; b1ValidateOffsetBits; advancedAtMostBits]; parameterless = true }

let validateOffsetBitsWeakeningLemma (b: Expr) (origOffset: Expr) (newOffset: Expr): Expr =
  FunctionCall { prefix = [bitStreamId]; id = "validateOffsetBitsWeakeningLemma"; tps = []; args = [b; origOffset; newOffset]; parameterless = true }

let validateOffsetBitsContentIrrelevancyLemma (b1: Expr) (buf: Expr) (bits: Expr): Expr =
  FunctionCall { prefix = [bitStreamId]; id = "validateOffsetBitsContentIrrelevancyLemma"; tps = []; args = [b1; buf; bits]; parameterless = true }

let arrayRangesEqReflexiveLemma (arr: Expr): Expr =
  FunctionCall { prefix = []; id = "arrayRangesEqReflexiveLemma"; tps = []; args = [arr]; parameterless = true }

let arrayRangesEqSlicedLemma (a1: Expr) (a2: Expr) (from: Expr) (tto: Expr) (fromSlice: Expr) (toSlice: Expr): Expr =
  FunctionCall { prefix = []; id = "arrayRangesEqSlicedLemma"; tps = []; args = [a1; a2; from; tto; fromSlice; toSlice]; parameterless = true }

let arrayUpdatedAtPrefixLemma (arr: Expr) (at: Expr) (v: Expr): Expr =
  FunctionCall { prefix = []; id = "arrayUpdatedAtPrefixLemma"; tps = []; args = [arr; at; v]; parameterless = true }

let arrayRangesEqTransitive (a1: Expr) (a2: Expr) (a3: Expr) (from: Expr) (mid: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "arrayRangesEqTransitive"; tps = []; args = [a1; a2; a3; from; mid; tto]; parameterless = true }

let arrayRangesEqImpliesEq (a1: Expr) (a2: Expr) (from: Expr) (at: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "arrayRangesEqImpliesEq"; tps = []; args = [a1; a2; from; at; tto]; parameterless = true }

let arrayRangesEq (a1: Expr) (a2: Expr) (from: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "arrayRangesEq"; tps = []; args = [a1; a2; from; tto]; parameterless = true }

let arrayBitRangesEq (a1: Expr) (a2: Expr) (fromBit: Expr) (toBit: Expr): Expr =
  FunctionCall { prefix = []; id = "arrayBitRangesEq"; tps = []; args = [a1; a2; fromBit; toBit]; parameterless = true }

let arrayBitRangesEqSlicedLemma (a1: Expr) (a2: Expr) (fromBit: Expr) (toBit: Expr) (fromSlice: Expr) (toSlice: Expr): Expr =
  FunctionCall { prefix = []; id = "arrayBitRangesEqSlicedLemma"; tps = []; args = [a1; a2; fromBit; toBit; fromSlice; toSlice]; parameterless = true }

let listRangesEqReflexiveLemma (arr: Expr): Expr =
  FunctionCall { prefix = []; id = "listRangesEqReflexiveLemma"; tps = []; args = [arr]; parameterless = true }

let listRangesEqSlicedLemma (a1: Expr) (a2: Expr) (from: Expr) (tto: Expr) (fromSlice: Expr) (toSlice: Expr): Expr =
  FunctionCall { prefix = []; id = "listRangesEqSlicedLemma"; tps = []; args = [a1; a2; from; tto; fromSlice; toSlice]; parameterless = true }

let listUpdatedAtPrefixLemma (arr: Expr) (at: Expr) (v: Expr): Expr =
  FunctionCall { prefix = []; id = "listUpdatedAtPrefixLemma"; tps = []; args = [arr; at; v]; parameterless = true }

let listRangesEqTransitive (a1: Expr) (a2: Expr) (a3: Expr) (from: Expr) (mid: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "listRangesEqTransitive"; tps = []; args = [a1; a2; a3; from; mid; tto]; parameterless = true }

let listRangesEqImpliesEq (a1: Expr) (a2: Expr) (from: Expr) (at: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "listRangesEqImpliesEq"; tps = []; args = [a1; a2; from; at; tto]; parameterless = true }

let listRangesEq (a1: Expr) (a2: Expr) (from: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "listRangesEq"; tps = []; args = [a1; a2; from; tto]; parameterless = true }

let listRangesAppendDropEq (a1: Expr) (a2: Expr) (v: Expr) (from: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "listRangesAppendDropEq"; tps = []; args = [a1; a2; v; from; tto]; parameterless = true }

let isnocIndex (ls: Expr) (v: Expr) (i: Expr): Expr =
  FunctionCall { prefix = ["ListSpecs"]; id = "isnocIndex"; tps = []; args = [ls; v; i]; parameterless = true }


let listApplyEqVecApply (vec: Expr) (i: Expr): Expr =
  FunctionCall { prefix = ["Vector"]; id = "listApplyEqVecApply"; tps = []; args = [vec; i]; parameterless = true }

let vecRangesEqReflexiveLemma (arr: Expr): Expr =
  FunctionCall { prefix = []; id = "vecRangesEqReflexiveLemma"; tps = []; args = [arr]; parameterless = true }

let vecRangesEqSlicedLemma (a1: Expr) (a2: Expr) (from: Expr) (tto: Expr) (fromSlice: Expr) (toSlice: Expr): Expr =
  FunctionCall { prefix = []; id = "vecRangesEqSlicedLemma"; tps = []; args = [a1; a2; from; tto; fromSlice; toSlice]; parameterless = true }

let vecUpdatedAtPrefixLemma (arr: Expr) (at: Expr) (v: Expr): Expr =
  FunctionCall { prefix = []; id = "vecUpdatedAtPrefixLemma"; tps = []; args = [arr; at; v]; parameterless = true }

let vecRangesEqTransitive (a1: Expr) (a2: Expr) (a3: Expr) (from: Expr) (mid: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "vecRangesEqTransitive"; tps = []; args = [a1; a2; a3; from; mid; tto]; parameterless = true }

let vecRangesEqImpliesEq (a1: Expr) (a2: Expr) (from: Expr) (at: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "vecRangesEqImpliesEq"; tps = []; args = [a1; a2; from; at; tto]; parameterless = true }

let vecRangesEq (a1: Expr) (a2: Expr) (from: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "vecRangesEq"; tps = []; args = [a1; a2; from; tto]; parameterless = true }

let vecRangesAppendDropEq (a1: Expr) (a2: Expr) (v: Expr) (from: Expr) (tto: Expr): Expr =
  FunctionCall { prefix = []; id = "vecRangesAppendDropEq"; tps = []; args = [a1; a2; v; from; tto]; parameterless = true }


let fromIntClass (cls: Asn1AcnAst.IntegerClass): IntegerType =
  match cls with
  | Asn1AcnAst.ASN1SCC_Int8 _ -> Byte
  | Asn1AcnAst.ASN1SCC_Int16 _ -> Short
  | Asn1AcnAst.ASN1SCC_Int32 _ -> Int
  | Asn1AcnAst.ASN1SCC_Int64 _ | Asn1AcnAst.ASN1SCC_Int _ -> Long
  | Asn1AcnAst.ASN1SCC_UInt8 _ -> UByte
  | Asn1AcnAst.ASN1SCC_UInt16 _ -> UShort
  | Asn1AcnAst.ASN1SCC_UInt32 _ -> UInt
  | Asn1AcnAst.ASN1SCC_UInt64 _ | Asn1AcnAst.ASN1SCC_UInt _ -> ULong

let rec fromAsn1TypeKind (t: Asn1AcnAst.Asn1TypeKind): Type =
  match t.ActualType with
  | Asn1AcnAst.Sequence sq -> ClassType {ClassType.prefix = []; id = sq.typeDef[Scala].typeName; tps = []; parameterless = false}
  | Asn1AcnAst.SequenceOf sqf -> ClassType {ClassType.prefix = []; id = sqf.typeDef[Scala].typeName; tps = []; parameterless = false}
  | Asn1AcnAst.Choice ch -> ClassType {ClassType.prefix = []; id = ch.typeDef[Scala].typeName; tps = []; parameterless = false}
  | Asn1AcnAst.Enumerated enm -> ClassType {ClassType.prefix = []; id = enm.typeDef[Scala].typeName; tps = []; parameterless = false}
  | Asn1AcnAst.Integer int -> IntegerType (fromIntClass int.intClass)
  | Asn1AcnAst.Boolean _ -> BooleanType
  | Asn1AcnAst.NullType _ -> IntegerType Byte
  | Asn1AcnAst.BitString bt -> ClassType {ClassType.prefix = []; id = bt.typeDef[Scala].typeName; tps = []; parameterless = false}
  | Asn1AcnAst.OctetString ot -> ClassType {ClassType.prefix = []; id = ot.typeDef[Scala].typeName; tps = []; parameterless = false}
  | Asn1AcnAst.IA5String _ -> vecTpe (IntegerType UByte)
  | Asn1AcnAst.Real _ -> DoubleType
  | t -> failwith $"TODO {t}"

let fromAcnInsertedType (t: Asn1AcnAst.AcnInsertedType): Type =
  match t with
  | Asn1AcnAst.AcnInsertedType.AcnInteger int -> IntegerType (fromIntClass int.intClass)
  | Asn1AcnAst.AcnInsertedType.AcnBoolean _ -> BooleanType
  | Asn1AcnAst.AcnInsertedType.AcnNullType _ -> IntegerType Byte
  | Asn1AcnAst.AcnInsertedType.AcnReferenceToEnumerated enm -> ClassType {ClassType.prefix = []; id = enm.enumerated.typeDef[Scala].typeName; tps = []; parameterless = false}
  | Asn1AcnAst.AcnInsertedType.AcnReferenceToIA5String _ -> vecTpe (IntegerType UByte)

let fromAsn1AcnType (t: Asn1AcnAst.Asn1AcnType): Type =
  match t with
  | Asn1AcnAst.Asn1AcnType.Acn t -> fromAcnInsertedType t
  | Asn1AcnAst.Asn1AcnType.Asn1 t -> fromAsn1TypeKind t.Kind

let fromAsn1AcnChildInfo (t: Asn1AcnAst.SeqChildInfo): Type =
  match t with
  | Asn1AcnAst.SeqChildInfo.AcnChild t -> fromAcnInsertedType t.Type
  | Asn1AcnAst.SeqChildInfo.Asn1Child t -> fromAsn1TypeKind t.Type.Kind

let fromSequenceOfLike (t: SequenceOfLike): Type =
  match t with
  | SqOf t -> fromAsn1TypeKind (Asn1AcnAst.SequenceOf t)
  | StrType t -> fromAsn1TypeKind (Asn1AcnAst.IA5String t)

let fromSequenceOfLikeElemTpe (t: SequenceOfLike): Type =
  match t with
  | SqOf t -> fromAsn1TypeKind t.child.Kind
  | StrType t -> IntegerType UByte

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
  | ExprTree (Let _ | LetGhost _ | LetTuple _ | Block _ | Assert _ | LetRec _) -> false
  | _ -> true

// TODO: Match case?
let noBracesSub (e: Tree): Tree list =
  match e with
  | ExprTree (Let l) -> [ExprTree l.body]
  | ExprTree (LetGhost l) -> [ExprTree l.body]
  | ExprTree (LetTuple l) -> [ExprTree l.body]
  | ExprTree (Ghost e) -> [ExprTree e]
  | ExprTree (Locally e) -> [ExprTree e]
  | ExprTree (IfExpr ite) -> [ExprTree ite.els; ExprTree ite.thn]
  | ExprTree (LetRec lr) -> [ExprTree lr.body]
  // TODO: match case and not matchexpr...
  | ExprTree (MatchExpr m) -> m.cases |> List.map (fun c -> ExprTree c.rhs)
  | _ -> []

let requiresBraces (e: Tree) (within: Tree option): bool =
  match e, within with
  | _, _ when isSimpleExpr e -> false
  | _, Some (ExprTree (Ghost _ | Locally _)) -> false
  | _, Some within when List.contains e (noBracesSub within) -> false
  | ExprTree (LetRec _), Some (ExprTree (LetRec _)) -> false
  | ExprTree (Block _), Some (ExprTree (Or _ | Not _ | And _)) -> true
  | _, Some _ ->
    // TODO
    false
  | _, _ -> false

let precedence (e: Expr): int =
  match e with
  | Mod _ -> 0
  | Or _ -> 1
  | And _ | SplitAnd _ -> 3
  | Leq _ -> 4
  | Equals _ | Not _ -> 5
  | Plus _ | Minus _ -> 7
  | Mult _ -> 8
  | _ -> 9

let requiresParentheses (curr: Tree) (parent: Tree option): bool =
  match curr, parent with
  | _, None -> false
  | _, Some (ExprTree (Let _ | LetGhost _ | LetTuple _ | FunctionCall _ | Assert _ | Check _ | IfExpr _ | MatchExpr _)) -> false
  | _, Some (ExprTree (MethodCall call)) -> not (List.contains curr (call.args |> List.map ExprTree))
  | ExprTree (IfExpr _ | MatchExpr _), _  -> true
  | ExprTree e1, Some (ExprTree e2) when precedence e1 > precedence e2 -> false
  | _, Some (ExprTree (LetRec _)) -> false
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
  | UnitType -> "Unit"
  | DoubleType -> "Double"
  | ArrayType at -> $"Array[{ppType at.tpe}]"
  | ClassType ct -> ppClassType ct
  | TupleType tps -> "(" + ((tps |> List.map ppType).StrJoin ", ") + ")"

and ppClassType (ct: ClassType): string =
  let tps =
    if ct.tps.IsEmpty then ""
    else "[" + ((ct.tps |> List.map ppType).StrJoin ", ") + "]"
  let id =
    if ct.prefix.IsEmpty then ct.id
    else (ct.prefix.StrJoin ".") + "." + ct.id
  id + tps

let ppAnnot (annot: Annot): string =
  match annot with
  | Opaque -> "@opaque"
  | InlineOnce -> "@inlineOnce"
  | GhostAnnot -> "@ghost"
  | Pure -> "@pure"

// TODO: Maybe have ctx.nest here already?
let rec pp (ctx: PrintCtx) (t: Tree): Line list =
  if requiresBraces t ctx.parent && ctx.parent <> Some t then
    [{txt = "{"; lvl = ctx.lvl}] @ ppBody ctx.inc t @  [{txt = "}"; lvl = ctx.lvl}]
  else ppBody ctx t

and ppExpr (ctx: PrintCtx) (e: Expr): Line list = pp ctx (ExprTree e)

// `prefix`(arg1, arg2, ..., argn)
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

// `prefix` {
//   stmts
// }
and joinBraces (ctx: PrintCtx) (prefix: string) (stmts: Line list): Line list =
  [{lvl = ctx.lvl; txt = $"{prefix} {{"}] @
  (stmts |> List.map (fun l -> l.inc)) @
  [{lvl = ctx.lvl; txt = $"}}"}]

and ppLetGeneric (ctx: PrintCtx) (theLet: Expr) (ltBdgs: Var list) (ltE: Expr) (ltBody: Expr) (annot: string list): Line list =
  let e2 = ppExpr (ctx.nestExpr theLet) ltE
  let body = ppExpr (ctx.nestExpr theLet) ltBody
  let annot = if annot.IsEmpty then "" else (annot.StrJoin " ") + " "
  let bdgs =
    if ltBdgs.Length = 1 then ltBdgs.Head.name
    else "(" + ((ltBdgs |> List.map (fun v -> v.name)).StrJoin ", ") + ")"
  let prepended = (prepend ctx $"{annot}val {bdgs} = " e2)
  prepended @ body
and ppLet (ctx: PrintCtx) (theLet: Expr) (lt: Let) (annot: string list): Line list =
  ppLetGeneric ctx theLet [lt.bdg] lt.e lt.body annot

and ppMatchExpr (ctx: PrintCtx) (mexpr: MatchExpr): Line list =
  let rec ppPattern (pat: Pattern): string =
    match pat with
    | Wildcard None -> "_"
    | Wildcard (Some var) -> var.name
    | ADTPattern pat ->
      let bdg = pat.binder |> Option.map (fun bdg -> $"${bdg.name} @ ") |> Option.defaultValue ""
      let subpats = (pat.subPatterns |> List.map ppPattern).StrJoin ", "
      $"{bdg}{pat.id}({subpats})"
    | TuplePattern pat ->
      let bdg = pat.binder |> Option.map (fun bdg -> $"${bdg.name} @ ") |> Option.defaultValue ""
      let subpats = (pat.subPatterns |> List.map ppPattern).StrJoin ", "
      $"{bdg}({subpats})"

  let ppMatchCase (ctx: PrintCtx) (cse: MatchCase): Line list =
    let pat = {txt = $"case {ppPattern cse.pattern} =>"; lvl = ctx.lvl}
    pat :: ppExpr (ctx.inc.nestExpr cse.rhs) cse.rhs

  let ctxNested = ctx.nestExpr (MatchExpr mexpr)
  let cases = mexpr.cases |> List.collect (ppMatchCase ctxNested.inc)
  let scrut = ppExpr ctxNested mexpr.scrut
  (append ctx " match {" scrut) @ cases @ [{txt = "}"; lvl = ctx.lvl}]

and ppIfExpr (ctx: PrintCtx) (ifexpr: IfExpr): Line list =
  let ctxNested = ctx.nestExpr (IfExpr ifexpr)
  let cond = ppExpr (ctxNested.nestExpr ifexpr.cond) ifexpr.cond
  let thn = ppExpr (ctxNested.inc.nestExpr ifexpr.thn) ifexpr.thn
  let els = ppExpr (ctxNested.inc.nestExpr ifexpr.els) ifexpr.els
  (append ctx ") {" (prepend ctx "if (" cond)) @ thn @ [{txt = "} else {"; lvl = ctx.lvl}] @ els @ [{txt = "}"; lvl = ctx.lvl}]

and ppFunDefLike (ctx: PrintCtx) (fd: FunDefLike): Line list =
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
    | Precond (Block stmts) ->
      joinBraces ctx.inc "require" (stmts |> List.collect (fun s -> ppExpr ctx.inc s))
    | Precond e ->
      joinCallLike ctx.inc [{txt = "require"; lvl = ctx.lvl + 1}] [ppExpr ctx.inc e] false
    | Measure (Block stmts) ->
      joinBraces ctx.inc "decreases" (stmts |> List.collect (fun s -> ppExpr ctx.inc s))
    | Measure e ->
      joinCallLike ctx.inc [{txt = "decreases"; lvl = ctx.lvl + 1}] [ppExpr ctx.inc e] false
    | LetSpec (v, e) -> (prepend ctx.inc $"val {v.name} = " (ppExpr ctx.inc e))
  )
  let hasBdgInSpec = fd.specs |> List.exists (fun s -> match s with LetSpec _ -> true | _ -> false)

  match fd.postcond, hasBdgInSpec with
  | Some (resVar, postcond), true ->
    let body = ppExpr ctx.inc.inc fd.body
    let postcond = ppExpr ctx.inc.inc postcond
    header @
    preSpecs @
    [{txt = ""; lvl = ctx.lvl}] @ // for Scala to avoid defining an anonymous class with bindings from above
    [{txt = "{"; lvl = ctx.lvl + 1}] @
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
  | FunDefTree fd -> ppFunDefLike ctx fd
  | LocalFunDefTree fd -> ppFunDefLike ctx fd

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

  | Unfold e2 ->
    joinCallLike ctx [line "unfold"] [ppExpr (ctx.nestExpr e2) e2] false

  | Let lt -> ppLet ctx e lt []

  | LetGhost lt -> ppLet ctx e lt ["@ghost"]

  | LetTuple lt -> ppLetGeneric ctx e lt.bdgs lt.e lt.body []

  | Assert pred ->
    let pred = ppExpr (ctx.nestExpr pred) pred
    joinCallLike ctx [line "assert"] [pred] false

  | Check pred ->
    let pred = ppExpr (ctx.nestExpr pred) pred
    joinCallLike ctx [line "check"] [pred] false

  | MethodCall call ->
    let recv = ppExpr (ctx.nestExpr call.recv) call.recv
    let args = call.args |> List.map (fun a -> ppExpr (ctx.nestExpr a) a)
    joinCallLike ctx (append ctx $".{call.id}" recv) args call.parameterless

  | FunctionCall call ->
    let id = if call.prefix.IsEmpty then call.id else (call.prefix.StrJoin ".") + "." + call.id
    let args = call.args |> List.map (fun a -> ppExpr (ctx.nestExpr a) a)
    let tps = if call.tps.IsEmpty then "" else "[" + (call.tps |> List.map ppType).StrJoin ", " + "]"
    joinCallLike ctx [line (id + tps)] args call.parameterless

  | LetRec lr ->
    let fds = lr.fds |> List.collect (fun fd -> ppFunDefLike (ctx.nest (LocalFunDefTree fd)) fd)
    let body = ppExpr (ctx.nestExpr lr.body) lr.body
    fds @ body

  | ApplyLetRec call ->
    let args = call.args |> List.map (fun a -> ppExpr (ctx.nestExpr a) a)
    joinCallLike ctx [line call.id] args true

  | Tuple args ->
    let args = args |> List.map (fun a -> ppExpr (ctx.nestExpr a) a)
    joinCallLike ctx [line ""] args false

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

  | ArrayUpdate (arr, ix, newVal) ->
    let recv = ppExpr (ctx.nestExpr arr) arr
    let ix = ppExpr (ctx.nestExpr ix) ix
    let newVal = ppExpr (ctx.nestExpr newVal) newVal
    let sel = joinCallLike ctx recv [ix] false
    join ctx " = " sel newVal

  | ClassCtor cc ->
    let ct = ppClassType cc.ct
    let args = cc.args |> List.map (fun a -> ppExpr (ctx.nestExpr a) a)
    joinCallLike ctx [line ct] args cc.ct.parameterless

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

  | UnitLit -> [line "()"]
  // TODO: optP nestExpr?
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

  | Mod (lhs, rhs) ->
    let lhs = ppExpr (ctx.nestExpr lhs) lhs
    let rhs = ppExpr (ctx.nestExpr rhs) rhs
    optP ctx (join ctx " % " lhs rhs)

  | IfExpr ifexpr -> optP ctx (ppIfExpr ctx ifexpr)

  | MatchExpr mexpr -> optP ctx (ppMatchExpr ctx mexpr)

  | IntCast (e2, from, tto) ->
    let e2 = ppExpr (ctx.nestExpr e2) e2
    let extMeth (id: string) =
      joinCallLike ctx (append ctx $".{id}" e2) [] true
    let objFn (id: string) =
     joinCallLike ctx [line id] [e2] false
    match from, tto with
    | ULong, Long | UInt, Int | UShort, Short | UByte, Byte -> extMeth "toRaw"
    | Long, ULong | Int, UInt | Short, UShort | Byte, UByte -> objFn "fromRaw"
    | _ when from = tto -> e2
    | _ -> failwith $"Unsupported conversion {from} -> {tto}"

  | SelectionExpr sel -> [line sel]

  | This -> [line "this"]

  | TripleQMark -> [line "???"]

  | EncDec stmt ->
    (stmt.Split [|'\n'|]) |> Array.toList |> List.map line


let showLines (t: Tree): string list =
  pp {curr = t; parents = []; lvl = 0} t |> List.map (fun line -> (String.replicate line.lvl "    ") + line.txt)

let show (t: Tree): string =
  (showLines t).StrJoin "\n"
