﻿module DAstUtilFunctions
open System
open System.Numerics
open FsUtils
open CommonTypes

open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst



type ProgrammingLanguage with
    member this.SpecExtention =
        match this with
        |C      -> "h"
        |Ada    -> "ads"
        |Python -> "py"
    member this.BodyExtention =
        match this with
        |C      -> "c"
        |Ada    -> "adb"
        |Python -> "py"
    member this.ArrName =
        match this with
        |C      -> "arr"
        |Ada    -> "Data"
        |Python -> ""
    member this.AssignOperator =
        match this with
        |C      -> "="
        |Ada    -> ":="
        |Python -> "="
    member this.ArrayAccess idx =
        match this with
        |C      -> "[" + idx + "]"
        |Ada    -> "(" + idx + ")"
        |Python -> "[" + idx + "]"
    member this.ExpOr e1 e2 =
        match this with
        |C      -> isvalid_c.ExpOr e1 e2
        |Ada    -> isvalid_a.ExpOr e1 e2
        |Python -> isvalid_p.ExpOr e1 e2
    member this.ExpAnd e1 e2 =
        match this with
        |C      -> isvalid_c.ExpAnd e1 e2
        |Ada    -> isvalid_a.ExpAnd e1 e2
        |Python -> isvalid_p.ExpAnd e1 e2
    member this.ExpAndMulti expList =
        match this with
        |C      -> isvalid_c.ExpAndMulit expList
        |Ada    -> isvalid_a.ExpAndMulit expList
        |Python -> isvalid_p.ExpAndMulit expList
    member this.ExpNot e  =
        match this with
        |C      -> isvalid_c.ExpNot e
        |Ada    -> isvalid_a.ExpNot e
        |Python -> isvalid_p.ExpNot e
    member this.ExpEqual e1 e2  =
        match this with
        |C      -> isvalid_c.ExpEqual e1 e2
        |Ada    -> isvalid_a.ExpEqual e1 e2
        |Python -> isvalid_p.ExpEqual e1 e2
    member this.ExpStringEqual e1 e2  =
        match this with
        |C      -> isvalid_c.ExpStringEqual e1 e2
        |Ada    -> isvalid_a.ExpStringEqual e1 e2
        |Python -> isvalid_p.ExpStringEqual e1 e2
    member this.ExpGt e1 e2  =
        match this with
        |C      -> isvalid_c.ExpGt e1 e2
        |Ada    -> isvalid_a.ExpGt e1 e2
        |Python -> isvalid_p.ExpGt e1 e2
    member this.ExpGte e1 e2  =
        match this with
        |C      -> isvalid_c.ExpGte e1 e2
        |Ada    -> isvalid_a.ExpGte e1 e2
        |Python -> isvalid_p.ExpGte e1 e2
    member this.ExpLt e1 e2  =
        match this with
        |C      -> isvalid_c.ExpLt e1 e2
        |Ada    -> isvalid_a.ExpLt e1 e2
        |Python -> isvalid_p.ExpLt e1 e2
    member this.ExpLte e1 e2  =
        match this with
        |C      -> isvalid_c.ExpLte e1 e2
        |Ada    -> isvalid_a.ExpLte e1 e2
        |Python -> isvalid_p.ExpLte e1 e2
    member this.StrLen exp =
        match this with
        |C      -> isvalid_c.StrLen exp
        |Ada    -> isvalid_a.StrLen exp
        |Python -> isvalid_p.StrLen exp
    member this.Length exp sAcc =
        match this with
        |C      -> isvalid_c.ArrayLen exp sAcc
        |Ada    -> isvalid_a.ArrayLen exp sAcc
        |Python -> isvalid_p.ArrayLen exp sAcc
    member this.ArrayStartIndex =
        match this with
        |C      -> 0
        |Ada    -> 1
        |Python -> 0
    member this.boolean =
        match this with
        |C      -> "flag"
        |Ada    -> "Boolean"
        |Python -> "bool"



type FuncParamType  with 
    member this.toPointer (l:ProgrammingLanguage) =
        POINTER (this.getPointer l)
    member this.getPointer (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> x
        | Ada, POINTER x    -> x
        | Ada, FIXARRAY x   -> x
        | C, VALUE x        -> sprintf "(&(%s))" x
        | C, POINTER x      -> x
        | C, FIXARRAY x     -> x
        | Python, VALUE x   -> x
        | Python, POINTER x -> x
        | Python, FIXARRAY x-> x
    member this.getValue (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> x
        | Ada, POINTER x    -> x
        | Ada, FIXARRAY x   -> x
        | C, VALUE x        -> x
        | C, POINTER x      -> sprintf "(*(%s))" x
        | C, FIXARRAY x     -> x
        | Python, VALUE x   -> x
        | Python, POINTER x -> x
        | Python, FIXARRAY x-> x
    member this.p  =
        match this with
        | VALUE x      -> x
        | POINTER x    -> x
        | FIXARRAY x   -> x
    member this.getAcces (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> "."
        | Ada, POINTER x    -> "."
        | Ada, FIXARRAY x   -> "."
        | C, VALUE x        -> "."
        | C, POINTER x      -> "->"
        | C, FIXARRAY x     -> ""
        | Python, VALUE x   -> "."
        | Python, POINTER x -> "."
        | Python, FIXARRAY x-> "."
        
    member this.getStar (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> ""
        | Ada, POINTER x    -> ""
        | Ada, FIXARRAY x   -> ""
        | C, VALUE x        -> ""
        | C, POINTER x      -> "*"
        | C, FIXARRAY x     -> ""
        | Python, VALUE x   -> ""
        | Python, POINTER x -> ""
        | Python, FIXARRAY x-> ""
    member this.getAmber (l:ProgrammingLanguage) =
        if this.getStar l = "*" then "&" else ""        
    member this.getArrayItem (l:ProgrammingLanguage) (idx:string) (childTypeIsString: bool) =
        match l with
        | Ada   -> 
            let newPath = sprintf "%s.Data(%s)" this.p idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | C     -> 
            let newPath = sprintf "%s%sarr[%s]" this.p (this.getAcces l) idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | Python-> 
            let newPath = sprintf "%s[%s]" this.p idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
    member this.getSeqChild (l:ProgrammingLanguage) (childName:string) (childTypeIsString: bool) =
        match l with
        | Ada   -> 
            let newPath = sprintf "%s.%s" this.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | C     -> 
            let newPath = sprintf "%s%s%s" this.p (this.getAcces l) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | Python-> 
            let newPath = sprintf "%s.%s" this.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)

    member this.getChChild (l:ProgrammingLanguage) (childName:string) (childTypeIsString: bool) =
        match l with
        | Ada   -> 
            let newPath = sprintf "%s.%s" this.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | C     -> 
            let newPath = sprintf "%s%su.%s" this.p (this.getAcces l) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | Python-> 
            let newPath = sprintf "%s.%s" this.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)

    member this.getChChildIsPresent (l:ProgrammingLanguage) (childPresentName:string)  =
        match l with
        | Ada   -> 
            sprintf "%s.kind = %s_PRESENT" this.p childPresentName
        | C     -> 
            sprintf "%s%skind == %s_PRESENT" this.p (this.getAcces l) childPresentName
        | Python-> 
            sprintf "%s.get_attribute_exists('%s')" this.p childPresentName


let getAccessFromScopeNodeList (ReferenceToType nodes)  (childTypeIsString: bool) (l:ProgrammingLanguage) (pVal : FuncParamType) =
    let handleNode zeroBasedSeqeuenceOfLevel (pVal : FuncParamType) (n:ScopeNode) (childTypeIsString: bool) = 
        match n with
        | MD _
        | TA _
        | PRM _
        | VA _              -> raise(BugErrorException "getAccessFromScopeNodeList")
        | SEQ_CHILD chName  -> [], pVal.getSeqChild l (ToC chName) childTypeIsString
        | CH_CHILD (chName,pre_name)  -> 
            
            [pVal.getChChildIsPresent l pre_name], pVal.getChChild l (ToC chName) childTypeIsString
        | SQF               -> 
            let curIdx = sprintf "i%d" (zeroBasedSeqeuenceOfLevel + 1)

            [], pVal.getArrayItem l curIdx childTypeIsString

    match nodes with
    | (MD md)::(TA tas)::(PRM prm)::[]  -> (VALUE (ToC (md + "_" + tas + "_" + prm)), [])
    | (MD md)::(TA tas):: xs            ->
        let length = Seq.length xs
        let ret = 
            xs |> 
            List.fold(fun (curPath, curCheckExp, zeroBasedSeqeuenceOfLevel, idx) n -> 
                let chekPath, newPath = handleNode zeroBasedSeqeuenceOfLevel curPath n (childTypeIsString && idx=length)
                let zeroBasedSeqeuenceOfLevel = match n with SQF -> zeroBasedSeqeuenceOfLevel + 1 | _ -> zeroBasedSeqeuenceOfLevel
                (newPath, chekPath@curCheckExp, zeroBasedSeqeuenceOfLevel, idx+1)) (pVal,[], 0, 1) |> (fun (a,chekPath,_,_) -> a, chekPath)
        ret 
    | _                                 -> raise(BugErrorException "getAccessFromScopeNodeList")



type LocalVariable with
    member this.VarName =
        match this with
        | SequenceOfIndex (i,_)   -> sprintf "i%d" i
        | IntegerLocalVariable(name,_)    -> name
        | Asn1SIntLocalVariable(name,_)   -> name
        | Asn1UIntLocalVariable(name,_)   -> name
        | FlagLocalVariable(name,_)       -> name
        | AcnInsertedChild(name,_)        -> name
        | BooleanLocalVariable(name,_)    -> name
    member this.GetDeclaration (l:ProgrammingLanguage) =
        match l, this with
        | C,    SequenceOfIndex (i,None)                  -> sprintf "int i%d;" i
        | C,    SequenceOfIndex (i,Some iv)               -> sprintf "int i%d=%d;" i iv
        | Ada,  SequenceOfIndex (i,None)                  -> sprintf "i%d:Integer;" i
        | Ada,  SequenceOfIndex (i,Some iv)               -> sprintf "i%d:Integer:=%d;" i iv
        | Python, SequenceOfIndex (i,None)                -> sprintf "i%d: int" i
        | Python, SequenceOfIndex (i,Some iv)             -> sprintf "i%d: int = %d" i iv
        | C,    IntegerLocalVariable (name,None)          -> sprintf "int %s;" name
        | C,    IntegerLocalVariable (name,Some iv)       -> sprintf "int %s=%d;" name iv
        | Ada,  IntegerLocalVariable (name,None)          -> sprintf "%s:Integer;" name
        | Ada,  IntegerLocalVariable (name,Some iv)       -> sprintf "%s:Integer:=%d;" name iv
        | Python, IntegerLocalVariable (name,None)        -> sprintf "%s: int" name
        | Python, IntegerLocalVariable (name,Some iv)     -> sprintf "%s: int = %d" name iv
        | C,    Asn1SIntLocalVariable (name,None)         -> sprintf "asn1SccSint %s;" name
        | C,    Asn1SIntLocalVariable (name,Some iv)      -> sprintf "asn1SccSint %s=%d;" name iv
        | Ada,  Asn1SIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1Int;" name
        | Ada,  Asn1SIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1Int:=%d;" name iv
        | Python, Asn1SIntLocalVariable (name,None)       -> sprintf "%s: int" name
        | Python, Asn1SIntLocalVariable (name,Some iv)    -> sprintf "%s: int = %d" name iv
        | C,    Asn1UIntLocalVariable (name,None)         -> sprintf "asn1SccUint %s;" name
        | C,    Asn1UIntLocalVariable (name,Some iv)      -> sprintf "asn1SccUint %s=%d;" name iv
        | Ada,  Asn1UIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1UInt;" name
        | Ada,  Asn1UIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1UInt:=%d;" name iv
        | Python, Asn1UIntLocalVariable (name,None)       -> sprintf "%s: int" name
        | Python, Asn1UIntLocalVariable (name,Some iv)    -> sprintf "%s: int = %d" name iv
        | C,    FlagLocalVariable (name,None)             -> sprintf "flag %s;" name
        | C,    FlagLocalVariable (name,Some iv)          -> sprintf "flag %s=%d;" name iv
        | Ada,  FlagLocalVariable (name,None)             -> sprintf "%s:adaasn1rtl.BIT;" name
        | Ada,  FlagLocalVariable (name,Some iv)          -> sprintf "%s:adaasn1rtl.BIT:=%d;" name iv
        | Python, FlagLocalVariable (name,None)            -> sprintf "%s: bool" name
        | Python, FlagLocalVariable (name,Some iv)         -> sprintf "%s: bool = %d" name iv
        | C,    BooleanLocalVariable (name,None)          -> sprintf "flag %s;" name
        | C,    BooleanLocalVariable (name,Some iv)       -> sprintf "flag %s=%s;" name (if iv then "TRUE" else "FALSE")
        | Ada,  BooleanLocalVariable (name,None)          -> sprintf "%s:Boolean;" name
        | Ada,  BooleanLocalVariable (name,Some iv)       -> sprintf "%s:Boolean:=%s;" name (if iv then "True" else "False")
        | Python, BooleanLocalVariable (name,None)        -> sprintf "%s: bool" name
        | Python, BooleanLocalVariable (name,Some iv)     -> sprintf "%s: bool = %s" name (if iv then "True" else "False")
        | C,    AcnInsertedChild(name, vartype)           -> sprintf "%s %s;" vartype name
        | Ada,    AcnInsertedChild(name, vartype)         -> sprintf "%s:%s;" name vartype
        | Python, AcnInsertedChild(name, vartype)         -> sprintf "%s = None" name


type TypeDefintionOrReference with 
    member this.longTypedefName  l =
        match this with
        | TypeDefinition  td ->
            td.typedefName
        | ReferenceToExistingDefinition ref ->
            match ref.programUnit with
            | Some pu -> 
                match l with
                | Ada   -> pu + "." + ref.typedefName
                | C     -> ref.typedefName
                | Python-> ref.typedefName
            | None    -> ref.typedefName
            


type Asn1AcnAst.NamedItem with
    member this.getBackendName l = 
        match l with
        | C         -> ToC this.c_name
        | Ada       -> ToC this.ada_name
        | Python    -> ToCPy this.py_name

type Integer with
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons
    //member this.IsUnsigned = isUnsigned this.uperRange

type Enumerated with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type Real with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type Boolean with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type StringType with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type OctetString with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type BitString with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type SequenceOf with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons

type Sequence with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons
    member this.Asn1Children =
        this.children |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)

type Choice with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons
    
let  choiceIDForNone (id:ReferenceToType) =  ToC (id.AcnAbsPath.Tail.StrJoin("_").Replace("#","elem")) + "_NONE"

type ReferenceType with
    member ref.AsTypeAssignmentInfo =  {TypeAssignmentInfo.modName = ref.baseInfo.modName.Value; tasName = ref.baseInfo.tasName.Value}

type TypeAssignment with
    member ref.AsTypeAssignmentInfo modName=  {TypeAssignmentInfo.modName = modName; tasName = ref.Name.Value}

type Asn1AcnAst.ChChildInfo with
    member this.presentWhenName = (ToC this.present_when_name) + "_PRESENT"

type ChChildInfo with
    member this.presentWhenName = (ToC this._present_when_name_private) + "_PRESENT"

type SeqChildInfo with
    member this.acnInsertetField =
        match this with
        | Asn1Child _    -> false
        | AcnChild _     -> true

    member this.Name =
        match this with
        | Asn1Child x    -> x.Name.Value
        | AcnChild x     -> x.Name.Value

    member this.Optionality =
        match this with
        | Asn1Child x    -> x.Optionality
        | AcnChild x     -> None


type Asn1AcnAst.NamedItem      with
    member this.CEnumName l =
        match l with
        | C     -> this.c_name
        | Ada   -> this.ada_name
        | Python-> this.py_name


type Asn1AcnAst.Asn1Type with
    member this.getParamType (l:ProgrammingLanguage) (c:Codec) =
        match l with
        | Ada   -> VALUE "val"
        | C     ->
            match c with
            | Encode  ->
                match this.Kind with
                | Asn1AcnAst.Integer      _ -> VALUE "val"
                | Asn1AcnAst.Real         _ -> VALUE "val"
                | Asn1AcnAst.IA5String    _ -> FIXARRAY "val"
                | Asn1AcnAst.NumericString _ -> FIXARRAY "val"
                | Asn1AcnAst.OctetString  _ -> POINTER "pVal"
                | Asn1AcnAst.NullType     _ -> VALUE "val"
                | Asn1AcnAst.BitString    _ -> POINTER "pVal"
                | Asn1AcnAst.Boolean      _ -> VALUE "val"
                | Asn1AcnAst.Enumerated   _ -> VALUE "val"
                | Asn1AcnAst.SequenceOf   _ -> POINTER "pVal"
                | Asn1AcnAst.Sequence     _ -> POINTER "pVal"
                | Asn1AcnAst.Choice       _ -> POINTER "pVal"
                | Asn1AcnAst.ReferenceType r -> r.resolvedType.getParamType l c
            | Decode  ->
                match this.Kind with
                | Asn1AcnAst.Integer      _ -> POINTER "pVal"
                | Asn1AcnAst.Real         _ -> POINTER "pVal"
                | Asn1AcnAst.IA5String    _ -> FIXARRAY "val"
                | Asn1AcnAst.NumericString    _ -> FIXARRAY "val"
                | Asn1AcnAst.OctetString  _ -> POINTER "pVal"
                | Asn1AcnAst.NullType     _ -> POINTER "pVal"
                | Asn1AcnAst.BitString    _ -> POINTER "pVal"
                | Asn1AcnAst.Boolean      _ -> POINTER "pVal"
                | Asn1AcnAst.Enumerated   _ -> POINTER "pVal"
                | Asn1AcnAst.SequenceOf   _ -> POINTER "pVal"
                | Asn1AcnAst.Sequence     _ -> POINTER "pVal"
                | Asn1AcnAst.Choice       _ -> POINTER "pVal"
                | Asn1AcnAst.ReferenceType r -> r.resolvedType.getParamType l c
        | Python -> 
            match c with
            | Encode    -> 
                match this.Kind with
                | Asn1AcnAst.SequenceOf   _ -> VALUE "self"
                | Asn1AcnAst.Sequence     _ 
                | Asn1AcnAst.Choice       _ -> VALUE "self"
                | _                         -> VALUE "self._value"
            | Decode    ->
                match this.Kind with
                | Asn1AcnAst.Sequence     _ 
                | Asn1AcnAst.Choice       _ -> VALUE "self"
                | _                         -> VALUE "value"
    member this.getParamValue (p:FuncParamType) (l:ProgrammingLanguage) (c:Codec) =
        match l with
        | Ada   -> p.p
        | C     ->
            match c with
            | Encode  ->
                match this.Kind with
                | Asn1AcnAst.Integer      _ -> p.getValue l
                | Asn1AcnAst.Real         _ -> p.getValue l
                | Asn1AcnAst.IA5String    _ -> p.getValue l //FIXARRAY "val"
                | Asn1AcnAst.NumericString _ -> p.getValue l// FIXARRAY "val"
                | Asn1AcnAst.OctetString  _ -> p.getPointer l
                | Asn1AcnAst.NullType     _ -> p.getValue l
                | Asn1AcnAst.BitString    _ -> p.getPointer l
                | Asn1AcnAst.Boolean      _ -> p.getValue l
                | Asn1AcnAst.Enumerated   _ -> p.getValue l
                | Asn1AcnAst.SequenceOf   _ -> p.getPointer l
                | Asn1AcnAst.Sequence     _ -> p.getPointer l
                | Asn1AcnAst.Choice       _ -> p.getPointer l
                | Asn1AcnAst.ReferenceType r -> r.resolvedType.getParamValue p l c
            | Decode  ->
                match this.Kind with
                | Asn1AcnAst.IA5String    _  -> p.getValue l //FIXARRAY "val"
                | Asn1AcnAst.NumericString _ -> p.getValue l// FIXARRAY "val"
                | Asn1AcnAst.ReferenceType r -> r.resolvedType.getParamValue p l c
                | _                          -> p.getPointer l
        | Python-> p.p
        


type Asn1Type
with
    member this.ActualType =
        match this.Kind with
        | ReferenceType t-> t.resolvedType.ActualType
        | Integer      _ -> this
        | Real         _ -> this
        | IA5String    _ -> this
        | OctetString  _ -> this
        | NullType     _ -> this
        | BitString    _ -> this
        | Boolean      _ -> this
        | Enumerated   _ -> this
        | SequenceOf   _ -> this
        | Sequence     _ -> this
        | Choice       _ -> this
        
    member this.typeDefinition =
        match this.Kind with
        | Integer      t -> t.typeDefinition
        | Real         t -> t.typeDefinition
        | IA5String    t -> t.typeDefinition
        | OctetString  t -> t.typeDefinition
        | NullType     t -> t.typeDefinition
        | BitString    t -> t.typeDefinition
        | Boolean      t -> t.typeDefinition
        | Enumerated   t -> t.typeDefinition
        | SequenceOf   t -> t.typeDefinition
        | Sequence     t -> t.typeDefinition
        | Choice       t -> t.typeDefinition
        | ReferenceType t-> t.typeDefinition

    member this.printValue =
        match this.Kind with
        | Integer      t -> t.printValue
        | Real         t -> t.printValue
        | IA5String    t -> t.printValue
        | OctetString  t -> t.printValue
        | NullType     t -> t.printValue
        | BitString    t -> t.printValue
        | Boolean      t -> t.printValue
        | Enumerated   t -> t.printValue
        | SequenceOf   t -> t.printValue
        | Sequence     t -> t.printValue
        | Choice       t -> t.printValue
        | ReferenceType t-> t.printValue

    member this.initialValue =
        match this.Kind with
        | Integer      t -> IntegerValue t.initialValue
        | Real         t -> RealValue t.initialValue
        | IA5String    t -> StringValue t.initialValue
        | OctetString  t -> OctetStringValue t.initialValue
        | NullType     t -> NullValue t.initialValue
        | BitString    t -> BitStringValue t.initialValue
        | Boolean      t -> BooleanValue t.initialValue
        | Enumerated   t -> EnumValue t.initialValue
        | SequenceOf   t -> SeqOfValue t.initialValue
        | Sequence     t -> SeqValue t.initialValue
        | Choice       t -> ChValue t.initialValue
        | ReferenceType t-> t.initialValue.kind

    member this.initFunction =
        match this.Kind with
        | Integer      t -> t.initFunction
        | Real         t -> t.initFunction
        | IA5String    t -> t.initFunction
        | OctetString  t -> t.initFunction
        | NullType     t -> t.initFunction
        | BitString    t -> t.initFunction
        | Boolean      t -> t.initFunction
        | Enumerated   t -> t.initFunction
        | SequenceOf   t -> t.initFunction
        | Sequence     t -> t.initFunction
        | Choice       t -> t.initFunction
        | ReferenceType t-> t.initFunction

    member this.equalFunction =
        match this.Kind with
        | Integer      t -> t.equalFunction
        | Real         t -> t.equalFunction
        | IA5String    t -> t.equalFunction
        | OctetString  t -> t.equalFunction
        | NullType     t -> t.equalFunction
        | BitString    t -> t.equalFunction
        | Boolean      t -> t.equalFunction
        | Enumerated   t -> t.equalFunction
        | SequenceOf   t -> t.equalFunction
        | Sequence     t -> t.equalFunction
        | Choice       t -> t.equalFunction
        | ReferenceType t-> t.equalFunction

    member this.isValidFunction =
        match this.Kind with
        | Integer      t -> t.isValidFunction
        | Real         t -> t.isValidFunction
        | IA5String    t -> t.isValidFunction
        | OctetString  t -> t.isValidFunction
        | NullType     t -> None
        | BitString    t -> t.isValidFunction
        | Boolean      t -> t.isValidFunction
        | Enumerated   t -> t.isValidFunction
        | SequenceOf   t -> t.isValidFunction
        | Sequence     t -> t.isValidFunction
        | Choice       t -> t.isValidFunction
        | ReferenceType t-> t.isValidFunction
    
    member this.getUperFunction (l:CommonTypes.Codec) =
        match l with
        | CommonTypes.Encode   -> this.uperEncFunction
        | CommonTypes.Decode   -> this.uperDecFunction
    
    member this.uperEncFunction =
         match this.Kind with
         | Integer      t ->t.uperEncFunction
         | Real         t ->t.uperEncFunction
         | IA5String    t ->t.uperEncFunction
         | OctetString  t ->t.uperEncFunction
         | NullType     t ->t.uperEncFunction
         | BitString    t ->t.uperEncFunction
         | Boolean      t ->t.uperEncFunction
         | Enumerated   t ->t.uperEncFunction
         | SequenceOf   t ->t.uperEncFunction
         | Sequence     t ->t.uperEncFunction
         | Choice       t ->t.uperEncFunction
         | ReferenceType t->t.uperEncFunction

    member this.uperDecFunction =
         match this.Kind with
         | Integer      t -> t.uperDecFunction
         | Real         t -> t.uperDecFunction
         | IA5String    t -> t.uperDecFunction
         | OctetString  t -> t.uperDecFunction
         | NullType     t -> t.uperDecFunction
         | BitString    t -> t.uperDecFunction
         | Boolean      t -> t.uperDecFunction
         | Enumerated   t -> t.uperDecFunction
         | SequenceOf   t -> t.uperDecFunction
         | Sequence     t -> t.uperDecFunction
         | Choice       t -> t.uperDecFunction
         | ReferenceType t-> t.uperDecFunction

    member this.uperMaxSizeInBits =
        match this.Kind with
        | Integer      t -> t.baseInfo.uperMaxSizeInBits
        | Real         t -> t.baseInfo.uperMaxSizeInBits
        | IA5String    t -> t.baseInfo.uperMaxSizeInBits
        | OctetString  t -> t.baseInfo.uperMaxSizeInBits
        | NullType     t -> t.baseInfo.uperMaxSizeInBits
        | BitString    t -> t.baseInfo.uperMaxSizeInBits
        | Boolean      t -> t.baseInfo.uperMaxSizeInBits
        | Enumerated   t -> t.baseInfo.uperMaxSizeInBits
        | SequenceOf   t -> t.baseInfo.uperMaxSizeInBits
        | Sequence     t -> t.baseInfo.uperMaxSizeInBits
        | Choice       t -> t.baseInfo.uperMaxSizeInBits
        | ReferenceType ref -> ref.baseInfo.resolvedType.uperMaxSizeInBits
    member this.uperMinSizeInBits =
        match this.Kind with
        | Integer      t -> t.baseInfo.uperMinSizeInBits
        | Real         t -> t.baseInfo.uperMinSizeInBits
        | IA5String    t -> t.baseInfo.uperMinSizeInBits
        | OctetString  t -> t.baseInfo.uperMinSizeInBits
        | NullType     t -> t.baseInfo.uperMinSizeInBits
        | BitString    t -> t.baseInfo.uperMinSizeInBits
        | Boolean      t -> t.baseInfo.uperMinSizeInBits
        | Enumerated   t -> t.baseInfo.uperMinSizeInBits
        | SequenceOf   t -> t.baseInfo.uperMinSizeInBits
        | Sequence     t -> t.baseInfo.uperMinSizeInBits
        | Choice       t -> t.baseInfo.uperMinSizeInBits
        | ReferenceType ref -> ref.baseInfo.resolvedType.uperMinSizeInBits


    member this.acnEncFunction : AcnFunction option =
        match this.Kind with
        | Integer      t -> Some (t.acnEncFunction)
        | Real         t -> Some (t.acnEncFunction)
        | IA5String    t -> Some (t.acnEncFunction)
        | OctetString  t -> Some (t.acnEncFunction)
        | NullType     t -> Some (t.acnEncFunction)
        | BitString    t -> Some (t.acnEncFunction)
        | Boolean      t -> Some (t.acnEncFunction)
        | Enumerated   t -> Some (t.acnEncFunction)
        | SequenceOf   t -> Some (t.acnEncFunction)
        | Sequence     t -> Some (t.acnEncFunction)
        | Choice       t -> Some (t.acnEncFunction)
        | ReferenceType t-> Some (t.acnEncFunction)

    member this.acnDecFunction : AcnFunction option =
        match this.Kind with
        | Integer      t -> Some (t.acnDecFunction)
        | Real         t -> Some (t.acnDecFunction)
        | IA5String    t -> Some (t.acnDecFunction)
        | OctetString  t -> Some (t.acnDecFunction)
        | NullType     t -> Some (t.acnDecFunction)
        | BitString    t -> Some (t.acnDecFunction)
        | Boolean      t -> Some (t.acnDecFunction)
        | Enumerated   t -> Some (t.acnDecFunction)
        | SequenceOf   t -> Some (t.acnDecFunction)
        | Sequence     t -> Some (t.acnDecFunction)
        | Choice       t -> Some (t.acnDecFunction)
        | ReferenceType t-> Some (t.acnDecFunction)
    member this.getAcnFunction (l:CommonTypes.Codec) =
        match l with
        | CommonTypes.Encode   -> this.acnEncFunction
        | CommonTypes.Decode   -> this.acnDecFunction

//    uperEncDecTestFunc  : EncodeDecodeTestFunc
//    acnEncDecTestFunc   : EncodeDecodeTestFunc
    member this.uperEncDecTestFunc =
        match this.Kind with
        | Integer      t -> t.uperEncDecTestFunc
        | Real         t -> t.uperEncDecTestFunc
        | IA5String    t -> t.uperEncDecTestFunc
        | OctetString  t -> t.uperEncDecTestFunc
        | NullType     t -> t.uperEncDecTestFunc
        | BitString    t -> t.uperEncDecTestFunc
        | Boolean      t -> t.uperEncDecTestFunc
        | Enumerated   t -> t.uperEncDecTestFunc
        | SequenceOf   t -> t.uperEncDecTestFunc
        | Sequence     t -> t.uperEncDecTestFunc
        | Choice       t -> t.uperEncDecTestFunc
        | ReferenceType t-> t.uperEncDecTestFunc

    member this.acnEncDecTestFunc =
        match this.Kind with
        | Integer      t -> t.acnEncDecTestFunc
        | Real         t -> t.acnEncDecTestFunc
        | IA5String    t -> t.acnEncDecTestFunc
        | OctetString  t -> t.acnEncDecTestFunc
        | NullType     t -> t.acnEncDecTestFunc
        | BitString    t -> t.acnEncDecTestFunc
        | Boolean      t -> t.acnEncDecTestFunc
        | Enumerated   t -> t.acnEncDecTestFunc
        | SequenceOf   t -> t.acnEncDecTestFunc
        | Sequence     t -> t.acnEncDecTestFunc
        | Choice       t -> t.acnEncDecTestFunc
        | ReferenceType t-> t.acnEncDecTestFunc

    member this.automaticTestCasesValues =
        match this.Kind with
        | Integer      t -> t.automaticTestCasesValues
        | Real         t -> t.automaticTestCasesValues
        | IA5String    t -> t.automaticTestCasesValues
        | OctetString  t -> t.automaticTestCasesValues
        | NullType     t -> []
        | BitString    t -> t.automaticTestCasesValues
        | Boolean      t -> t.automaticTestCasesValues
        | Enumerated   t -> t.automaticTestCasesValues
        | SequenceOf   t -> t.automaticTestCasesValues
        | Sequence     t -> t.automaticTestCasesValues
        | Choice       t -> t.automaticTestCasesValues
        | ReferenceType t-> t.automaticTestCasesValues

    member this.typeDefintionOrReference : TypeDefintionOrReference =
        match this.Kind with
        | Integer      t -> t.definitionOrRef
        | Real         t -> t.definitionOrRef
        | IA5String    t -> t.definitionOrRef
        | OctetString  t -> t.definitionOrRef
        | NullType     t -> t.definitionOrRef
        | BitString    t -> t.definitionOrRef
        | Boolean      t -> t.definitionOrRef
        | Enumerated   t -> t.definitionOrRef
        | SequenceOf   t -> t.definitionOrRef
        | Sequence     t -> t.definitionOrRef
        | Choice       t -> t.definitionOrRef
        | ReferenceType t-> t.definitionOrRef

    member this.isIA5String =
        match this.Kind with
        | IA5String    _ -> true
        | _              -> false

    member this.asn1Name = 
        match this.id with
        | ReferenceToType((MD _)::(TA tasName)::[])   -> Some tasName
        | _                                                                     -> None

    member this.getParamType (l:ProgrammingLanguage) (c:Codec) =
        match l with
        | Ada   -> VALUE "val"
        | C     ->
            match c with
            | Encode  ->
                match this.Kind with
                | Integer      _ -> VALUE "val"
                | Real         _ -> VALUE "val"
                | IA5String    _ -> FIXARRAY "val"
                | OctetString  _ -> POINTER "pVal"
                | NullType     _ -> VALUE "val"
                | BitString    _ -> POINTER "pVal"
                | Boolean      _ -> VALUE "val"
                | Enumerated   _ -> VALUE "val"
                | SequenceOf   _ -> POINTER "pVal"
                | Sequence     _ -> POINTER "pVal"
                | Choice       _ -> POINTER "pVal"
                | ReferenceType r -> r.resolvedType.getParamType l c
            | Decode  ->
                match this.Kind with
                | Integer      _ -> POINTER "pVal"
                | Real         _ -> POINTER "pVal"
                | IA5String    _ -> FIXARRAY "val"
                | OctetString  _ -> POINTER "pVal"
                | NullType     _ -> POINTER "pVal"
                | BitString    _ -> POINTER "pVal"
                | Boolean      _ -> POINTER "pVal"
                | Enumerated   _ -> POINTER "pVal"
                | SequenceOf   _ -> POINTER "pVal"
                | Sequence     _ -> POINTER "pVal"
                | Choice       _ -> POINTER "pVal"
                | ReferenceType r -> r.resolvedType.getParamType l c
        | Python -> 
            match c with
            | Encode -> 
                match this.Kind with
                | SequenceOf    _   -> VALUE "self"
                | Sequence      _ 
                | Choice        _   -> VALUE "self"
                | _                 -> VALUE "self._value"
            | Decode    ->
                match this.Kind with
                | Sequence     _ 
                | Choice       _    -> VALUE "self"
                | _                 -> VALUE "value"
    member this.tasInfo =
        match this.typeAssignmentInfo with
        | Some (TypeAssignmentInfo tasInfo)  -> Some tasInfo
        | Some (ValueAssignmentInfo _)  -> None
        | None          ->
            match this.inheritInfo with
            | Some tasInfo  -> Some tasInfo.AsTasInfo
            | None          -> None



//let getValueType (r:AstRoot) (v:Asn1GenericValue) =
//    r.typesMap.[v.refToType]

type AstRoot with
    member this.getValueAssignmentByName (modName:String) (vasName:string) =
        match this.Files |> Seq.collect(fun f -> f.Modules) |> Seq.tryFind(fun m -> m.Name.Value = modName) with
        | None  -> raise(SemanticError(emptyLocation, (sprintf "No module exists with name '%s'" modName)))
        | Some m ->
            match m.ValueAssignments |> Seq.tryFind(fun vas -> vas.Name.Value = vasName) with
            |None   -> raise(SemanticError(emptyLocation, (sprintf "No value assignment exists with name '%s'" vasName)))
            | Some vas -> vas

    member r.Modules = r.Files |> List.collect(fun f -> f.Modules)
    member r.getModuleByName (name:StringLoc)  = 
        let (n,loc) = name.AsTupple
        match r.Modules |> Seq.tryFind( fun m -> m.Name = name)  with
        | Some(m) -> m
        | None    -> raise(SemanticError(loc, sprintf "No Module Defined with name: %s" n ))


type Asn1File with
    member this.FileNameWithoutExtension = System.IO.Path.GetFileNameWithoutExtension this.FileName

let getValueByUperRange (r:uperRange<'T>) (z:'T) = 
    match r with
    | Concrete (a,b)    -> if a <= z && z <= b then z else a
    | NegInf  b         -> if z <= b then z else b              //(-inf, b]
    | PosInf a          -> if a <= z then z else a               //[a, +inf)
    | Full              -> z

let rec mapValue (v:Asn1AcnAst.Asn1Value) =
    let newVKind = 
        match v.kind with
        | Asn1AcnAst.IntegerValue     v ->  IntegerValue        v.Value 
        | Asn1AcnAst.RealValue        v ->  RealValue           v.Value 
        | Asn1AcnAst.StringValue      v ->  StringValue         v.Value 
        | Asn1AcnAst.BooleanValue     v ->  BooleanValue        v.Value 
        | Asn1AcnAst.BitStringValue   v ->  BitStringValue      v.Value 
        | Asn1AcnAst.OctetStringValue v ->  OctetStringValue    (v |> List.map(fun z -> z.Value))
        | Asn1AcnAst.EnumValue        v ->  EnumValue           v.Value 
        | Asn1AcnAst.SeqOfValue       v ->  SeqOfValue          (v |> List.map mapValue)
        | Asn1AcnAst.SeqValue         v ->  SeqValue            (v |> List.map (fun n -> {NamedValue.name = n.name.Value; Value = mapValue n.Value}))
        | Asn1AcnAst.ChValue          n ->  ChValue             {NamedValue.name = n.name.Value; Value = mapValue n.Value}
        | Asn1AcnAst.NullValue        v ->  NullValue           v
        | Asn1AcnAst.RefValue     ((md,ts),v) ->  RefValue            ((md.Value, ts.Value), mapValue v)
    {Asn1Value.kind = newVKind; id=v.id; loc = v.loc}

type Asn1Value with
    member this.getBackendName (l:ProgrammingLanguage) =
        match this.id with
        | ReferenceToValue (typePath,(VA2 vasName)::[]) -> ToC vasName
        | ReferenceToValue (typePath, vasPath)      -> 
            let longName = (typePath.Tail |> List.map (fun i -> i.StrValue))@ (vasPath |> List.map (fun i -> i.StrValue))  |> Seq.StrJoin "_"
            ToC2(longName.Replace("#","elem").L1)


let hasAcnEncodeFunction (encFunc : AcnFunction option) acnParameters  =
    match encFunc with
    | None  -> false
    | Some fnc ->
        match acnParameters with
        | [] ->
            let p : FuncParamType = VALUE "dummy"
            match fnc.funcBody [] p with
            | None   -> false
            | Some _ -> true
        | _     -> false
                
let hasUperEncodeFunction (encFunc : UPerFunction option)  =
    match encFunc with
    | None  -> false
    | Some fnc ->
            let p : FuncParamType = VALUE "dummy"
            match fnc.funcBody p with
            | None   -> false
            | Some _ -> true
