module DAstUtilFunctions
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
    member this.BodyExtention =
        match this with
        |C      -> "c"
        |Ada    -> "adb"
    member this.ArrName =
        match this with
        |C      -> "arr"
        |Ada    -> "Data"
    member this.AssignOperator =
        match this with
        |C      -> "="
        |Ada    -> ":="
    member this.ArrayAccess idx =
        match this with
        |C      -> "[" + idx + "]"
        |Ada    -> "(" + idx + ")"
    member this.ExpOr e1 e2 =
        match this with
        |C      -> isvalid_c.ExpOr e1 e2
        |Ada    -> isvalid_a.ExpOr e1 e2
    member this.ExpAnd e1 e2 =
        match this with
        |C      -> isvalid_c.ExpAnd e1 e2
        |Ada    -> isvalid_a.ExpAnd e1 e2
    member this.ExpAndMulti expList =
        match this with
        |C      -> isvalid_c.ExpAndMulit expList
        |Ada    -> isvalid_a.ExpAndMulit expList
    member this.ExpNot e  =
        match this with
        |C      -> isvalid_c.ExpNot e
        |Ada    -> isvalid_a.ExpNot e
    member this.ExpEqual e1 e2  =
        match this with
        |C      -> isvalid_c.ExpEqual e1 e2
        |Ada    -> isvalid_a.ExpEqual e1 e2
    member this.ExpStringEqual e1 e2  =
        match this with
        |C      -> isvalid_c.ExpStringEqual e1 e2
        |Ada    -> isvalid_a.ExpStringEqual e1 e2
    member this.ExpGt e1 e2  =
        match this with
        |C      -> isvalid_c.ExpGt e1 e2
        |Ada    -> isvalid_a.ExpGt e1 e2
    member this.ExpGte e1 e2  =
        match this with
        |C      -> isvalid_c.ExpGte e1 e2
        |Ada    -> isvalid_a.ExpGte e1 e2
    member this.ExpLt e1 e2  =
        match this with
        |C      -> isvalid_c.ExpLt e1 e2
        |Ada    -> isvalid_a.ExpLt e1 e2
    member this.ExpLte e1 e2  =
        match this with
        |C      -> isvalid_c.ExpLte e1 e2
        |Ada    -> isvalid_a.ExpLte e1 e2
    member this.StrLen exp =
        match this with
        |C      -> isvalid_c.StrLen exp
        |Ada    -> isvalid_a.StrLen exp
    member this.Length exp sAcc =
        match this with
        |C      -> isvalid_c.ArrayLen exp sAcc
        |Ada    -> isvalid_a.ArrayLen exp sAcc
    member this.ArrayStartIndex =
        match this with
        |C      -> 0
        |Ada    -> 1
    member this.boolean =
        match this with
        |C      -> "flag"
        |Ada    -> "Boolean"
    member this.toHex n =
        match this with
        |C      -> sprintf "0x%x" n
        |Ada    -> sprintf "16#%x#" n
        

(*
    if <sRemainingItemsVar> >= 16#10000# then
        <sCurBlockSize> := 16#10000#;
        uper_asn1_rtl.UPER_Enc_ConstraintWholeNumber(bs, 16#C4#, 0, 8);
    elsif <sRemainingItemsVar> >= 16#C000# then
        <sCurBlockSize> := 16#C000#;
        uper_asn1_rtl.UPER_Enc_ConstraintWholeNumber(bs, 16#C3#, 0, 8);
    elsif <sRemainingItemsVar> >= 16#8000# then
        <sCurBlockSize> := 16#8000#;
        uper_asn1_rtl.UPER_Enc_ConstraintWholeNumber(bs, 16#C2#, 0, 8);
    else 
        <sCurBlockSize> := 16#4000#;
        uper_asn1_rtl.UPER_Enc_ConstraintWholeNumber(bs, 16#C1#, 0, 8);
    end if;

*)
    
    member l.multiIf (ifParts : List<string*string>) (elsePart : string option) =
    //    let tst = [("a==1","do1"); ("a==2","do2")]
    //let test (l:ProgrammingLanguage) (ifParts : List<string*string>) (elsePart : string option) = 
        let if_ i l bExp =
            match l with
            | Ada -> if i=0 then (sprintf "if %s then" bExp) else (sprintf "elsif %s then" bExp)
            | C   -> if i=0 then (sprintf "if (%s) {" bExp) else (sprintf "} else if (%s) {" bExp)
        let else_ l =
            match l with
            | Ada -> "else"
            | C   -> "} else {"
        let endif l =
            match l with
            | Ada -> "end if;"
            | C   -> "}"
        match ifParts with
        | _::_  ->
            let ifParts = ifParts |> List.mapi(fun i (a,b) -> (i,a,b))
            let aaa = 
                seq {
                    for (i, bExp, statemnt) in ifParts do
                        yield if_ i l bExp
                        yield header_c.indentation statemnt
                    match elsePart with
                    | Some elsePart ->
                        yield else_ l
                        yield header_c.indentation elsePart
                    | None          -> ()
                    yield endif l
                } |> Seq.toList
            aaa
        | []    ->
            match elsePart with
            | Some elsePart ->
                [elsePart]
            | None          -> []




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
    member this.getValue (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> x
        | Ada, POINTER x    -> x
        | Ada, FIXARRAY x   -> x
        | C, VALUE x        -> x
        | C, POINTER x      -> sprintf "(*(%s))" x
        | C, FIXARRAY x     -> x
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
        
    member this.getStar (l:ProgrammingLanguage) =
        match l, this with
        | Ada, VALUE x      -> ""
        | Ada, POINTER x    -> ""
        | Ada, FIXARRAY x   -> ""
        | C, VALUE x        -> ""
        | C, POINTER x      -> "*"
        | C, FIXARRAY x     -> ""
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
    member this.getSeqChild (l:ProgrammingLanguage) (childName:string) (childTypeIsString: bool) =
        match l with
        | Ada   -> 
            let newPath = sprintf "%s.%s" this.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | C     -> 
            let newPath = sprintf "%s%s%s" this.p (this.getAcces l) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)

    member this.getChChild (l:ProgrammingLanguage) (childName:string) (childTypeIsString: bool) =
        match l with
        | Ada   -> 
            let newPath = sprintf "%s.%s" this.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        | C     -> 
            let newPath = sprintf "%s%su.%s" this.p (this.getAcces l) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)

    member this.getChChildIsPresent (l:ProgrammingLanguage) (childPresentName:string)  =
        match l with
        | Ada   -> 
            sprintf "%s.kind = %s_PRESENT" this.p childPresentName
        | C     -> 
            sprintf "%s%skind == %s_PRESENT" this.p (this.getAcces l) childPresentName

let getAccessFromScopeNodeList (ReferenceToType nodes)  (childTypeIsString: bool) (l:ProgrammingLanguage) (pVal : CallerScope) =
    let handleNode zeroBasedSeqeuenceOfLevel (pVal : CallerScope) (n:ScopeNode) (childTypeIsString: bool) = 
        match n with
        | MD _
        | TA _
        | PRM _
        | VA _              -> raise(BugErrorException "getAccessFromScopeNodeList")
        | SEQ_CHILD chName  -> [], {pVal with arg = pVal.arg.getSeqChild l (ToC chName) childTypeIsString}
        | CH_CHILD (chName,pre_name)  -> 
            
            [pVal.arg.getChChildIsPresent l pre_name], {pVal with arg = pVal.arg.getChChild l (ToC chName) childTypeIsString}
        | SQF               -> 
            let curIdx = sprintf "i%d" (zeroBasedSeqeuenceOfLevel + 1)

            [], {pVal with arg = pVal.arg.getArrayItem l curIdx childTypeIsString}

    match nodes with
    | (MD md)::(TA tas)::(PRM prm)::[]  -> ({CallerScope.modName = pVal.modName; arg = VALUE (ToC (md + "_" + tas + "_" + prm))}, [])
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
        | GenericLocalVariable lv         -> lv.name
    member this.GetDeclaration (l:ProgrammingLanguage) =
        match l, this with
        | C,    SequenceOfIndex (i,None)                  -> sprintf "int i%d;" i
        | C,    SequenceOfIndex (i,Some iv)               -> sprintf "int i%d=%d;" i iv
        | Ada,  SequenceOfIndex (i,None)                  -> sprintf "i%d:Integer;" i
        | Ada,  SequenceOfIndex (i,Some iv)               -> sprintf "i%d:Integer:=%d;" i iv
        | C,    IntegerLocalVariable (name,None)          -> sprintf "int %s;" name
        | C,    IntegerLocalVariable (name,Some iv)       -> sprintf "int %s=%d;" name iv
        | Ada,  IntegerLocalVariable (name,None)          -> sprintf "%s:Integer;" name
        | Ada,  IntegerLocalVariable (name,Some iv)       -> sprintf "%s:Integer:=%d;" name iv
        | C,    Asn1SIntLocalVariable (name,None)         -> sprintf "asn1SccSint %s;" name
        | C,    Asn1SIntLocalVariable (name,Some iv)      -> sprintf "asn1SccSint %s=%d;" name iv
        | Ada,  Asn1SIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1Int;" name
        | Ada,  Asn1SIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1Int:=%d;" name iv
        | C,    Asn1UIntLocalVariable (name,None)         -> sprintf "asn1SccUint %s;" name
        | C,    Asn1UIntLocalVariable (name,Some iv)      -> sprintf "asn1SccUint %s=%d;" name iv
        | Ada,  Asn1UIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1UInt;" name
        | Ada,  Asn1UIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1UInt:=%d;" name iv
        | C,    FlagLocalVariable (name,None)             -> sprintf "flag %s;" name
        | C,    FlagLocalVariable (name,Some iv)          -> sprintf "flag %s=%d;" name iv
        | Ada,  FlagLocalVariable (name,None)             -> sprintf "%s:adaasn1rtl.BIT;" name
        | Ada,  FlagLocalVariable (name,Some iv)          -> sprintf "%s:adaasn1rtl.BIT:=%d;" name iv
        | C,    BooleanLocalVariable (name,None)          -> sprintf "flag %s;" name
        | C,    BooleanLocalVariable (name,Some iv)       -> sprintf "flag %s=%s;" name (if iv then "TRUE" else "FALSE")
        | Ada,  BooleanLocalVariable (name,None)          -> sprintf "%s:Boolean;" name
        | Ada,  BooleanLocalVariable (name,Some iv)       -> sprintf "%s:Boolean:=%s;" name (if iv then "True" else "False")
        | C,    AcnInsertedChild(name, vartype)           -> sprintf "%s %s;" vartype name
        | Ada,    AcnInsertedChild(name, vartype)         -> sprintf "%s:%s;" name vartype
        | C,    GenericLocalVariable lv                   ->
            sprintf "%s%s %s%s;" (if lv.isStatic then "static " else "") lv.varType lv.name (if lv.arrSize.IsNone then "" else "["+lv.arrSize.Value+"]")
        | Ada,    GenericLocalVariable lv                   ->
            match lv.initExp with
            | Some initExp  -> sprintf "%s : %s := %s;" lv.name lv.varType  initExp
            | None          -> sprintf "%s : %s;" lv.name lv.varType  


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
            | None    -> ref.typedefName
            
    member this.getAsn1Name (typePrefix : string) =
        let typedefName = 
            match this with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref -> ref.typedefName
        let idx = typedefName.IndexOf typePrefix
        match idx < 0 with
        | true      -> typedefName.Replace("_","-")
        | false     -> typedefName.Remove(idx, typePrefix.Length).Replace("_","-")



type Asn1AcnAst.NamedItem with
    member this.getBackendName (defOrRef:TypeDefintionOrReference option) l = 
        match l with
        | C         -> ToC this.c_name
        | Ada       -> 
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + this.ada_name
            | Some (TypeDefinition td) when td.baseType.IsSome && td.baseType.Value.programUnit.IsSome  -> td.baseType.Value.programUnit.Value + "." + this.ada_name
            | _       -> ToC this.ada_name

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

type ObjectIdentifier with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons


type Sequence with 
    member this.Cons     = this.baseInfo.cons
    member this.WithCons = this.baseInfo.withcons
    member this.AllCons  = this.baseInfo.cons@this.baseInfo.withcons
    member this.Asn1Children =
        this.children |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)

type Asn1Child with
    member this.getBackendName l = 
        match l with
        | C         -> this._c_name
        | Ada       -> this._ada_name

type Asn1AcnAst.Asn1Child with
    member this.getBackendName l = 
        match l with
        | C         -> this._c_name
        | Ada       -> this._ada_name

type ChChildInfo with
    member this.getBackendName l = 
        match l with
        | C         -> this._c_name
        | Ada       -> this._ada_name

type Asn1AcnAst.ChChildInfo with
    member this.getBackendName l = 
        match l with
        | C         -> this._c_name
        | Ada       -> this._ada_name


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
    member this.presentWhenName (defOrRef:TypeDefintionOrReference option) l = 
        match l with
        | C     -> (ToC this._present_when_name_private) + "_PRESENT"
        | Ada   ->
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + ((ToC this._present_when_name_private) + "_PRESENT")
            | _       -> (ToC this._present_when_name_private) + "_PRESENT"

        


type Asn1AcnAst.NamedItem      with
    member this.CEnumName l =
        match l with
        | C     -> this.c_name
        | Ada   -> this.ada_name


type Asn1AcnAst.Asn1Type with
    member this.getParamTypeSuffix (l:ProgrammingLanguage) (suf:string) (c:Codec) =
        match l with
        | Ada   -> {CallerScope.modName = this.id.ModName; arg= VALUE ("val" + suf) }
        | C     ->
            match c with
            | Encode  ->
                match this.Kind with
                | Asn1AcnAst.Integer         _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.Real            _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.IA5String       _ -> {CallerScope.modName = this.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.NumericString   _ -> {CallerScope.modName = this.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.OctetString     _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.NullType        _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.BitString       _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Boolean         _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.Enumerated      _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.SequenceOf      _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Sequence        _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Choice          _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ObjectIdentifier _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.TimeType _         -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ReferenceType r -> r.resolvedType.getParamTypeSuffix l suf c
            | Decode  ->
                match this.Kind with
                | Asn1AcnAst.Integer            _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Real               _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.IA5String          _ -> {CallerScope.modName = this.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.NumericString      _ -> {CallerScope.modName = this.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.OctetString        _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.NullType           _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.BitString          _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Boolean            _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Enumerated         _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.SequenceOf         _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Sequence           _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Choice             _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ObjectIdentifier _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.TimeType _ -> {CallerScope.modName = this.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ReferenceType r -> r.resolvedType.getParamTypeSuffix l suf c
    member this.getParamType (l:ProgrammingLanguage) (c:Codec) =
        this.getParamTypeSuffix l "" c
    member this.getParamValue (p:FuncParamType) (l:ProgrammingLanguage) (c:Codec) =
        match l with
        | Ada   -> p.p
        | C     ->
            match c with
            | Encode  ->
                match this.Kind with
                | Asn1AcnAst.Integer      _ -> p.getPointer l
                | Asn1AcnAst.Real         _ -> p.getPointer l
                | Asn1AcnAst.IA5String    _ -> p.getValue l //FIXARRAY "val"
                | Asn1AcnAst.NumericString _ -> p.getValue l// FIXARRAY "val"
                | Asn1AcnAst.OctetString  _ -> p.getPointer l
                | Asn1AcnAst.NullType     _ -> p.getPointer l
                | Asn1AcnAst.BitString    _ -> p.getPointer l
                | Asn1AcnAst.Boolean      _ -> p.getPointer l
                | Asn1AcnAst.Enumerated   _ -> p.getPointer l
                | Asn1AcnAst.SequenceOf   _ -> p.getPointer l
                | Asn1AcnAst.Sequence     _ -> p.getPointer l
                | Asn1AcnAst.Choice       _ -> p.getPointer l
                | Asn1AcnAst.ObjectIdentifier _ -> p.getPointer l
                | Asn1AcnAst.TimeType _ -> p.getPointer l
                | Asn1AcnAst.ReferenceType r -> r.resolvedType.getParamValue p l c
            | Decode  ->
                match this.Kind with
                | Asn1AcnAst.IA5String    _  -> p.getValue l //FIXARRAY "val"
                | Asn1AcnAst.NumericString _ -> p.getValue l// FIXARRAY "val"
                | Asn1AcnAst.ReferenceType r -> r.resolvedType.getParamValue p l c
                | _                          -> p.getPointer l
        


type Asn1Type
with
    member this.getActualType (r:AstRoot) =
        match this.Kind with
        | ReferenceType t-> 
            let md =  r.Files |> List.collect(fun f -> f.Modules) |> Seq.find(fun m -> m.Name.Value = t.baseInfo.modName.Value) //t.baseInfo.modName
            let ts = md.TypeAssignments |> Seq.find(fun ts -> ts.Name.Value = t.baseInfo.tasName.Value) //t.baseInfo.modName
            ts.Type.getActualType r
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
        | ObjectIdentifier _ -> this
        | TimeType     _  -> this
    


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
        | ObjectIdentifier _ -> this
        | TimeType     _  -> this
        
    member this.FT_TypeDefintion =
        match this.Kind with
        | Integer      t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | Real         t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | ObjectIdentifier t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | TimeType t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | IA5String    t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_StringTypeDefinition d)) |> Map.ofList
        | OctetString  t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList
        | NullType     t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | BitString    t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList
        | Boolean      t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_PrimitiveTypeDefinition d)) |> Map.ofList
        | Enumerated   t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_EnumeratedTypeDefinition d)) |> Map.ofList
        | SequenceOf   t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_SizeableTypeDefinition d)) |> Map.ofList
        | Sequence     t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_SequenceTypeDefinition d)) |> Map.ofList
        | Choice       t -> t.baseInfo.typeDef   |> Map.toList |> List.map (fun (l, d) -> (l, FE_ChoiceTypeDefinition d)) |> Map.ofList
        | ReferenceType t-> t.baseInfo.typeDef   
                                                 
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
        | ObjectIdentifier t -> t.printValue
        | TimeType  t       -> t.printValue

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
        | ObjectIdentifier t -> ObjOrRelObjIdValue  t.initialValue
        | TimeType t        -> TimeValue t.initialValue

    member this.ConstraintsAsn1Str = 
        match this.Kind with
        | Integer      t -> t.constraintsAsn1Str
        | Real         t -> t.constraintsAsn1Str
        | IA5String    t -> t.constraintsAsn1Str
        | OctetString  t -> t.constraintsAsn1Str
        | NullType     t -> t.constraintsAsn1Str
        | BitString    t -> t.constraintsAsn1Str
        | Boolean      t -> t.constraintsAsn1Str
        | Enumerated   t -> t.constraintsAsn1Str
        | SequenceOf   t -> t.constraintsAsn1Str
        | Sequence     t -> t.constraintsAsn1Str
        | Choice       t -> t.constraintsAsn1Str
        | ReferenceType t-> t.constraintsAsn1Str
        | ObjectIdentifier t -> t.constraintsAsn1Str
        | TimeType t        -> t.constraintsAsn1Str














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
        | ObjectIdentifier t -> t.initFunction
        | TimeType t        -> t.initFunction

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
        | ObjectIdentifier t -> t.equalFunction
        | TimeType t        -> t.equalFunction

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
        | ObjectIdentifier t-> t.isValidFunction
        | SequenceOf   t -> t.isValidFunction
        | Sequence     t -> t.isValidFunction
        | Choice       t -> t.isValidFunction
        | ReferenceType t-> t.isValidFunction
        | TimeType t        -> t.isValidFunction
    
    member this.getUperFunction (l:CommonTypes.Codec) =
        match l with
        | CommonTypes.Encode   -> this.uperEncFunction
        | CommonTypes.Decode   -> this.uperDecFunction
    member this.getXerFunction (l:CommonTypes.Codec) =
        match l with
        | CommonTypes.Encode   -> this.xerEncFunction
        | CommonTypes.Decode   -> this.xerDecFunction
    
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
         | ObjectIdentifier t -> t.uperEncFunction
         | TimeType t        -> t.uperEncFunction

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
         | ObjectIdentifier t -> t.uperDecFunction
         | TimeType t        -> t.uperDecFunction


    member this.xerEncFunction =
         match this.Kind with
         | Integer      t ->t.xerEncFunction
         | Real         t ->t.xerEncFunction
         | IA5String    t ->t.xerEncFunction
         | OctetString  t ->t.xerEncFunction
         | NullType     t ->t.xerEncFunction
         | BitString    t ->t.xerEncFunction
         | Boolean      t ->t.xerEncFunction
         | Enumerated   t ->t.xerEncFunction
         | SequenceOf   t ->t.xerEncFunction
         | Sequence     t ->t.xerEncFunction
         | Choice       t ->t.xerEncFunction
         | ReferenceType t->t.xerEncFunction
         | ObjectIdentifier t -> t.xerEncFunction
         | TimeType t        -> t.xerEncFunction

    member this.xerDecFunction =
         match this.Kind with
         | Integer      t -> t.xerDecFunction
         | Real         t -> t.xerDecFunction
         | IA5String    t -> t.xerDecFunction
         | OctetString  t -> t.xerDecFunction
         | NullType     t -> t.xerDecFunction
         | BitString    t -> t.xerDecFunction
         | Boolean      t -> t.xerDecFunction
         | Enumerated   t -> t.xerDecFunction
         | SequenceOf   t -> t.xerDecFunction
         | Sequence     t -> t.xerDecFunction
         | Choice       t -> t.xerDecFunction
         | ReferenceType t-> t.xerDecFunction
         | ObjectIdentifier t -> t.xerDecFunction
         | TimeType t        -> t.xerDecFunction


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
        | ObjectIdentifier t -> t.baseInfo.uperMaxSizeInBits
        | TimeType t        -> t.baseInfo.uperMaxSizeInBits

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
        | ObjectIdentifier t -> t.baseInfo.uperMinSizeInBits
        | ReferenceType ref -> ref.baseInfo.resolvedType.uperMinSizeInBits
        | TimeType t        -> t.baseInfo.uperMinSizeInBits


    member this.acnMaxSizeInBits =
        match this.Kind with
        | Integer      t -> t.baseInfo.acnMaxSizeInBits
        | Real         t -> t.baseInfo.acnMaxSizeInBits
        | IA5String    t -> t.baseInfo.acnMaxSizeInBits
        | OctetString  t -> t.baseInfo.acnMaxSizeInBits
        | NullType     t -> t.baseInfo.acnMaxSizeInBits
        | BitString    t -> t.baseInfo.acnMaxSizeInBits
        | Boolean      t -> t.baseInfo.acnMaxSizeInBits
        | Enumerated   t -> t.baseInfo.acnMaxSizeInBits
        | SequenceOf   t -> t.baseInfo.acnMaxSizeInBits
        | Sequence     t -> t.baseInfo.acnMaxSizeInBits
        | Choice       t -> t.baseInfo.acnMaxSizeInBits
        | ObjectIdentifier t -> t.baseInfo.acnMaxSizeInBits
        | ReferenceType ref -> ref.baseInfo.resolvedType.acnMaxSizeInBits
        | TimeType t        -> t.baseInfo.acnMaxSizeInBits

    member this.acnMinSizeInBits =
        match this.Kind with
        | Integer      t -> t.baseInfo.acnMinSizeInBits
        | Real         t -> t.baseInfo.acnMinSizeInBits
        | IA5String    t -> t.baseInfo.acnMinSizeInBits
        | OctetString  t -> t.baseInfo.acnMinSizeInBits
        | NullType     t -> t.baseInfo.acnMinSizeInBits
        | BitString    t -> t.baseInfo.acnMinSizeInBits
        | Boolean      t -> t.baseInfo.acnMinSizeInBits
        | Enumerated   t -> t.baseInfo.acnMinSizeInBits
        | SequenceOf   t -> t.baseInfo.acnMinSizeInBits
        | Sequence     t -> t.baseInfo.acnMinSizeInBits
        | Choice       t -> t.baseInfo.acnMinSizeInBits
        | ObjectIdentifier t -> t.baseInfo.acnMinSizeInBits
        | ReferenceType ref -> ref.baseInfo.resolvedType.acnMinSizeInBits
        | TimeType t        -> t.baseInfo.acnMinSizeInBits

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
        | ObjectIdentifier t -> Some (t.acnEncFunction)
        | TimeType t        -> Some (t.acnEncFunction)

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
        | ObjectIdentifier t -> Some (t.acnDecFunction)
        | ReferenceType t-> Some (t.acnDecFunction)
        | TimeType t        -> Some (t.acnDecFunction)

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
        | ObjectIdentifier t -> t.uperEncDecTestFunc
        | ReferenceType t-> t.uperEncDecTestFunc
        | TimeType t        -> t.uperEncDecTestFunc

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
        | ObjectIdentifier t -> t.acnEncDecTestFunc
        | ReferenceType t-> t.acnEncDecTestFunc
        | TimeType t        -> t.acnEncDecTestFunc

    member this.xerEncDecTestFunc =
        match this.Kind with
        | Integer      t -> t.xerEncDecTestFunc
        | Real         t -> t.xerEncDecTestFunc
        | IA5String    t -> t.xerEncDecTestFunc
        | OctetString  t -> t.xerEncDecTestFunc
        | NullType     t -> t.xerEncDecTestFunc
        | BitString    t -> t.xerEncDecTestFunc
        | Boolean      t -> t.xerEncDecTestFunc
        | Enumerated   t -> t.xerEncDecTestFunc
        | SequenceOf   t -> t.xerEncDecTestFunc
        | Sequence     t -> t.xerEncDecTestFunc
        | Choice       t -> t.xerEncDecTestFunc
        | ObjectIdentifier t -> t.xerEncDecTestFunc
        | ReferenceType t-> t.xerEncDecTestFunc
        | TimeType t        -> t.xerEncDecTestFunc

    member this.getEncDecTestFunc (e:Asn1Encoding) =
        match e with
        | Asn1Encoding.UPER  -> this.uperEncDecTestFunc
        | Asn1Encoding.ACN   -> this.acnEncDecTestFunc
        | Asn1Encoding.XER   -> this.xerEncDecTestFunc
        | Asn1Encoding.BER   -> None
        
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
        | ObjectIdentifier t -> t.automaticTestCasesValues
        | ReferenceType t-> t.automaticTestCasesValues
        | TimeType t        -> t.automaticTestCasesValues

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
        | ObjectIdentifier t -> t.definitionOrRef
        | ReferenceType t-> t.definitionOrRef
        | TimeType t        -> t.definitionOrRef

    member this.isIA5String =
        match this.Kind with
        | IA5String    _    -> true
        | ReferenceType r   -> this.ActualType.isIA5String
        | _                 -> false

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
                | ObjectIdentifier t -> POINTER "pVal"
                | ReferenceType r -> r.resolvedType.getParamType l c
                | TimeType _        -> POINTER "pVal"
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
                | ObjectIdentifier t -> POINTER "pVal"
                | TimeType _        -> POINTER "pVal"
                | ReferenceType r -> r.resolvedType.getParamType l c
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

type Asn1Module with
    member this.ExportedTypes =
        match this.Exports with
        | Asn1Ast.All   -> 
            let importedTypes = this.Imports |> List.collect(fun imp -> imp.Types) |> List.map(fun x -> x.Value)
            (this.TypeAssignments |> List.map(fun x -> x.Name.Value))@importedTypes
        | Asn1Ast.OnlySome(typesAndVars)    ->
            typesAndVars |> List.filter(fun x -> System.Char.IsUpper (x.Chars 0))
    member this.ExportedVars =
        match this.Exports with
        | Asn1Ast.All   -> this.ValueAssignments |> List.map(fun x -> x.Name.Value)
        | Asn1Ast.OnlySome(typesAndVars)    ->
            typesAndVars |> List.filter(fun x -> not (System.Char.IsUpper (x.Chars 0)))



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
    member this.TypeAssignments = this.Modules |> List.collect(fun m -> m.TypeAssignments)

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
        | Asn1AcnAst.ObjOrRelObjIdValue (a,b)   -> ObjOrRelObjIdValue (Asn1DefinedObjectIdentifierValue(a,b))
        | Asn1AcnAst.TimeValue        r  -> TimeValue r.Value
    {Asn1Value.kind = newVKind; id=v.id; loc = v.loc}


let emitComponent (c:ResolvedObjectIdentifierValueCompoent) =
    match c with
    | ResObjInteger            nVal             -> (nVal.Value, None)
    | ResObjNamedDefValue      (label,_,nVal)   -> (nVal, Some label.Value)
    | ResObjNamedIntValue      (label,nVal)   -> (nVal.Value, Some label.Value)
    | ResObjRegisteredKeyword  (label,nVal)   -> (nVal, Some label.Value)
    | ResObjDefinedValue       (_,_,nVal)     -> (nVal, None)

type ObjectIdenfierValue with
    member this.Values =
        match this with
        | InternalObjectIdentifierValue intList      -> intList |> List.map(fun i -> (i, None))
        | Asn1DefinedObjectIdentifierValue (resolvedComponents, _)  ->
            resolvedComponents |> List.map emitComponent

type Asn1Value with
    member this.getBackendName (l:ProgrammingLanguage) =
        match this.id with
        | ReferenceToValue (typePath,(VA2 vasName)::[]) -> ToC vasName
        | ReferenceToValue (typePath, vasPath)      -> 
            let longName = (typePath.Tail |> List.map (fun i -> i.StrValue))@ (vasPath |> List.map (fun i -> i.StrValue))  |> Seq.StrJoin "_"
            ToC2(longName.Replace("#","elem").L1)

    member this.BaseValue =
        match this.kind with
        | RefValue(_, bv)   -> bv
        | _                 -> this
    member this.ActualValue =
        match this.kind with
        | RefValue(_, bv)   -> bv.ActualValue
        | _                 -> this

type Asn1ValueKind with
    member this.BaseValue =
        match this with
        | RefValue(_, bv)   -> bv.kind
        | _                 -> this
    member this.ActualValue =
        match this with
        | RefValue(_, bv)   -> bv.kind.ActualValue
        | _                 -> this

type SeqChildInfo with
    member this.acnInsertetField =
        match this with
        | Asn1Child _    -> false
        | AcnChild _     -> true

    member this.Name =
        match this with
        | Asn1Child x    -> x.Name.Value
        | AcnChild x     -> x.Name.Value
    member this.getBackendName l =
        match this with 
        | AcnChild z    -> z.c_name
        | Asn1Child z   -> z.getBackendName l
    member this.savePosition =
        match this with 
        | AcnChild z -> 
            match z.Type with
            | Asn1AcnAst.AcnNullType nt when nt.acnProperties.savePosition   ->  true
            | _     -> false
        | Asn1Child z ->
            match z.Type.Kind with
            | NullType nt when nt.baseInfo.acnProperties.savePosition         -> true
            | _                     -> false
    member this.Optionality =
        match this with
        | Asn1Child x    -> x.Optionality
        | AcnChild x     -> None
    member this.acnMaxSizeInBits =
        match this with
        | Asn1Child x    -> x.Type.acnMaxSizeInBits
        | AcnChild x     -> x.Type.acnMaxSizeInBits
    member this.acnMinSizeInBits =
        match this with
        | Asn1Child x    -> x.Type.acnMinSizeInBits
        | AcnChild x     -> x.Type.acnMinSizeInBits
    member this.Comments =
        match this with
        | Asn1Child x    -> x.Comments
        | AcnChild x     -> [||]

let hasAcnEncodeFunction (encFunc : AcnFunction option) acnParameters  =
    match encFunc with
    | None  -> false
    | Some fnc ->
        match acnParameters with
        | [] ->
            let p = {CallerScope.modName = ""; arg = VALUE "dummy"}
            let ret,_ = fnc.funcBody emptyState [] p
            match ret with
            | None   -> false
            | Some _ -> true
        | _     -> false
                
let hasUperEncodeFunction (encFunc : UPerFunction option)  =
    match encFunc with
    | None  -> false
    | Some fnc ->
            let p = {CallerScope.modName = ""; arg = VALUE "dummy"}
            match fnc.funcBody p with
            | None   -> false
            | Some _ -> true

let hasXerEncodeFunction (encFunc : XerFunction option)  =
    match encFunc with
    | None  -> false
    | Some fnc ->
            let p = {CallerScope.modName = ""; arg = VALUE "dummy"}
            let errCode = {ErroCode.errCodeName = "DUMMY_ERR"; errCodeValue=0}
            match fnc.funcBody_e errCode p None  with
            | None   -> false
            | Some _ -> true


let AdaUses (r:AstRoot) =
    seq {
        for f in r.Files do
            for m in f.Modules do
                for tas in m.TypeAssignments do
                    yield sprintf "%s:%s" tas.Name.Value (ToC m.Name.Value);
    } |> Seq.iter(fun l -> System.Console.WriteLine l)

let rec GetMySelfAndChildren (t:Asn1Type) = 
    seq {
        match t.Kind with
        | SequenceOf(conType) ->  yield! GetMySelfAndChildren conType.childType
        | Sequence seq ->
            for ch in seq.Asn1Children do 
                yield! GetMySelfAndChildren ch.Type
        | Choice(ch)-> 
            for ch in ch.children do 
                yield! GetMySelfAndChildren ch.chType
        |_ -> ()    
        yield t
    } |> Seq.toList


let rec GetMySelfAndChildren2 l (t:Asn1Type) (p:CallerScope)= 
    seq {
        match t.Kind with
        | SequenceOf(conType) ->  
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = "0" //sprintf "i%d" ii
            yield! GetMySelfAndChildren2 l conType.childType ({p with arg = p.arg.getArrayItem l i conType.childType.isIA5String})
        | Sequence seq ->
            for ch in seq.Asn1Children do 
                yield! GetMySelfAndChildren2 l ch.Type ({p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String})
        | Choice(ch)-> 
            for ch in ch.children do 
                yield! GetMySelfAndChildren2 l ch.chType ({p with arg = p.arg.getChChild l (ch.getBackendName l) ch.chType.isIA5String})
        |_ -> ()    
        yield (t,p)
    } |> Seq.toList


let getFuncNameGeneric (typeDefinition:TypeDefintionOrReference) nameSuffix  =
    match typeDefinition with
    | ReferenceToExistingDefinition  refEx  -> None
    | TypeDefinition   td                   -> Some (td.typedefName + nameSuffix)

let getFuncNameGeneric2 (typeDefinition:TypeDefintionOrReference) =
    match typeDefinition with
    | ReferenceToExistingDefinition  refEx  -> None
    | TypeDefinition   td                   -> Some (td.typedefName)




let nestItems (l:ProgrammingLanguage) (retVarName:string (*ret or result*)) children = 
    let joinItems2 =  
        match l with  
        | C -> equal_c.JoinItems2  
        | Ada  when retVarName = "result"    -> uper_a.JoinItems2
        | Ada  -> isvalid_a.JoinTwoIfFirstOk

    let printChild (content:string) (soNestedContent:string option) = 
        match soNestedContent with
        | None                -> content
        | Some sNestedContent -> joinItems2 content sNestedContent
    let rec printChildren children : Option<string> = 
        match children with
        |[]     -> None
        |x::xs  -> Some (printChild x (printChildren xs))
    printChildren children
#if false
let nestItems_dbg  children = 
    let joinItems2 =  sprintf """
    %s
    if (ret) {
        %s
    }
    """

    let printChild (content:string) (soNestedContent:string option) = 
        match soNestedContent with
        | None                -> content
        | Some sNestedContent -> joinItems2 content sNestedContent
    let rec printChildren children : Option<string> = 
        match children with
        |[]     -> None
        |x::xs  -> Some (printChild x (printChildren xs))
    printChildren children
#endif