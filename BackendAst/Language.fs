module Language
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open AbstractMacros

[<AbstractClass>]
type ILangGeneric () =
    abstract member getPointer      : FuncParamType -> string;
    abstract member getValue        : FuncParamType -> string;
    abstract member getAcces        : FuncParamType -> string;
    abstract member getStar         : FuncParamType -> string;
    abstract member getAmber        : FuncParamType -> string;
    abstract member toPointer       : FuncParamType -> FuncParamType;
    abstract member getArrayItem    : FuncParamType -> (string) -> (bool) -> FuncParamType;
    abstract member intValueToSting : BigInteger -> bool -> string;
    abstract member getNamedItemBackendName  :TypeDefintionOrReference option -> Asn1AcnAst.NamedItem -> string

    abstract member getAsn1ChildBackendName0  : Asn1AcnAst.Asn1Child -> string
    abstract member getAsn1ChChildBackendName0: Asn1AcnAst.ChChildInfo -> string

    abstract member getAsn1ChildBackendName  : Asn1Child -> string
    abstract member getAsn1ChChildBackendName: ChChildInfo -> string

    abstract member Length          : string -> string -> string
    abstract member typeDef         : Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition> -> FE_PrimitiveTypeDefinition
    abstract member getTypeDefinition : Map<ProgrammingLanguage, FE_TypeDefinition> -> FE_TypeDefinition
    abstract member getSeqChild     : FuncParamType -> string -> bool -> FuncParamType;
    abstract member getChChild      : FuncParamType -> string -> bool -> FuncParamType;
    abstract member getLocalVariableDeclaration : LocalVariable -> string;
    abstract member getLongTypedefName : TypeDefintionOrReference -> string;
    abstract member ArrayAccess     : string -> string;

    abstract member presentWhenName : TypeDefintionOrReference option -> ChChildInfo -> string;
    abstract member getParamTypeSuffix : Asn1AcnAst.Asn1Type -> string -> Codec -> CallerScope;
    abstract member getParamValue   : Asn1AcnAst.Asn1Type -> FuncParamType -> Codec -> string

    abstract member getParamType    : Asn1AcnAst.Asn1Type -> Codec -> CallerScope;
    abstract member rtlModuleName   : string
    abstract member hasModules      : bool
    abstract member AssignOperator  : string
    abstract member TrueLiteral     : string

    default this.getAmber (fpt:FuncParamType) =
        if this.getStar fpt = "*" then "&" else ""        
    default this.toPointer  (fpt:FuncParamType) =
        POINTER (this.getPointer fpt)
    default this.getParamType    (t:Asn1AcnAst.Asn1Type) (c:Codec) : CallerScope =
        this.getParamTypeSuffix t "" c


type LanguageMacros = {
    lg      : ILangGeneric;
    equal   : IEqual;
    typeDef : ITypeDefinition;
    isvalid : IIsValid
    vars    : IVariables
}


type LangGeneric_c() =
    inherit ILangGeneric()
        override _.intValueToSting (i:BigInteger) isUnsigned =
            match isUnsigned with
            | true   -> sprintf "%sUL" (i.ToString())
            | false  -> sprintf "%sLL" (i.ToString())

        override _.getPointer  (fpt:FuncParamType) =
            match fpt with
            |VALUE x        -> sprintf "(&(%s))" x
            |POINTER x      -> x
            |FIXARRAY x     -> x

        override this.getValue  (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> x
            | POINTER x      -> sprintf "(*(%s))" x
            | FIXARRAY x     -> x

        override this.getAcces  (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> "."
            | POINTER x      -> "->"
            | FIXARRAY x     -> ""

        override this.ArrayAccess idx = "[" + idx + "]"


        override this.getStar  (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> ""
            | POINTER x      -> "*"
            | FIXARRAY x     -> ""
        override this.getArrayItem (fpt:FuncParamType) (idx:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s%sarr[%s]" fpt.p (this.getAcces fpt) idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.getNamedItemBackendName (defOrRef:TypeDefintionOrReference option) (nm:Asn1AcnAst.NamedItem) = 
            ToC nm.c_name


        override this.Length exp sAcc =
            isvalid_c.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[C]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[C]


        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._c_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._c_name
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._c_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._c_name


        override this.rtlModuleName  = ""
        override this.AssignOperator = "="
        override this.TrueLiteral = "TRUE"
        override this.hasModules = false
        override this.getSeqChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s%s%s" fpt.p (this.getAcces fpt) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.getChChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) : FuncParamType =
            let newPath = sprintf "%s%su.%s" fpt.p (this.getAcces fpt) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
            

        override this.presentWhenName (defOrRef:TypeDefintionOrReference option) (ch:ChChildInfo) : string =
            (ToC ch._present_when_name_private) + "_PRESENT"
        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CallerScope =
            match c with
            | Encode  ->
                match t.Kind with
                | Asn1AcnAst.Integer         _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.Real            _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.IA5String       _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.NumericString   _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.OctetString     _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.NullType        _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.BitString       _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Boolean         _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.Enumerated      _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.SequenceOf      _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Sequence        _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Choice          _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ObjectIdentifier _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.TimeType _         -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ReferenceType r -> 
                    this.getParamTypeSuffix r.resolvedType suf c
            | Decode  ->
                match t.Kind with
                | Asn1AcnAst.Integer            _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Real               _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.IA5String          _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.NumericString      _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("val" + suf) }
                | Asn1AcnAst.OctetString        _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.NullType           _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.BitString          _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Boolean            _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Enumerated         _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.SequenceOf         _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Sequence           _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Choice             _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ObjectIdentifier _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.TimeType _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.ReferenceType r -> this.getParamTypeSuffix r.resolvedType suf c
        
        override this.getParamValue  (t:Asn1AcnAst.Asn1Type) (p:FuncParamType)  (c:Codec) =
            match c with
            | Encode  ->
                match t.Kind with
                | Asn1AcnAst.Integer      _ -> this.getPointer p
                | Asn1AcnAst.Real         _ -> this.getPointer p
                | Asn1AcnAst.IA5String    _ -> this.getValue   p //FIXARRAY "val"
                | Asn1AcnAst.NumericString _-> this.getValue   p// FIXARRAY "val"
                | Asn1AcnAst.OctetString  _ -> this.getPointer p
                | Asn1AcnAst.NullType     _ -> this.getPointer p
                | Asn1AcnAst.BitString    _ -> this.getPointer p
                | Asn1AcnAst.Boolean      _ -> this.getPointer p
                | Asn1AcnAst.Enumerated   _ -> this.getPointer p
                | Asn1AcnAst.SequenceOf   _ -> this.getPointer p
                | Asn1AcnAst.Sequence     _ -> this.getPointer p
                | Asn1AcnAst.Choice       _ -> this.getPointer p
                | Asn1AcnAst.ObjectIdentifier _ -> this.getPointer p
                | Asn1AcnAst.TimeType _ -> this.getPointer p
                | Asn1AcnAst.ReferenceType r -> this.getParamValue r.resolvedType p  c
            | Decode  ->
                match t.Kind with
                | Asn1AcnAst.IA5String    _  -> this.getValue p //FIXARRAY "val"
                | Asn1AcnAst.NumericString _ -> this.getValue p// FIXARRAY "val"
                | Asn1AcnAst.ReferenceType r -> this.getParamValue r.resolvedType p  c
                | _                          -> this.getPointer p

        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "int i%d;" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "int i%d=%d;" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "int %s;" name
            | IntegerLocalVariable (name,Some iv)       -> sprintf "int %s=%d;" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "asn1SccSint %s;" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "asn1SccSint %s=%d;" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "asn1SccUint %s;" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "asn1SccUint %s=%d;" name iv
            | FlagLocalVariable (name,None)             -> sprintf "flag %s;" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "flag %s=%d;" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "flag %s;" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "flag %s=%s;" name (if iv then "TRUE" else "FALSE")
            | AcnInsertedChild(name, vartype)           -> sprintf "%s %s;" vartype name
            | GenericLocalVariable lv                   ->
                sprintf "%s%s %s%s;" (if lv.isStatic then "static " else "") lv.varType lv.name (if lv.arrSize.IsNone then "" else "["+lv.arrSize.Value+"]")

            
        override this.getLongTypedefName (tdr:TypeDefintionOrReference) : string =
            match tdr with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref -> ref.typedefName
            

type LangGeneric_a() =
    inherit ILangGeneric()
        override this.rtlModuleName  = "adaasn1rtl."
        override this.AssignOperator = ":="
        override this.TrueLiteral = "True"
        override this.hasModules = true

        override _.intValueToSting (i:BigInteger) _ = i.ToString()

        override _.getPointer  (fpt:FuncParamType) =
            match fpt with
            | VALUE x      -> x
            | POINTER x    -> x
            | FIXARRAY x   -> x

        override this.getValue  (fpt:FuncParamType) =
            match fpt with
            | VALUE x      -> x
            | POINTER x    -> x
            | FIXARRAY x   -> x
        override this.getAcces  (fpt:FuncParamType) =
            match fpt with
            | VALUE _      -> "."
            | POINTER _    -> "."
            | FIXARRAY _   -> "."
        override this.getStar  (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> ""
            | POINTER x      -> ""
            | FIXARRAY x     -> ""
        override this.getArrayItem (fpt:FuncParamType) (idx:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s.Data(%s)" fpt.p idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.ArrayAccess idx = "(" + idx + ")"

        override this.getNamedItemBackendName (defOrRef:TypeDefintionOrReference option) (nm:Asn1AcnAst.NamedItem) = 
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + nm.ada_name
            | Some (TypeDefinition td) when td.baseType.IsSome && td.baseType.Value.programUnit.IsSome  -> td.baseType.Value.programUnit.Value + "." + nm.ada_name
            | _       -> ToC nm.ada_name
        override this.Length exp sAcc =
            isvalid_a.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[Ada]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[Ada]
        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._ada_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._ada_name
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._ada_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._ada_name

        override this.getSeqChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s.%s" fpt.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.getChChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) : FuncParamType =
            let newPath = sprintf "%s.%s" fpt.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)


        override this.presentWhenName (defOrRef:TypeDefintionOrReference option) (ch:ChChildInfo) : string =
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + ((ToC ch._present_when_name_private) + "_PRESENT")
            | _       -> (ToC ch._present_when_name_private) + "_PRESENT"
        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CallerScope =
            {CallerScope.modName = t.id.ModName; arg= VALUE ("val" + suf) }

        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "i%d:Integer;" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "i%d:Integer:=%d;" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "%s:Integer;" name
            | IntegerLocalVariable (name,Some iv)       -> sprintf "%s:Integer:=%d;" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1Int;" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1Int:=%d;" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1UInt;" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1UInt:=%d;" name iv
            | FlagLocalVariable (name,None)             -> sprintf "%s:adaasn1rtl.BIT;" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "%s:adaasn1rtl.BIT:=%d;" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "%s:Boolean;" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "%s:Boolean:=%s;" name (if iv then "True" else "False")
            | AcnInsertedChild(name, vartype)         -> sprintf "%s:%s;" name vartype
            | GenericLocalVariable lv                   ->
                match lv.initExp with
                | Some initExp  -> sprintf "%s : %s := %s;" lv.name lv.varType  initExp
                | None          -> sprintf "%s : %s;" lv.name lv.varType  

        override this.getLongTypedefName (tdr:TypeDefintionOrReference) : string =
            match tdr with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref ->
                match ref.programUnit with
                | Some pu -> pu + "." + ref.typedefName
                | None    -> ref.typedefName

        override this.getParamValue  (t:Asn1AcnAst.Asn1Type) (p:FuncParamType)  (c:Codec) =
            p.p

(*
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
        


*)