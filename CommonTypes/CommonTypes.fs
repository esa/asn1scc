module CommonTypes
open FsUtils
open System
open System.Numerics
open System.IO

let c_keyworkds =  [ "auto"; "break"; "case"; "char"; "const"; "continue"; "default"; "do"; "double"; "else"; "enum"; "extern"; "float"; "for"; "goto"; "if"; "int"; "long"; "register"; "return"; "short"; "signed"; "sizeof"; "static"; "struct"; "switch"; "typedef"; "union"; "unsigned"; "void"; "volatile"; "while"; ]
let ada_keyworkds =  [ "abort"; "else"; "new"; "return"; "abs"; "elsif"; "not"; "reverse"; "abstract"; "end"; "null"; "accept"; "entry"; "select"; "access"; "exception"; "of"; "separate"; "aliased"; "exit"; "or"; "some"; "all"; "others"; "subtype"; "and"; "for"; "out"; "synchronized"; "array"; "function"; "overriding"; "at"; "tagged"; "generic"; "package"; "task"; "begin"; "goto"; "pragma"; "terminate"; "body"; "private"; "then"; "if"; "procedure"; "type"; "case"; "in"; "protected"; "constant"; "interface"; "until"; "is"; "raise"; "use"; "declare"; "range"; "delay"; "limited"; "record"; "when"; "delta"; "loop"; "rem"; "while"; "digits"; "renames"; "with"; "do"; "mod"; "requeue"; "xor" ]

type ProgrammingLanguage =
    |C
    |Ada
    with 
        member l.cmp (s1:string) (s2:string) =
            match l with
            |C          -> s1 = s2
            |Ada        -> s1.icompare s2
        member l.DefinitionsFileExt = 
            match l with
            |C          -> ".h"
            |Ada        -> ".ads"
        member l.keywords = 
            match l with
            |C          -> c_keyworkds
            |Ada        -> ada_keyworkds

type Codec =
    |Encode
    |Decode
 with
    member this.suffix =
        match this with
        | Encode    -> "_Encode"
        | Decode    -> "_Decode"


type Asn1Encoding =
    | UPER
    | ACN
    | BER
    | XER


type EnumRenamePolicy =
    | NoRenamePolicy
    | SelectiveEnumerants
    | AllEnumerants


type Input = {
    name : string
    contents : Stream
}

type FieldPrefix =
    | FieldPrefixAuto   
    | FieldPrefixUserValue  of string

type CommandLineSettings = {
    asn1Files : Input list
    acnFiles  : Input list
    encodings: Asn1Encoding list
    GenerateEqualFunctions : bool
    generateAutomaticTestCases : bool
    TypePrefix:string
    CheckWithOss:bool
    AstXmlAbsFileName:string
    IcdUperHtmlFileName:string
    IcdAcnHtmlFileName:string
    custom_Stg_Ast_Version : int
    mappingFunctionsModule : string option
    integerSizeInBytes : BigInteger            //currently only the value of 8 bytes (64 bits) is supported
    renamePolicy :  EnumRenamePolicy
    fieldPrefix  : FieldPrefix option
    targetLanguages : ProgrammingLanguage list
}
with 
  member this.SIntMax =
    match this.integerSizeInBytes with
    | _ when this.integerSizeInBytes = 8I     -> BigInteger(Int64.MaxValue)
    | _ when this.integerSizeInBytes = 4I     -> BigInteger(Int32.MaxValue)
    | _     -> BigInteger.Pow(2I, int (this.integerSizeInBytes*8I - 1I)) - 1I
  member this.SIntMin =
    match this.integerSizeInBytes with
    | _ when this.integerSizeInBytes = 8I     -> BigInteger(Int64.MinValue)
    | _ when this.integerSizeInBytes = 4I     -> BigInteger(Int32.MinValue)
    | _     -> -BigInteger.Pow(2I, int(this.integerSizeInBytes*8I - 1I))
  member this.UIntMax =
    match this.integerSizeInBytes with
    | _ when this.integerSizeInBytes = 8I     -> BigInteger(UInt64.MaxValue)
    | _ when this.integerSizeInBytes = 4I     -> BigInteger(UInt32.MaxValue)
    | _     -> BigInteger.Pow(2I, int (this.integerSizeInBytes*8I)) - 1I
  member this.IntMax (isUnsigned:bool) =
    match isUnsigned with
    | true          -> this.UIntMax
    | false         -> this.SIntMax
  member this.IntMin (isUnsigned:bool) =
    match isUnsigned with
    | true          -> 0I
    | false         -> this.SIntMin

type ScopeNode =
    | MD of string          //MODULE
    | TA of string          //TYPE ASSIGNMENT
    | VA of string          //VALUE ASSIGNMENT
    | SEQ_CHILD of string   //SEQUENCE child
    | CH_CHILD of string*string    //CHOICE child, choice child present when name
    | PRM of string         //ACN parameter
    | SQF                   //SEQUENCE OF CHILD

type ReferenceToType = 
    | ReferenceToType of ScopeNode list


type InheritanceInfo = {
    modName : string
    tasName : string
    hasAdditionalConstraints : bool //indicates that the new type has additional constraints e.g. BaseType(200..400) vs BaseType
}


type TypeAssignmentInfo = {
    modName : string
    tasName : string
}

type ValueAssignmentInfo = {
    modName : string
    vasName : string
}

type AssignmentInfo =
    | TypeAssignmentInfo    of TypeAssignmentInfo
    | ValueAssignmentInfo   of ValueAssignmentInfo


type InheritanceInfo with
    member this.AsTasInfo =
        {
            TypeAssignmentInfo.tasName = this.tasName
            modName = this.modName
        }



type ScopeNode with
    member this.AsString =
        match this with
        | MD strVal
        | TA strVal
        | VA strVal
        | PRM strVal
        | SEQ_CHILD strVal
        | CH_CHILD (strVal,_) -> strVal
        | SQF             -> "#"
    member this.StrValue = this.AsString


type VarScopNode =
    | VA2 of string      //VALUE ASSIGNMENT
    | DV        //DEFAULT VALUE
    | NI  of string      //NAMED ITEM VALUE (enum)
    | CON of int         // constraint index
    | SQOV of int             //SEQUENCE OF VALUE (value index)
    | SQCHILD   of string   //child value (SEQUENCE, CHOICE)
    | VL of int         //value index
    | IMG of int        //non ASN.1 value. Required when constructing values for types in backends
    with
        member this.StrValue =
            match this with
            | VA2 strVal -> strVal
            | DV        -> "DV"
            | NI    ni  -> ni
            | VL   idx  -> "v" + idx.ToString()    
            | IMG  idx  -> "img" + idx.ToString()    
            | CON idx   -> "c" + idx.ToString()
            | SQOV i     -> sprintf"[%d]" i
            | SQCHILD  s-> s

        override this.ToString() = this.StrValue


type ReferenceToValue = 
    | ReferenceToValue of (ScopeNode list)*(VarScopNode list)
    with
        member this.ModName =
            match this with
            | ReferenceToValue (path,_) -> 
                match path with
                | (MD modName)::_    -> modName
                | _                               -> raise(BugErrorException "Did not find module at the begining of the scope path")


type ReferenceToType with 
    member this.AsString =
        match this with
        | ReferenceToType sn -> sn |> Seq.map(fun x -> x.AsString) |> Seq.StrJoin "."
        member this.ToScopeNodeList = 
            match this with
            | ReferenceToType path -> path 
        member this.ModName =
            match this with
            | ReferenceToType path -> 
                match path with
                | (MD modName)::_    -> modName
                | _                               -> raise(BugErrorException "Did not find module at the begining of the scope path")
        member this.tasInfo =
            match this with
            | ReferenceToType path -> 
                match path with
                | (MD modName)::(TA tasName)::[]    -> Some ({TypeAssignmentInfo.modName = modName; tasName=tasName})
                | _                                 -> None
        member this.AcnAbsPath =
            match this with
            | ReferenceToType path -> path |> List.map (fun i -> i.StrValue) 
        member this.getSeqChildId (childName:string) =
            match this with
            | ReferenceToType path -> ReferenceToType (path@[SEQ_CHILD childName])
        member this.getSeqOfChildId =
            match this with
            | ReferenceToType path -> ReferenceToType (path@[SQF])

        member this.getChildId (childName:string) (presentWhenName:string)=
            match this with
            | ReferenceToType path -> ReferenceToType (path@[CH_CHILD (childName, presentWhenName)])
        member this.getParamId (paramName:string) =
            match this with
            | ReferenceToType ((MD mdName)::(TA tasName)::[]) -> ReferenceToType ((MD mdName)::(TA tasName)::[PRM paramName])
            | _                                                                         -> raise(BugErrorException "Cannot add parameter here. Only within TAS scope")

        member this.appendLongChildId (childRelativePath:string list) =
            match this with
            | ReferenceToType path -> 
                let newTail = 
                    childRelativePath |> 
                    List.map(fun s ->SEQ_CHILD s)
                ReferenceToType (path@newTail)
        member this.beginsWith (md:string) (ts:string)= 
            match this with
            | ReferenceToType((MD mdName)::(TA tasName)::[])   -> mdName = md && tasName = ts
            | _                                                                          -> false
        member this.lastItem =
            match this with
            | ReferenceToType path -> 
                match path |> List.rev |> List.head with
                | SEQ_CHILD name   -> name
                | CH_CHILD (name,_)    -> name
                | _                             -> raise (BugErrorException "error in lastitem")
        member this.parentTypeId =
            match this with
            | ReferenceToType path -> 
                let pathPar = path |> List.rev |> List.tail |> List.rev
                match pathPar with
                | [] 
                | _::[]     -> None
                | _         -> Some (ReferenceToType pathPar)
        member this.SeqeuenceOfLevel =
            match this with
            | ReferenceToType path -> path |> List.filter(fun n -> match n with SQF -> true | _ -> false) |> Seq.length
        static member createFromModAndTasName (modName : string) ((tasName : string))=
            ReferenceToType((MD modName)::(TA tasName)::[])





let rec foldMap func state lst =
    match lst with
    | []        -> [],state
    | h::tail   -> 
        let procItem, newState = func state h
        let restList, finalState = tail |> foldMap func newState
        procItem::restList, finalState



type FE_TypeDefinitionKindInternal =
    | FEI_NewTypeDefinition                       //type
    | FEI_NewSubTypeDefinition of ReferenceToType    //subtype
    | FEI_Reference2RTL
    | FEI_Reference2OtherType of ReferenceToType
    override this.ToString() = 
        match this with
        | FEI_NewTypeDefinition                       -> "NewTypeDefinition"
        | FEI_NewSubTypeDefinition subId              -> sprintf "NewSubTypeDefinition %s" subId.AsString
        | FEI_Reference2RTL                           -> "FE_Reference2RTL"
        | FEI_Reference2OtherType otherId             -> sprintf "FE_Reference2OtherType %s" otherId.AsString





type FE_PrimitiveTypeDefinitionKind =
    | PrimitiveNewTypeDefinition                       //type
    | PrimitiveNewSubTypeDefinition of FE_PrimitiveTypeDefinition    //subtype
    | PrimitiveReference2RTL
    | PrimitiveReference2OtherType 
    override this.ToString() = 
        match this with
        | PrimitiveNewTypeDefinition            -> "NewTypeDefinition"
        | PrimitiveNewSubTypeDefinition   sub   -> sprintf "NewSubTypeDefinition %s.%s" sub.programUnit sub.typeName
        | PrimitiveReference2RTL                -> "FE_Reference2RTL"
        | PrimitiveReference2OtherType          -> "FE_Reference2OtherType" 

and FE_PrimitiveTypeDefinition = {
    asn1Name        : string
    asn1Module      : string option
    typeName        : string            //e.g. MyInt, Asn1SccInt, Asn1SccUInt
    programUnit     : string            //the program unit where this type is defined
    kind            : FE_PrimitiveTypeDefinitionKind
}

type FE_NonPrimitiveTypeDefinitionKind<'SUBTYPE> =
    | NonPrimitiveNewTypeDefinition                       //type
    | NonPrimitiveNewSubTypeDefinition of 'SUBTYPE    //subtype
    | NonPrimitiveReference2OtherType 
    override this.ToString() = 
        match this with
        | NonPrimitiveNewTypeDefinition                       -> "NewTypeDefinition"
        | NonPrimitiveNewSubTypeDefinition subId              -> sprintf "NewSubTypeDefinition %s" (subId.ToString())
        | NonPrimitiveReference2OtherType                     -> "FE_Reference2OtherType"


type FE_StringTypeDefinition = {
    asn1Name        : string
    asn1Module      : string option
    programUnit     : string            //the program unit where this type is defined
    typeName        : string            //e.g. MyInt, Asn1SccInt, Asn1SccUInt
    encoding_range  : string
    index           : string
    alpha           : string
    alpha_set       : string
    alpha_index     : string
    kind            : FE_NonPrimitiveTypeDefinitionKind<FE_StringTypeDefinition>
}

type FE_SizeableTypeDefinition = {
    asn1Name        : string
    asn1Module      : string option
    programUnit     : string            //the program unit where this type is defined
    typeName        : string            //e.g. MyInt, Asn1SccInt, Asn1SccUInt
    index           : string
    array           : string
    length_index          : string
    kind            : FE_NonPrimitiveTypeDefinitionKind<FE_SizeableTypeDefinition>
}


type FE_SequenceTypeDefinition = {
    asn1Name        : string
    asn1Module      : string option
    programUnit     : string            //the program unit where this type is defined
    typeName        : string            //e.g. MyInt, Asn1SccInt, Asn1SccUInt
    exist           : string
    kind            : FE_NonPrimitiveTypeDefinitionKind<FE_SequenceTypeDefinition>
}

type FE_ChoiceTypeDefinition = {
    asn1Name        : string
    asn1Module      : string option
    programUnit     : string            //the program unit where this type is defined
    typeName        : string            //e.g. MyInt, Asn1SccInt, Asn1SccUInt
    index_range     : string
    selection       : string
    kind            : FE_NonPrimitiveTypeDefinitionKind<FE_ChoiceTypeDefinition>
}

type FE_EnumeratedTypeDefinition = {
    asn1Name        : string
    asn1Module      : string option
    programUnit     : string            //the program unit where this type is defined
    typeName        : string            //e.g. MyInt, Asn1SccInt, Asn1SccUInt
    index_range     : string
    kind            : FE_NonPrimitiveTypeDefinitionKind<FE_EnumeratedTypeDefinition>
}



type FE_TypeDefinition = 
    | FE_PrimitiveTypeDefinition   of FE_PrimitiveTypeDefinition
    | FE_SequenceTypeDefinition    of FE_SequenceTypeDefinition
    | FE_StringTypeDefinition      of FE_StringTypeDefinition
    | FE_SizeableTypeDefinition    of FE_SizeableTypeDefinition
    | FE_ChoiceTypeDefinition      of FE_ChoiceTypeDefinition
    | FE_EnumeratedTypeDefinition  of FE_EnumeratedTypeDefinition

    with 
        member this.typeName = 
            match this with
            | FE_PrimitiveTypeDefinition  a    -> a.typeName
            | FE_SequenceTypeDefinition   a    -> a.typeName
            | FE_StringTypeDefinition     a    -> a.typeName
            | FE_SizeableTypeDefinition   a    -> a.typeName
            | FE_ChoiceTypeDefinition     a    -> a.typeName
            | FE_EnumeratedTypeDefinition a    -> a.typeName
        member this.programUnit = 
            match this with
            | FE_PrimitiveTypeDefinition  a    -> a.programUnit
            | FE_SequenceTypeDefinition   a    -> a.programUnit
            | FE_StringTypeDefinition     a    -> a.programUnit
            | FE_SizeableTypeDefinition   a    -> a.programUnit
            | FE_ChoiceTypeDefinition     a    -> a.programUnit
            | FE_EnumeratedTypeDefinition a    -> a.programUnit
        member this.kind = 
            match this with
            | FE_PrimitiveTypeDefinition  a    -> a.kind.ToString()
            | FE_SequenceTypeDefinition   a    -> a.kind.ToString()
            | FE_StringTypeDefinition     a    -> a.kind.ToString()
            | FE_SizeableTypeDefinition   a    -> a.kind.ToString()
            | FE_ChoiceTypeDefinition     a    -> a.kind.ToString()
            | FE_EnumeratedTypeDefinition a    -> a.kind.ToString()

        member this.asn1Name = 
            match this with
            | FE_PrimitiveTypeDefinition  a    -> a.asn1Name
            | FE_SequenceTypeDefinition   a    -> a.asn1Name
            | FE_StringTypeDefinition     a    -> a.asn1Name
            | FE_SizeableTypeDefinition   a    -> a.asn1Name
            | FE_ChoiceTypeDefinition     a    -> a.asn1Name
            | FE_EnumeratedTypeDefinition a    -> a.asn1Name

        member this.asn1Module = 
            match this with
            | FE_PrimitiveTypeDefinition  a    -> a.asn1Module
            | FE_SequenceTypeDefinition   a    -> a.asn1Module
            | FE_StringTypeDefinition     a    -> a.asn1Module
            | FE_SizeableTypeDefinition   a    -> a.asn1Module
            | FE_ChoiceTypeDefinition     a    -> a.asn1Module
            | FE_EnumeratedTypeDefinition a    -> a.asn1Module

