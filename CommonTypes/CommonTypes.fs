﻿module CommonTypes
open FsUtils
open System
open System.Numerics

let c_keyworkds =  [ "auto"; "break"; "case"; "char"; "const"; "continue"; "default"; "do"; "double"; "else"; "enum"; "extern"; "float"; "for"; "goto"; "if"; "int"; "long"; "register"; "return"; "short"; "signed"; "sizeof"; "static"; "struct"; "switch"; "typedef"; "union"; "unsigned"; "void"; "volatile"; "while"; ]
let ada_keyworkds =  [ "abort"; "else"; "new"; "return"; "abs"; "elsif"; "not"; "reverse"; "abstract"; "end"; "null"; "accept"; "entry"; "select"; "access"; "exception"; "of"; "separate"; "aliased"; "exit"; "or"; "some"; "all"; "others"; "subtype"; "and"; "for"; "out"; "synchronized"; "array"; "function"; "overriding"; "at"; "tagged"; "generic"; "package"; "task"; "begin"; "goto"; "pragma"; "terminate"; "body"; "private"; "then"; "if"; "procedure"; "type"; "case"; "in"; "protected"; "constant"; "interface"; "until"; "is"; "raise"; "use"; "declare"; "range"; "delay"; "limited"; "record"; "when"; "delta"; "loop"; "rem"; "while"; "digits"; "renames"; "with"; "do"; "mod"; "requeue"; "xor" ]
let py_keywords = ["False"; "None"; "True"; "and"; "as"; "assert"; "break"; "class"; "continue"; "def"; "del"; "elif"; "else"; "except"; "finally"; "for"; "from"; "global"; "if"; "import"; "in"; "is"; "lambda"; "nonlocal"; "not"; "or"; "pass"; "raise"; "return"; "try"; "while"; "with"; "yield"]

type ProgrammingLanguage =
    |C
    |Ada
    |Spark
    |Python
    |Html
    |Unknown
    with 
        member l.cmp (s1:string) (s2:string) =
            match l with
            |C          -> s1 = s2
            |Ada
            |Spark      -> s1.icompare s2
            |Python     -> s1 = s2
            |Html       -> s1 = s2
            |Unknown    -> s1 = s2
        member l.DefinitionsFileExt = 
            match l with
            |C          -> ".h"
            |Ada
            |Spark      -> ".ads"
            |Python     -> ".py"
            |Html       -> ""
            |Unknown    -> ""
        member l.keywords = 
            match l with
            |C          -> c_keyworkds
            |Ada
            |Spark      -> ada_keyworkds
            |Python     -> py_keywords
            |Html       -> []
            |Unknown    -> []

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


type CommandLineSettings = {
    asn1Files : string list
    acnFiles  : string list
    encodings: Asn1Encoding list
    GenerateEqualFunctions : bool
    generateAutomaticTestCases : bool
    TypePrefix:string
    CheckWithOss:bool
    AstXmlAbsFileName:string
    IcdUperHtmlFileName:string
    IcdAcnHtmlFileName:string
    mappingFunctionsModule : string option
    integerSizeInBytes : int            //currently only the value of 8 bytes (64 bits) is supported
    renamePolicy :  EnumRenamePolicy
    
}
with 
  member this.SIntMax =
    match this.integerSizeInBytes with
    | 8     -> BigInteger(Int64.MaxValue)
    | 4     -> BigInteger(Int32.MaxValue)
    | _     -> BigInteger.Pow(2I, this.integerSizeInBytes*8 - 1) - 1I
  member this.SIntMin =
    match this.integerSizeInBytes with
    | 8     -> BigInteger(Int64.MinValue)
    | 4     -> BigInteger(Int32.MinValue)
    | _     -> -BigInteger.Pow(2I, this.integerSizeInBytes*8 - 1)
  member this.UIntMax =
    match this.integerSizeInBytes with
    | 8     -> BigInteger(UInt64.MaxValue)
    | 4     -> BigInteger(UInt32.MaxValue)
    | _     -> BigInteger.Pow(2I, this.integerSizeInBytes*8) - 1I
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
