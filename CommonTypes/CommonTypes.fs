module CommonTypes
open FsUtils
open System
open System.Globalization
open System.Numerics
open System.IO
open Antlr.Runtime.Tree
open Antlr.Runtime

let c_keyworkds =  [ "auto"; "break"; "case"; "char"; "const"; "continue"; "default"; "do"; "double"; "else"; "enum"; "extern"; "float"; "for"; "goto"; "if"; "int"; "long"; "register"; "return"; "short"; "signed"; "sizeof"; "static"; "struct"; "switch"; "typedef"; "union"; "unsigned"; "void"; "volatile"; "while"; ]
let ada_keyworkds =  [ "abort"; "else"; "new"; "return"; "abs"; "elsif"; "not"; "reverse"; "abstract"; "end"; "null"; "accept"; "entry"; "select"; "access"; "exception"; "of"; "separate"; "aliased"; "exit"; "or"; "some"; "all"; "others"; "subtype"; "and"; "for"; "out"; "synchronized"; "array"; "function"; "overriding"; "at"; "tagged"; "generic"; "package"; "task"; "begin"; "goto"; "pragma"; "terminate"; "body"; "private"; "then"; "if"; "procedure"; "type"; "case"; "in"; "protected"; "constant"; "interface"; "until"; "is"; "raise"; "use"; "declare"; "range"; "delay"; "limited"; "record"; "when"; "delta"; "loop"; "rem"; "while"; "digits"; "renames"; "with"; "do"; "mod"; "requeue"; "xor" ]


type UserErrorSeverity = 
    | ERROR
    | WARNING
    | INFO

type FuncParamType =
  | VALUE       of string
  | POINTER     of string
  | FIXARRAY    of string
  with
    member this.p  =
        match this with
        | VALUE x      -> x
        | POINTER x    -> x
        | FIXARRAY x   -> x


type UserError = {
    line : int
    charPosInline : int
    filePath : string
    msg : string
    fullMessage : string
    severity : UserErrorSeverity
}



type TimeTypeClass =
    |Asn1LocalTime                      of int
    |Asn1UtcTime                        of int
    |Asn1LocalTimeWithTimeZone          of int
    |Asn1Date
    |Asn1Date_LocalTime                 of int
    |Asn1Date_UtcTime                   of int
    |Asn1Date_LocalTimeWithTimeZone     of int

type Asn1DateValue = {
    years  : BigInteger
    months : BigInteger
    days   : BigInteger 
}

type Asn1TimeValue = {
    hours : BigInteger
    mins  : BigInteger
    secs  : BigInteger
    secsFraction : (BigInteger*BigInteger) option
}



type Asn1TimeZoneValue = {
    sign : BigInteger
    hours : BigInteger
    mins  : BigInteger
}

type Asn1DateTimeValue =
    |Asn1LocalTimeValue                     of Asn1TimeValue
    |Asn1UtcTimeValue                       of Asn1TimeValue
    |Asn1LocalTimeWithTimeZoneValue         of Asn1TimeValue*Asn1TimeZoneValue
    |Asn1DateValue                          of Asn1DateValue
    |Asn1Date_LocalTimeValue                of Asn1DateValue*Asn1TimeValue
    |Asn1Date_UtcTimeValue                  of Asn1DateValue*Asn1TimeValue
    |Asn1Date_LocalTimeWithTimeZoneValue    of Asn1DateValue*Asn1TimeValue*Asn1TimeZoneValue

type Asn1DateTimeValueLoc = PrimitiveWithLocation<Asn1DateTimeValue>


let timeTypeToAsn1Str tmcl = 
    match tmcl with
    |Asn1LocalTime                      _ -> "TIME"
    |Asn1UtcTime                        _ -> "TIME"
    |Asn1LocalTimeWithTimeZone          _ -> "TIME"
    |Asn1Date                             -> "TIME"
    |Asn1Date_LocalTime                 _ -> "TIME"
    |Asn1Date_UtcTime                   _ -> "TIME"
    |Asn1Date_LocalTimeWithTimeZone     _ -> "TIME"

let createTimeValueFromString timeClass (strL:StringLoc) =
    let pow b e = BigInteger.Pow (b,e)
    let str = strL.Value
    let pr (s:string) size mn mx = 
        if s.Length <> size then
            raise(SemanticError(strL.Location, "Invalid TIME VALUE"))
        match BigInteger.TryParse s with
        | true, num when num >= mn && num <=mx -> num
        | _         -> raise(SemanticError(strL.Location, "Invalid TIME VALUE"))
    let checkForInvalidCharacters (str:string) (charSet: Set<char> ) =
        let invalidChars = str.ToCharArray() |> Seq.filter (fun c -> not (charSet.Contains c)) |> Seq.toList
        match invalidChars with
        | []    -> ()
        | c::_  -> raise(SemanticError(strL.Location, (sprintf "Invalid character '%c'" c)))
    (*16:53:49.0123+02:00*)
    let parseTimeValue (str:string) = 
        let parseSeconds (secStr:string) =
            match secStr.Contains(".") with
            | false -> pr secStr 2 0I 59I, None
            | true  ->
                let p = secStr.Split('.')
                let s = pr p.[0] 2 0I 59I
                let fraction = 
                    match p.[1].Length with
                    | 0 -> None
                    | _ ->
                        let mx = (pow 10I p.[1].Length) - 1I
                        Some (pr p.[1] p.[1].Length 0I mx, mx)
                s,fraction

        let validChars = ['0'..'9']@[':';'.'] |> Set.ofList
        checkForInvalidCharacters str validChars
        let parts = str.Split(':')
        if parts.Length <> 3 then
            raise(SemanticError(strL.Location, "Invalid TIME VALUE"))
        let scs, fraction = parseSeconds parts.[2]
        {Asn1TimeValue.hours = pr parts.[0] 2 0I 23I; mins = pr parts.[1] 2 0I 59I; secs = scs; secsFraction = fraction}
    let parseUtcTimeValue (str:string) = 
        match str.EndsWith("Z") with
        | false -> raise(SemanticError(strL.Location, "Invalid TIME VALUE. Expecting a 'Z' at the end"))
        | true  ->                        parseTimeValue (str.Substring(0, str.Length-1))
    let parseDateValue (str:string) =
        (*expecting 2020-05-16*)
        let validChars = ['0'..'9']@['-'] |> Set.ofList
        checkForInvalidCharacters str validChars
        let parts = str.Split('-')
        {Asn1DateValue.years = pr parts.[0] parts.[0].Length 0I 9999999999I; months = pr parts.[1] 2 1I 12I; days = pr parts.[2] 2 1I 31I}

    let parseTimeValueWithTimeZone (str:string) =
        //let str = "2020-05-16T16:53:49+02:00"
        if str.Length <= 6 then
            raise(SemanticError(strL.Location, "Invalid TIME VALUE."))
        let tm = str.Substring(0, str.Length-6)
        let sign = str.Chars (str.Length-6)
        if not (sign = '-' || sign = '+') then
            raise(SemanticError(strL.Location, "Invalid TIME VALUE."))
        let tz = str.Substring(str.Length-5)
        if (tz.Length <> 5) then
            raise(SemanticError(strL.Location, "Invalid TIME VALUE."))
        let time = parseTimeValue tm 
        let parts =tz.Split(':')
        let tz = {Asn1TimeZoneValue.sign = (if sign = '+' then 1I else (-1I)); hours = pr parts.[0] 2 0I 23I; mins = pr parts.[1] 2 0I 59I}
        (time, tz)
    let splitDateTimeString (str:string) fnc =
        let parts = str.Split('T')
        if parts.Length <> 2 then
            raise(SemanticError(strL.Location, "Invalid DATE TIME VALUE."))
        (parseDateValue parts.[0], fnc parts.[1])
    let ret = 
        match timeClass with
        |Asn1LocalTime               _   -> Asn1LocalTimeValue (parseTimeValue str)
        |Asn1UtcTime                 _   -> Asn1UtcTimeValue  (parseUtcTimeValue str)
        |Asn1LocalTimeWithTimeZone   _   -> Asn1LocalTimeWithTimeZoneValue (parseTimeValueWithTimeZone str)
        |Asn1Date                       -> Asn1DateValue (parseDateValue str)
        |Asn1Date_LocalTime          _   -> Asn1Date_LocalTimeValue(splitDateTimeString str  parseTimeValue)
        |Asn1Date_UtcTime            _   -> Asn1Date_UtcTimeValue(splitDateTimeString str  parseUtcTimeValue)
        |Asn1Date_LocalTimeWithTimeZone _ -> 
            let (a,(b,c)) = splitDateTimeString str  parseTimeValueWithTimeZone
            Asn1Date_LocalTimeWithTimeZoneValue (a,b,c)
    {Asn1DateTimeValueLoc.Value = ret; Location = strL.Location}

(*
       myDate   "2020-05-16",
       myTime   "16:53:49",
       myTimeZ  "16:53:49Z",
       myTimeOfDayLD "16:53:49+02:00",
       myTimeFrac   "16:53:49.123",
       myDateTime  "2020-05-16T16:53:49",
       myDateTimeZ   "2020-05-16T16:53:49Z",
       myDateTimeLD   "2020-05-16T16:53:49+02:00"
*)
let asn1DateTimeValueToString (tv:Asn1DateTimeValue) =
    let asn1TimeValueToString (tv:Asn1TimeValue) =
        match tv.secsFraction with
        | None          -> sprintf "%A:%A:%A" tv.hours tv.mins tv.secs
        | Some (f,_)    -> sprintf "%A:%A:%A.%A" tv.hours tv.mins tv.secs f
    let timeZoneToString (tz:Asn1TimeZoneValue) =
        sprintf "%c%A:%A" (if tz.sign = 1I then '+' else '-') tz.hours tz.mins
    let asn1DateValueToString (dt:Asn1DateValue) =
        sprintf "%A-%A-%A" dt.years dt.months dt.days
    match tv with
    |Asn1LocalTimeValue                     tv         -> asn1TimeValueToString tv
    |Asn1UtcTimeValue                       tv         -> (asn1TimeValueToString tv) + "Z"
    |Asn1LocalTimeWithTimeZoneValue        (tv,tz)     -> (asn1TimeValueToString tv) + (timeZoneToString tz)
    |Asn1DateValue                          dt         -> asn1DateValueToString dt
    |Asn1Date_LocalTimeValue                (dt,tv)    -> (asn1DateValueToString dt) + (asn1TimeValueToString tv)
    |Asn1Date_UtcTimeValue                  (dt,tv)    -> (asn1DateValueToString dt) + (asn1TimeValueToString tv) + "Z"
    |Asn1Date_LocalTimeWithTimeZoneValue    (dt,tv, tz)-> (asn1DateValueToString dt) + (asn1TimeValueToString tv) + (timeZoneToString tz)


let someTests () =
(*
    |Asn1Date
    |Asn1Date_LocalTime
    |Asn1Date_UtcTime
    |Asn1Date_LocalTimeWithTimeZone

*)
    let loc str = { StringLoc.Value = str; Location = {srcFilename = "a.asn1"; srcLine = 20; charPos = 5    }}
    let a1 = createTimeValueFromString  (Asn1LocalTime 3) (loc "23:53:49.234")
    let a1 = createTimeValueFromString  (Asn1UtcTime 0) (loc "23:53:49Z")
    let a1 = createTimeValueFromString  (Asn1LocalTimeWithTimeZone 3) (loc "23:53:49.234+02:00")
    let a1 = createTimeValueFromString  Asn1Date  (loc "2020-05-16")
    let a1 = createTimeValueFromString  (Asn1Date_LocalTime 2) (loc "2020-05-16T16:53:49.21")
    let a1 = createTimeValueFromString  (Asn1Date_UtcTime 2) (loc "2020-05-16T16:53:59.21Z")
    let a1 = createTimeValueFromString  (Asn1Date_LocalTimeWithTimeZone 2) (loc "2020-05-16T16:53:59.21-11:30")
    0

(*
let getDateTimeFromAsn1TimeStringValue timeClass (str:StringLoc) =
    try
        let dt = 
            match timeClass with
            |Asn1LocalTime                  _ -> DateTime.ParseExact(str.Value, "HH:mm:ss.FFF", CultureInfo.InvariantCulture) 
            |Asn1UtcTime                    _ -> DateTime.Parse(str.Value) (*.ToUniversalTime ()*)
            |Asn1LocalTimeWithTimeZone      _ -> DateTime.ParseExact(str.Value, "HH:mm:ss.FFFK", CultureInfo.InvariantCulture) 
            |Asn1Date                       -> DateTime.ParseExact(str.Value, "yyyy-MM-dd", CultureInfo.InvariantCulture) 
            |Asn1Date_LocalTime             _ -> DateTime.ParseExact(str.Value, "yyyy-MM-dd'T'HH:mm:ss.FFF", CultureInfo.InvariantCulture) 
            |Asn1Date_UtcTime               _ -> DateTime.Parse(str.Value) (*.ToUniversalTime ()*)
            |Asn1Date_LocalTimeWithTimeZone _ -> DateTime.ParseExact(str.Value, "yyyy-MM-dd'T'HH:mm:ss.FFFK", CultureInfo.InvariantCulture) 
        {DateTimeLoc.Value = dt; Location = str.Location}
    with
        | :? System.FormatException as e -> raise(SemanticError(str.Location, "Invalid TIME VALUE"))


*)

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


type ObjectIdentifierValueCompoent =
    | ObjInteger            of IntLoc                               //integer form
    | ObjNamedDefValue      of StringLoc*(StringLoc*StringLoc)      //named form, points to an integer value
    | ObjNamedIntValue      of StringLoc*IntLoc                     //name form
    | ObjRegisteredKeyword  of StringLoc*BigInteger
    | ObjDefinedValue       of StringLoc*StringLoc                  //value assignment to Integer value or ObjectIdentifier or RelativeObject

type ResolvedObjectIdentifierValueCompoent =
    | ResObjInteger            of IntLoc                               //integer form
    | ResObjNamedDefValue      of StringLoc*(StringLoc*StringLoc)*BigInteger      //named form, int VAS, int value
    | ResObjNamedIntValue      of StringLoc*IntLoc                     //name form
    | ResObjRegisteredKeyword  of StringLoc*BigInteger
    | ResObjDefinedValue       of StringLoc*StringLoc*BigInteger        //BigInteger value originates from int value assignment or ObjectIdentifier or RelativeObject value assignment


type Asn1Encoding =
    | UPER
    | ACN
    | BER
    | XER


type EnumRenamePolicy =
    | NoRenamePolicy
    | SelectiveEnumerants
    | AllEnumerants
    | AlwaysPrefixTypeName

[<NoEquality; NoComparison>]
type SIZE = {
        uper    : BigInteger
        acn     : BigInteger
    }
    with
        override x.ToString() = 
            x.uper.ToString()


type Input = {
    name : string
    contents : Stream
}

type FieldPrefix =
    | FieldPrefixAuto   
    | FieldPrefixUserValue  of string

type Targets =
    | X86
    | Stm32
    | Msp430
    | AllBoards


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



(*
let rec foldMap func state lst =
    match lst with
    | []        -> [],state
    | h::tail   -> 
        let procItem, newState = func state h
        let restList, finalState = tail |> foldMap func newState
        procItem::restList, finalState

let foldMap = RemoveParamterizedTypes.foldMap
*)


let foldMap func state lst =
    let rec loop acc func state lst =
        match lst with
        | []        -> acc |> List.rev , state
        | h::tail   -> 
            let procItem, newState = func state h
            //let restList, finalState = tail |> loop func newState
            //procItem::restList, finalState
            loop (procItem::acc) func newState tail
    loop [] func state lst

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


type TypeDefinitionBaseKind =
    | NewTypeDefinition                       //type
    | NewSubTypeDefinition 
    | Reference2RTL
    | Reference2OtherType 



type FE_PrimitiveTypeDefinitionKind =
    | PrimitiveNewTypeDefinition                       //type
    | PrimitiveNewSubTypeDefinition of FE_PrimitiveTypeDefinition    //subtype
    | PrimitiveReference2RTL
    | PrimitiveReference2OtherType 
    member this.BaseKind =
        match this with
        | PrimitiveNewTypeDefinition            -> NewTypeDefinition
        | PrimitiveNewSubTypeDefinition   sub   -> NewTypeDefinition
        | PrimitiveReference2RTL                -> Reference2RTL
        | PrimitiveReference2OtherType          -> Reference2OtherType
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
    member this.BaseKind =
        match this with
        | NonPrimitiveNewTypeDefinition            -> NewTypeDefinition
        | NonPrimitiveNewSubTypeDefinition   sub   -> NewTypeDefinition
        | NonPrimitiveReference2OtherType          -> Reference2OtherType
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
with
    member this.longTypedefName l callerProgramUnit =
        this.longTypedefName2 (l=Ada) callerProgramUnit
    
    member this.longTypedefName2 bHasUnits callerProgramUnit =
        let z n = this.programUnit + "." + n
        match bHasUnits with
        | false             -> this
        | true   when this.programUnit = callerProgramUnit   -> this
        | true           -> {this with typeName = z this.typeName; encoding_range = z this.encoding_range; index = z this.index; alpha = z this.alpha; alpha_set = z this.alpha_set; alpha_index = z this.alpha_index}

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
with
    member this.longTypedefName l callerProgramUnit =
        this.longTypedefName2 (l=Ada) callerProgramUnit

    member this.longTypedefName2 bHasUnits callerProgramUnit =
        let z n = this.programUnit + "." + n
        match bHasUnits with
        | false             -> this
        | true   when this.programUnit = callerProgramUnit   -> this
        | true           -> {this with typeName = z this.typeName; index = z this.index; array = z this.array; length_index = z this.length_index}

type FE_SequenceTypeDefinition = {
    asn1Name        : string
    asn1Module      : string option
    programUnit     : string            //the program unit where this type is defined
    typeName        : string            //e.g. MyInt, Asn1SccInt, Asn1SccUInt
    exist           : string
    extention_function_potisions : string
    kind            : FE_NonPrimitiveTypeDefinitionKind<FE_SequenceTypeDefinition>
}
with
    member this.longTypedefName2 bHasUnits callerProgramUnit =
        let z n = this.programUnit + "." + n
        match bHasUnits with
        | false             -> this
        | true   when this.programUnit = callerProgramUnit   -> this
        | true   -> {this with typeName = z this.typeName; exist = z this.exist}
    member this.longTypedefName l callerProgramUnit =
        this.longTypedefName2 (l=Ada) callerProgramUnit

type FE_ChoiceTypeDefinition = {
    asn1Name        : string
    asn1Module      : string option
    programUnit     : string            //the program unit where this type is defined
    typeName        : string            //e.g. MyInt, Asn1SccInt, Asn1SccUInt
    index_range     : string
    selection       : string
    kind            : FE_NonPrimitiveTypeDefinitionKind<FE_ChoiceTypeDefinition>
}
with
    member this.longTypedefName2 bHasModules callerProgramUnit =
        let z n = this.programUnit + "." + n
        match bHasModules with
        | false             -> this
        | true   when this.programUnit = callerProgramUnit   -> this
        | true           -> {this with typeName = z this.typeName; index_range = z this.index_range; selection = z this.selection}
    
    member this.longTypedefName l callerProgramUnit =
        this.longTypedefName2 (l=Ada) callerProgramUnit

type FE_EnumeratedTypeDefinition = {
    asn1Name        : string
    asn1Module      : string option
    programUnit     : string            //the program unit where this type is defined
    typeName        : string            //e.g. MyInt, Asn1SccInt, Asn1SccUInt
    index_range     : string
    kind            : FE_NonPrimitiveTypeDefinitionKind<FE_EnumeratedTypeDefinition>
}
with
    member this.longTypedefName l callerProgramUnit =
        this.longTypedefName2 (l=Ada) callerProgramUnit

    member this.longTypedefName2 bHasUnits callerProgramUnit =
        let z n = this.programUnit + "." + n
        match bHasUnits with
        | false             -> this
        | true   when this.programUnit = callerProgramUnit   -> this
        | true           -> {this with typeName = z this.typeName; index_range = z this.index_range}


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
        member this.BaseKind =
            match this with
            | FE_PrimitiveTypeDefinition  a    -> a.kind.BaseKind
            | FE_SequenceTypeDefinition   a    -> a.kind.BaseKind
            | FE_StringTypeDefinition     a    -> a.kind.BaseKind
            | FE_SizeableTypeDefinition   a    -> a.kind.BaseKind
            | FE_ChoiceTypeDefinition     a    -> a.kind.BaseKind
            | FE_EnumeratedTypeDefinition a    -> a.kind.BaseKind


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


type AntlrParserResult = {
    rootItem    : ITree
    fileName    : string
    tokens      : IToken array
}


type ContainedInOctOrBitString =
    | ContainedInOctString
    | ContainedInBitString


type IntegerOrDefinedValue =
    | IDV_IntegerValue         of IntLoc                    //integer literal i.e. 5
    | IDV_DefinedValue   of (StringLoc*StringLoc)     // reference to an integer value assignment defined in another module


type Asn1SccOperationMode =
    | Asn1Compiler
    | LanguagerServer

type NamedBit0 = {
    Name:StringLoc
    _value : IntegerOrDefinedValue
    Comments: string array
}

type NamedBit1 = {
    Name:StringLoc
    resolvedValue : BigInteger
    _value : IntegerOrDefinedValue
    Comments: string array
}

type uperRange<'a> =
    | Concrete      of 'a*'a    // [a, b]
    | NegInf        of 'a       // (-inf, b]
    | PosInf        of 'a       // [a, +inf)
    | Full                      // (-inf, +inf)

let emptyTypeError l = raise(SemanticError(l, "The constraints defined for this type do not allow any value"))

let rec uperUnion r1 r2 =
    match r1,r2 with
    | (Full,_)                              -> Full
    | (PosInf(a), PosInf(b))                -> PosInf(min a b)
    | (PosInf(a), NegInf(b))                -> Full
    | (PosInf(a1), Concrete(a,b))           -> PosInf(min a1 a)
    | (NegInf(a), NegInf(b))                -> NegInf(max a b)
    | (NegInf(a), PosInf(b))                -> Full
    | (NegInf(a1), Concrete(a,b))           -> NegInf(max a1 b)
    | (Concrete(a1,b1), Concrete(a2,b2))    -> Concrete(min a1 a2, max b1 b2)
    | _                                     -> uperUnion r2 r1

let rec uperIntersection r1 r2 (l:SrcLoc) =
    match r1,r2 with
    | (Full,_)                      -> r2
    | (PosInf(a), PosInf(b))        -> PosInf(max a b)
    | (PosInf(a), NegInf(b))        -> if a<=b then Concrete(a,b) else emptyTypeError l
    | (PosInf(a1), Concrete(a,b))   -> if a1>b then emptyTypeError l
                                        elif a1<=a then r1 
                                        else Concrete(a1,b) 
    | (NegInf(a), NegInf(b))        -> NegInf(min a b)
    | (NegInf(a), PosInf(b))        -> if a>=b then Concrete(b,a) else emptyTypeError l
    | (NegInf(a1), Concrete(a,b))   -> if a1<a then emptyTypeError l
                                        elif a1<b then Concrete(a1,b)
                                        else r2
    | (Concrete(a1,b1), Concrete(a2,b2)) -> if a1<=a2 && a2<=b1 && b1<=b2 then Concrete(a2,b1)
                                            elif a2<=a1 && a1<=b2 && b2<=b1 then Concrete(a1, b2)
                                            elif a2<=a1 && b1<=b2 then r1
                                            elif a1<=a2 && b2<=b1 then r2
                                            else emptyTypeError l
    | _                             ->  uperIntersection r2 r1 l





[<AbstractClass>]
type IProgrammingLanguage () =
    abstract member indentation : sStatement:string -> string;

    


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
    integerSizeInBytes : BigInteger            //currently only the value of 4 or 8 bytes (32/64 bits) is supported
    floatingPointSizeInBytes : BigInteger       // 8 or 4
    slim : bool
    target : Targets option
    renamePolicy :  EnumRenamePolicy
    fieldPrefix  : FieldPrefix option
    targetLanguages : ProgrammingLanguage list
    objectIdentifierMaxLength : BigInteger
    streamingModeSupport      : bool
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
  member this.hasXer =
    this.encodings |> Seq.contains (XER)
  member this.hasAcn =
    this.encodings |> Seq.contains (ACN)
  member this.hasUper =
    this.encodings |> Seq.contains (UPER)
