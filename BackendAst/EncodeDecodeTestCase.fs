module EncodeDecodeTestCase
open System
open System.Numerics
open System.Globalization
open System.IO

open FsUtils
open CommonTypes
open Asn1AcnAst
open Asn1Fold
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language



let getFuncName (r:Asn1AcnAst.AstRoot)  (sEncoding:string) (typeId:ReferenceToType) (td:FE_TypeDefinition)=
    match typeId.tasInfo with
    | None -> None
    | Some _ -> Some (td.typeName + "_" + sEncoding + "enc_dec")



type StatementKind =
    //|Update_DecIn     of AcnTypes.AcnParameter
    |Encode_input
    |Decode_output
    |Validate_output
    |Compare_input_output
    |Write_bitstream_to_file


let OptFlatMap fun1 u =
    match u with
    | None  -> None
    | Some uu ->
       match uu with
       | None   -> None
       | Some uuu -> fun1 uuu

let rec getAmberDecode (t:Asn1AcnAst.Asn1Type) =
    match t.Kind with
    | Asn1AcnAst.IA5String    _ -> ""
    | Asn1AcnAst.NumericString _ -> ""
    | Asn1AcnAst.ReferenceType z -> getAmberDecode z.resolvedType
    | _                          -> "&"

let _createUperEncDecFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionOrReference) (eqFunc:EqualFunction) (isValidFunc: IsValidFunction option) (encFunc : UPerFunction option) (decFunc : UPerFunction option)   (us:State)  =
    let sEnc = lm.lg.atc.uperPrefix

    let funcName            = getFuncName r  sEnc t.id (lm.lg.getTypeDefinition t.FT_TypeDefinition)
    let modName = ToC t.id.AcnAbsPath.Head

    let printCodec_body         = lm.atc.PrintCodec_body
    let printCodec_body_header  = lm.atc.PrintCodec_spec
    let joinItems               = lm.atc.JoinItems
    let encode                  = lm.atc.Codec_Encode
    let decode                  = lm.atc.Codec_Decode
    let validateOutput          = lm.atc.Codec_validate_output
    let compareInputWithOutput  = lm.atc.Codec_compare_input_with_output
    let write_bitstreamToFile   = lm.atc.Codec_write_bitstreamToFile

    let p   = lm.lg.getParamType t Encode //  t.getParamType l Encode
    let varName = p.arg.rootId
    let sStar = lm.lg.getStar p.arg //p.arg.getStar l
    let sAmberDecode = getAmberDecode t
    let sAmberIsValid = getAmberDecode t

    match funcName  with
    | None              -> None, us
    | Some funcName     ->

        let printStatement stm sNestedContent =
            let content=
                match stm with
                |Encode_input           -> option {
                                                let! encF = encFunc
                                                let! encFunName = encF.funcName
                                                return encode modName encFunName varName
                                           }
                |Decode_output          -> option {
                                                let! decF = decFunc
                                                let! decFunName = decF.funcName
                                                return decode modName decFunName (typeDefinition.longTypedefName2 lm.lg.hasModules) sEnc sAmberDecode
                                           }

                |Validate_output        ->
                                           option {
                                                let! f = isValidFunc
                                                let! fname = f.funcName
                                                return validateOutput modName fname sAmberIsValid
                                           }
                |Compare_input_output   ->
                                           option {
                                                let! fname = eqFunc.isEqualFuncName
                                                return compareInputWithOutput modName fname varName sAmberIsValid
                                           }
                |Write_bitstream_to_file -> option {
                                                return write_bitstreamToFile ()
                                            }
            joinItems (content.orElse "") sNestedContent

        match hasUperEncodeFunction encFunc with
        | true  ->
            let sNestedStatements =
                let rec printStatements statements : string option =
                    match statements with
                    |[]     -> None
                    |x::xs  ->
                        match printStatements xs with
                        | None                 -> Some (printStatement x  None)
                        | Some childrenCont    -> Some (printStatement x  (Some childrenCont))
                printStatements [Encode_input; Decode_output; Validate_output; Compare_input_output; Write_bitstream_to_file]

            let func =
                printCodec_body modName funcName (typeDefinition.longTypedefName2 lm.lg.hasModules) sStar varName "" (sNestedStatements.orElse "") "UPER"
            let funcDef = printCodec_body_header funcName  modName (typeDefinition.longTypedefName2 lm.lg.hasModules) sStar varName
            let ret =
                {
                    EncodeDecodeTestFunc.funcName   = funcName
                    func                            = func
                    funcDef                         = funcDef
                }
            Some ret, us
        | false -> None, us

let createUperEncDecFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionOrReference) (eqFunc:EqualFunction) (isValidFunc: IsValidFunction option) (encFunc : UPerFunction option) (decFunc : UPerFunction option)   (us:State)  =
    match r.args.generateAutomaticTestCases with
    | true  -> _createUperEncDecFunction r lm t typeDefinition eqFunc isValidFunc encFunc decFunc  us
    | false -> None, us


let _createAcnEncDecFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionOrReference) (eqFunc:EqualFunction) (isValidFunc: IsValidFunction option) (encFunc : AcnFunction option) (decFunc : AcnFunction option)   (us:State)  =
    //let sEnc = match ProgrammingLanguage.ActiveLanguages.Head with | Scala -> "ACN" | _ -> lm.lg.atc.acnPrefix
    let sEnc = lm.lg.atc.acnPrefix

    let funcName            = getFuncName r sEnc t.id (lm.lg.getTypeDefinition t.FT_TypeDefinition)
    let modName             = ToC t.id.AcnAbsPath.Head

    let printCodec_body             = lm.atc.PrintCodec_body
    let printCodec_body_header      = lm.atc.PrintCodec_spec
    let joinItems                   = lm.atc.JoinItems
    let encode                      = lm.atc.Codec_Encode
    let decode                      = lm.atc.Codec_Decode
    let validateOutput              = lm.atc.Codec_validate_output
    let compareInputWithOutput      = lm.atc.Codec_compare_input_with_output
    let write_bitstreamToFile       = lm.atc.Codec_write_bitstreamToFile

    let p  = lm.lg.getParamType t Encode
    let varName = p.arg.rootId
    let sStar = lm.lg.getStar p.arg
    let sAmberDecode = getAmberDecode t
    let sAmberIsValid = getAmberDecode t

    match hasAcnEncodeFunction encFunc t.acnParameters t.id.tasInfo with
    | false -> None, us
    | true  ->
        match funcName  with
        | None              -> None, us
        | Some funcName     ->
            let printStatement stm sNestedContent =
                let content=
                    match stm with
                    |Encode_input           -> option {
                                                    let! encF = encFunc
                                                    let! encFunName = encF.funcName
                                                    return encode modName encFunName varName
                                               }
                    |Decode_output          -> option {
                                                    let! decF = decFunc
                                                    let! decFunName = decF.funcName
                                                    return decode modName decFunName (typeDefinition.longTypedefName2 lm.lg.hasModules) sEnc sAmberDecode
                                               }

                    |Validate_output        ->
                                               option {
                                                    let! f = isValidFunc
                                                    let! fname = f.funcName
                                                    return validateOutput modName fname sAmberIsValid
                                               }
                    |Compare_input_output   ->
                                               option {
                                                    let! fname = eqFunc.isEqualFuncName
                                                    return compareInputWithOutput modName fname varName sAmberIsValid
                                               }
                    |Write_bitstream_to_file -> option {
                                                    return write_bitstreamToFile ()
                                                }
                joinItems (content.orElse "") sNestedContent

            match hasAcnEncodeFunction encFunc t.acnParameters t.id.tasInfo with
            | true  ->
                let sNestedStatements =
                    let rec printStatements statements : string option =
                        match statements with
                        |[]     -> None
                        |x::xs  ->
                            match printStatements xs with
                            | None                 -> Some (printStatement x  None)
                            | Some childrenCont    -> Some (printStatement x  (Some childrenCont))

                    printStatements [Encode_input; Decode_output; Validate_output; Compare_input_output; Write_bitstream_to_file]

                let func = printCodec_body modName funcName (typeDefinition.longTypedefName2 lm.lg.hasModules) sStar varName sEnc (sNestedStatements.orElse "") "ACN"
                let funcDef = printCodec_body_header funcName modName (typeDefinition.longTypedefName2 lm.lg.hasModules) sStar varName

                let ret =
                    {
                        EncodeDecodeTestFunc.funcName   = funcName
                        func                            = func
                        funcDef                         = funcDef
                    }
                Some ret, us
            | false -> None, us


let createAcnEncDecFunction (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionOrReference) (eqFunc:EqualFunction) (isValidFunc: IsValidFunction option) (encFunc : AcnFunction option) (decFunc : AcnFunction option)   (us:State)  =
    match r.args.generateAutomaticTestCases with
    | true  -> _createAcnEncDecFunction r lm t typeDefinition eqFunc isValidFunc encFunc decFunc  us
    | false -> None, us

let _createXerEncDecFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionOrReference) (eqFunc:EqualFunction) (isValidFunc: IsValidFunction option) (encFunc : XerFunctionRec) (decFunc : XerFunctionRec)   (us:State)  =
    let sEnc = lm.lg.atc.xerPrefix
    let funcName            = getFuncName r sEnc t.id (lm.lg.getTypeDefinition t.FT_TypeDefinition)
    let modName = ToC t.id.AcnAbsPath.Head

    let printCodec_body                 = lm.atc.PrintCodec_body_XER
    let printCodec_body_header          = lm.atc.PrintCodec_spec
    let joinItems                       = lm.atc.JoinItems
    let write_bitstreamToFile           = lm.atc.Codec_write_CharstreamToFile
    let encode                          = lm.atc.Codec_Encode
    let decode                          = lm.atc.Codec_Decode_XER
    let validateOutput                  = lm.atc.Codec_validate_output
    let compareInputWithOutput          = lm.atc.Codec_compare_input_with_output

    let p   = lm.lg.getParamType t Encode
    let varName = p.arg.rootId
    let sStar = lm.lg.getStar p.arg
    let sAmberDecode = getAmberDecode t
    let sAmberIsValid = getAmberDecode t

    match funcName  with
    | None              -> None, us
    | Some funcName     ->

        let printStatement stm sNestedContent =
            let content=
                match stm with
                |Encode_input           -> option {
                                                let encF = encFunc
                                                let! encFunName = encF.funcName
                                                return encode modName encFunName varName
                                           }
                |Decode_output          -> option {
                                                let decF = decFunc
                                                let! decFunName = decF.funcName
                                                return decode modName decFunName (typeDefinition.longTypedefName2 lm.lg.hasModules) sEnc sAmberDecode
                                           }

                |Validate_output        ->
                                           option {
                                                let! f = isValidFunc
                                                let! fname = f.funcName
                                                return validateOutput modName fname sAmberIsValid
                                           }
                |Compare_input_output   ->
                                           option {
                                                let! fname = eqFunc.isEqualFuncName
                                                return compareInputWithOutput modName fname varName sAmberIsValid
                                           }
                |Write_bitstream_to_file -> option {
                                                    return write_bitstreamToFile ()
                                                }
            joinItems (content.orElse "") sNestedContent

        let sNestedStatements =
            let rec printStatements statements : string option =
                match statements with
                |[]     -> None
                |x::xs  ->
                    match printStatements xs with
                    | None                 -> Some (printStatement x  None)
                    | Some childrenCont    -> Some (printStatement x  (Some childrenCont))
            printStatements [Encode_input; Decode_output; Validate_output; Compare_input_output; Write_bitstream_to_file]

        let func = printCodec_body modName funcName (typeDefinition.longTypedefName2 lm.lg.hasModules) sStar varName sEnc (sNestedStatements.orElse "")
        let funcDef = printCodec_body_header funcName  modName (typeDefinition.longTypedefName2 lm.lg.hasModules) sStar varName
        let ret =
            {
                EncodeDecodeTestFunc.funcName   = funcName
                func                            = func
                funcDef                         = funcDef
            }
        Some ret, us


let createXerEncDecFunction (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionOrReference) (eqFunc:EqualFunction) (isValidFunc: IsValidFunction option) (encFunc : XerFunction) (decFunc : XerFunction)   (us:State)  =
    match r.args.generateAutomaticTestCases with
    | true  ->
        match encFunc, decFunc with
        | XerFunction encFunc, XerFunction decFunc ->
            _createXerEncDecFunction r lm t typeDefinition eqFunc isValidFunc encFunc decFunc  us
        | XerFunctionDummy, XerFunctionDummy -> None, us
        | _         -> raise (BugErrorException "createXerEncDecFunction")
    | false -> None, us

(*
Automatic Test case values

*)




let foldGenericCon  (c:GenericConstraint<'v>)  =
    foldGenericConstraint
        (fun _ e1 e2 b s      -> e1@e2, s)
        (fun _ e1 e2 s        -> e1@e2, s)
        (fun _ e s            -> [], s)
        (fun _ e1 e2 s        -> e1, s)
        (fun _ e s            -> e, s)
        (fun _ e1 e2 s        -> e1@e2, s)
        (fun _ v  s           -> [v] ,s)
        c
        0 |> fst



let foldRangeCon  getNext getPrev min max zero (c:RangeTypeConstraint<'v1,'v1>)  =
    foldRangeTypeConstraint
        (fun _ e1 e2 b s      -> e1@e2, s)    //union
        (fun _ e1 e2 s        -> e1@e2, s)    //Intersection
        (fun _ e s            -> [], s)       //AllExcept
        (fun _ e1 e2 s        -> e1, s)       //ExceptConstraint
        (fun _ e s            -> e, s)        //RootConstraint
        (fun _ e1 e2 s        -> e1@e2, s)    //RootConstraint2
        (fun _ v  s         -> [v] ,s)        // SingleValueConstraint
        (fun _ v1 v2  minIsIn maxIsIn s   ->  //RangeConstraint
            [(if minIsIn then v1 else (getNext v1));(if maxIsIn then v2 else (getPrev v2))], s)
        (fun _ v1 minIsIn s   -> [(if minIsIn then v1 else (getNext v1)); max], s) //Constraint_val_MAX
        (fun _ v2 maxIsIn s   -> [min; (if maxIsIn then v2 else (getPrev v2))], s) //Constraint_MIN_val
        c
        0 |> fst

let foldSizableConstraint  (c:SizableTypeConstraint<'v>) =
    foldSizableTypeConstraint2
        (fun _ e1 e2 b s      -> e1@e2, s)
        (fun _ e1 e2 s        -> e1@e2, s)
        (fun _ e s            -> [], s)
        (fun _ e1 e2 s        -> e1, s)
        (fun _ e s            -> e, s)
        (fun _ e1 e2 s        -> e1@e2, s)
        (fun _ v  s           -> [v] ,s)
        (fun _ intCon s       -> [], s)
        c
        0 |> fst



let IntegerAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) =
    let orig = o.AllCons
    let allCons = DastValidate2.getIntSimplifiedConstraints r o.isUnsigned o.AllCons
    let min = r.args.IntMin o.isUnsigned
    let max = r.args.IntMax o.isUnsigned
    let getNext a = match a < max with true -> a + 1I | false -> max
    let getPrev a = match a > min with true -> a - 1I | false -> min
    match allCons with
    | []    -> [min; 0I; max] |> Seq.distinct |> Seq.toList
    | _     ->
        let ret = allCons |> List.collect (foldRangeCon  getNext getPrev min max 0I ) |> Seq.distinct |> Seq.toList
        let aaa = ret |> List.filter (isValidValueRanged allCons)
        aaa

let RealAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) =
    let allCons = o.AllCons
    let min, max =
        match r.args.floatingPointSizeInBytes = 4I with
        | true  -> (double Single.MinValue/10.0), (double Single.MaxValue/10.0)
        | false ->
            match o.acnEncodingClass with
            | Real_uPER                           -> Double.MinValue, Double.MaxValue
            | Real_IEEE754_32_big_endian          -> (double Single.MinValue), (double Single.MaxValue)
            | Real_IEEE754_64_big_endian          -> Double.MinValue, Double.MaxValue
            | Real_IEEE754_32_little_endian       -> (double Single.MinValue), (double Single.MaxValue)
            | Real_IEEE754_64_little_endian       -> Double.MinValue, Double.MaxValue


    match allCons with
    | []    -> [min; 0.0; max]
    | _     ->
        let ret = allCons |> List.collect (foldRangeCon id id min max 0.0 ) |> Seq.distinct |> Seq.toList
        let aaa = ret |> List.filter (isValidValueRanged allCons)
        aaa

let EnumeratedAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) =
    let allItems = o.items |> List.map(fun z -> z.Name.Value)
    match o.AllCons with
    | [] -> allItems
    | _  -> allItems |> List.filter (isValidValueGeneric o.AllCons (=))

let EnumeratedAutomaticTestCaseValues2 (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) =
    let allItems = o.items
    match o.AllCons with
    | [] -> allItems
    | _  -> allItems |> List.filter (isValidValueGeneric o.AllCons (fun a b -> a = b.Name.Value))

let BooleanAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) =
    let allItems = [true; false]
    match o.AllCons with
    | [] -> allItems
    | _  -> allItems |> List.filter (isValidValueGeneric o.AllCons (=))


let ObjectIdentifierAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) =
    let sv = o.AllCons |> List.map(fun c -> foldGenericCon  c ) |> List.collect id
    match sv with
    | []    -> [[0I; 1I]; [0I .. r.args.objectIdentifierMaxLength - 1I]]
    | _     -> sv |> List.map (fun (resLis,_) -> resLis |> List.map(fun c -> DAstUtilFunctions.emitComponent c |> fst))


let TimeTypeAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) =
    let sv = o.AllCons |> List.map(fun c -> foldGenericCon  c ) |> List.collect id
    match sv with
    | []    ->
        let tv = {Asn1TimeValue.hours = 0I; mins=0I; secs=0I; secsFraction = None}
        let tz = {Asn1TimeZoneValue.sign = 1I; hours = 2I; mins=0I}
        let dt = {Asn1DateValue.years = 2019I; months = 12I; days = 25I}
        match o.timeClass with
        |Asn1LocalTime                      (fr) -> [Asn1LocalTimeValue (tv)]
        |Asn1UtcTime                        (fr) -> [Asn1UtcTimeValue (tv)]
        |Asn1LocalTimeWithTimeZone          (fr) -> [Asn1LocalTimeWithTimeZoneValue (tv,tz)]
        |Asn1Date                                -> [Asn1DateValue dt]
        |Asn1Date_LocalTime                 (fr) -> [Asn1Date_LocalTimeValue (dt,tv)]
        |Asn1Date_UtcTime                   (fr) -> [Asn1Date_UtcTimeValue(dt,tv)]
        |Asn1Date_LocalTimeWithTimeZone     (fr) -> [Asn1Date_LocalTimeWithTimeZoneValue(dt,tv,tz)]

    | _     -> sv |> List.filter (isValidValueGeneric o.AllCons (=))


let StringAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) =
    let maxItems = 32767I
    match o.minSize.uper > maxItems with
    | true  -> []   // the generated string will be very large
    | false ->
        match o.uperCharSet |> Seq.filter(fun c -> not (System.Char.IsControl c)) |> Seq.toList with
        | chr::_    ->
            let s1 = System.String(chr, int o.minSize.uper)
            match o.minSize.uper = o.maxSize.uper || o.maxSize.uper > maxItems with
            | true  -> [s1]
            | false ->
                let s2 = System.String(chr, int o.maxSize.uper)
                [s1;s2]
        | []        -> []

let OctetStringAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) =
    let valsFromSingleValueConstraints = o.AllCons |> List.collect (foldSizableConstraint ) |> Seq.toList |> List.map(fun (z,_) -> z |> List.map(fun a -> a.Value)) |> Seq.distinct |> Seq.toList
    let maxItems = 70000I
    match valsFromSingleValueConstraints with
    | []    ->
        match o.minSize.uper > maxItems with
        | true  -> []   // the generated string will be very large
        | false ->
            let s1 = [1 .. int o.minSize.uper] |> List.map (fun i -> 0uy)
            match o.minSize.uper = o.maxSize.uper  || o.maxSize.uper > maxItems with
            | true  -> [s1]
            | false ->
                let s2 = [1 .. int o.maxSize.uper] |> List.map (fun i -> 0uy)
                [s1;s2]
    | _     -> valsFromSingleValueConstraints

let BitStringAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) =
    let valsFromSingleValueConstraints = o.AllCons |> List.collect (foldSizableConstraint ) |> Seq.toList |> List.map(fun (z,_) -> z.Value) |> Seq.distinct |> Seq.toList
    let maxItems = 70000I
    match valsFromSingleValueConstraints with
    | []    ->
        match o.minSize.uper > maxItems with
        | true  -> []   // the generated string will be very large
        | false ->
            let s1 = System.String('0', int o.minSize.uper)
            match o.minSize.uper = o.maxSize.uper  || o.maxSize.uper > maxItems with
            | true  -> [s1]
            | false ->
                let s2 = System.String('0', int o.maxSize.uper)
                [s1;s2]
    | _     -> valsFromSingleValueConstraints
