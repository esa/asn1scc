﻿module EncodeDecodeTestCase
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

let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (sEncoding:string) (typeId:ReferenceToType) =
    typeId.tasInfo |> Option.map (fun x -> ToC2(r.args.TypePrefix + x.tasName + "_" + sEncoding + "enc_dec"))


type StatementKind =
    //|Update_DecIn     of AcnTypes.AcnParameter       
    |Encode_input
    |Decode_output
    |Validate_output
    |Compare_input_output


let OptFlatMap fun1 u =
    match u with
    | None  -> None
    | Some uu ->
       match uu with
       | None   -> None
       | Some uuu -> fun1 uuu

let rec getAmberIsValid (t:Asn1AcnAst.Asn1Type) = 
    match t.Kind with
    | Asn1AcnAst.Integer      _ ->  ""
    | Asn1AcnAst.Real         _ -> ""
    | Asn1AcnAst.IA5String    _ -> ""
    | Asn1AcnAst.NumericString _ -> ""
    | Asn1AcnAst.OctetString  _ -> "&"
    | Asn1AcnAst.NullType     _ -> ""
    | Asn1AcnAst.BitString    _ -> "&"
    | Asn1AcnAst.Boolean      _ -> ""
    | Asn1AcnAst.Enumerated   _ -> ""
    | Asn1AcnAst.SequenceOf   _ -> "&"
    | Asn1AcnAst.Sequence     _ -> "&"
    | Asn1AcnAst.Choice       _ -> "&"
    | Asn1AcnAst.ReferenceType z -> getAmberIsValid z.resolvedType

let rec getAmberDecode (t:Asn1AcnAst.Asn1Type) = 
    match t.Kind with
    | Asn1AcnAst.IA5String    _ -> ""
    | Asn1AcnAst.NumericString _ -> ""
    | Asn1AcnAst.ReferenceType z -> getAmberDecode z.resolvedType
    | _                          -> "&"

let createUperEncDecFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionCommon) (eqFunc:EqualFunction) (isValidFunc: IsValidFunction option) (encFunc : UPerFunction option) (decFunc : UPerFunction option)   (us:State)  =
    let sEnc = match l with C -> "" | Ada -> "UPER_" | Python -> ""
    let funcName            = getFuncName r l sEnc t.id
    let modName = ToC t.id.AcnAbsPath.Head

    let printCodec_body         = match l with C -> test_cases_c.PrintCodec_body        | Ada -> test_cases_a.PrintCodec_body   | Python -> test_cases_p.PrintCodec_body
    let printCodec_body_header  = match l with C -> test_cases_c.PrintCodec_body_header | Ada -> test_cases_a.PrintCodec_spec   | Python -> test_cases_p.PrintCodec_body_header
    let joinItems               = match l with C -> test_cases_c.JoinItems              | Ada -> test_cases_a.JoinItems         | Python -> test_cases_p.JoinItems

    let p : FuncParamType = t.getParamType l Encode
    let varName = p.p
    let sStar = p.getStar l
    let sAmberDecode = getAmberDecode t
    let sAmberIsValid = getAmberIsValid t
   
    match funcName  with
    | None              -> None, us
    | Some funcName     -> 
        
        let printStatement stm sNestedContent = 
            let encode = match l with C -> test_cases_c.Codec_Encode   | Ada -> test_cases_a.Codec_Encode | Python -> test_cases_p.Codec_Encode
            let decode = match l with C -> test_cases_c.Codec_Decode   | Ada -> test_cases_a.Codec_Decode | Python -> test_cases_p.Codec_Decode
            let validateOutput = match l with C -> test_cases_c.Codec_validate_output   | Ada -> test_cases_a.Codec_validate_output | Python -> test_cases_p.Codec_validate_output
            let compareInputWithOutput = match l with C -> test_cases_c.Codec_compare_input_with_output   | Ada -> test_cases_a.Codec_compare_input_with_output | Python -> test_cases_p.Codec_compare_input_with_output
            let content= 
                match stm with
                |Encode_input           -> option {
                                                let! encF = encFunc
                                                let! encFunName = match l with C | Ada -> encF.funcName | Python -> Some (sEnc.ToLower())
                                                return encode modName encFunName varName
                                           }
                |Decode_output          -> option {
                                                let! decF = decFunc
                                                let! decFunName = match l with C | Ada -> decF.funcName | Python -> Some (sEnc.ToLower())
                                                return decode modName decFunName typeDefinition.name sEnc sAmberDecode 
                                           }
                    
                |Validate_output        -> 
                                           option {
                                                let! f = isValidFunc
                                                let! fname = f.funcName
                                                return validateOutput modName fname sAmberIsValid
                                           }
                |Compare_input_output   -> 
                                           option {
                                                let! fname = match l with C | Ada -> eqFunc.isEqualFuncName | Python -> Some sEnc
                                                return compareInputWithOutput modName fname varName sAmberIsValid
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
                printStatements [Encode_input; Decode_output; Validate_output; Compare_input_output]

            let func = printCodec_body modName funcName typeDefinition.name sStar varName sEnc (sNestedStatements.orElse "")
            let funcDef = printCodec_body_header funcName  modName typeDefinition.name sStar varName
            let ret = 
                {
                    EncodeDecodeTestFunc.funcName   = funcName
                    func                            = func 
                    funcDef                         = funcDef
                }
            Some ret, us
        | false -> None, us

let createAcnEncDecFunction (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefinitionCommon) (eqFunc:EqualFunction) (isValidFunc: IsValidFunction option) (encFunc : AcnFunction option) (decFunc : AcnFunction option)   (us:State)  =
    let sEnc = "ACN_"

    let funcName            = getFuncName r l sEnc t.id
    let modName             = ToC t.id.AcnAbsPath.Head

    let printCodec_body         = match l with C -> test_cases_c.PrintCodec_body        | Ada -> test_cases_a.PrintCodec_body   | Python -> test_cases_p.PrintCodec_body
    let printCodec_body_header  = match l with C -> test_cases_c.PrintCodec_body_header | Ada -> test_cases_a.PrintCodec_spec   | Python -> test_cases_p.PrintCodec_body_header
    let joinItems               = match l with C -> test_cases_c.JoinItems              | Ada -> test_cases_a.JoinItems         | Python -> test_cases_p.JoinItems

    let p : FuncParamType = t.getParamType l Encode
    let varName = p.p
    let sStar = p.getStar l
    let sAmberDecode = getAmberDecode t
    let sAmberIsValid = getAmberIsValid t

    match hasAcnEncodeFunction encFunc t.acnParameters  with
    | false -> None, us
    | true  ->
        match funcName  with
        | None              -> None, us
        | Some funcName     -> 
            let printStatement stm sNestedContent = 
                let encode = match l with C -> test_cases_c.Codec_Encode   | Ada -> test_cases_a.Codec_Encode | Python -> test_cases_p.Codec_Encode
                let decode = match l with C -> test_cases_c.Codec_Decode   | Ada -> test_cases_a.Codec_Decode | Python -> test_cases_p.Codec_Decode
                let validateOutput = match l with C -> test_cases_c.Codec_validate_output   | Ada -> test_cases_a.Codec_validate_output | Python -> test_cases_p.Codec_validate_output
                let compareInputWithOutput = match l with C -> test_cases_c.Codec_compare_input_with_output   | Ada -> test_cases_a.Codec_compare_input_with_output | Python -> test_cases_p.Codec_compare_input_with_output
                let content= 
                    match stm with
                    |Encode_input           -> option {
                                                    let! encF = encFunc
                                                    let! encFunName = match l with C | Ada -> encF.funcName | Python -> Some (sEnc.ToLower())
                                                    return encode modName encFunName varName
                                               }
                    |Decode_output          -> option {
                                                    let! decF = decFunc
                                                    let! decFunName = match l with C | Ada -> decF.funcName | Python -> Some (sEnc.ToLower())
                                                    return decode modName decFunName typeDefinition.name sEnc sAmberDecode 
                                               }
                    
                    |Validate_output        -> 
                                               option {
                                                    let! f = isValidFunc
                                                    let! fname = f.funcName
                                                    return validateOutput modName fname sAmberIsValid
                                               }
                    |Compare_input_output   -> 
                                               option {
                                                    let! fname = match l with C | Ada -> eqFunc.isEqualFuncName | Python -> Some sEnc
                                                    return compareInputWithOutput modName fname varName sAmberIsValid
                                               }                
                joinItems (content.orElse "") sNestedContent

            match hasAcnEncodeFunction encFunc t.acnParameters with
            | true  -> 
                let sNestedStatements = 
                    let rec printStatements statements : string option = 
                        match statements with
                        |[]     -> None
                        |x::xs  -> 
                            match printStatements xs with
                            | None                 -> Some (printStatement x  None)
                            | Some childrenCont    -> Some (printStatement x  (Some childrenCont))

                    printStatements [Encode_input; Decode_output; Validate_output; Compare_input_output]

                let func = printCodec_body modName funcName typeDefinition.name sStar varName sEnc (sNestedStatements.orElse "")
                let funcDef = printCodec_body_header funcName modName typeDefinition.name sStar varName
        
                let ret = 
                    {
                        EncodeDecodeTestFunc.funcName   = funcName
                        func                            = func 
                        funcDef                         = funcDef
                    }
                Some ret, us
            | false -> None, us



(*
Automatic Test case values

*)




let foldGenericCon (l:ProgrammingLanguage) (c:GenericConstraint<'v>)  =
    foldGenericConstraint
        (fun e1 e2 b s      -> e1@e2, s)
        (fun e1 e2 s        -> e1@e2, s)
        (fun e s            -> [], s)
        (fun e1 e2 s        -> e1, s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> e1@e2, s)
        (fun v  s           -> [v] ,s)
        c
        0 |> fst 



let foldRangeCon  getNext getPrev min max zero (c:RangeTypeConstraint<'v1,'v1>)  =
    foldRangeTypeConstraint        
        (fun e1 e2 b s      -> e1@e2, s)    //union
        (fun e1 e2 s        -> e1@e2, s)    //Intersection
        (fun e s            -> [], s)       //AllExcept
        (fun e1 e2 s        -> e1, s)       //ExceptConstraint
        (fun e s            -> e, s)        //RootConstraint
        (fun e1 e2 s        -> e1@e2, s)    //RootConstraint2
        (fun v  s         -> [v] ,s)        // SingleValueConstraint
        (fun v1 v2  minIsIn maxIsIn s   ->  //RangeContraint
            [(if minIsIn then v1 else (getNext v1));(if maxIsIn then v2 else (getPrev v2))], s)
        (fun v1 minIsIn s   -> [(if minIsIn then v1 else (getNext v1)); max], s) //Contraint_val_MAX
        (fun v2 maxIsIn s   -> [min; (if maxIsIn then v2 else (getPrev v2))], s) //Contraint_MIN_val
        c
        0 |> fst 

let foldSizableConstraint  (c:SizableTypeConstraint<'v>) =
    foldSizableTypeConstraint2
        (fun e1 e2 b s      -> e1@e2, s)
        (fun e1 e2 s        -> e1@e2, s)
        (fun e s            -> [], s)
        (fun e1 e2 s        -> e1, s)
        (fun e s            -> e, s)
        (fun e1 e2 s        -> e1@e2, s)
        (fun v  s           -> [v] ,s)
        (fun intCon s       -> [], s)
        c
        0 |> fst



let IntegerAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) =
    let allCons = DAstValidate.getIntSimplifiedConstraints r o.isUnsigned o.AllCons
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
        match o.acnEncodingClass with
        | Real_uPER                           -> Double.MinValue, Double.MaxValue
        | Real_IEEE754_32_big_endian          -> (double Single.MinValue), (double Single.MaxValue)
        | Real_IEEE754_64_big_endian          -> Double.MinValue, Double.MaxValue
        | Real_IEEE754_32_little_endian       -> (double Single.MinValue), (double Single.MaxValue)
        | Real_IEEE754_64_little_endian       -> Double.MinValue, Double.MaxValue

    
    match allCons with
    | []    -> [min; 0.0; max] 
    | _     -> 
        allCons |> List.collect (foldRangeCon id id min max 0.0 ) |> Seq.distinct |> Seq.toList

let EnumeratedAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) =
    let allItems = o.items |> List.map(fun z -> z.Name.Value)
    match o.AllCons with
    | [] -> allItems
    | _  -> allItems |> List.filter (isValidValueGeneric o.AllCons (=))
    
let BooleanAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) =
    let allItems = [true; false]
    match o.AllCons with
    | [] -> allItems
    | _  -> allItems |> List.filter (isValidValueGeneric o.AllCons (=))
    

let maxItems = 1000
let StringAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) =
    match o.minSize > maxItems with
    | true  -> []   // the generated string will be very large
    | false ->  
        match o.uperCharSet |> Seq.filter(fun c -> not (System.Char.IsControl c)) |> Seq.toList with
        | chr::_    -> 
            let s1 = System.String(chr, o.minSize) 
            match o.minSize = o.maxSize  || o.maxSize > maxItems with
            | true  -> [s1] 
            | false ->
                let s2 = System.String(chr, o.maxSize) 
                [s1;s2]
        | []        -> []

let OctetStringAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) =
    let valsFromSingleValueConstraints = o.AllCons |> List.collect (foldSizableConstraint ) |> Seq.toList |> List.map(fun (z,_) -> z |> List.map(fun a -> a.Value)) |> Seq.distinct |> Seq.toList
    match valsFromSingleValueConstraints with
    | []    ->
        match o.minSize > maxItems with
        | true  -> []   // the generated string will be very large
        | false ->  
            let s1 = [1 .. o.minSize] |> List.map (fun i -> 0uy)
            match o.minSize = o.maxSize  || o.maxSize > maxItems with
            | true  -> [s1] 
            | false ->
                let s2 = [1 .. o.maxSize] |> List.map (fun i -> 0uy)
                [s1;s2]
    | _     -> valsFromSingleValueConstraints

let BitStringAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) =
    let valsFromSingleValueConstraints = o.AllCons |> List.collect (foldSizableConstraint ) |> Seq.toList |> List.map(fun (z,_) -> z.Value) |> Seq.distinct |> Seq.toList
    match valsFromSingleValueConstraints with
    | []    ->
        match o.minSize > maxItems with
        | true  -> []   // the generated string will be very large
        | false ->  
            let s1 = System.String('0', o.minSize)
            match o.minSize = o.maxSize  || o.maxSize > maxItems with
            | true  -> [s1] 
            | false ->
                let s2 = System.String('0', o.maxSize)
                [s1;s2]
    | _     -> valsFromSingleValueConstraints
let SequenceOfAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (childType:Asn1Type) =
    match o.minSize > maxItems with
    | true  -> []   // the generated string will be very large
    | false ->  
        let generateValue (childVal:Asn1Value) =
            let s1 = [1 .. o.minSize] |> List.map (fun i -> childVal)
            match o.minSize = o.maxSize  || o.maxSize > maxItems with
            | true  -> [s1] 
            | false ->
                let s2 = [1 .. o.maxSize] |> List.map (fun i -> childVal)
                [s1;s2]
        childType.automaticTestCasesValues |> List.collect generateValue
(*
let rec permutation (a:int list list) =
    match a with
    | []    -> [[]]
    | a1::xs ->
        let rest = permutation xs
        seq {
            for i1 in a1 do
                for subList in rest do
                    yield i1::subList
        } |> Seq.toList
*)

let SequenceAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (children:SeqChildInfo list) =
    let asn1Children = 
        children |> 
        List.choose(fun c -> match c with Asn1Child x -> Some x | _ -> None) |> 
        List.filter(fun z -> match z.Type.Kind with NullType _ -> false | _ -> true) |>
        List.filter(fun z -> match z.Optionality with Some Asn1AcnAst.AlwaysAbsent -> false | _ -> true)

    let HandleChild (ch:Asn1Child) =
        let childValues = ch.Type.automaticTestCasesValues
        match childValues with
        | []  -> None
        | x1::_ -> Some ({NamedValue.name = ch.Name.Value; Value = x1})
    let caseAllChildren = asn1Children |> List.choose HandleChild
    let caseAllMandatoyrChildren = 
        asn1Children |> 
        List.filter(fun z -> 
            match z.Optionality with 
            | None  -> true
            | Some Asn1AcnAst.AlwaysPresent -> true
            | _             -> false) |>
        List.choose HandleChild
    [caseAllChildren; caseAllMandatoyrChildren]
    (*

    //let allChildren = 
    let rec generateCases (children : Asn1Child list) =
        match children with
        | []        -> [[]]
        | x1::xs    -> 
            // generate this component test cases (x1) and the rest and the join them.
            // However, if this component (x1) is optional with present-when conditions then no test case
            // is generated for this component because we might generated wrong test cases 
            let optChildCount = 
                children |> 
                List.filter(fun c -> 
                    match c.Optionality with
                    | Some (Asn1AcnAst.Optional opt) when opt.acnPresentWhen.IsSome -> true
                    | _                                                             -> false
                ) |> Seq.length 
            let rest = generateCases xs
            seq {
                match x1.Optionality with
                | Some (Asn1AcnAst.Optional opt) when optChildCount > 1 && opt.acnPresentWhen.IsSome  ->  yield! rest       // do not generate test case for this item
                | _                               -> 
                    
                    let ths = x1.Type.automaticTestCasesValues |> List.map(fun v -> {NamedValue.name = x1.Name.Value; Value = v}) 
                    for i1 in ths do    
                        for lst in rest do
                            yield i1::lst
            } |> Seq.mapi(fun i x -> (i,x)) |> Seq.takeWhile(fun (i,x) -> i<10) |> Seq.map snd |> Seq.toList
    let ret = generateCases asn1Children
    ret
    *)
let ChoiceAutomaticTestCaseValues (r:Asn1AcnAst.AstRoot)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (children:ChChildInfo list) =
    seq {
        for ch in children do
            for v in ch.chType.automaticTestCasesValues do
                yield {NamedValue.name = ch.Name.Value; Value = v}
    } |> Seq.toList