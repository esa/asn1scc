(*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*)

module automatic_test_cases

open System.Numerics
open FsUtils
open Ast
open System.IO
open VisitTree
open uPER
open CloneTree
open c_utils
open CommonTypes


let GetEncodingString = function    
    | UPER  -> ""
    | ACN   -> "ACN_"
    | BER   -> "BER_"
    | XER   -> "XER_"




let CreateHeaderFile (f:Asn1File) outDir (newFileExt:string) (r:AstRoot) (acn:AcnTypes.AcnAstResolved)  =
    let fileNameNoExtUpper = f.FileNameWithoutExtension.ToUpper() + newFileExt.Replace(".","_").ToUpper()
    let allImportedModules = f.Modules |> Seq.collect(fun m -> m.Imports) |> Seq.map(fun imp -> imp.Name.Value) |> Seq.distinct
    let includedModules  = seq {   
        for file in r.Files do
            if file.FileName <> f.FileName then
                if file.Modules |> Seq.exists (fun m -> allImportedModules |> Seq.exists(fun x -> x = m.Name.Value)) then
                    yield file.FileNameWithoutExtension } |> Seq.toList 
    let sortedTas = c_h.SortTypeAssignments f r acn
    let printTas (tas:TypeAssignment) =
        let sTasName = tas.GetCName r.TypePrefix
        let sStar = (TypeStar tas.Type r)
        r.Encodings |> Seq.map(fun enc -> c_aux.PrintTypeAssignment_header sTasName sStar (GetEncodingString enc)) |> Seq.StrJoin "\n"
    let tases = sortedTas |> Seq.map(fun (m,tas) -> printTas tas ) 
    let arrsUnnamedVariables = []
    let content = c_aux.PrintAutomaticTestCasesHeaderFile (ToC fileNameNoExtUpper) f.FileNameWithoutExtension tases 
    let fileName = Path.Combine(outDir, (f.FileNameWithoutExtension+newFileExt).ToLower())
    File.WriteAllText(fileName, content.Replace("\r",""))

type StatementKind =
    |Update_DecIn     of AcnTypes.AcnParameter       
    |Encode_input
    |Decode_output
    |Validate_output
    |Compare_input_output


let PrintTypeAss (tas:TypeAssignment) (m:Asn1Module) (f:Asn1File) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) = 
    let sTasName = tas.GetCName r.TypePrefix
    let sStar = (TypeStar tas.Type r)
    let sAmber = if sStar = "*" then "&" else ""
//    r.Encodings |> Seq.map(fun enc->    
//                            let sEnc = GetEncodingString enc
//                            let ret = c_aux.PrintTypeAssignment_source sTasName sStar sAmber sEnc [] []
//                            ret
//                            ) |> Seq.StrJoin "\n\n"
    let PrintCodec (t:TypeAssignment)  = 
        let PrintCodeAux (enc:Asn1Encoding) =
            let sEnc = GetEncodingString enc
            let sTasName = GetTasCName t.Name.Value r.TypePrefix
            let prmsEncInDecOut, prmsDecIn = 
                match enc with
                | ACN  -> 
                    let allParams = acn.Parameters |> List.filter(fun p -> p.ModName = m.Name.Value && p.TasName = t.Name.Value) 
                    allParams |> List.filter(fun x -> x.Mode = AcnTypes.EncodeDecodeMode), allParams |> List.filter(fun x -> x.Mode = AcnTypes.DecodeMode)
                | _      -> [],[]
            let arrsEncInDecOutParamsNames = prmsEncInDecOut |> List.map(fun p -> ToC p.Name)
            let arrsDecInParamsNames = prmsDecIn |> List.map(fun p -> ToC p.Name)
            let arrsParamNames = arrsEncInDecOutParamsNames@arrsDecInParamsNames
            let statements =
                seq {
                    for p in prmsDecIn do
                            yield Update_DecIn p
                    yield Encode_input
                    yield Decode_output
                    yield Validate_output
                    yield Compare_input_output
                } |> Seq.toList

            let printStatement stm sNestedContent = 
                let content= 
                    match stm with
                    |Update_DecIn(p)        -> c_aux.Codec_UpdateDecInParam sTasName (ToC p.Name)
                    |Encode_input           -> c_aux.Codec_Encode sTasName sEnc arrsEncInDecOutParamsNames
                    |Decode_output          -> 
                        match enc with
                        | XER   -> c_aux.Codec_Decode_XER sTasName sEnc sAmber arrsDecInParamsNames arrsEncInDecOutParamsNames
                        | _     -> c_aux.Codec_Decode sTasName sEnc sAmber arrsDecInParamsNames arrsEncInDecOutParamsNames
                    |Validate_output        -> c_aux.Codec_validate_output sTasName sAmber
                    |Compare_input_output   -> c_aux.Codec_compare_input_with_output sTasName sAmber arrsEncInDecOutParamsNames
                c_aux.JoinItems content  sNestedContent

            let rec printStatements  = function
                |[]     -> null
                |x::xs  -> printStatement x  (printStatements xs)

            let GetValueFromParam (p:AcnTypes.AcnParameter) =
                let asn1Type = Ast.AcnAsn1Type2Asn1Type p.Asn1Type
                let value = {Asn1Value.Kind = (acn_backend_logic.GenerateValues asn1Type m.Name r acn |> Seq.head); Location = emptyLocation}
                c_variables.PrintAsn1Value value asn1Type false (sTasName,0) {m with Name ="".AsLoc} r

                    

            let arrsEncInDecOutParamsLocalVars = prmsEncInDecOut |> Seq.map(fun p -> c_aux.Codec_declare_EncInDecOut_variable (ToC p.Name) (c_h.DeclateAcnAsn1Type p.Asn1Type {m with Name = "".AsLoc} r) (GetValueFromParam p) )
            let arrsDecInLocalVars = prmsDecIn |> Seq.map(fun p -> c_aux.Codec_declare_DecIn_variable  (ToC p.Name) (c_h.DeclateAcnAsn1Type p.Asn1Type {m with Name = "".AsLoc} r) )
            let sNestedStatements = printStatements statements
            match enc with
            | XER   -> c_aux.PrintCodec_body_XER sTasName sStar sEnc arrsEncInDecOutParamsLocalVars arrsDecInLocalVars sNestedStatements
            | _     -> c_aux.PrintCodec_body sTasName sStar sEnc arrsEncInDecOutParamsLocalVars arrsDecInLocalVars sNestedStatements
        r.Encodings |> Seq.map PrintCodeAux

    PrintCodec tas |> Seq.StrJoin "\n\n"


let CreateSourceFile (f:Asn1File) outDir (newFileExt:string) (r:AstRoot) (acn:AcnTypes.AcnAstResolved)  =
    let headerFileName = (System.IO.Path.GetFileNameWithoutExtension (f.FileNameWithoutExtension + newFileExt)).ToLower()
    let allImportedModules = f.Modules |> Seq.collect(fun m -> m.Imports) |> Seq.map(fun imp -> imp.Name.Value) |> Seq.distinct
    let includedModules  = seq {   
        for file in r.Files do
            if file.FileName <> f.FileName then
                if file.Modules |> Seq.exists (fun m -> allImportedModules |> Seq.exists(fun x -> x = m.Name.Value)) then
                    yield file.FileNameWithoutExtension } |> Seq.toList 
    let sortedTas = c_h.SortTypeAssignments f r acn
    let tases = sortedTas |> Seq.map(fun (m,tas) -> PrintTypeAss tas m f r acn ) 
    let arrsUnnamedVariables = []
    let printUpdateParamDeclarations (p:AcnTypes.AcnParameter) =
        let foundTas = sortedTas |> List.tryFind(fun (m,tas)-> tas.Name.Value = p.TasName && p.ModName = m.Name.Value)
        match foundTas with
        | None -> ""
        | Some(m, tas) ->
                let sStar = (TypeStar tas.Type r)
                let prmType = c_h.PrintParamType p m r
                let prmStar = (TypeStar (Ast.AcnAsn1Type2Asn1Type p.Asn1Type) r)
                let sTasName = GetTasCName tas.Name.Value r.TypePrefix
                let prmName = ToC p.Name
                c_aux.ACN_UpdateParamDecl sTasName sStar prmType prmName prmStar

    let updateDeclarations = acn.Parameters |> List.map printUpdateParamDeclarations
    let content = c_aux.PrintAutomaticTestCasesSourceFile headerFileName  updateDeclarations tases 
    let fileName = Path.Combine(outDir, (f.FileNameWithoutExtension+newFileExt).ToLower())
    File.WriteAllText(fileName, content.Replace("\r",""))





let CreateTestCases (r:AstRoot) (acn:AcnTypes.AcnAstResolved) outDir  =
    r.Files |> Seq.iter(fun f -> 
                                CreateSourceFile f outDir "_auto_tcs.c" r acn
                                CreateHeaderFile f outDir "_auto_tcs.h" r acn
                                )  


let TestSuiteFileName = "testsuite"


let CreateMainFile outDir =
    let content = c_aux.PrintMain TestSuiteFileName
    let outFileName = Path.Combine(outDir, "mainprogram.c")
    File.WriteAllText(outFileName, content.Replace("\r",""))


let CreateMakeFile (r:AstRoot) (acn:AcnTypes.AcnAstResolved) outDir  =
    let files = r.Files |> Seq.map(fun x -> x.FileNameWithoutExtension.ToLower() )
    let content = c_aux.PrintMakeFile files
    let outFileName = Path.Combine(outDir, "Makefile")
    File.WriteAllText(outFileName, content.Replace("\r",""))


let CreateTestSuiteFile (r:AstRoot) (acn:AcnTypes.AcnAstResolved) outDir vasName =
    let includedPackages =  r.Files |> Seq.map(fun x -> x.FileNameWithoutExtension.ToLower() + "_auto_tcs")
    let PrintTestCase (v:ValueAssignment) (m:Asn1Module) =
        let tasName = match v.Type.Kind with
                      | ReferenceType(md, ts, _)   -> ts.Value
                      | _                       -> ""
        let sTasName = spark_variables.GetTasNameByKind v.Type.Kind m r
        let sStar = (TypeStar v.Type r)
        let sAmber = if sStar = "*" then "&" else ""

        let packageName = ToC m.Name.Value
        let sValue =  c_variables.PrintAsn1Value v.Value v.Type false (sTasName,0) m r    
        let sAsn1Val0 = (PrintAsn1.PrintValueAss v m).Replace("\"","'")
        let sAsn1Val = match sAsn1Val0.Length>1000 with
                       | true -> sAsn1Val0.Substring(0,1000)
                       | false -> sAsn1Val0

        

        let GetDatFile (enc:Asn1Encoding) = 
            let bGenerateDatFile = (r.CheckWithOss && v.Name.Value = "testPDU")
            match bGenerateDatFile, enc with
            | false,_     -> ""
            | true, ACN   -> ""
            | true, XER   -> c_aux.PrintSuite_call_codec_generate_dat_file sTasName sAmber (GetEncodingString enc) "Byte"
            | true, BER   -> c_aux.PrintSuite_call_codec_generate_dat_file sTasName sAmber (GetEncodingString enc) "Byte"
            | true, uPER  -> c_aux.PrintSuite_call_codec_generate_dat_file sTasName sAmber (GetEncodingString enc) "Bit"

        let bStatic = match (Ast.GetActualType v.Type r).Kind with Integer | Enumerated(_) -> false | _ -> true
        
        r.Encodings |> Seq.map(fun e -> match e with
                                        | UPER  -> c_aux.PrintSuite_call_codec sTasName sAmber (GetEncodingString e) sValue sAsn1Val (ToC v.Name.Value) bStatic (GetDatFile e)
                                        | ACN   -> c_aux.PrintSuite_call_codec sTasName sAmber (GetEncodingString e) sValue sAsn1Val (ToC v.Name.Value) bStatic (GetDatFile e)
                                        | XER   -> c_aux.PrintSuite_call_codec sTasName sAmber (GetEncodingString e) sValue sAsn1Val (ToC v.Name.Value) bStatic (GetDatFile e)
                                        | BER   -> c_aux.PrintSuite_call_codec sTasName sAmber (GetEncodingString e) sValue sAsn1Val (ToC v.Name.Value) bStatic (GetDatFile e)
                                 ) |> Seq.StrJoin "\n\n"
        
    
    let funcs = seq {
            for m in r.Modules do
                let autoVases = m.TypeAssignments |> Seq.collect(fun x -> acn_backend_logic.GenerateVases x m.Name r acn) |> Seq.toList
                for v in m.ValueAssignments@autoVases do
                    match v.Type.Kind with
                    | ReferenceType(md,ts, _)  -> 
                        let actMod = r.GetModuleByName md
                        if vasName = "ALL" || v.Name.Value = vasName then
                            yield PrintTestCase v actMod
                    | _                 -> ()
        }

    let contentC = c_aux.PrintTestSuiteSource TestSuiteFileName includedPackages funcs
    let outCFileName = Path.Combine(outDir, TestSuiteFileName + ".c")
    File.WriteAllText(outCFileName, contentC.Replace("\r",""))

    let contentH = c_aux.PrintTestSuiteHeader()
    let outHFileName = Path.Combine(outDir, TestSuiteFileName + ".h")
    File.WriteAllText(outHFileName, contentH.Replace("\r",""))
