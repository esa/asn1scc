module test_cases_rust
open System
open System.Numerics
open CommonTypes

let rtlModuleName () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "rtlModuleName" []

let PrintAutomaticTestCasesSpecFile (sModNameUpperCase:string) (sModName:string) (arrsIncludedModules:seq<string>) (arrsTestFunctions:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "PrintAutomaticTestCasesSpecFile" [("sModNameUpperCase",(if sModNameUpperCase = null then null else ST.StrHelper sModNameUpperCase:>Object) );("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("arrsIncludedModules",(arrsIncludedModules|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsTestFunctions",(arrsTestFunctions|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let Codec_Encode_XER (sModName:string) (sFuncName:string) (sVal:string) (sTasName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_Encode_XER" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) )]

let PrintAutomaticTestCasesBodyFile (sModName:string) (sTetscaseSpecFileName:string) (arrsIncludedModules:seq<string>) (arrsTasNames:seq<string>) (arrsTypeAssignments:seq<string>) (bXer:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "PrintAutomaticTestCasesBodyFile" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sTetscaseSpecFileName",(if sTetscaseSpecFileName = null then null else ST.StrHelper sTetscaseSpecFileName:>Object) );("arrsIncludedModules",(arrsIncludedModules|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsTasNames",(arrsTasNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsTypeAssignments",(arrsTypeAssignments|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bXer",bXer :>Object)]

let Codec_Encode (sModName:string) (sFuncName:string) (sVal:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_Encode" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) )]

let Codec_Decode (sModName:string) (sFuncName:string) (sTasName:string) (sEnc:string) (sAmber:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_Decode" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sEnc",(if sEnc = null then null else ST.StrHelper sEnc:>Object) );("sAmber",(if sAmber = null then null else ST.StrHelper sAmber:>Object) )]

let Codec_Decode_exact_length (sModName:string) (sFuncName:string) (sTasName:string) (sEnc:string) (sAmber:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_Decode_exact_length" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sEnc",(if sEnc = null then null else ST.StrHelper sEnc:>Object) );("sAmber",(if sAmber = null then null else ST.StrHelper sAmber:>Object) )]

let Codec_Decode_XER (sModName:string) (sFuncName:string) (sTasName:string) (sEnc:string) (sAmber:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_Decode_XER" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sEnc",(if sEnc = null then null else ST.StrHelper sEnc:>Object) );("sAmber",(if sAmber = null then null else ST.StrHelper sAmber:>Object) )]

let Codec_validate_output (sModName:string) (sFuncName:string) (sAmber:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_validate_output" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sAmber",(if sAmber = null then null else ST.StrHelper sAmber:>Object) )]

let Codec_compare_input_with_output (sModName:string) (sFuncName:string) (sVal:string) (sAmber:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_compare_input_with_output" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("sAmber",(if sAmber = null then null else ST.StrHelper sAmber:>Object) )]

let Codec_write_CharstreamToFile () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_write_CharstreamToFile" []

let Codec_write_bitstreamToFile () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_write_bitstreamToFile" []

let JoinItems (sPart:string) (soNestedPart:string option) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "JoinItems" [("sPart",(if sPart = null then null else ST.StrHelper sPart:>Object) );("soNestedPart",(if soNestedPart.IsNone then null else ST.StrHelper soNestedPart.Value:>Object) )]

let Codec_declare_EncInDecOut_variable (sPrmName:string) (sType:string) (sPrmValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_declare_EncInDecOut_variable" [("sPrmName",(if sPrmName = null then null else ST.StrHelper sPrmName:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) );("sPrmValue",(if sPrmValue = null then null else ST.StrHelper sPrmValue:>Object) )]

let Codec_declare_DecIn_variable (sPrmName:string) (sType:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "Codec_declare_DecIn_variable" [("sPrmName",(if sPrmName = null then null else ST.StrHelper sPrmName:>Object) );("sType",(if sType = null then null else ST.StrHelper sType:>Object) )]

let PrintCodec_spec (sFuncName:string) (sModName:string) (sTasName:string) (sStar:string) (sVal:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "PrintCodec_spec" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) )]

let PrintCodec_body (sModName:string) (sFuncName:string) (sTasName:string) (sStar:string) (sVal:string) (sEnc:string) (sNestedStatements:string) (sCodecClass:string) (sEncodingEnumValue:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "PrintCodec_body" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("sEnc",(if sEnc = null then null else ST.StrHelper sEnc:>Object) );("sNestedStatements",(if sNestedStatements = null then null else ST.StrHelper sNestedStatements:>Object) );("sCodecClass",(if sCodecClass = null then null else ST.StrHelper sCodecClass:>Object) );("sEncodingEnumValue",(if sEncodingEnumValue = null then null else ST.StrHelper sEncodingEnumValue:>Object) )]

let PrintCodec_body_XER (sModName:string) (sFuncName:string) (sTasName:string) (sStar:string) (sVal:string) (sEnc:string) (sNestedStatements:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "PrintCodec_body_XER" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sStar",(if sStar = null then null else ST.StrHelper sStar:>Object) );("sVal",(if sVal = null then null else ST.StrHelper sVal:>Object) );("sEnc",(if sEnc = null then null else ST.StrHelper sEnc:>Object) );("sNestedStatements",(if sNestedStatements = null then null else ST.StrHelper sNestedStatements:>Object) )]

let PrintMain (sTestSuiteFilename:string) (arrsProgramUnitNames:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "PrintMain" [("sTestSuiteFilename",(if sTestSuiteFilename = null then null else ST.StrHelper sTestSuiteFilename:>Object) );("arrsProgramUnitNames",(arrsProgramUnitNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let PrintSuite_call_codec_generate_dat_file (sModName:string) (sTasName:string) (sAmber:string) (sEnc:string) (sStreamName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "PrintSuite_call_codec_generate_dat_file" [("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sAmber",(if sAmber = null then null else ST.StrHelper sAmber:>Object) );("sEnc",(if sEnc = null then null else ST.StrHelper sEnc:>Object) );("sStreamName",(if sStreamName = null then null else ST.StrHelper sStreamName:>Object) )]

let PrintATCRunnerDefinition () =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "PrintATCRunnerDefinition" []

let PrintATCRunner (sTestSuiteFilename:string) (arrsIncludedModules:seq<string>) (arrsVars:seq<string>) (arrsTestFunctions:seq<string>) (arrsUsedPackages:seq<string>) (arrsInitCalls:seq<string>) (bGenerateDatFile:bool) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "PrintATCRunner" [("sTestSuiteFilename",(if sTestSuiteFilename = null then null else ST.StrHelper sTestSuiteFilename:>Object) );("arrsIncludedModules",(arrsIncludedModules|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsVars",(arrsVars|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsTestFunctions",(arrsTestFunctions|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsUsedPackages",(arrsUsedPackages|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsInitCalls",(arrsInitCalls|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("bGenerateDatFile",bGenerateDatFile :>Object)]

let invokeTestCaseAsFunc (sFuncName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "invokeTestCaseAsFunc" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]

let emitTestCaseAsFunc_h (sFuncName:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "emitTestCaseAsFunc_h" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) )]

let emitTestCaseAsFunc_dummy_init (sTypeName:string) (sFuncName:string) (sDummyVarname:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "emitTestCaseAsFunc_dummy_init" [("sTypeName",(if sTypeName = null then null else ST.StrHelper sTypeName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sDummyVarname",(if sDummyVarname = null then null else ST.StrHelper sDummyVarname:>Object) )]

let emitTestCaseAsFunc_dummy_init_function (sTypeName:string) (sFuncName:string) (sDummyVarname:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "emitTestCaseAsFunc_dummy_init_function" [("sTypeName",(if sTypeName = null then null else ST.StrHelper sTypeName:>Object) );("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("sDummyVarname",(if sDummyVarname = null then null else ST.StrHelper sDummyVarname:>Object) )]

let emitTestCaseAsFunc (sFuncName:string) (arrsVars:seq<string>) (sModName:string) (sTasName:string) (sAmber:string) (sEnc:string) (bValueAssignment:bool) (sInitializeTcData:string) (bStatic:bool) (sGenerateDatFile:string) (arrsDummyInitStatementsNeededForStatementCoverage:seq<string>) (sInitAmber:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "emitTestCaseAsFunc" [("sFuncName",(if sFuncName = null then null else ST.StrHelper sFuncName:>Object) );("arrsVars",(arrsVars|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sModName",(if sModName = null then null else ST.StrHelper sModName:>Object) );("sTasName",(if sTasName = null then null else ST.StrHelper sTasName:>Object) );("sAmber",(if sAmber = null then null else ST.StrHelper sAmber:>Object) );("sEnc",(if sEnc = null then null else ST.StrHelper sEnc:>Object) );("bValueAssignment",bValueAssignment :>Object);("sInitializeTcData",(if sInitializeTcData = null then null else ST.StrHelper sInitializeTcData:>Object) );("bStatic",bStatic :>Object);("sGenerateDatFile",(if sGenerateDatFile = null then null else ST.StrHelper sGenerateDatFile:>Object) );("arrsDummyInitStatementsNeededForStatementCoverage",(arrsDummyInitStatementsNeededForStatementCoverage|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("sInitAmber",(if sInitAmber = null then null else ST.StrHelper sInitAmber:>Object) )]

let printTestCaseFileDef (sThisFile:string) (arrsIncludedModules:seq<string>) (arrsTestFunctionDefs:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "printTestCaseFileDef" [("sThisFile",(if sThisFile = null then null else ST.StrHelper sThisFile:>Object) );("arrsIncludedModules",(arrsIncludedModules|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsTestFunctionDefs",(arrsTestFunctionDefs|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let printTestCaseFileBody (sThisFile:string) (arrsIncludedModules:seq<string>) (arrsTestFunctionBodies:seq<string>) (arrsProgramUnitNames:seq<string>) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "printTestCaseFileBody" [("sThisFile",(if sThisFile = null then null else ST.StrHelper sThisFile:>Object) );("arrsIncludedModules",(arrsIncludedModules|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsTestFunctionBodies",(arrsTestFunctionBodies|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object);("arrsProgramUnitNames",(arrsProgramUnitNames|>Seq.map (fun s ->  if s = null then null else (ST.StrHelper s):>Object) |> Seq.toArray) :>Object)]

let decodingCaseKind (sBody:string) (sIdentifier:string) =
    ST.lang <- CommonTypes.ProgrammingLanguage.Ada; ST.double2StringPlain <- true
    ST.call "test_cases_rust" "decodingCaseKind" [("sBody",(if sBody = null then null else ST.StrHelper sBody:>Object) );("sIdentifier",(if sIdentifier = null then null else ST.StrHelper sIdentifier:>Object) )]

