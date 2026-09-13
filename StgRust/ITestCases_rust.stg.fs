module ITestCases_rust
open System
open System.Numerics
open CommonTypes

type ITestCases_rust() =
    inherit AbstractMacros.ITestCases()
        override this.rtlModuleName  () =
            test_cases_rust.rtlModuleName  () 
        override this.PrintAutomaticTestCasesSpecFile  (sModNameUpperCase:string) (sModName:string) (arrsIncludedModules:seq<string>) (arrsTestFunctions:seq<string>) =
            test_cases_rust.PrintAutomaticTestCasesSpecFile  sModNameUpperCase sModName arrsIncludedModules arrsTestFunctions 
        override this.Codec_Encode_XER  (sModName:string) (sFuncName:string) (sVal:string) (sTasName:string) =
            test_cases_rust.Codec_Encode_XER  sModName sFuncName sVal sTasName 
        override this.PrintAutomaticTestCasesBodyFile  (sModName:string) (sTetscaseSpecFileName:string) (arrsIncludedModules:seq<string>) (arrsTasNames:seq<string>) (arrsTypeAssignments:seq<string>) (bXer:bool) =
            test_cases_rust.PrintAutomaticTestCasesBodyFile  sModName sTetscaseSpecFileName arrsIncludedModules arrsTasNames arrsTypeAssignments bXer 
        override this.Codec_Encode  (sModName:string) (sFuncName:string) (sVal:string) =
            test_cases_rust.Codec_Encode  sModName sFuncName sVal 
        override this.Codec_Decode  (sModName:string) (sFuncName:string) (sTasName:string) (sEnc:string) (sAmber:string) =
            test_cases_rust.Codec_Decode  sModName sFuncName sTasName sEnc sAmber 
        override this.Codec_Decode_exact_length  (sModName:string) (sFuncName:string) (sTasName:string) (sEnc:string) (sAmber:string) =
            test_cases_rust.Codec_Decode_exact_length  sModName sFuncName sTasName sEnc sAmber 
        override this.Codec_Decode_XER  (sModName:string) (sFuncName:string) (sTasName:string) (sEnc:string) (sAmber:string) =
            test_cases_rust.Codec_Decode_XER  sModName sFuncName sTasName sEnc sAmber 
        override this.Codec_validate_output  (sModName:string) (sFuncName:string) (sAmber:string) =
            test_cases_rust.Codec_validate_output  sModName sFuncName sAmber 
        override this.Codec_compare_input_with_output  (sModName:string) (sFuncName:string) (sVal:string) (sAmber:string) =
            test_cases_rust.Codec_compare_input_with_output  sModName sFuncName sVal sAmber 
        override this.Codec_write_CharstreamToFile  () =
            test_cases_rust.Codec_write_CharstreamToFile  () 
        override this.Codec_write_bitstreamToFile  () =
            test_cases_rust.Codec_write_bitstreamToFile  () 
        override this.JoinItems  (sPart:string) (soNestedPart:string option) =
            test_cases_rust.JoinItems  sPart soNestedPart 
        override this.Codec_declare_EncInDecOut_variable  (sPrmName:string) (sType:string) (sPrmValue:string) =
            test_cases_rust.Codec_declare_EncInDecOut_variable  sPrmName sType sPrmValue 
        override this.Codec_declare_DecIn_variable  (sPrmName:string) (sType:string) =
            test_cases_rust.Codec_declare_DecIn_variable  sPrmName sType 
        override this.PrintCodec_spec  (sFuncName:string) (sModName:string) (sTasName:string) (sStar:string) (sVal:string) =
            test_cases_rust.PrintCodec_spec  sFuncName sModName sTasName sStar sVal 
        override this.PrintCodec_body  (sModName:string) (sFuncName:string) (sTasName:string) (sStar:string) (sVal:string) (sEnc:string) (sNestedStatements:string) (sCodecClass:string) (sEncodingEnumValue:string) =
            test_cases_rust.PrintCodec_body  sModName sFuncName sTasName sStar sVal sEnc sNestedStatements sCodecClass sEncodingEnumValue 
        override this.PrintCodec_body_XER  (sModName:string) (sFuncName:string) (sTasName:string) (sStar:string) (sVal:string) (sEnc:string) (sNestedStatements:string) =
            test_cases_rust.PrintCodec_body_XER  sModName sFuncName sTasName sStar sVal sEnc sNestedStatements 
        override this.PrintMain  (sTestSuiteFilename:string) (arrsProgramUnitNames:seq<string>) =
            test_cases_rust.PrintMain  sTestSuiteFilename arrsProgramUnitNames 
        override this.PrintSuite_call_codec_generate_dat_file  (sModName:string) (sTasName:string) (sAmber:string) (sEnc:string) (sStreamName:string) =
            test_cases_rust.PrintSuite_call_codec_generate_dat_file  sModName sTasName sAmber sEnc sStreamName 
        override this.PrintATCRunnerDefinition  () =
            test_cases_rust.PrintATCRunnerDefinition  () 
        override this.PrintATCRunner  (sTestSuiteFilename:string) (arrsIncludedModules:seq<string>) (arrsVars:seq<string>) (arrsTestFunctions:seq<string>) (arrsUsedPackages:seq<string>) (arrsInitCalls:seq<string>) (bGenerateDatFile:bool) =
            test_cases_rust.PrintATCRunner  sTestSuiteFilename arrsIncludedModules arrsVars arrsTestFunctions arrsUsedPackages arrsInitCalls bGenerateDatFile 
        override this.invokeTestCaseAsFunc  (sFuncName:string) =
            test_cases_rust.invokeTestCaseAsFunc  sFuncName 
        override this.emitTestCaseAsFunc_h  (sFuncName:string) =
            test_cases_rust.emitTestCaseAsFunc_h  sFuncName 
        override this.emitTestCaseAsFunc_dummy_init  (sTypeName:string) (sFuncName:string) (sDummyVarname:string) =
            test_cases_rust.emitTestCaseAsFunc_dummy_init  sTypeName sFuncName sDummyVarname 
        override this.emitTestCaseAsFunc_dummy_init_function  (sTypeName:string) (sFuncName:string) (sDummyVarname:string) =
            test_cases_rust.emitTestCaseAsFunc_dummy_init_function  sTypeName sFuncName sDummyVarname 
        override this.emitTestCaseAsFunc  (sFuncName:string) (arrsVars:seq<string>) (sModName:string) (sTasName:string) (sAmber:string) (sEnc:string) (bValueAssignment:bool) (sInitializeTcData:string) (bStatic:bool) (sGenerateDatFile:string) (arrsDummyInitStatementsNeededForStatementCoverage:seq<string>) (sInitAmber:string) =
            test_cases_rust.emitTestCaseAsFunc  sFuncName arrsVars sModName sTasName sAmber sEnc bValueAssignment sInitializeTcData bStatic sGenerateDatFile arrsDummyInitStatementsNeededForStatementCoverage sInitAmber 
        override this.printTestCaseFileDef  (sThisFile:string) (arrsIncludedModules:seq<string>) (arrsTestFunctionDefs:seq<string>) =
            test_cases_rust.printTestCaseFileDef  sThisFile arrsIncludedModules arrsTestFunctionDefs 
        override this.printTestCaseFileBody  (sThisFile:string) (arrsIncludedModules:seq<string>) (arrsTestFunctionBodies:seq<string>) (arrsProgramUnitNames:seq<string>) =
            test_cases_rust.printTestCaseFileBody  sThisFile arrsIncludedModules arrsTestFunctionBodies arrsProgramUnitNames 
        override this.decodingCaseKind  (sBody:string) (sIdentifier:string) =
            test_cases_rust.decodingCaseKind  sBody sIdentifier 
