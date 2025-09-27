module LangGeneric_c
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open Language
open System.IO
open System

let getAccess_c (sel: AccessPath) =
    match sel.selectionType with
    | ByPointer -> "->"
    | _ -> "."

let getAccess2_c (acc: AccessStep) =
    match acc with
    | ValueAccess (sel, _, _) -> $".{sel}"
    | PointerAccess (sel, _, _) -> $"->{sel}"
    | ArrayAccess (ix, _) -> $"[{ix}]"

#if false
let createBitStringFunction_funcBody_c handleFragmentation (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefinitionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErrorCode) (p:CallerScope) =
    let ii = id.SequenceOfLevel + 1;
    let i = sprintf "i%d" (id.SequenceOfLevel + 1)
    let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))

    let funcBodyContent, localVariables =
        let nStringLength =
            match isFixedSize,  codec with
            | true , _    -> []
            | false, Encode -> []
            | false, Decode -> [Asn1SIntLocalVariable ("nCount", None)]

        match minSize with
        | _ when maxSize < 65536I && isFixedSize   -> uper_c.bitString_FixSize p.arg.p (getAccess_c p.arg) (minSize) errCode.errCodeName codec , nStringLength
        | _ when maxSize < 65536I && (not isFixedSize)  -> uper_c.bitString_VarSize p.arg.p (getAccess_c p.arg) (minSize) (maxSize) errCode.errCodeName nSizeInBits codec, nStringLength
        | _                                                ->
            handleFragmentation p codec errCode ii (uperMaxSizeInBits) minSize maxSize "" 1I true false
    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}
#endif

type LangBasic_c() =
    inherit ILangBasic()
        override this.cmp (s1:string) (s2:string) = s1 = s2
        override this.isCaseSensitive = true
        override this.keywords = c_keywords
        override this.isKeyword (token) = c_keywords.Contains token
        override this.OnTypeNameConflictTryAppendModName = true
        override this.declare_IntegerNoRTL = "", "asn1SccSint", "INTEGER"
        override this.declare_PosIntegerNoRTL = "", "asn1SccUint" , "INTEGER"
        override this.getRealRtlTypeName   = "", "asn1Real", "REAL"
        override this.getObjectIdentifierRtlTypeName  relativeId =
            let asn1Name = if relativeId then "RELATIVE-OID" else "OBJECT IDENTIFIER"
            "", "Asn1ObjectIdentifier", asn1Name
        override this.getTimeRtlTypeName  timeClass =
            let asn1Name = "TIME"
            match timeClass with
            | Asn1LocalTime                    _ -> "", "Asn1LocalTime", asn1Name
            | Asn1UtcTime                      _ -> "", "Asn1UtcTime", asn1Name
            | Asn1LocalTimeWithTimeZone        _ -> "", "Asn1TimeWithTimeZone", asn1Name
            | Asn1Date                           -> "", "Asn1Date", asn1Name
            | Asn1Date_LocalTime               _ -> "", "Asn1DateLocalTime", asn1Name
            | Asn1Date_UtcTime                 _ -> "", "Asn1DateUtcTime", asn1Name
            | Asn1Date_LocalTimeWithTimeZone   _ -> "", "Asn1DateTimeWithTimeZone", asn1Name
        override this.getNullRtlTypeName  = "", "NullType", "NULL"
        override this.getBoolRtlTypeName = "","flag","BOOLEAN"


type LangGeneric_c() =
    inherit ILangGeneric()
        override _.ArrayStartIndex = 0

        override _.intValueToString (i:BigInteger) (intClass:Asn1AcnAst.IntegerClass) =
            match intClass with
            | Asn1AcnAst.ASN1SCC_Int8     _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int16    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int32    _ ->
                // Note: In C, there is no such thing as "negative literal",
                // but instead, it's a positive number that is negated, as such
                // we need to be careful and use INT_MIN instead to avoid overflow.
                if i = BigInteger Int32.MinValue then "INT_MIN" else sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int64    _ ->
                if i = BigInteger Int64.MinValue then "LLONG_MIN" else sprintf "%sLL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int      _ ->
                if i = BigInteger Int64.MinValue then "LLONG_MIN" else sprintf "%sLL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt8    _ ->  sprintf "%sUL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt16   _ ->  sprintf "%sUL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt32   _ ->  sprintf "%sUL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt64   _ ->  sprintf "%sUL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt     _ ->  sprintf "%sUL" (i.ToString())

        override _.asn1SccIntValueToString (i: BigInteger) _ = i.ToString()
        override _.doubleValueToString (v:double) =
            v.ToString(FsUtils.doubleParseString, System.Globalization.NumberFormatInfo.InvariantInfo)

        override _.initializeString stringSize = sprintf "{ [0 ... %d] = 0x0 }" stringSize

        override _.supportsInitExpressions = false
        override _.requiresHandlingOfEmptySequences = true
        override _.requiresHandlingOfZeroArrays = true


        override this.getPointer (sel: AccessPath) =
            let str = sel.joined this
            match sel.selectionType with
            | ByValue -> $"(&({str}))"
            | _ -> str

        override this.getValue (sel: AccessPath) =
            let str = sel.joined this
            match sel.selectionType with
            | ByPointer -> $"(*({str}))"
            | _ -> str

        override this.getValueUnchecked (sel: AccessPath) _ = this.getValue sel
        override this.getPointerUnchecked (sel: AccessPath) _ = this.getPointer sel
        override this.joinSelectionUnchecked (sel: AccessPath) _ = sel.joined this
        override this.getAccess  (sel: AccessPath) = getAccess_c sel

        override this.getAccess2 (acc: AccessStep) = getAccess2_c acc
        override this.getPtrPrefix _ = ""

        override this.getPtrSuffix (sel: AccessPath) =
            match sel.selectionType with
            | ByPointer -> "*"
            | _ -> ""

        override this.getStar (sel: AccessPath) =
            match sel.selectionType with
            | ByPointer -> "*"
            | _ -> ""

        override this.setNamedItemBackendName0 (nm:Asn1Ast.NamedItem) (newValue:string) : Asn1Ast.NamedItem =
            {nm with c_name = newValue}
        override this.getNamedItemBackendName0 (nm:Asn1Ast.NamedItem)  = nm.c_name

        override this.getArrayItem (sel: AccessPath) (idx:string) (childTypeIsString: bool) =
            (sel.appendSelection "arr" ArrayElem false).append (ArrayAccess (idx, if childTypeIsString then ArrayElem else ByValue))

        override this.getNamedItemBackendName (defOrRef:TypeDefinitionOrReference option) (nm:Asn1AcnAst.NamedItem) =
            ToC nm.c_name
        override this.getNamedItemBackendName2 (_:string) (_:string) (nm:Asn1AcnAst.NamedItem) =
            ToC nm.c_name
        override this.decodeEmptySeq _ = None
        override this.decode_nullType _ = None

        override this.Length exp sAcc =
            isvalid_c.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[C]
        override this.definitionOrRef (d:Map<ProgrammingLanguage, TypeDefinitionOrReference>) = d.[C]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[C]
        override this.getEnumTypeDefinition (td:Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) = td.[C]
        override this.getStrTypeDefinition (td:Map<ProgrammingLanguage, FE_StringTypeDefinition>) = td.[C]
        override this.getChoiceTypeDefinition (td:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) = td.[C]
        override this.getSequenceTypeDefinition (td:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) = td.[C]
        override this.getSizeableTypeDefinition (td:Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) = td.[C]

        override _.getChildInfoName (ch:Asn1Ast.ChildInfo)  = ch.c_name
        override _.setChildInfoName (ch:Asn1Ast.ChildInfo) (newValue:string) = {ch with c_name = newValue}
        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._c_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._c_name
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._c_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._c_name
        override _.getChoiceChildPresentWhenName (ch:Asn1AcnAst.Choice ) (c:Asn1AcnAst.ChChildInfo) : string =
            (ToC c.present_when_name) + "_PRESENT"

        override this.getRtlFiles  (encodings:Asn1Encoding list) (_ :string list) =
            let encRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN ) with true -> ["asn1crt_encoding"] | false -> []
            let uperRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN) with true -> ["asn1crt_encoding_uper"] | false -> []
            let acnRtl = match encodings |> Seq.exists(fun e -> e = ACN) with true -> ["asn1crt_encoding_acn"] | false -> []
            let xerRtl = match encodings |> Seq.exists(fun e -> e = XER) with true -> ["asn1crt_encoding_xer"] | false -> []
            encRtl@uperRtl@acnRtl@xerRtl



        override this.getEmptySequenceInitExpression _ = "{}"
        override this.callFuncWithNoArgs () = "()"
        override this.rtlModuleName  = ""
        override this.AssignOperator = "="
        override this.TrueLiteral = "TRUE"
        override this.FalseLiteral = "FALSE"
        override this.emptyStatement = ""
        override this.bitStreamName = "BitStream"
        override this.unaryNotOperator    = "!"
        override this.modOp               = "%"
        override this.eqOp                = "=="
        override this.neqOp               = "!="
        override this.andOp               = "&&"
        override this.orOp                = "||"
        override this.initMethod           = InitMethod.Procedure
        override _.decodingKind = InPlace
        override _.usesWrappedOptional = false
        override this.castExpression (sExp:string) (sCastType:string) = sprintf "(%s)(%s)" sCastType sExp
        override this.createSingleLineComment (sText:string) = sprintf "/*%s*/" sText

        override _.SpecNameSuffix = ""
        override _.SpecExtension = "h"
        override _.BodyExtension = "c"
        override _.Keywords  = CommonTypes.c_keywords


        override _.getValueAssignmentName (vas: ValueAssignment) = vas.c_name

        override this.hasModules = false
        override this.allowsSrcFilesWithNoFunctions = true
        override this.requiresValueAssignmentsInSrcFile = true
        override this.supportsStaticVerification = false

        override this.getSeqChildIsPresent (sel: AccessPath) (childName:string) =
            sprintf "%s%sexist.%s" (sel.joined this) (this.getAccess sel) childName

        override this.getSeqChild (sel: AccessPath) (childName:string) (childTypeIsString: bool) (childIsOptional: bool) =
            sel.appendSelection childName (if childTypeIsString then ArrayElem else ByValue) childIsOptional

        override this.getChChild (sel: AccessPath) (childName:string) (childTypeIsString: bool): AccessPath =
            (sel.appendSelection "u" ByValue false).appendSelection childName (if childTypeIsString then ArrayElem else ByValue) false

        override this.choiceIDForNone (typeIdsSet:Map<string,int>) (id:ReferenceToType) =
            let prefix = ToC ((id.AcnAbsPath.Tail |> Seq.StrJoin("_")).Replace("#","elem"))
            match typeIdsSet.TryFind prefix with
            | None  -> prefix + "_NONE"
            | Some a when a = 1 -> prefix + "_NONE"
            | Some a            -> ToC ((id.AcnAbsPath |> Seq.StrJoin("_")).Replace("#","elem")) + "_NONE"

        override this.presentWhenName (defOrRef:TypeDefinitionOrReference option) (ch:ChChildInfo) : string =
            (ToC ch._present_when_name_private) + "_PRESENT"
        override this.presentWhenName0 (defOrRef:TypeDefinitionOrReference option) (ch:Asn1AcnAst.ChChildInfo) : string =
            (ToC ch.present_when_name) + "_PRESENT"
        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CodegenScope =
            let rec getRecvType (kind: Asn1AcnAst.Asn1TypeKind) =
                match kind with
                | Asn1AcnAst.NumericString _ | Asn1AcnAst.IA5String _ -> ArrayElem
                | Asn1AcnAst.ReferenceType r -> getRecvType r.resolvedType.Kind
                | _ -> ByPointer
            let recvId = "pVal" + suf
            {CodegenScope.modName = t.id.ModName; accessPath = AccessPath.emptyPath recvId (getRecvType t.Kind) }

        override this.getParamValue  (t:Asn1AcnAst.Asn1Type) (sel: AccessPath)  (c:Codec) =
            match t.Kind with
            | Asn1AcnAst.IA5String    _  -> this.getValue sel //FIXARRAY "val"
            | Asn1AcnAst.NumericString _ -> this.getValue sel// FIXARRAY "val"
            | Asn1AcnAst.ReferenceType r -> this.getParamValue r.resolvedType sel  c
            | _                          -> this.getPointer sel

        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "int i%d;" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "int i%d=%s;" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "int %s;" name
            | IntegerLocalVariable (name,Some iv)       -> sprintf "int %s=%s;" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "asn1SccSint %s;" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "asn1SccSint %s=%s;" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "asn1SccUint %s;" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "asn1SccUint %s=%s;" name iv
            | FlagLocalVariable (name,None)             -> sprintf "flag %s;" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "flag %s=%s;" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "flag %s;" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "flag %s=%s;" name iv
            | AcnInsertedChild(name, vartype, initVal)  -> sprintf "%s %s;" vartype name
            | GenericLocalVariable lv                   ->
                sprintf "%s%s %s%s;" (if lv.isStatic then "static " else "") lv.varType lv.name (if lv.arrSize.IsNone then "" else "["+lv.arrSize.Value+"]")


        override this.getLongTypedefName (tdr:TypeDefinitionOrReference) : string =
            match tdr with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref -> ref.typedefName

        //override this.getEnmLongTypedefName (td:FE_EnumeratedTypeDefinition) _ = td;

        override this.toHex n = sprintf "0x%x" n

        override this.bitStringValueToByteArray (v : BitStringValue) = FsUtils.bitStringValueToByteArray (StringLoc.ByValue v)

        override this.uper =
            {
                Uper_parts.createLv = (fun name -> Asn1SIntLocalVariable(name,None))
                requires_sBlockIndex  = true
                requires_sBLJ = false
                requires_charIndex = false
                requires_IA5String_i = true
                count_var            = Asn1SIntLocalVariable ("nCount", None)
                requires_presenceBit = true
                catd                 = false
                //createBitStringFunction = createBitStringFunction_funcBody_c
                seqof_lv = (fun id minSize maxSize -> [SequenceOfIndex (id.SequenceOfLevel + 1, None)])
                exprMethodCall        = fun _ _ -> ""
            }
        override this.acn =
            {
                Acn_parts.null_valIsUnReferenced = true
                checkBitPatternPresentResult = true
                getAcnContainingByLocVars = fun _ -> []
                getAcnDepSizeDeterminantLocVars =
                    fun  sReqBytesForUperEncoding ->
                        [
                            GenericLocalVariable {GenericLocalVariable.name = "arr"; varType = "byte"; arrSize = Some sReqBytesForUperEncoding; isStatic = true; initExp = None}
                            GenericLocalVariable {GenericLocalVariable.name = "bitStrm"; varType = "BitStream"; arrSize = None; isStatic = false; initExp = None}
                        ]
                choice_handle_always_absent_child = false
                createLocalVariableEnum =
                    (fun rtlIntType -> GenericLocalVariable {GenericLocalVariable.name = "intVal"; varType= rtlIntType; arrSize= None; isStatic = false; initExp=None })
                choice_requires_tmp_decoding = false
            }
        override this.init =
            {
                Initialize_parts.zeroIA5String_localVars    = fun _ -> []
                choiceComponentTempInit                     = false
                initMethSuffix                              = fun _ -> ""
            }
        override this.atc =
            {
                Atc_parts.uperPrefix = ""
                acnPrefix            = "ACN_"
                xerPrefix            = "XER_"
                berPrefix            = "BER_"
            }

        override this.CreateMakeFile (r:AstRoot)  (di:DirInfo) =
            let files = r.Files |> Seq.map(fun x -> (Path.GetFileNameWithoutExtension x.FileName).ToLower() )
            let content = aux_c.PrintMakeFile files (r.args.integerSizeInBytes = 4I) (r.args.floatingPointSizeInBytes = 4I) r.args.streamingModeSupport
            let outFileName = Path.Combine(di.srcDir, "Makefile")
            File.WriteAllText(outFileName, content.Replace("\r",""))

        override this.CreateAuxFiles (r:AstRoot)  (di:DirInfo) (arrsSrcTstFiles : string list, arrsHdrTstFiles:string list) =
            let CreateCMainFile (r:AstRoot)  outDir  =
                //Main file for test cass
                let printMain =    test_cases_c.PrintMain //match l with C -> test_cases_c.PrintMain | Ada -> test_cases_c.PrintMain
                let content = printMain "testsuite"
                let outFileName = Path.Combine(outDir, "mainprogram.c")
                File.WriteAllText(outFileName, content.Replace("\r",""))


            let generateVisualStudioProject (r:DAst.AstRoot) outDir (arrsSrcTstFilesX, arrsHdrTstFilesX) =
                let extrSrcFiles, extrHdrFiles =
                    r.args.encodings |>
                    List.collect(fun e ->
                        match e with
                        | Asn1Encoding.UPER -> ["asn1crt_encoding";"asn1crt_encoding_uper"]
                        | Asn1Encoding.ACN  -> ["asn1crt_encoding";"asn1crt_encoding_uper"; "asn1crt_encoding_acn"]
                        | Asn1Encoding.BER  -> ["asn1crt_encoding";"asn1crt_encoding_ber"]
                        | Asn1Encoding.XER  -> ["asn1crt_encoding";"asn1crt_encoding_xer"]
                    ) |>
                    List.distinct |>
                    List.map(fun a -> a + ".c", a + ".h") |>
                    List.unzip

                let arrsSrcTstFiles = (r.programUnits |> List.map (fun z -> z.testcase_bodyFileName))
                let arrsHdrTstFiles = (r.programUnits |> List.map (fun z -> z.testcase_specFileName))
                let vcprjContent = xml_outputs.emitVisualStudioProject
                                    ((r.programUnits |> List.map (fun z -> z.bodyFileName))@extrSrcFiles)
                                    ((r.programUnits |> List.map (fun z -> z.specFileName))@extrHdrFiles)
                                    (arrsSrcTstFiles@arrsSrcTstFilesX)
                                    (arrsHdrTstFiles@arrsHdrTstFilesX)
                let vcprjFileName = Path.Combine(outDir, "VsProject.vcxproj")
                File.WriteAllText(vcprjFileName, vcprjContent)

                //generate Visual Studio Solution file
                File.WriteAllText((Path.Combine(outDir, "VsProject.sln")), (aux_c.emitVisualStudioSolution()))


            CreateCMainFile r  di.srcDir
            generateVisualStudioProject r di.srcDir (arrsSrcTstFiles, arrsHdrTstFiles)
        //AlwaysPresentRtlFuncNames
        override this.getAlwaysPresentRtlFuncNames (args:CommandLineSettings) :  string list =
            let xerFuncs = 
                match args.encodings |> Seq.exists(fun e -> e = XER) with
                | false -> []
                | true  -> ["ByteStream_AttachBuffer";"ByteStream_Init"]
            [
                "ByteStream_GetLength"
                "BitStream_AttachBuffer2"
                "BitStream_AttachBuffer"
                "BitStream_Init"
                "BitStream_GetLength"
            ]@xerFuncs

        override this.RtlFuncNames : string list =
            [
                "BitStream_EncodeNonNegativeInteger32Neg"
                "BitStream_ReadByteArray"
                "GetLengthSIntHelper"
                "DecodeRealAsBinaryEncoding"
                "GetCharIndex"
                "Asn1Real_Equal"
                "ObjectIdentifier_Init"
                "ObjectIdentifier_equal"
                "ObjectIdentifier_isValid"
                "RelativeOID_isValid"
                "ByteStream_Init"
                "BitStream_Init2"
                "ByteStream_AttachBuffer"
                "BitStream_AttachBuffer2"
                "ByteStream_GetLength"
                "fetchData"
                "pushData"
                "bitstream_fetch_data_if_required"
                "bitstream_push_data_if_required"
                "BitString_equal"
                "BitStream_AppendNBitZero"
                "BitStream_EncodeNonNegativeInteger"
                "BitStream_AppendNBitOne"
                "BitStream_EncodeNonNegativeIntegerNeg"
                "BitStream_DecodeNonNegativeInteger"
                "BitStream_ReadPartialByte"
                "BitStream_AppendPartialByte"
                "BitStream_Init"
                "BitStream_AttachBuffer"
                "BitStream_AppendBit"
                "BitStream_AppendBits"
                "BitStream_AppendByte"
                "BitStream_AppendByte0"
                "BitStream_GetLength"
                "BitStream_AppendBitOne"
                "BitStream_AppendBitZero"
                "BitStream_PeekBit"
                "BitStream_ReadBit"
                "BitStream_ReadBits"
                "BitStream_ReadByte"
                "BitStream_EncodeUnConstraintWholeNumber"
                "BitStream_EncodeSemiConstraintWholeNumber"
                "BitStream_EncodeSemiConstraintPosWholeNumber"
                "BitStream_EncodeConstraintWholeNumber"
                "BitStream_EncodeConstraintPosWholeNumber"
                "BitStream_DecodeUnConstraintWholeNumber"
                "BitStream_DecodeSemiConstraintWholeNumber"
                "BitStream_DecodeSemiConstraintPosWholeNumber"
                "BitStream_DecodeConstraintWholeNumber"
                "BitStream_DecodeConstraintPosWholeNumber"
                "BitStream_DecodeConstraintWholeNumberInt8"
                "BitStream_DecodeConstraintWholeNumberInt16"
                "BitStream_DecodeConstraintWholeNumberInt32"
                "BitStream_DecodeConstraintPosWholeNumberUInt8"
                "BitStream_DecodeConstraintPosWholeNumberUInt16"
                "BitStream_DecodeConstraintPosWholeNumberUInt32"
                "GetNumberOfBitsForNonNegativeInteger"
                "CalculateMantissaAndExponent"
                "GetDoubleByMantissaAndExp"
                "GetLengthInBytesOfSInt"
                "GetLengthInBytesOfUInt"
                "BitStream_EncodeReal"
                "BitStream_DecodeReal"
                "BitStream_DecodeReal_fp32"
                "BitStream_AppendByteArray"
                "BitStream_EncodeOctetString_no_length"
                "BitStream_DecodeOctetString_no_length"
                "BitStream_EncodeOctetString_fragmentation"
                "BitStream_DecodeOctetString_fragmentation"
                "BitStream_EncodeOctetString"
                "BitStream_DecodeOctetString"
                "BitStream_EncodeBitString"
                "BitStream_DecodeBitString"
                "BitStream_checkBitPatternPresent"
                "BitStream_ReadBits_nullterminated"
                "Acn_AlignToNextByte"
                "Acn_AlignToNextWord"
                "Acn_AlignToNextDWord"
                "Acn_Enc_Int_PositiveInteger_ConstSize"
                "Acn_Enc_Int_PositiveInteger_ConstSize_8"
                "Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_16"
                "Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_32"
                "Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_64"
                "Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_16"
                "Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_32"
                "Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_64"
                "Acn_Enc_Int_PositiveInteger_VarSize_LengthEmbedded"
                "Acn_Enc_Int_TwosComplement_ConstSize"
                "Acn_Enc_Int_TwosComplement_ConstSize_8"
                "Acn_Enc_Int_TwosComplement_ConstSize_big_endian_16"
                "Acn_Enc_Int_TwosComplement_ConstSize_big_endian_32"
                "Acn_Enc_Int_TwosComplement_ConstSize_big_endian_64"
                "Acn_Enc_Int_TwosComplement_ConstSize_little_endian_16"
                "Acn_Enc_Int_TwosComplement_ConstSize_little_endian_32"
                "Acn_Enc_Int_TwosComplement_ConstSize_little_endian_64"
                "Acn_Enc_Int_TwosComplement_VarSize_LengthEmbedded"
                "Acn_Enc_Int_BCD_ConstSize"
                "Acn_Enc_Int_BCD_VarSize_LengthEmbedded"
                "Acn_Enc_Int_BCD_VarSize_NullTerminated"
                "Acn_Enc_SInt_ASCII_ConstSize"
                "Acn_Enc_SInt_ASCII_VarSize_LengthEmbedded"
                "Acn_Enc_SInt_ASCII_VarSize_NullTerminated"
                "Acn_Enc_UInt_ASCII_ConstSize"
                "Acn_Enc_UInt_ASCII_VarSize_LengthEmbedded"
                "Acn_Enc_UInt_ASCII_VarSize_NullTerminated"
                "Acn_Dec_Int_PositiveInteger_ConstSize"
                "Acn_Dec_Int_PositiveInteger_ConstSize_8"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64"
                "Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbedded"
                "Acn_Dec_Int_TwosComplement_ConstSize"
                "Acn_Dec_Int_TwosComplement_ConstSize_8"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64"
                "Acn_Dec_Int_TwosComplement_VarSize_LengthEmbedded"
                "Acn_Dec_Int_BCD_ConstSize"
                "Acn_Dec_Int_BCD_VarSize_LengthEmbedded"
                "Acn_Dec_Int_BCD_VarSize_NullTerminated"
                "Acn_Dec_SInt_ASCII_ConstSize"
                "Acn_Dec_SInt_ASCII_VarSize_LengthEmbedded"
                "Acn_Dec_SInt_ASCII_VarSize_NullTerminated"
                "Acn_Dec_UInt_ASCII_ConstSize"
                "Acn_Dec_UInt_ASCII_VarSize_LengthEmbedded"
                "Acn_Dec_UInt_ASCII_VarSize_NullTerminated"
                "Acn_Dec_Int_PositiveInteger_ConstSizeUInt8"
                "Acn_Dec_Int_PositiveInteger_ConstSizeUInt16"
                "Acn_Dec_Int_PositiveInteger_ConstSizeUInt32"
                "Acn_Dec_Int_PositiveInteger_ConstSize_8UInt8"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt16"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_16UInt8"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt32"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt16"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_32UInt8"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt32"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt16"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_64UInt8"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt16"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_16UInt8"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt32"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt16"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_32UInt8"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt32"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt16"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_64UInt8"
                "Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt8"
                "Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt16"
                "Acn_Dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt32"
                "Acn_Dec_Int_TwosComplement_ConstSizeInt8"
                "Acn_Dec_Int_TwosComplement_ConstSizeInt16"
                "Acn_Dec_Int_TwosComplement_ConstSizeInt32"
                "Acn_Dec_Int_TwosComplement_ConstSize_8Int8"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int16"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_16Int8"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int32"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int16"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_32Int8"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int32"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int16"
                "Acn_Dec_Int_TwosComplement_ConstSize_big_endian_64Int8"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int16"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_16Int8"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int32"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int16"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_32Int8"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int32"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int16"
                "Acn_Dec_Int_TwosComplement_ConstSize_little_endian_64Int8"
                "Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt8"
                "Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt16"
                "Acn_Dec_Int_TwosComplement_VarSize_LengthEmbeddedInt32"
                "Acn_Dec_Int_BCD_ConstSizeUInt8"
                "Acn_Dec_Int_BCD_ConstSizeUInt16"
                "Acn_Dec_Int_BCD_ConstSizeUInt32"
                "Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt8"
                "Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt16"
                "Acn_Dec_Int_BCD_VarSize_LengthEmbeddedUInt32"
                "Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt8"
                "Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt16"
                "Acn_Dec_Int_BCD_VarSize_NullTerminatedUInt32"
                "Acn_Dec_SInt_ASCII_ConstSizeInt8"
                "Acn_Dec_SInt_ASCII_ConstSizeInt16"
                "Acn_Dec_SInt_ASCII_ConstSizeInt32"
                "Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt8"
                "Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt16"
                "Acn_Dec_SInt_ASCII_VarSize_LengthEmbeddedInt32"
                "Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt8"
                "Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt16"
                "Acn_Dec_SInt_ASCII_VarSize_NullTerminatedInt32"
                "Acn_Dec_UInt_ASCII_ConstSizeUInt8"
                "Acn_Dec_UInt_ASCII_ConstSizeUInt16"
                "Acn_Dec_UInt_ASCII_ConstSizeUInt32"
                "Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt8"
                "Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt16"
                "Acn_Dec_UInt_ASCII_VarSize_LengthEmbeddedUInt32"
                "Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt8"
                "Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt16"
                "Acn_Dec_UInt_ASCII_VarSize_NullTerminatedUInt32"
                "BitStream_ReadBitPattern"
                "BitStream_ReadBitPattern_ignore_value"
                "BitStream_DecodeTrueFalseBoolean"
                "Acn_Enc_Real_IEEE754_32_big_endian"
                "Acn_Enc_Real_IEEE754_64_big_endian"
                "Acn_Enc_Real_IEEE754_32_little_endian"
                "Acn_Enc_Real_IEEE754_64_little_endian"
                "Acn_Dec_Real_IEEE754_32_big_endian"
                "Acn_Dec_Real_IEEE754_64_big_endian"
                "Acn_Dec_Real_IEEE754_32_little_endian"
                "Acn_Dec_Real_IEEE754_64_little_endian"
                "Acn_Dec_Real_IEEE754_32_big_endian_fp32"
                "Acn_Dec_Real_IEEE754_32_little_endian_fp32"
                "Acn_Enc_String_Ascii_FixSize"
                "Acn_Enc_String_Ascii_Null_Terminated"
                "Acn_Enc_String_Ascii_Null_Terminated_mult"
                "Acn_Enc_String_Ascii_External_Field_Determinant"
                "Acn_Enc_String_Ascii_Internal_Field_Determinant"
                "Acn_Enc_String_CharIndex_FixSize"
                "Acn_Enc_String_CharIndex_External_Field_Determinant"
                "Acn_Enc_String_CharIndex_Internal_Field_Determinant"
                "Acn_Enc_IA5String_CharIndex_External_Field_Determinant"
                "Acn_Enc_IA5String_CharIndex_Internal_Field_Determinant"
                "Acn_Dec_String_Ascii_FixSize"
                "Acn_Dec_String_Ascii_Null_Terminated"
                "Acn_Dec_String_Ascii_Null_Terminated_mult"
                "Acn_Dec_String_Ascii_External_Field_Determinant"
                "Acn_Dec_String_Ascii_Internal_Field_Determinant"
                "Acn_Dec_String_CharIndex_FixSize"
                "Acn_Dec_String_CharIndex_External_Field_Determinant"
                "Acn_Dec_String_CharIndex_Internal_Field_Determinant"
                "Acn_Dec_IA5String_CharIndex_External_Field_Determinant"
                "Acn_Dec_IA5String_CharIndex_Internal_Field_Determinant"
                "Acn_Enc_String_CharIndex_private"
                "Acn_Enc_Length"
                "Acn_Dec_Length"
                "milbus_encode"
                "milbus_decode"
                "ObjectIdentifier_uper_encode"
                "ObjectIdentifier_uper_decode"
                "RelativeOID_uper_encode"
                "RelativeOID_uper_decode"
                "Acn_Dec_String_CharIndex_private"
                "Acn_Dec_String_Ascii_private"
                "Acn_Enc_String_Ascii_private"
                "Acn_Get_Int_Size_BCD"
                "To_UInt"
                "Encode_UnsignedInteger"
                "Acn_Dec_Int_PositiveInteger_ConstSize_little_endian_N"
                "Acn_Dec_Int_PositiveInteger_ConstSize_big_endian_N"
                "Acn_Enc_Int_PositiveInteger_ConstSize_little_endian_N"
                "Acn_Enc_Int_PositiveInteger_ConstSize_big_endian_B"
                "RequiresReverse"
                "ObjectIdentifier_uper_decode_length"
                "ObjectIdentifier_subidentifiers_uper_decode"
                "ObjectIdentifier_subidentifiers_uper_encode"
                "BitStream_DecodeNonNegativeInteger32Neg"
             ]
        override this.detectFunctionCalls (sourceCode: string) (functionName: string) : string list =
            let knownCases =
                [
                    ("Acn_Enc_Real_IEEE754_32_little_endian", ["RequiresReverse";"BitStream_AppendByte0"])
                    ("Acn_Enc_Real_IEEE754_32_big_endian", ["RequiresReverse";"BitStream_AppendByte0"])
                    ("Acn_Enc_Real_IEEE754_64_big_endian", ["RequiresReverse";"BitStream_AppendByte0"])
                    ("Acn_Enc_Real_IEEE754_32_little_endian", ["RequiresReverse";"BitStream_AppendByte0"])
                    ("Acn_Enc_Real_IEEE754_64_little_endian", ["RequiresReverse";"BitStream_AppendByte0"])
                    ("Acn_Dec_Real_IEEE754_32_big_endian", ["RequiresReverse";"BitStream_ReadByte"])
                    ("Acn_Dec_Real_IEEE754_32_big_endian_fp32", ["RequiresReverse";"BitStream_ReadByte"])
                    ("Acn_Dec_Real_IEEE754_64_big_endian", ["RequiresReverse";"BitStream_ReadByte"])
                    ("Acn_Dec_Real_IEEE754_32_little_endian", ["RequiresReverse";"BitStream_ReadByte"])
                    ("Acn_Dec_Real_IEEE754_32_little_endian_fp32", ["RequiresReverse";"BitStream_ReadByte"])
                    ("Acn_Dec_Real_IEEE754_64_little_endian", ["RequiresReverse";"BitStream_ReadByte"])
                ] |> Map.ofList
            match knownCases |> Map.tryFind functionName  with
            | None ->
                let allFunctionNames = this.RtlFuncNames
                let pattern = @"[^\s]+\s+" + functionName + @"\s*\([^\)]*\)\s*\{"
                let regex = System.Text.RegularExpressions.Regex(pattern)
                let matches = regex.Matches(sourceCode)
                match matches.Count with
                | 0 -> []
                | _ ->
                    //get the function body
                    let matchValue = matches.[0]
                    let endIndex =
                        let mutable depth = 1
                        let mutable i = matchValue.Index + matchValue.Length
                        while depth > 0 && i < sourceCode.Length do
                            match sourceCode.[i] with
                            | '{' -> depth <- depth + 1
                            | '}' -> depth <- depth - 1
                            | _ -> ()
                            i <- i + 1
                        i
                    let startIndex =
                        let lineStartIndex = sourceCode.LastIndexOf('\n', matchValue.Index) + 1
                        if lineStartIndex > 0 then
                            lineStartIndex
                        else
                            0
                    let functionBody = sourceCode.Substring(startIndex, endIndex - startIndex)
                    //get all function calls
                    allFunctionNames |> List.filter (fun x -> functionName <> x &&  functionBody.Contains(x))
            | Some x -> x


        override this.removeFunctionFromHeader (sourceCode: string) (functionName: string) : string =
            let lines = sourceCode.Split([|'\n'|])
            let newLines = lines |> Seq.filter (fun x -> not (x.Contains(functionName)))
            newLines |> String.concat "\n"

        override this.removeFunctionFromBody (sourceCode: string) (functionName: string) : string =
            let pattern = @"[^\s]+\s+" + functionName + @"\s*\([^\)]*\)\s*\{"
            let regex = System.Text.RegularExpressions.Regex(pattern)
            let matches = regex.Matches(sourceCode)

            if matches.Count = 0 then
                sourceCode
            else
                let matchValue = matches.[0]
                let endIndex =
                    let mutable depth = 1
                    let mutable i = matchValue.Index + matchValue.Length
                    while depth > 0 && i < sourceCode.Length do
                        match sourceCode.[i] with
                        | '{' -> depth <- depth + 1
                        | '}' -> depth <- depth - 1
                        | _ -> ()
                        i <- i + 1
                    i
                let startIndex =
                    let lineStartIndex = sourceCode.LastIndexOf('\n', matchValue.Index) + 1
                    if lineStartIndex > 0 then
                        lineStartIndex
                    else
                        0
                sourceCode.Remove(startIndex, endIndex - startIndex)

        override this.getDirInfo (target:Targets option) rootDir =
            {rootDir = rootDir; srcDir=rootDir;asn1rtlDir=rootDir;boardsDir=rootDir}

        override this.getTopLevelDirs (target:Targets option) = []

        override this.getChChildIsPresent   (arg:AccessPath) (chParent:string) (pre_name:string) =
            sprintf "%s%skind %s %s_PRESENT" (arg.joined this) (this.getAccess arg) this.eqOp pre_name
