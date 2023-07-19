module LangGeneric_scala
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open Language
open System.IO

let getAccess_scala  (fpt:FuncParamType) =
    match fpt with
    | VALUE x        -> "."
    | POINTER x      -> "."
    | FIXARRAY x     -> ""

#if false
let createBitStringFunction_funcBody_c handleFragmentation (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefintionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErroCode) (p:CallerScope) = 
    let ii = id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" (id.SeqeuenceOfLevel + 1)
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


type LangGeneric_scala() =
    inherit ILangGeneric()
        override _.ArrayStartIndex = 0

        override _.intValueToString (i:BigInteger) (intClass:Asn1AcnAst.IntegerClass) =
            match intClass with
            | Asn1AcnAst.ASN1SCC_Int8     _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int16    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int32    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int64    _ ->  sprintf "%sL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int      _ ->  sprintf "%sL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt8    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt16   _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt32   _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt64   _ ->  sprintf "%sL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt     _ ->  sprintf "%sL" (i.ToString())

        override _.doubleValueToString (v:double) = 
            v.ToString(FsUtils.doubleParseString, System.Globalization.NumberFormatInfo.InvariantInfo)

        override _.initializeString stringSize = sprintf "{ [0 ... %d] = 0x0 }" stringSize
        
        override _.supportsInitExpressions = false

        override _.getPointer  (fpt:FuncParamType) =
            match fpt with
            |VALUE x        -> sprintf "%s" x
            |POINTER x      -> sprintf "%s" x
            |FIXARRAY x     -> sprintf "%s" x

        override this.getValue (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> x
            | POINTER x      -> sprintf "%s" x
            | FIXARRAY x     -> sprintf "%s" x

        override this.getAccess  (fpt:FuncParamType) = getAccess_scala fpt

        override this.ArrayAccess idx = "(" + idx + ")"

        override this.getPtrPrefix (fpt: FuncParamType) = 
            match fpt with
            | VALUE x        -> "Ref["
            | POINTER x      -> "Ref["
            | FIXARRAY x     -> "Ref["

        override this.getPtrSuffix (fpt: FuncParamType) = 
            match fpt with
            | VALUE x        -> "]"
            | POINTER x      -> "]"
            | FIXARRAY x     -> "]"

        override this.getStar  (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> ""
            | POINTER x      -> "Ref[]"
            | FIXARRAY x     -> ""

        override this.getArrayItem (fpt:FuncParamType) (idx:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s%sarr(%s)" fpt.p (this.getAccess fpt) idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        
        override this.getNamedItemBackendName (defOrRef:TypeDefintionOrReference option) (nm:Asn1AcnAst.NamedItem) = 
            let itemname = 
                match defOrRef with
                | Some (TypeDefinition td) -> td.typedefName + "." + ToC nm.scala_name
                | _ -> ToC nm.scala_name
            itemname

        override this.getNamedItemBackendName2 (_:string) (_:string) (nm:Asn1AcnAst.NamedItem) = 
            ToC nm.scala_name
            
        override this.decodeEmptySeq _ = None
        override this.decode_nullType _ = None

        override this.Length exp sAcc =
            isvalid_scala.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[Scala]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[Scala]
        override this.getEnmTypeDefintion (td:Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) = td.[Scala]
        override this.getStrTypeDefinition (td:Map<ProgrammingLanguage, FE_StringTypeDefinition>) = td.[Scala]
        override this.getChoiceTypeDefinition (td:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) = td.[Scala]
        override this.getSequenceTypeDefinition (td:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) = td.[Scala]
        override this.getSizeableTypeDefinition (td:Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) = td.[Scala]
        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._scala_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._scala_name        
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._scala_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._scala_name

        override this.getRtlFiles  (encodings:Asn1Encoding list) (_ :string list) =
            let encRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN ) with true -> ["asn1crt_encoding"] | false -> []
            let uperRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN) with true -> ["asn1crt_encoding_uper"] | false -> []
            let acnRtl = match encodings |> Seq.exists(fun e -> e = ACN) with true -> ["asn1crt_encoding_acn"] | false -> []
            let xerRtl = match encodings |> Seq.exists(fun e -> e = XER) with true -> ["asn1crt_encoding_xer"] | false -> []
            encRtl@uperRtl@acnRtl@xerRtl

        override this.getEmptySequenceInitExpression () = "{}"
        override this.callFuncWithNoArgs () = "()"
        override this.rtlModuleName  = ""
        override this.AssignOperator = "="
        override this.TrueLiteral = "true"
        override this.FalseLiteral = "false"
        override this.emtyStatement = ""
        override this.bitStreamName = "BitStream"
        override this.unaryNotOperator    = "!"  
        override this.modOp               = "%"  
        override this.eqOp                = "==" 
        override this.neqOp               = "!=" 
        override this.andOp               = "&&" 
        override this.orOp                = "||" 
        override this.initMetod           = InitMethod.Procedure

        override this.castExpression (sExp:string) (sCastType:string) = sprintf "(%s)(%s)" sCastType sExp
        override this.createSingleLineComment (sText:string) = sprintf "/*%s*/" sText
         
        override _.SpecNameSuffix = "Def"
        override _.SpecExtention = "scala"
        override _.BodyExtention = "scala"

        override _.getValueAssignmentName (vas: ValueAssignment) = vas.scala_name

        override this.hasModules = false
        override this.allowsSrcFilesWithNoFunctions = true
        override this.requiresValueAssignmentsInSrcFile = true
        override this.supportsStaticVerification = false
        
        override this.getSeqChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s%s%s" fpt.p (this.getAccess fpt) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.getChChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) : FuncParamType =
            let newPath = sprintf "e" // always use e for pattern matching
            //let newPath = sprintf "%s%s%s" fpt.p (this.getAccess fpt) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
            
        override this.choiceIDForNone (typeIdsSet:Map<string,int>) (id:ReferenceToType) =  
            let prefix = ToC (id.AcnAbsPath.Tail.StrJoin("_").Replace("#","elem"))
            match typeIdsSet.TryFind prefix with
            | None  -> prefix + "_NONE" 
            | Some a when a = 1 -> prefix + "_NONE" 
            | Some a            -> ToC (id.AcnAbsPath.StrJoin("_").Replace("#","elem")) + "_NONE" 

        override this.presentWhenName (defOrRef:TypeDefintionOrReference option) (ch:ChChildInfo) : string =
            let parentName = 
                match defOrRef with
                | Some a -> match a with
                            | ReferenceToExistingDefinition b -> ""
                            | TypeDefinition c -> c.typedefName + "."
                | None -> ""
            parentName + (ToC ch._present_when_name_private) + "_PRESENT"
        
        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CallerScope =
            match c with
            | Encode  ->
                match t.Kind with
                | Asn1AcnAst.Integer         _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.Real            _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf)    }
                | Asn1AcnAst.IA5String       _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("pVal" + suf) }
                | Asn1AcnAst.NumericString   _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("pVal" + suf) }
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
                | Asn1AcnAst.ReferenceType r -> this.getParamTypeSuffix r.resolvedType suf c
            | Decode  ->
                match t.Kind with
                | Asn1AcnAst.Integer            _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.Real               _ -> {CallerScope.modName = t.id.ModName; arg= POINTER ("pVal" + suf) }
                | Asn1AcnAst.IA5String          _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("pVal" + suf) }
                | Asn1AcnAst.NumericString      _ -> {CallerScope.modName = t.id.ModName; arg= FIXARRAY ("pVal" + suf) }
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
        
        override this.getParamValue (t:Asn1AcnAst.Asn1Type) (p:FuncParamType) (c:Codec) =
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
                | Asn1AcnAst.ReferenceType r -> this.getParamValue r.resolvedType p c
            | Decode  ->
                match t.Kind with
                | Asn1AcnAst.IA5String    _  -> this.getValue p //FIXARRAY "val"
                | Asn1AcnAst.NumericString _ -> this.getValue p// FIXARRAY "val"
                | Asn1AcnAst.ReferenceType r -> this.getParamValue r.resolvedType p c
                | _                          -> this.getPointer p

        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "var i%d: Int = 0" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "var i%d: Int = %d" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "var %s: Int = 0" name
            | IntegerLocalVariable (name,Some iv)       -> sprintf "var %s: Int = %d" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "var %s: Int = 0" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "var %s: Int = %d" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "var %s: UInt = 0" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "var %s: UInt = %d" name iv
            | FlagLocalVariable (name,None)             -> sprintf "var %s: Boolean = false" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "var %s: Boolean = %d" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "var %s: Boolean = false" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "var %s: Boolean = %s" name (if iv then "true" else "false")
            | AcnInsertedChild(name, vartype)           -> sprintf "var %s: %s" name vartype
            
            | GenericLocalVariable lv                   ->
                sprintf "var %s%s: %s%s = %s" (if lv.isStatic then "static " else "") lv.name lv.varType (if lv.arrSize.IsNone then "" else "["+lv.arrSize.Value+"]") (if lv.initExp.IsNone then "" else lv.initExp.Value)

            
        override this.getLongTypedefName (tdr:TypeDefintionOrReference) : string =
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
                seqof_lv              =
                  (fun id minSize maxSize -> [SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)])
            }
        override this.acn = 
            {
                Acn_parts.null_valIsUnReferenced = true
                checkBitPatternPresentResult = true
                getAcnDepSizeDeterminantLocVars = 
                    fun  sReqBytesForUperEncoding ->
                        [
                            GenericLocalVariable {GenericLocalVariable.name = "arr"; varType = "byte"; arrSize = Some sReqBytesForUperEncoding; isStatic = true; initExp = None}
                            GenericLocalVariable {GenericLocalVariable.name = "bitStrm"; varType = "BitStream"; arrSize = None; isStatic = false; initExp = None}
                        ]
                choice_handle_always_absent_child = false
                choice_requires_tmp_decoding = false
            }
        override this.init = 
            {
                Initialize_parts.zeroIA5String_localVars    = fun _ -> []
                choiceComponentTempInit                     = false
            }
        override this.atc =
            {
                Atc_parts.uperPrefix = ""
                acnPrefix            = "ACN_"
                xerPrefix            = "XER_"
                berPrefix            = "BER_"
            }

        override this.CreateMakeFile (r:AstRoot)  (di:OutDirectories.DirInfo) =
            let files = r.Files |> Seq.map(fun x -> (Path.GetFileNameWithoutExtension x.FileName).ToLower() )
            let content = aux_scala.PrintMakeFile files (r.args.integerSizeInBytes = 4I) (r.args.floatingPointSizeInBytes = 4I) r.args.streamingModeSupport
            let outFileName = Path.Combine(di.srcDir, "Makefile")
            File.WriteAllText(outFileName, content.Replace("\r",""))

        override this.CreateAuxFiles (r:AstRoot)  (di:OutDirectories.DirInfo) (arrsSrcTstFiles : string list, arrsHdrTstFiles:string list) =
            let CreateScalaMainFile (r:AstRoot)  outDir  =
                // Main file for test cass    
                let printMain =    test_cases_scala.PrintMain //match l with C -> test_cases_c.PrintMain | Ada -> test_cases_c.PrintMain
                let content = printMain "testsuite"
                let outFileName = Path.Combine(outDir, "mainprogram.scala")
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
                    List.map(fun a -> a + ".scala", a + "Def.scala") |>
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
                File.WriteAllText((Path.Combine(outDir, "VsProject.sln")), (aux_scala.emitVisualStudioSolution()))


            CreateScalaMainFile r di.srcDir
            generateVisualStudioProject r di.srcDir (arrsSrcTstFiles, arrsHdrTstFiles)
