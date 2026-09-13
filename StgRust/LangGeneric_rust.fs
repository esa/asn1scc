module LangGeneric_rust
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open Language
open System.IO
open System

let getAccess_rust (sel: AccessPath) =
    "."  // Rust uses . for all member access, no -> like C

let getAccess2_rust (acc: AccessStep) =
    match acc with
    | ValueAccess (sel, _, _) -> $".{sel}"
    | PointerAccess (sel, _, _) -> $".{sel}"
    | ArrayAccess (ix, _) -> $"[{ix}]"

type LangBasic_rust() =
    inherit ILangBasic()
        override this.cmp (s1:string) (s2:string) = s1 = s2
        override this.isCaseSensitive = true
        override this.keywords = rust_keywords
        override this.isKeyword (token) = rust_keywords.Contains token
        override this.OnTypeNameConflictTryAppendModName = true
        override this.declare_IntegerNoRTL = "", "i64", "INTEGER"
        override this.declare_PosIntegerNoRTL = "", "u64", "INTEGER"
        override this.getRealRtlTypeName   = "", "f64", "REAL"
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
        override this.getNullRtlTypeName  = "", "()", "NULL"
        override this.getBoolRtlTypeName = "","bool","BOOLEAN"


type LangGeneric_rust() =
    inherit ILangGeneric()
        override this.isObjectOriented = false
        override this.nullTerminatorByte = Some 0uy

        override _.ArrayStartIndex = 0

        override _.intValueToString (i:BigInteger) (intClass:Asn1AcnAst.IntegerClass) =
            match intClass with
            | Asn1AcnAst.ASN1SCC_Int8     _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int16    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int32    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int64    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int      _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt8    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt16   _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt32   _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt64   _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt     _ ->  sprintf "%s" (i.ToString())

        override _.asn1SccIntValueToString (i: BigInteger) _ = i.ToString()
        override _.doubleValueToString (v:double) =
            v.ToString(FsUtils.doubleParseString, System.Globalization.NumberFormatInfo.InvariantInfo)

        override _.initializeString (asciiCode:BigInteger option) stringSize =
            match asciiCode with
            | Some ac -> sprintf "{ let mut _v: [u8; %d] = [0; %d]; _v[%d] = 0x%X; _v }" (stringSize+1) (stringSize+1) stringSize (int ac)
            | None ->   sprintf "[0; %d]" (stringSize+1)

        override _.supportsInitExpressions = true
        override _.requiresHandlingOfEmptySequences = true
        override _.requiresHandlingOfZeroArrays = true


        override this.getPointer (sel: AccessPath) =
            // For Rust, getPointer returns the dereferenced path for ByPointer (top-level
            // &mut parameters) and the raw path for all other selection types.
            // - ByPointer: `*pVal` — correct for decode assignment targets (e.g. `*pVal = val`)
            // - ByValue/ArrayElem: `pVal.child` — correct for decode assignment (e.g. `pVal.child = val`)
            // Call sites that need a reference (`&`, `&mut`) must add the prefix in STG templates.
            let str = sel.joined this
            match sel.selectionType with
            | ByPointer -> $"*{str}"
            | _ -> str

        override this.getValue (sel: AccessPath) =
            let str = sel.joined this
            match sel.selectionType with
            | ByPointer -> $"*{str}"
            | _ -> str

        override this.getValueUnchecked (sel: AccessPath) _ = this.getValue sel
        override this.getPointerUnchecked (sel: AccessPath) _ = this.getPointer sel
        override this.joinSelectionUnchecked (sel: AccessPath) _ = sel.joined this
        override this.getAccess  (sel: AccessPath) = getAccess_rust sel

        override this.getAccess2 (acc: AccessStep) = getAccess2_rust acc
        override this.getPtrPrefix _ = ""

        override this.getPtrSuffix (sel: AccessPath) =
            match sel.selectionType with
            | ByPointer -> ""
            | _ -> ""

        override this.getStar (sel: AccessPath) =
            match sel.selectionType with
            | ByPointer -> ""
            | _ -> ""

        override this.setNamedItemBackendName0 (nm:Asn1Ast.NamedItem) (newValue:string) : Asn1Ast.NamedItem =
            {nm with rust_name = newValue}
        override this.getNamedItemBackendName0 (nm:Asn1Ast.NamedItem)  = nm.rust_name

        override this.getArrayItem (sel: AccessPath) (idx:string) (childTypeIsString: bool) =
            (sel.appendSelection "arr" ArrayElem false).append (ArrayAccess (idx, if childTypeIsString then ArrayElem else ByValue))

        override this.getNamedItemBackendName (defOrRef:TypeDefinitionOrReference option) (nm:Asn1AcnAst.NamedItem) =
            match defOrRef with
            | Some dr -> sprintf "%s::%s" (this.getLongTypedefName dr) (ToC nm.rust_name)
            | None -> ToC nm.rust_name
        override this.getNamedItemBackendName2 (modName:string) (curProgramUnitName:string) (nm:Asn1AcnAst.NamedItem) =
            ToC nm.rust_name
        override this.decodeEmptySeq _ = None
        override this.decode_nullType _ = None

        override this.Length exp sAcc =
            isvalid_rust.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[Rust]
        override this.definitionOrRef (d:Map<ProgrammingLanguage, TypeDefinitionOrReference>) = d.[Rust]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[Rust]
        override this.getEnumTypeDefinition (td:Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) = td.[Rust]
        override this.getStrTypeDefinition (td:Map<ProgrammingLanguage, FE_StringTypeDefinition>) = td.[Rust]
        override this.getChoiceTypeDefinition (td:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) = td.[Rust]
        override this.getSequenceTypeDefinition (td:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) = td.[Rust]
        override this.getSizeableTypeDefinition (td:Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) = td.[Rust]

        override _.getChildInfoName (ch:Asn1Ast.ChildInfo)  = ch.rust_name
        override _.setChildInfoName (ch:Asn1Ast.ChildInfo) (newValue:string) = {ch with rust_name = newValue}
        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._rust_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._rust_name
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._rust_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._rust_name
        override _.getChoiceChildPresentWhenName (ch:Asn1AcnAst.Choice ) (c:Asn1AcnAst.ChChildInfo) (_currentModule:string) : string =
            let name = ToC c.present_when_name
            if name.EndsWith("_PRESENT") then name.Substring(0, name.Length - 8) else name

        override this.getRtlFiles  (encodings:Asn1Encoding list) (_ :string list) =
            // Rust RTL is a single crate (asn1rust); no separate files per encoding.
            // Return empty list — the Rust build uses Cargo, not individual file includes.
            []


        override this.getEmptySequenceInitExpression sTypeDefName = $"{sTypeDefName} {{}}"
        override this.callFuncWithNoArgs () = "()"
        override this.rtlModuleName  = ""
        override this.AssignOperator = "="
        override this.TrueLiteral = "true"
        override this.FalseLiteral = "false"
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
        override _.padArraysWithDefaultValues = true
        override _.amberDecodePrefix = "&mut "
        override this.castExpression (sExp:string) (sCastType:string) = sprintf "%s as %s" sExp sCastType
        override this.createSingleLineComment (sText:string) = sprintf "// %s" sText

        override _.SpecNameSuffix = "Def"
        override _.SpecExtension = "rs"
        override _.BodyExtension = "rs"
        override _.Keywords  = CommonTypes.rust_keywords


        override _.getValueAssignmentName (vas: ValueAssignment) = vas.rust_name

        override this.hasModules = false
        override _.usesBooleanPresenceBits = true
        override this.allowsSrcFilesWithNoFunctions = true
        override this.requiresValueAssignmentsInSrcFile = true
        override this.supportsStaticVerification = false

        override this.getSeqChildIsPresent (sel: AccessPath) (childName:string) =
            sprintf "%s%s%s" (sel.joined this) (this.getAccess sel) ("exist." + childName)

        override this.getSeqChild (sel: AccessPath) (childName:string) (childTypeIsString: bool) (childIsOptional: bool) =
            sel.appendSelection childName (if childTypeIsString then ArrayElem else ByValue) childIsOptional

        override this.getChChild (sel: AccessPath) (childName:string) (childTypeIsString: bool): AccessPath =
            AccessPath.emptyPath childName (if childTypeIsString then ArrayElem else ByPointer)

        override this.choiceIDForNone (typeIdsSet:Map<string,int>) (id:ReferenceToType) =
            let prefix = ToC ((id.AcnAbsPath.Tail |> Seq.StrJoin("_")).Replace("#","elem"))
            match typeIdsSet.TryFind prefix with
            | None  -> prefix + "_NONE"
            | Some a when a = 1 -> prefix + "_NONE"
            | Some a            -> ToC ((id.AcnAbsPath |> Seq.StrJoin("_")).Replace("#","elem")) + "_NONE"

        override this.presentWhenName (defOrRef:TypeDefinitionOrReference option) (ch:ChChildInfo) : string =
            ToC ch._present_when_name_private
        override this.presentWhenName0 (defOrRef:TypeDefinitionOrReference option) (ch:Asn1AcnAst.ChChildInfo) : string =
            let name = ToC ch.present_when_name
            if name.EndsWith("_PRESENT") then name.Substring(0, name.Length - 8) else name
        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CodegenScope =
            let rec getRecvType (kind: Asn1AcnAst.Asn1TypeKind) =
                match kind with
                | Asn1AcnAst.ReferenceType r -> getRecvType r.resolvedType.Kind
                | _ -> ByPointer
            let recvId = "pVal" + suf
            {CodegenScope.modName = t.id.ModName; accessPath = AccessPath.emptyPath recvId (getRecvType t.Kind) }

        override this.getParamValue  (t:Asn1AcnAst.Asn1Type) (sel: AccessPath)  (c:Codec) =
            match t.Kind with
            | Asn1AcnAst.IA5String    _  -> this.getValue sel
            | Asn1AcnAst.NumericString _ -> this.getValue sel
            | Asn1AcnAst.ReferenceType r -> this.getParamValue r.resolvedType sel  c
            | _                          ->
                match c with
                | Decode -> this.getPointer sel
                | Encode -> this.getPointer sel

        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "let mut i%d: usize = 0;" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "let mut i%d: usize = %s;" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "let mut %s: i32 = 0;" name
            | IntegerLocalVariable (name,Some iv)      -> sprintf "let mut %s: i32 = %s;" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "let mut %s: i64 = 0;" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "let mut %s: i64 = %s;" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "let mut %s: u64 = 0;" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "let mut %s: u64 = %s;" name iv
            | FlagLocalVariable (name,None)             -> sprintf "let mut %s: bool = false;" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "let mut %s: bool = %s;" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "let mut %s: bool = false;" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "let mut %s: bool = %s;" name iv
            | AcnInsertedChild(name, vartype, initVal)  ->
                let initExpr =
                    if initVal.StartsWith("[") && not (vartype.StartsWith("[")) then "Default::default()"
                    elif initVal = "" then "Default::default()"
                    else initVal
                sprintf "let mut %s: %s = %s;" name vartype initExpr
            | GenericLocalVariable lv                   ->
                let prefix = if lv.isStatic then "static mut " else "let mut "
                let varType = if lv.arrSize.IsNone then lv.varType else sprintf "[%s; %s]" lv.varType lv.arrSize.Value
                match lv.initExp with
                | Some initVal -> sprintf "%s%s: %s = %s;" prefix lv.name varType initVal
                | None ->
                    let initVal =
                        match lv.varType with
                        | "u64" | "i64" | "u32" | "i32" | "u16" | "i16" | "u8" | "i8" | "usize" | "isize" -> " = 0"
                        | "bool" -> " = false"
                        | _ -> ""
                    if lv.isStatic then
                        sprintf "%s%s: %s = Default::default();" prefix lv.name varType
                    else
                        sprintf "%s%s: %s%s;" prefix lv.name varType initVal


        override this.getLongTypedefName (tdr:TypeDefinitionOrReference) : string =
            match tdr with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref -> ref.typedefName

        override this.toHex n = sprintf "0x%x" n

        override this.bitStringValueToByteArray (v : BitStringValue) = FsUtils.bitStringValueToByteArray (StringLoc.ByValue v)

        override this.uper =
            {
                Uper_parts.createLv = (fun name -> Asn1SIntLocalVariable(name,None))
                requires_sBlockIndex  = true
                requires_sBLJ = false
                requires_charIndex = false
                requires_IA5String_i = true
                count_var            = Asn1SIntLocalVariable ("n_count", None)
                requires_presenceBit = true
                catd                 = true
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
                            GenericLocalVariable {GenericLocalVariable.name = "arr"; varType = "u8"; arrSize = Some sReqBytesForUperEncoding; isStatic = false; initExp = Some (sprintf "[0u8; %s]" sReqBytesForUperEncoding)}
                            GenericLocalVariable {GenericLocalVariable.name = "bit_strm"; varType = "BitStream"; arrSize = None; isStatic = false; initExp = None}
                        ]
                choice_handle_always_absent_child = false
                createLocalVariableEnum =
                    (fun rtlIntType -> GenericLocalVariable {GenericLocalVariable.name = "int_val"; varType= rtlIntType; arrSize= None; isStatic = false; initExp=None })
                choice_requires_tmp_decoding = true
            }
        override this.init =
            {
                Initialize_parts.zeroIA5String_localVars    = fun _ -> []
                zeroOctetString_localVars                   = fun _ -> []
                zeroBitString_localVars                     = fun _ -> []
                choiceComponentTempInit                     = true
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
            let content = aux_rust.PrintMakeFile files (r.args.integerSizeInBytes = 4I) (r.args.floatingPointSizeInBytes = 4I) r.args.streamingModeSupport
            let outFileName = Path.Combine(di.srcDir, "Makefile")
            File.WriteAllText(outFileName, content.Replace("\r",""))

        override this.CreateAuxFiles (r:AstRoot)  (di:DirInfo) (arrsSrcTstFiles : string list, arrsHdrTstFiles:string list) =
            let CreateRustMainFile (r:AstRoot)  outDir  =
                //Main file for test cases
                let printMain = test_cases_rust.PrintMain
                let formatMod (modName: string) =
                    if String.IsNullOrEmpty(modName) then ""
                    elif Char.IsDigit(modName.[0]) then
                        sprintf "#[path = \"%s.rs\"] mod _%s;\n#[path = \"%sDef.rs\"] mod _%sDef;" modName modName modName modName
                    else
                        sprintf "mod %s;\nmod %sDef;" modName modName
                let pduMods = r.programUnits |> List.collect (fun pu -> [formatMod pu.name; formatMod pu.testcase_name])
                let tcMods = arrsSrcTstFiles |> List.map (fun f -> formatMod (Path.GetFileNameWithoutExtension(f)))
                let content = printMain "testsuite" (pduMods @ tcMods)
                let outFileName = Path.Combine(outDir, "mainprogram.rs")
                File.WriteAllText(outFileName, content.Replace("\r",""))

            CreateRustMainFile r  di.srcDir

        //AlwaysPresentRtlFuncNames
        override this.getAlwaysPresentRtlFuncNames (args:CommandLineSettings) :  string list =
            []

        override this.RtlFuncNames : string list =
            []

        override this.detectFunctionCalls (sourceCode: string) (functionName: string) : string list =
            []
        override this.removeFunctionFromHeader (sourceCode: string) (functionName: string) : string =
            sourceCode
        override this.removeFunctionFromBody (sourceCode: string) (functionName: string) : string =
            sourceCode

        override this.getDirInfo (target:Targets option) rootDir =
            // Runtime .rs files go to rootDir/asn1rust/src/ so the runtime is a
            // separate crate (asn1rust) that the user's binary project depends on.
            {rootDir = rootDir; srcDir=rootDir;asn1rtlDir=Path.Combine(rootDir, "asn1rust", "src");boardsDir=rootDir}

        override this.getTopLevelDirs (target:Targets option) =
            [Path.Combine("asn1rust", "src")]

        override this.getChChildIsPresent   (arg:AccessPath) (chParent:string) (pre_name:string) =
            let idx =
                let mutable found = -1
                let mutable i = 0
                while i < pre_name.Length - 1 && found < 0 do
                    if pre_name.[i] = '_' && System.Char.IsLower(pre_name.[i+1]) then
                        found <- i
                    i <- i + 1
                if found >= 0 then found
                else pre_name.LastIndexOf('_')
            let vPat =
                if System.String.IsNullOrEmpty(chParent) then
                    if idx > 0 then sprintf "ASN1SCC_%s::%s" (pre_name.Substring(0, idx)) pre_name
                    else pre_name
                else sprintf "%s::%s" chParent pre_name
            let varName =
                if idx > 0 then pre_name.Substring(idx + 1) else pre_name
            sprintf "let %s(ref %s) = %s" vPat varName (arg.joined this)

        override this.wrapIA5StringValue typeRef modName literal =
            this.getQualifiedTypeName typeRef modName + " { arr: " + literal + " }"

        override _.formatEnumValueInit (enumTd: FE_EnumeratedTypeDefinition) itemCName _defaultValue =
            let typeNameNoPrefix = if enumTd.typeName.StartsWith("ASN1SCC_") then enumTd.typeName.Substring(8) else enumTd.typeName
            enumTd.typeName + "::" + typeNameNoPrefix + "_" + itemCName

        override _.charToNumericValueExpression charValue = sprintf "b%s" charValue

        override _.charLiteralFromAsciiCode asciiCode =
            sprintf "b'%c'" (char (int asciiCode))

        // ─────────────────────────────────────────────────────────────────────
        // Overrides for the language-specific ILangGeneric members added to
        // eliminate hard-coded `match ProgrammingLanguage.ActiveLanguages.Head
        // with | Rust -> ...` blocks in the F# backend.
        // Each override mirrors the Rust-specific behaviour that was previously
        // inlined in DAstVariables.fs, DastValidate2.fs, DastTestCaseCreation.fs,
        // DAstInitialize.fs, DAstEqual.fs, DAstTypeDefinition.fs, etc.
        // ─────────────────────────────────────────────────────────────────────

        /// Rust null-type init literal is the unit value `()`.
        override _.nullTypeInitExpression = "()"

        /// Rust complex-type default init is `Default::default()`.
        override _.complexTypeDefaultInit = "Default::default()"

        /// Rust uses no function-definition annotations.
        override _.funcDefAnnotations _ = []

        /// Wrap an IA5String constant-init in a struct literal when a typedef
        /// name is available, otherwise return the init string as-is.
        override _.wrapIA5StringConstantInit tdName initStr =
            if tdName = "" then initStr
            else sprintf "%s { arr: %s }" tdName initStr

        /// Rust uses inline init expressions (`initExpressionFnc()`) rather
        /// than function-call form for complex child types.
        override _.useInlineInitExpression = true

        /// Rust pads byte arrays to their maximum size in value literals.
        /// (The existing `padArraysWithDefaultValues` is also `true` for Rust,
        /// but it is a separate abstract member without a default; this member
        /// is the one with a default used by the value-literal code path.)
        override _.padByteArraysToMaxSize = true

        /// Rust wraps optional values in `Some(...)`.
        override _.wrapOptionalValueInSome childValue =
            sprintf "Some(%s)" childValue

        /// Rust absent optional literal is `None`.
        override _.absentOptionalExpression = "None"

        /// Rust uses `&` (borrow) for IA5String test-case ambers; all other
        /// types use the caller's default (handled by the call site, not here).
        override _.getAmberForType kind =
            match kind with
            | Asn1AcnAst.IA5String _ -> ("&", "&")
            | _ -> ("", "")

        /// Rust validation literals for character sets are byte-slice `b"..."`
        /// literals, with escape sequences for control characters.
        override _.charSetToValidationLiteral (v: string) =
            if v.Length > 1 then
                sprintf "b\"%s\"" v
            elif v.Length = 1 then
                let c = v.ToCharArray().[0]
                if   c = CommonTypes.CharCR  then "b\"\\r\""
                elif c = CommonTypes.CharLF  then "b\"\\n\""
                elif c = CommonTypes.CharHT  then "b\"\\t\""
                elif c = CommonTypes.CharNul then "b\"\\0\""
                else sprintf "b\"%c\"" c
            else
                "b\"\""

        /// Rust choice-child comparison temp vars use suffixes "1" and "2".
        /// `childName` is already the backend child name (passed by the call
        /// site via `getAsn1ChChildBackendName0 o`).
        override _.getChoiceChildComparisonNames _ _ _ childName =
            (childName + "1", childName + "2")

        /// Rust uses the default `extractDefaultInitValue` for choice test-case
        /// init — no override needed (default returns "" which triggers that path).

        /// Rust generates a `Default` impl for each enumerated type.
        override _.generateEnumDefaultImpl typeName firstEnumName =
            sprintf "impl Default for %s { fn default() -> Self { %s::%s } }" typeName typeName firstEnumName

        /// Rust ACN decode functions use snake_case suffixes (_i8, _i16, etc.)
        /// instead of the C-style PascalCase suffixes (Int8, Int16, etc.).
        override _.getIntDecFuncSuffix intClass =
            match intClass with
            | Asn1AcnAst.ASN1SCC_Int8      _ -> "_i8"
            | Asn1AcnAst.ASN1SCC_Int16     _ -> "_i16"
            | Asn1AcnAst.ASN1SCC_Int32     _ -> "_i32"
            | Asn1AcnAst.ASN1SCC_Int64     _ -> ""
            | Asn1AcnAst.ASN1SCC_Int       _ -> ""
            | Asn1AcnAst.ASN1SCC_UInt8     _ -> "_u8"
            | Asn1AcnAst.ASN1SCC_UInt16    _ -> "_u16"
            | Asn1AcnAst.ASN1SCC_UInt32    _ -> "_u32"
            | Asn1AcnAst.ASN1SCC_UInt64    _ -> ""
            | Asn1AcnAst.ASN1SCC_UInt      _ -> ""





/// LanguageMacros wiring for the Rust backend.
/// Models after `c_macro` in Program.fs:165.
/// The implementation class modules (IInit_rust, IEqual_rust, etc.) are generated
/// from the .stg files by parseStg2 (reading backends.xml) at build time.
/// The .stg.fs files won't exist until the MSBuild target runs.
let rust_macro =
        {
            LanguageMacros.equal   = new IEqual_rust.IEqual_rust()
            init                    = new IInit_rust.IInit_rust()
            typeDef                 = new ITypeDefinition_rust.ITypeDefinition_rust()
            lg                      = new LangGeneric_rust()
            isvalid                 = new IIsValid_rust.IIsValid_rust()
            vars                    = new IVariables_rust.IVariables_rust()
            uper                    = new IUper_rust.IUper_rust()
            acn                     = new IAcn_rust.IAcn_rust()
            atc                     = new ITestCases_rust.ITestCases_rust()
            xer                     = new IXer_rust.IXer_rust()
            src                     = new ISrcBody_rust.ISrcBody_rust()
            encodings               = []
        }
