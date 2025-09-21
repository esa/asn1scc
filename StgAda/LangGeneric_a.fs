module LangGeneric_a
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open Language
open System.IO
open System

(****** Ada Implementation ******)


let getAccess2_a  (acc: AccessStep) =
    match acc with
    | ValueAccess (sel, _, _) -> $".{sel}"
    | PointerAccess (sel, _, _) -> $".{sel}"
    | ArrayAccess (ix, _) -> $"({ix})"

#if false
let createBitStringFunction_funcBody_Ada handleFragmentation (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefinitionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErrorCode) (p:CallerScope) =
    let ii = id.SequenceOfLevel + 1;
    let i = sprintf "i%d" (id.SequenceOfLevel + 1)

    let typeDefinitionName =
        match typeDefinition with
        | TypeDefinition  td ->
            td.typedefName
        | ReferenceToExistingDefinition ref ->
            match ref.programUnit with
            | Some pu -> pu + "." + ref.typedefName
            | None    -> ref.typedefName

    let funcBodyContent, localVariables =
        let nStringLength =
            match isFixedSize with
            | true  -> []
            | false ->
                match codec with
                | Encode    -> []
                | Decode    -> [IntegerLocalVariable ("nStringLength", None)]
        let iVar = SequenceOfIndex (id.SequenceOfLevel + 1, None)

        let nBits = 1I
        let internalItem = uper_a.InternalItem_bit_str p.arg.p i  errCode.errCodeName codec
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))
        match minSize with
        | _ when maxSize < 65536I && isFixedSize  -> uper_a.octet_FixedSize p.arg.p typeDefinitionName i internalItem (minSize) nBits nBits 0I codec, iVar::nStringLength
        | _ when maxSize < 65536I && (not isFixedSize) -> uper_a.octet_VarSize p.arg.p "."  typeDefinitionName i internalItem ( minSize) (maxSize) nSizeInBits nBits nBits 0I errCode.errCodeName codec , iVar::nStringLength
        | _                                                ->
            let funcBodyContent, fragmentationLvars = handleFragmentation p codec errCode ii (uperMaxSizeInBits) minSize maxSize internalItem nBits true false
            let fragmentationLvars = fragmentationLvars |> List.addIf (not isFixedSize) (iVar)
            (funcBodyContent,fragmentationLvars)

    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}
#endif




//let getBoardDirs (l:ProgrammingLanguage) target =
//    getBoardNames l target |> List.map(fun s -> Path.Combine(boardsDirName l target , s))
let ada_keywords_upper = ada_keywords |> Set.map(fun s -> s.ToUpper())

type LangBasic_ada() =
    inherit ILangBasic()
        override this.cmp (s1:string) (s2:string) = s1.icompare s2
        override this.isCaseSensitive = false
        override this.keywords = ada_keywords 
        override this.isKeyword (token:string) = ada_keywords_upper.Contains (token.ToUpper())
        override this.OnTypeNameConflictTryAppendModName = false
        override this.declare_IntegerNoRTL = "adaasn1rtl", "Asn1Int", "INTEGER"
        override this.declare_PosIntegerNoRTL = "adaasn1rtl", "Asn1UInt" , "INTEGER"
        override this.getRealRtlTypeName   = "adaasn1rtl", "Asn1Real", "REAL"
        override this.getObjectIdentifierRtlTypeName  relativeId =
            let asn1Name = if relativeId then "RELATIVE-OID" else "OBJECT IDENTIFIER"
            "adaasn1rtl", "Asn1ObjectIdentifier", asn1Name

        override this.getTimeRtlTypeName  timeClass =
            let asn1Name = "TIME"
            match timeClass with
            | Asn1LocalTime                    _ -> "adaasn1rtl", "Asn1LocalTime", asn1Name
            | Asn1UtcTime                      _ -> "adaasn1rtl", "Asn1UtcTime", asn1Name
            | Asn1LocalTimeWithTimeZone        _ -> "adaasn1rtl", "Asn1TimeWithTimeZone", asn1Name
            | Asn1Date                           -> "adaasn1rtl", "Asn1Date", asn1Name
            | Asn1Date_LocalTime               _ -> "adaasn1rtl", "Asn1DateLocalTime", asn1Name
            | Asn1Date_UtcTime                 _ -> "adaasn1rtl", "Asn1DateUtcTime", asn1Name
            | Asn1Date_LocalTimeWithTimeZone   _ -> "adaasn1rtl", "Asn1DateTimeWithTimeZone", asn1Name
        override this.getNullRtlTypeName  = "adaasn1rtl", "Asn1NullType", "NULL"
        override this.getBoolRtlTypeName = "adaasn1rtl", "Asn1Boolean", "BOOLEAN"




type LangGeneric_a() =
    inherit ILangGeneric()
        override _.ArrayStartIndex = 1
        override this.getEmptySequenceInitExpression _ = "(null record)"
        override this.callFuncWithNoArgs () = ""

        override this.rtlModuleName  = "adaasn1rtl."
        override this.AssignOperator = ":="
        override this.TrueLiteral = "True"
        override this.FalseLiteral = "False"
        override this.hasModules = true
        override this.allowsSrcFilesWithNoFunctions = false
        override this.requiresValueAssignmentsInSrcFile = false
        override this.supportsStaticVerification = true
        override this.emptyStatement = "null;"
        override this.bitStreamName = "adaasn1rtl.encoding.BitStreamPtr"
        override this.unaryNotOperator    = "not"
        override this.modOp               = "mod"
        override this.eqOp                = "="
        override this.neqOp               = "<>"
        override this.andOp               = "and"
        override this.orOp                = "or"
        override this.initMethod           = InitMethod.Function
        override _.decodingKind = InPlace
        override _.usesWrappedOptional = false
        override this.castExpression (sExp:string) (sCastType:string) = sprintf "%s(%s)" sCastType sExp
        override this.createSingleLineComment (sText:string) = sprintf "--%s" sText

        override _.SpecNameSuffix = ""
        override _.SpecExtension = "ads"
        override _.BodyExtension = "adb"
        override _.Keywords  = CommonTypes.ada_keywords
        override _.isCaseSensitive = false


        override _.doubleValueToString (v:double) =
            v.ToString(FsUtils.doubleParseString, System.Globalization.NumberFormatInfo.InvariantInfo)
        override _.intValueToString (i:BigInteger) _ = i.ToString()
        override _.asn1SccIntValueToString (i: BigInteger) _ = i.ToString()
        override _.initializeString (_) = "(others => adaasn1rtl.NUL)"

        override _.supportsInitExpressions = true

        override this.getPointer (sel: AccessPath) = sel.joined this

        override this.getValue (sel: AccessPath) = sel.joined this
        override this.getValueUnchecked (sel: AccessPath) _ = this.getValue sel
        override this.getPointerUnchecked (sel: AccessPath) _ = this.getPointer sel
        override this.joinSelectionUnchecked (sel: AccessPath) _ = sel.joined this
        override this.getAccess (sel: AccessPath) = "."

        override this.getAccess2 (acc: AccessStep) = getAccess2_a acc

        override this.getPtrPrefix _ = ""

        override this.getPtrSuffix _ = ""

        override this.getStar _ = ""

        override this.getArrayItem (sel: AccessPath) (idx:string) (childTypeIsString: bool) =
            (sel.appendSelection "Data" ArrayElem false).append (ArrayAccess (idx, if childTypeIsString then ArrayElem else ByValue))

        override this.choiceIDForNone (typeIdsSet:Map<string,int>) (id:ReferenceToType) =
            let prefix = ToC ((id.AcnAbsPath.Tail |> Seq.StrJoin("_")).Replace("#","elem"))
            prefix + "_NONE"


        override this.getNamedItemBackendName (defOrRef:TypeDefinitionOrReference option) (nm:Asn1AcnAst.NamedItem) =
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + nm.ada_name
            | Some (TypeDefinition td) when td.baseType.IsSome && td.baseType.Value.programUnit.IsSome  -> td.baseType.Value.programUnit.Value + "." + nm.ada_name
            | _       -> ToC nm.ada_name

        override this.setNamedItemBackendName0 (nm:Asn1Ast.NamedItem) (newValue:string) : Asn1Ast.NamedItem =
            {nm with ada_name = newValue}
        override this.getNamedItemBackendName0 (nm:Asn1Ast.NamedItem)  = nm.ada_name

        override this.getNamedItemBackendName2 (defModule:string) (curProgramUnitName:string) (itm:Asn1AcnAst.NamedItem) =

            match (ToC defModule) = ToC curProgramUnitName with
            | true  -> ToC itm.ada_name
            | false -> ((ToC defModule) + "." + (ToC itm.ada_name))


        override this.Length exp sAcc =
            isvalid_a.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[Ada]
        override this.definitionOrRef (d:Map<ProgrammingLanguage, TypeDefinitionOrReference>) = d.[Ada]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[Ada]
        override this.getEnumTypeDefinition (td:Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) = td.[Ada]
        override this.getStrTypeDefinition (td:Map<ProgrammingLanguage, FE_StringTypeDefinition>) = td.[Ada]
        override this.getChoiceTypeDefinition (td:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) = td.[Ada]
        override this.getSequenceTypeDefinition (td:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) = td.[Ada]
        override this.getSizeableTypeDefinition (td:Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) = td.[Ada]

        override _.getValueAssignmentName (vas: ValueAssignment) = vas.ada_name

        override _.getChildInfoName (ch:Asn1Ast.ChildInfo)  = ch.ada_name
        override _.setChildInfoName (ch:Asn1Ast.ChildInfo) (newValue:string) = {ch with ada_name = newValue}

        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._ada_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._ada_name
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._ada_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._ada_name
        override _.getChoiceChildPresentWhenName (ch:Asn1AcnAst.Choice ) (c:Asn1AcnAst.ChChildInfo) : string =
            (ToC c.present_when_name) + "_PRESENT"

        override this.getRtlFiles  (encodings:Asn1Encoding list) (arrsTypeAssignments :string list) =
            let uperRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN) with true -> ["adaasn1rtl.encoding.uper"] | false -> []
            let acnRtl =
                match arrsTypeAssignments |> Seq.exists(fun s -> s.Contains "adaasn1rtl.encoding.acn") with true -> ["adaasn1rtl.encoding.acn"] | false -> []
            let xerRtl = match encodings |> Seq.exists(fun e -> e = XER) with true -> ["adaasn1rtl.encoding.xer"] | false -> []

            //adaasn1rtl.encoding is included by .uper or .acn or .xer. So, do not include it otherwise you get a warning
            let encRtl = []//match r.args.encodings |> Seq.exists(fun e -> e = UPER || e = ACN || e = XER) with true -> [] | false -> ["adaasn1rtl.encoding"]
            encRtl@uperRtl@acnRtl@xerRtl |> List.distinct

        override this.getSeqChildIsPresent (sel: AccessPath) (childName:string) =
            sprintf "%s%sexist.%s = 1" (sel.joined this) (this.getAccess sel) childName

        override this.getSeqChild (sel: AccessPath) (childName:string) (childTypeIsString: bool) (childIsOptional: bool) =
            sel.appendSelection childName (if childTypeIsString then ArrayElem else ByValue) childIsOptional

        override this.getChChild (sel: AccessPath) (childName:string) (childTypeIsString: bool) : AccessPath =
            sel.appendSelection childName (if childTypeIsString then ArrayElem else ByValue) false

        override this.presentWhenName (defOrRef:TypeDefinitionOrReference option) (ch:ChChildInfo) : string =
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + ((ToC ch._present_when_name_private) + "_PRESENT")
            | _       -> (ToC ch._present_when_name_private) + "_PRESENT"
        override this.presentWhenName0 (defOrRef:TypeDefinitionOrReference option) (ch:Asn1AcnAst.ChChildInfo) : string =
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + ((ToC ch.present_when_name) + "_PRESENT")
            | _       -> (ToC ch.present_when_name) + "_PRESENT"
        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CodegenScope =
            {CodegenScope.modName = t.id.ModName; accessPath = AccessPath.emptyPath ("val" + suf) ByValue}

        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "i%d:Integer;" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "i%d:Integer:=%s;" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "%s:Integer;" name
            | IntegerLocalVariable (name,Some iv)       -> sprintf "%s:Integer:=%s;" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1Int;" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1Int:=%s;" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1UInt;" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1UInt:=%s;" name iv
            | FlagLocalVariable (name,None)             -> sprintf "%s:adaasn1rtl.BIT;" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "%s:adaasn1rtl.BIT:=%s;" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "%s:Boolean;" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "%s:Boolean:=%s;" name iv
            | AcnInsertedChild(name, vartype, initVal)  -> sprintf "%s:%s;" name vartype
            | GenericLocalVariable lv                   ->
                match lv.initExp with
                | Some initExp  -> sprintf "%s : %s := %s;" lv.name lv.varType  initExp
                | None          -> sprintf "%s : %s;" lv.name lv.varType

        override this.getLongTypedefName (tdr:TypeDefinitionOrReference) : string =
            match tdr with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref ->
                match ref.programUnit with
                | Some pu -> pu + "." + ref.typedefName
                | None    -> ref.typedefName

        override this.decodeEmptySeq p = Some (uper_a.decode_empty_sequence_emptySeq p)
        override this.decode_nullType p = Some (uper_a.decode_nullType p)



        override this.getParamValue  (t:Asn1AcnAst.Asn1Type) (sel: AccessPath)  (c:Codec) =
            sel.joined this

        override this.toHex n = sprintf "16#%x#" n

        override this.bitStringValueToByteArray (v : BitStringValue) =
            v.ToCharArray() |> Array.map(fun c -> if c = '0' then 0uy else 1uy)

        override this.uper =
            {
                Uper_parts.createLv = (fun name -> IntegerLocalVariable(name,None))
                requires_sBlockIndex  = false
                requires_sBLJ = true
                requires_charIndex = true
                requires_IA5String_i = false
                count_var            = IntegerLocalVariable ("nStringLength", None)
                requires_presenceBit = false
                catd                 = false
                //createBitStringFunction = createBitStringFunction_funcBody_Ada
                seqof_lv              =
                  (fun id minSize maxSize ->
                    if maxSize >= 65536I && maxSize = minSize then
                        []
                    else
                        [SequenceOfIndex (id.SequenceOfLevel + 1, None)])
                exprMethodCall        = fun _ _ -> ""
            }
        override this.acn =
            {
                Acn_parts.null_valIsUnReferenced = false
                checkBitPatternPresentResult = false
                getAcnContainingByLocVars =
                    fun  sReqBytesForUperEncoding ->
                        [
                            GenericLocalVariable {GenericLocalVariable.name = "tmpBs"; varType = "adaasn1rtl.encoding.BitStream"; arrSize = None; isStatic = false;initExp = Some (sprintf "adaasn1rtl.encoding.BitStream_init(%s)" sReqBytesForUperEncoding)}
                        ]
                getAcnDepSizeDeterminantLocVars =
                    fun  sReqBytesForUperEncoding ->
                        [
                            GenericLocalVariable {GenericLocalVariable.name = "tmpBs"; varType = "adaasn1rtl.encoding.BitStream"; arrSize = None; isStatic = false;initExp = Some (sprintf "adaasn1rtl.encoding.BitStream_init(%s)" sReqBytesForUperEncoding)}
                        ]
                createLocalVariableEnum =
                    (fun rtlIntType -> GenericLocalVariable {GenericLocalVariable.name = "intVal"; varType= rtlIntType; arrSize= None; isStatic = false; initExp=None })
                choice_handle_always_absent_child = true
                choice_requires_tmp_decoding = false
          }
        override this.init =
            {
                Initialize_parts.zeroIA5String_localVars    = fun ii -> [SequenceOfIndex (ii, None)]
                choiceComponentTempInit                     = false
                initMethSuffix                              = fun _ -> ""
            }

        override this.atc =
            {
                Atc_parts.uperPrefix = "UPER_"
                acnPrefix            = "ACN_"
                xerPrefix            = "XER_"
                berPrefix            = "BER_"
            }

        override _.getBoardNames (target:Targets option) =
            match target with
            | None              -> ["x86"]  //default board
            | Some X86          -> ["x86"]
            | Some Stm32        -> ["stm32"]
            | Some Msp430       -> ["msp430"]
            | Some AllBoards    -> ["x86";"stm32";"msp430"]

        override this.getBoardDirs (target:Targets option) =
            let boardsDirName = match target with None -> "" | Some _ -> "boards"
            this.getBoardNames target |> List.map(fun s -> Path.Combine(boardsDirName , s))


        override this.CreateMakeFile (r:AstRoot)  (di:DirInfo) =
            let boardNames = this.getBoardNames r.args.target
            let writeBoard boardName =
                let mods = aux_a.rtlModuleName()::(r.programUnits |> List.map(fun pu -> pu.name.ToLower() ))
                let content = aux_a.PrintMakeFile boardName (sprintf "asn1_%s.gpr" boardName) mods
                let fileName = if boardNames.Length = 1 || boardName = "x86" then "Makefile" else ("Makefile." + boardName)
                let outFileName = Path.Combine(di.rootDir, fileName)
                File.WriteAllText(outFileName, content.Replace("\r",""))
            this.getBoardNames r.args.target |> List.iter writeBoard

        override this.getDirInfo (target:Targets option) rootDir =
            match target with
            | None -> {rootDir = rootDir; srcDir=rootDir;asn1rtlDir=rootDir;boardsDir=rootDir}
            | Some _   ->
                {
                    rootDir = rootDir;
                    srcDir=Path.Combine(rootDir, "src");
                    asn1rtlDir=Path.Combine(rootDir, "asn1rtl");
                    boardsDir=Path.Combine(rootDir, "boards")
                }

        override this.getTopLevelDirs (target:Targets option) =
            match target with
            | None -> []
            | Some _   -> ["src"; "asn1rtl"; "boards"]

        override _.CreateAuxFiles (r:AstRoot)  (di:DirInfo) (arrsSrcTstFiles : string list, arrsHdrTstFiles:string list) =
            ()
        override this.getChChildIsPresent   (arg:AccessPath) (chParent:string)  (pre_name:string) =
            sprintf "%s%skind %s %s_PRESENT" (arg.joined this) (this.getAccess arg) this.eqOp pre_name
