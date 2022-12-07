module LangGeneric_a
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open Language
open System.IO

(****** Ada Implementation ******)

let getAcces_a  (_:FuncParamType) = "."
#if false
let createBitStringFunction_funcBody_Ada handleFragmentation (codec:CommonTypes.Codec) (id : ReferenceToType) (typeDefinition:TypeDefintionOrReference) isFixedSize  uperMaxSizeInBits minSize maxSize (errCode:ErroCode) (p:CallerScope) = 
    let ii = id.SeqeuenceOfLevel + 1;
    let i = sprintf "i%d" (id.SeqeuenceOfLevel + 1)

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
        let iVar = SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)

        let nBits = 1I
        let internalItem = uper_a.InternalItem_bit_str p.arg.p i  errCode.errCodeName codec 
        let nSizeInBits = GetNumberOfBitsForNonNegativeInteger ( (maxSize - minSize))
        match minSize with
        | _ when maxSize < 65536I && isFixedSize  -> uper_a.octect_FixedSize p.arg.p typeDefinitionName i internalItem (minSize) nBits nBits 0I codec, iVar::nStringLength 
        | _ when maxSize < 65536I && (not isFixedSize) -> uper_a.octect_VarSize p.arg.p "."  typeDefinitionName i internalItem ( minSize) (maxSize) nSizeInBits nBits nBits 0I errCode.errCodeName codec , iVar::nStringLength
        | _                                                -> 
            let funcBodyContent, fragmentationLvars = handleFragmentation p codec errCode ii (uperMaxSizeInBits) minSize maxSize internalItem nBits true false
            let fragmentationLvars = fragmentationLvars |> List.addIf (not isFixedSize) (iVar)
            (funcBodyContent,fragmentationLvars)

    {UPERFuncBodyResult.funcBody = funcBodyContent; errCodes = [errCode]; localVariables = localVariables; bValIsUnReferenced=false; bBsIsUnReferenced=false}    
#endif







type LangGeneric_a() =
    inherit ILangGeneric()
        override _.ArrayStartIndex = 1
        override this.getEmptySequenceInitExpression () = "(null record)"
        override this.callFuncWithNoArgs () = ""

        override this.rtlModuleName  = "adaasn1rtl."
        override this.AssignOperator = ":="
        override this.TrueLiteral = "True"
        override this.FalseLiteral = "False"
        override this.hasModules = true
        override this.allowsSrcFilesWithNoFunctions = false
        override this.requiresValueAssignmentsInSrcFile = false
        override this.supportsStaticVerification = true 
        override this.emtyStatement = "null;"
        override this.bitStreamName = "adaasn1rtl.encoding.BitStreamPtr"
        override this.unaryNotOperator    = "not"
        override this.modOp               = "mod"
        override this.eqOp                = "="
        override this.neqOp               = "<>"
        override this.andOp               = "and"
        override this.orOp                = "or"
        override this.initMetod           = InitMethod.Function

        override this.castExpression (sExp:string) (sCastType:string) = sprintf "%s(%s)" sCastType sExp
        override this.createSingleLineComment (sText:string) = sprintf "--%s" sText

        override _.SpecExtention = "ads"
        override _.BodyExtention = "adb"

        override _.doubleValueToSting (v:double) = 
            v.ToString(FsUtils.doubleParseString, System.Globalization.NumberFormatInfo.InvariantInfo)
        override _.intValueToSting (i:BigInteger) _ = i.ToString()

        override _.initializeString (_) = "(others => adaasn1rtl.NUL)"
        
        override _.supportsInitExpressions = true

        override _.getPointer  (fpt:FuncParamType) =
            match fpt with
            | VALUE x      -> x
            | POINTER x    -> x
            | FIXARRAY x   -> x

        override this.getValue  (fpt:FuncParamType) =
            match fpt with
            | VALUE x      -> x
            | POINTER x    -> x
            | FIXARRAY x   -> x
        override this.getAcces  (fpt:FuncParamType) = getAcces_a fpt

        override this.getStar  (fpt:FuncParamType) =
            match fpt with
            | VALUE x        -> ""
            | POINTER x      -> ""
            | FIXARRAY x     -> ""
        override this.getArrayItem (fpt:FuncParamType) (idx:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s.Data(%s)" fpt.p idx
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.ArrayAccess idx = "(" + idx + ")"

        override this.choiceIDForNone (typeIdsSet:Map<string,int>) (id:ReferenceToType) =  
            let prefix = ToC (id.AcnAbsPath.Tail.StrJoin("_").Replace("#","elem"))
            prefix + "_NONE"


        override this.getNamedItemBackendName (defOrRef:TypeDefintionOrReference option) (nm:Asn1AcnAst.NamedItem) = 
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + nm.ada_name
            | Some (TypeDefinition td) when td.baseType.IsSome && td.baseType.Value.programUnit.IsSome  -> td.baseType.Value.programUnit.Value + "." + nm.ada_name
            | _       -> ToC nm.ada_name
        override this.getNamedItemBackendName2 (id:ReferenceToType) (curProgamUnitName:string) (itm:Asn1AcnAst.NamedItem) = 
            let typeModName = id.ModName
            match (ToC typeModName) = curProgamUnitName with
            | true  -> ToC itm.ada_name
            | false -> ((ToC typeModName) + "." + (ToC itm.ada_name))


        override this.Length exp sAcc =
            isvalid_a.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[Ada]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[Ada]
        override this.getEnmTypeDefintion (td:Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) = td.[Ada]
        override this.getStrTypeDefinition (td:Map<ProgrammingLanguage, FE_StringTypeDefinition>) = td.[Ada]
        override this.getChoiceTypeDefinition (td:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) = td.[Ada]
        override this.getSequenceTypeDefinition (td:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) = td.[Ada]
        override this.getSizeableTypeDefinition (td:Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) = td.[Ada]

        override _.getValueAssignmentName (vas: ValueAssignment) = vas.ada_name

        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._ada_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._ada_name
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._ada_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._ada_name

        override this.getRtlFiles  (encodings:Asn1Encoding list) (arrsTypeAssignments :string list) =
            let uperRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN) with true -> ["adaasn1rtl.encoding.uper"] | false -> []
            let acnRtl = 
                match arrsTypeAssignments |> Seq.exists(fun s -> s.Contains "adaasn1rtl.encoding.acn") with true -> ["adaasn1rtl.encoding.acn"] | false -> []
            let xerRtl = match encodings |> Seq.exists(fun e -> e = XER) with true -> ["adaasn1rtl.encoding.xer"] | false -> []

            //adaasn1rtl.encoding is included by .uper or .acn or .xer. So, do not include it otherwise you get a warning
            let encRtl = []//match r.args.encodings |> Seq.exists(fun e -> e = UPER || e = ACN || e = XER) with true -> [] | false -> ["adaasn1rtl.encoding"]
            encRtl@uperRtl@acnRtl@xerRtl |> List.distinct


        override this.getSeqChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) =
            let newPath = sprintf "%s.%s" fpt.p childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)
        override this.getChChild (fpt:FuncParamType) (childName:string) (childTypeIsString: bool) : FuncParamType =
            let newPath = sprintf "%s.%s" fpt.p childName
            //let newPath = sprintf "%s%su.%s" fpt.p (this.getAcces fpt) childName
            if childTypeIsString then (FIXARRAY newPath) else (VALUE newPath)


        override this.presentWhenName (defOrRef:TypeDefintionOrReference option) (ch:ChChildInfo) : string =
            match defOrRef with
            | Some (ReferenceToExistingDefinition r) when r.programUnit.IsSome -> r.programUnit.Value + "." + ((ToC ch._present_when_name_private) + "_PRESENT")
            | _       -> (ToC ch._present_when_name_private) + "_PRESENT"
        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CallerScope =
            {CallerScope.modName = t.id.ModName; arg= VALUE ("val" + suf) }

        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "i%d:Integer;" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "i%d:Integer:=%d;" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "%s:Integer;" name
            | IntegerLocalVariable (name,Some iv)       -> sprintf "%s:Integer:=%d;" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1Int;" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1Int:=%d;" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "%s:adaasn1rtl.Asn1UInt;" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "%s:adaasn1rtl.Asn1UInt:=%d;" name iv
            | FlagLocalVariable (name,None)             -> sprintf "%s:adaasn1rtl.BIT;" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "%s:adaasn1rtl.BIT:=%d;" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "%s:Boolean;" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "%s:Boolean:=%s;" name (if iv then "True" else "False")
            | AcnInsertedChild(name, vartype)         -> sprintf "%s:%s;" name vartype
            | GenericLocalVariable lv                   ->
                match lv.initExp with
                | Some initExp  -> sprintf "%s : %s := %s;" lv.name lv.varType  initExp
                | None          -> sprintf "%s : %s;" lv.name lv.varType  

        override this.getLongTypedefName (tdr:TypeDefintionOrReference) : string =
            match tdr with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref ->
                match ref.programUnit with
                | Some pu -> pu + "." + ref.typedefName
                | None    -> ref.typedefName

        override this.decodeEmptySeq p = Some (uper_a.decode_empty_sequence_emptySeq p)
        override this.decode_nullType p = Some (uper_a.decode_nullType p)



        override this.getParamValue  (t:Asn1AcnAst.Asn1Type) (p:FuncParamType)  (c:Codec) =
            p.p

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
                        [SequenceOfIndex (id.SeqeuenceOfLevel + 1, None)])
            }
        override this.acn = 
            {
                Acn_parts.null_valIsUnReferenced = false
                checkBitPatternPresentResult = false
                getAcnDepSizeDeterminantLocVars = 
                    fun  sReqBytesForUperEncoding ->
                        [
                            GenericLocalVariable {GenericLocalVariable.name = "tmpBs"; varType = "adaasn1rtl.encoding.BitStream"; arrSize = None; isStatic = false;initExp = Some (sprintf "adaasn1rtl.encoding.BitStream_init(%s)" sReqBytesForUperEncoding)}
                        ]
                choice_handle_always_absent_child = true
                choice_requires_tmp_decoding = false
          }
        override this.init = 
            {
                Initialize_parts.zeroIA5String_localVars    = fun ii -> [SequenceOfIndex (ii, None)]
                choiceComponentTempInit                     = false
            }

        override this.atc =
            {
                Atc_parts.uperPrefix = "UPER_"
                acnPrefix            = "ACN_"
                xerPrefix            = "XER_"
                berPrefix            = "BER_"
            }


        override this.CreateMakeFile (r:AstRoot)  (di:OutDirectories.DirInfo) =
            let boardNames = OutDirectories.getBoardNames Ada r.args.target
            let writeBoard boardName = 
                let mods = aux_a.rtlModuleName()::(r.programUnits |> List.map(fun pu -> pu.name.ToLower() ))
                let content = aux_a.PrintMakeFile boardName (sprintf "asn1_%s.gpr" boardName) mods
                let fileName = if boardNames.Length = 1 || boardName = "x86" then "Makefile" else ("Makefile." + boardName)
                let outFileName = Path.Combine(di.rootDir, fileName)
                File.WriteAllText(outFileName, content.Replace("\r",""))
            OutDirectories.getBoardNames Ada r.args.target |> List.iter writeBoard

        override this.CreateAuxFiles (r:AstRoot)  (di:OutDirectories.DirInfo) (arrsSrcTstFiles : string list, arrsHdrTstFiles:string list) =
            ()