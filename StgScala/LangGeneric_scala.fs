module LangGeneric_scala
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open Language
open System.IO
open System
open Asn1AcnAstUtilFunctions
open ProofGen
open ProofAst

let rec resolveReferenceType(t: Asn1TypeKind): Asn1TypeKind =
    match t with
    | ReferenceType rt -> resolveReferenceType rt.resolvedType.Kind
    | _ -> t

let isJVMPrimitive (t: Asn1TypeKind) =
    match resolveReferenceType t with
    | Integer _ | Real _ | NullType _ | Boolean _ -> true
    | _ -> false

let initMethSuffix k =
    match isJVMPrimitive k with
    | false ->
        match k with
        | BitString bitString -> ""
        | _ -> "()"
    | true -> ""

let isEnumForJVMelseFalse (k: Asn1TypeKind): bool =
    match ST.lang with
    | Scala ->
        match resolveReferenceType k with
        | Enumerated e -> true
        | _ -> false
    | _ -> false

let isSequenceForJVMelseFalse (k: Asn1TypeKind): bool =
    match ST.lang with
    | Scala ->
        match k with
        | Sequence s -> true
        | _ -> false
    | _ -> false

let isOctetStringForJVMelseFalse (k: Asn1TypeKind): bool =
    match ST.lang with
    | Scala ->
        match k with
        | OctetString s -> true
        | _ -> false
    | _ -> false

let uperExprMethodCall (k: Asn1TypeKind) (sChildInitExpr: string) =
    let isSequence = isSequenceForJVMelseFalse k
    let isEnum = isEnumForJVMelseFalse k
    let isOctetString = isOctetStringForJVMelseFalse k

    match isSequence || sChildInitExpr.Equals("null") || isEnum || isOctetString with
    | true -> ""
    | false -> initMethSuffix k

let getAccess2_scala (acc: AccessStep) =
    match acc with
    | ValueAccess (sel, _, _) -> $".{sel}"
    | PointerAccess (sel, _, _) -> $".{sel}"
    | ArrayAccess (ix, _) -> $"({ix})"

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

let srcDirName = Path.Combine("src", "main", "scala", "asn1src")
let asn1rtlDirName  = Path.Combine("src", "main", "scala", "asn1scala")

type LangBasic_scala() =
    inherit ILangBasic()
        override this.cmp (s1:string) (s2:string) = s1 = s2
        override this.isCaseSensitive = true
        override this.keywords = scala_keywords
        override this.isKeyword (token) = c_keywords.Contains token
        override this.OnTypeNameConflictTryAppendModName = true
        override this.declare_IntegerNoRTL = "", "Int", "INTEGER"
        override this.declare_PosIntegerNoRTL = "", " asn1SccUint" , "INTEGER"
        override this.getRealRtlTypeName   = "", "asn1Real", "REAL"
        override this.getObjectIdentifierRtlTypeName  relativeId =
            let asn1Name = if relativeId then "RELATIVE-OID" else "OBJECT IDENTIFIER"
            "", "Asn1ObjectIdentifier", asn1Name
        override this.getTimeRtlTypeName timeClass =
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
        override this.getBoolRtlTypeName = "","Boolean","BOOLEAN"

type LangGeneric_scala() =
    inherit ILangGeneric()
        override _.ArrayStartIndex = 0

        override _.intValueToString (i:BigInteger) (intClass:Asn1AcnAst.IntegerClass) =
            match intClass with
            | Asn1AcnAst.ASN1SCC_Int8     _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int16    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int32    _ ->  sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int64    _ ->  sprintf "%sL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int _ when
                i >= BigInteger System.Int32.MinValue &&
                i <= BigInteger System.Int32.MaxValue ->
                    sprintf "%s" (i.ToString())
            | Asn1AcnAst.ASN1SCC_Int      _ ->  sprintf "%sL" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt8    _ ->  sprintf "UByte.fromRaw(%s)" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt16   _ ->  sprintf "UShort.fromRaw(%s)" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt32   _ ->  sprintf "UInt.fromRaw(%s)" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt64   _ ->  sprintf "ULong.fromRaw(%sL)" (i.ToString())
            | Asn1AcnAst.ASN1SCC_UInt     _ ->  sprintf "ULong.fromRaw(%sL)" (i.ToString())

        override _.asn1SccIntValueToString (i: BigInteger) (unsigned: bool) =
            let iStr = i.ToString()
            if unsigned then $"ULong.fromRaw({iStr})" else iStr

        override _.doubleValueToString (v:double) =
            v.ToString(FsUtils.doubleParseString, System.Globalization.NumberFormatInfo.InvariantInfo)

        override _.initializeString stringSize = sprintf "Vector.fill[UByte](%d.toInt+1)(0x0.toRawUByte)" stringSize

        override _.supportsInitExpressions = false

        override this.getPointer (sel: AccessPath) = sel.joined this

        override this.getValue (sel: AccessPath) = sel.joined this
        override this.getValueUnchecked (sel: AccessPath) (kind: UncheckedAccessKind) = this.joinSelectionUnchecked sel kind
        override this.getPointerUnchecked (sel: AccessPath) (kind: UncheckedAccessKind) = this.joinSelectionUnchecked sel kind
        override _.joinSelectionUnchecked (sel: AccessPath) (kind: UncheckedAccessKind) =
            let len = sel.steps.Length
            List.fold (fun str (ix, accessor) ->
                let accStr =
                    match accessor with
                    | ValueAccess (id, _, isOpt) ->
                        if isOpt && (kind = FullAccess || ix < len - 1) then $".{id}.get" else $".{id}"
                    | PointerAccess (id, _, isOpt) ->
                        if isOpt && (kind = FullAccess || ix < len - 1) then $".{id}.get" else $".{id}"
                    | ArrayAccess (ix, _) -> $"({ix})"
                $"{str}{accStr}"
            ) sel.rootId (List.indexed sel.steps)
        override this.getAccess (sel: AccessPath) = "."

        override this.getAccess2 (acc: AccessStep) = getAccess2_scala acc

        override this.getPtrPrefix _ = ""

        override this.getPtrSuffix _ = ""

        override this.getStar _ = ""

        override _.real_annotations = ["extern"]

        override this.getArrayItem (sel: AccessPath) (idx:string) (childTypeIsString: bool) =
            (sel.appendSelection "arr" ArrayElem false).append (ArrayAccess (idx, if childTypeIsString then ArrayElem else ByValue))

        override this.getNamedItemBackendName (defOrRef: TypeDefinitionOrReference option) (nm: Asn1AcnAst.NamedItem) =
            let itemname =
                match defOrRef with
                | Some (TypeDefinition td) -> td.typedefName + "." + ToC nm.scala_name
                | Some (ReferenceToExistingDefinition rted) -> rted.typedefName + "." + ToC nm.scala_name
                | _ -> ToC nm.scala_name
            itemname

        override this.getNamedItemBackendName0 (nm:Asn1Ast.NamedItem)  = nm.scala_name
        override this.setNamedItemBackendName0 (nm:Asn1Ast.NamedItem) (newValue:string) : Asn1Ast.NamedItem =
            {nm with scala_name = newValue}

        override this.getNamedItemBackendName2 (_:string) (_:string) (nm:Asn1AcnAst.NamedItem) =
            ToC nm.scala_name

        override this.decodeEmptySeq _ = None
        override this.decode_nullType _ = None

        override this.Length exp sAcc =
            isvalid_scala.ArrayLen exp sAcc

        override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[Scala]
        override this.definitionOrRef (d:Map<ProgrammingLanguage, TypeDefinitionOrReference>) = d.[Scala]
        override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[Scala]
        override this.getEnumTypeDefinition (td:Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) = td.[Scala]
        override this.getStrTypeDefinition (td:Map<ProgrammingLanguage, FE_StringTypeDefinition>) = td.[Scala]
        override this.getChoiceTypeDefinition (td:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) = td.[Scala]
        override this.getSequenceTypeDefinition (td:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) = td.[Scala]
        override this.getSizeableTypeDefinition (td:Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) = td.[Scala]
        override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._scala_name
        override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._scala_name
        override _.getChildInfoName (ch:Asn1Ast.ChildInfo)  = ch.scala_name
        override _.setChildInfoName (ch:Asn1Ast.ChildInfo) (newValue:string) = {ch with scala_name = newValue}
        override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._scala_name
        override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._scala_name
        override _.getChoiceChildPresentWhenName (ch:Asn1AcnAst.Choice ) (c:Asn1AcnAst.ChChildInfo) : string =
            ch.typeDef[Scala].typeName + "." + (ToC c.present_when_name) + "_PRESENT"

        override this.getRtlFiles  (encodings:Asn1Encoding list) (_ :string list) =
            let encRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN ) with true -> ["asn1crt_encoding"] | false -> []
            let uperRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN) with true -> ["asn1crt_encoding_uper"] | false -> []
            let acnRtl = match encodings |> Seq.exists(fun e -> e = ACN) with true -> ["asn1crt_encoding_acn"] | false -> []
            let xerRtl = match encodings |> Seq.exists(fun e -> e = XER) with true -> ["asn1crt_encoding_xer"] | false -> []
            encRtl@uperRtl@acnRtl@xerRtl

        override this.getEmptySequenceInitExpression sTypeDefName = $"{sTypeDefName}()"
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
        override _.decodingKind = Copy
        override _.usesWrappedOptional = true
        override this.castExpression (sExp:string) (sCastType:string) = sprintf "(%s)(%s)" sCastType sExp
        override this.createSingleLineComment (sText:string) = sprintf "/*%s*/" sText

        override _.SpecNameSuffix = "Def"
        override _.SpecExtension = "scala"
        override _.BodyExtension = "scala"
        override _.Keywords  = CommonTypes.scala_keywords


        override _.getValueAssignmentName (vas: ValueAssignment) = vas.scala_name

        override this.hasModules = false
        override this.allowsSrcFilesWithNoFunctions = true
        override this.requiresValueAssignmentsInSrcFile = true
        override this.supportsStaticVerification = false

        override this.getSeqChildIsPresent (sel: AccessPath) (childName: string) =
            sprintf "%s%s%s.isDefined" (sel.joined this) (this.getAccess sel) childName

        override this.getSeqChild (sel: AccessPath) (childName:string) (childTypeIsString: bool) (childIsOptional: bool) =
            sel.appendSelection childName (if childTypeIsString then ArrayElem else ByValue) childIsOptional

        override this.getChChild (sel: AccessPath) (childName:string) (childTypeIsString: bool) : AccessPath =
            AccessPath.emptyPath childName (if childTypeIsString then ArrayElem else ByValue)

        override this.choiceIDForNone (typeIdsSet:Map<string,int>) (id:ReferenceToType) = ""

        override this.presentWhenName (defOrRef:TypeDefinitionOrReference option) (ch:ChChildInfo) : string =
            let parentName =
                match defOrRef with
                | Some a -> match a with
                            | ReferenceToExistingDefinition b -> b.typedefName + "."
                            | TypeDefinition c -> c.typedefName + "."
                | None -> ""
            parentName + (ToC ch._present_when_name_private) + "_PRESENT"

        override this.presentWhenName0 (defOrRef:TypeDefinitionOrReference option) (ch:Asn1AcnAst.ChChildInfo) : string =
            let parentName =
                match defOrRef with
                | Some a -> match a with
                            | ReferenceToExistingDefinition b -> b.typedefName + "."
                            | TypeDefinition c -> c.typedefName + "."
                | None -> ""
            parentName + (ToC ch.present_when_name) + "_PRESENT"

        override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CodegenScope =
            let rec getRecvType (kind: Asn1AcnAst.Asn1TypeKind) =
                match kind with
                | Asn1AcnAst.NumericString _ | Asn1AcnAst.IA5String _ -> ArrayElem // TODO: For Ada, this is Value no matter what?
                | Asn1AcnAst.ReferenceType r -> getRecvType r.resolvedType.Kind
                | _ -> ByPointer
            let recvId = "pVal" + suf
            {CodegenScope.modName = t.id.ModName; accessPath = AccessPath.emptyPath recvId (getRecvType t.Kind) }

        override this.getParamValue (t:Asn1AcnAst.Asn1Type) (p:AccessPath) (c:Codec) =
            p.joined this


        override this.getLocalVariableDeclaration (lv:LocalVariable) : string  =
            match lv with
            | SequenceOfIndex (i,None)                  -> sprintf "var i%d: Int = 0" i
            | SequenceOfIndex (i,Some iv)               -> sprintf "var i%d: Int = %s" i iv
            | IntegerLocalVariable (name,None)          -> sprintf "var %s: Int = 0" name
            | IntegerLocalVariable (name,Some iv)       -> sprintf "var %s: Int = %s" name iv
            | Asn1SIntLocalVariable (name,None)         -> sprintf "var %s: Int = 0" name
            | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "var %s: Int = %s" name iv
            | Asn1UIntLocalVariable (name,None)         -> sprintf "var %s: UInt = 0" name
            | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "var %s: UInt = %s" name iv
            | FlagLocalVariable (name,None)             -> sprintf "var %s: Boolean = false" name
            | FlagLocalVariable (name,Some iv)          -> sprintf "var %s: Boolean = %s" name iv
            | BooleanLocalVariable (name,None)          -> sprintf "var %s: Boolean = false" name
            | BooleanLocalVariable (name,Some iv)       -> sprintf "var %s: Boolean = %s" name iv
            | AcnInsertedChild(name, vartype, initVal)  ->
                sprintf "var %s: %s = %s" name vartype initVal

            | GenericLocalVariable lv                   ->
                sprintf "var %s%s: %s%s = %s" (if lv.isStatic then "static " else "") lv.name lv.varType (if lv.arrSize.IsNone then "" else "["+lv.arrSize.Value+"]") (if lv.initExp.IsNone then "" else lv.initExp.Value)


        override this.getLongTypedefName (tdr:TypeDefinitionOrReference) : string =
            match tdr with
            | TypeDefinition  td -> td.typedefName
            | ReferenceToExistingDefinition ref -> ref.typedefName


        override this.toHex n = sprintf "0x%x" n

        override this.bitStringValueToByteArray (v : BitStringValue) = FsUtils.bitStringValueToByteArray (StringLoc.ByValue v)

        override this.generateSequenceAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (nestingScope: NestingScope) (sel: AccessPath) (codec: Codec): string list =
            let fds = generateSequenceAuxiliaries r enc t sq nestingScope sel codec
            fds |> List.collect (fun fd -> [show (FunDefTree fd); ""])

        override this.generateIntegerAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (int: Asn1AcnAst.Integer) (nestingScope: NestingScope) (sel: AccessPath) (codec: Codec): string list =
            let fds = generateIntegerAuxiliaries r enc t int nestingScope sel codec
            fds |> List.collect (fun fd -> [show (FunDefTree fd); ""])

        override this.generateBooleanAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (boolean: Asn1AcnAst.Boolean) (nestingScope: NestingScope) (sel: AccessPath) (codec: Codec): string list =
            let fds = generateBooleanAuxiliaries r enc t boolean nestingScope sel codec
            fds |> List.collect (fun fd -> [show (FunDefTree fd); ""])

        override this.generateSequenceOfLikeAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (o: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): string list * string option =
            let fds, call = generateSequenceOfLikeAuxiliaries r enc o pg codec
            fds |> List.collect (fun fd -> [show (FunDefTree fd); ""]), Some (show (ExprTree call))

        override this.generateOptionalAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (soc: SequenceOptionalChild) (codec: Codec): string list * string =
            let fds, call = generateOptionalAuxiliaries r enc soc codec
            let innerFns = fds |> List.collect (fun fd -> [show (FunDefTree fd); ""])
            innerFns, show (ExprTree call)

        override this.generateChoiceAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (ch: Asn1AcnAst.Choice) (nestingScope: NestingScope) (sel: AccessPath) (codec: Codec): string list =
            let fds = generateChoiceAuxiliaries r enc t ch nestingScope sel codec
            fds |> List.collect (fun fd -> [show (FunDefTree fd); ""])

        override this.generateNullTypeAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (nt: Asn1AcnAst.NullType) (nestingScope: NestingScope) (sel: AccessPath) (codec: Codec): string list =
            let fds = generateNullTypeAuxiliaries r enc t nt nestingScope sel codec
            fds |> List.collect (fun fd -> [show (FunDefTree fd); ""])

        override this.generateEnumAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (enm: Asn1AcnAst.Enumerated) (nestingScope: NestingScope) (sel: AccessPath) (codec: Codec): string list =
            let fds = generateEnumAuxiliaries r enc t enm nestingScope sel codec
            fds |> List.collect (fun fd -> [show (FunDefTree fd); ""])

        override this.adaptAcnFuncBody (r: Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (funcBody: AcnFuncBody) (isValidFuncName: string option) (t: Asn1AcnAst.Asn1Type) (codec: Codec): AcnFuncBody =
            let shouldWrap  =
                match t.Kind with
                | Asn1AcnAst.ReferenceType rt -> rt.hasExtraConstrainsOrChildrenOrAcnArgs
                | Asn1AcnAst.Sequence _ | Asn1AcnAst.Choice _ | Asn1AcnAst.SequenceOf _ -> true
                | _ -> false

            let rec collectAllAcnChildren (tpe: Asn1AcnAst.Asn1TypeKind): Asn1AcnAst.AcnChild list =
                match tpe.ActualType with
                | Asn1AcnAst.Sequence sq ->
                    sq.children |> List.collect (fun c ->
                    match c with
                    | Asn1AcnAst.AcnChild c -> [c]
                    | Asn1AcnAst.Asn1Child c -> collectAllAcnChildren c.Type.Kind
                    )
                | _ -> []

            let newFuncBody (s: State)
                            (err: ErrorCode)
                            (prms: (AcnGenericTypes.RelativePath * AcnGenericTypes.AcnParameter) list)
                            (nestingScope: NestingScope)
                            (p: CodegenScope): (AcnFuncBodyResult option) * State =
                if not nestingScope.isInit && shouldWrap then
                    let recP = {p with accessPath = p.accessPath.asLastOrSelf}
                    let recNS = NestingScope.init t.acnMaxSizeInBits t.uperMaxSizeInBits ((p, t) :: nestingScope.parents)
                    let res, s = funcBody s err prms recNS recP
                    match res with
                    | Some res ->
                        assert (not nestingScope.parents.IsEmpty)
                        let fds, call = wrapAcnFuncBody r deps t res.funcBody codec nestingScope p recP
                        let fdsStr = fds |> List.map (fun fd -> show (FunDefTree fd))
                        let callStr = show (ExprTree call)
                        // TODO: Hack to determine how to change the "result variable"
                        let resultExpr =
                            match res.resultExpr with
                            | Some res when res = recP.accessPath.asIdentifier -> Some p.accessPath.asIdentifier
                            | Some res -> Some res
                            | None -> None
                        Some {res with funcBody = callStr; resultExpr = resultExpr; auxiliaries = res.auxiliaries @ fdsStr}, s
                    | None -> None, s
                else funcBody s err prms nestingScope p

            newFuncBody

        override this.generatePrecond (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (codec: Codec): string list =
            let precond = generatePrecond r enc t codec
            [show (ExprTree precond)]

        override this.generatePostcond (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (funcNameBase: string) (p: CodegenScope) (t: Asn1AcnAst.Asn1Type) (codec: Codec) =
            match enc with
            | ACN ->
                let errTpe = IntegerType Int
                let postcondExpr =
                    match codec with
                    | Encode ->
                        let resPostcond = {Var.name = "res"; tpe = eitherTpe errTpe (IntegerType Int)}
                        let decodePureId = $"{t.FT_TypeDefinition.[Scala].typeName}_ACN_Decode_pure"
                        generateEncodePostcondExpr r t p.accessPath resPostcond decodePureId
                    | Decode ->
                        let resPostcond = {Var.name = "res"; tpe = eitherMutTpe errTpe (fromAsn1TypeKind t.Kind)}
                        generateDecodePostcondExpr r t resPostcond
                Some (show (ExprTree postcondExpr))
            | _ -> Some (show (ExprTree (BoolLit true)))

        override this.generateSequenceChildProof (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec): string list =
            generateSequenceChildProof r enc stmts pg codec

        override this.generateSequenceProof (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (nestingScope: NestingScope) (sel: AccessPath) (codec: Codec): string list =
            let proof = generateSequenceProof r enc t sq nestingScope sel codec
            proof |> Option.map (fun p -> show (ExprTree p)) |> Option.toList

        // override this.generateChoiceProof (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (ch: Asn1AcnAst.Choice) (stmt: string) (sel: Selection) (codec: Codec): string =
        //     let proof = generateChoiceProof enc t ch stmt sel codec
        //     show (ExprTree proof)

        override this.generateSequenceOfLikeProof (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (o: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): SequenceOfLikeProofGenResult option =
            generateSequenceOfLikeProof r enc o pg codec

        override this.generateIntFullyConstraintRangeAssert (topLevelTd: string) (p: CodegenScope) (codec: Codec): string option =
            None
            // TODO: Need something better than that
            (*
            match codec with
            | Encode -> Some $"assert({topLevelTd}_IsConstraintValid(pVal).isRight)" // TODO: HACK: When for CHOICE, `p` gets reset to the choice variant name, so we hardcode "pVal" here...
            | Decode -> None
            *)
        override this.generateOctetStringInvariants (minSize : SIZE) (maxSize : SIZE): string list =
            let inv = octetStringInvariants minSize maxSize This
            [$"require({show (ExprTree inv)})"]

        override this.generateBitStringInvariants (minSize : SIZE) (maxSize : SIZE): string list =
            let inv = bitStringInvariants minSize maxSize This
            [$"require({show (ExprTree inv)})"]

        override this.generateSequenceInvariants  (children: Asn1AcnAst.Asn1Child list): string list =
            let inv = sequenceInvariants children This
            inv |> Option.map (fun inv -> $"require({show (ExprTree inv)})") |> Option.toList

        override this.generateSequenceOfInvariants (minSize : SIZE) (maxSize : SIZE) : string list =
            let inv = sequenceOfInvariants minSize maxSize This
            [$"require({show (ExprTree inv)})"]

        override this.generateSequenceSizeDefinitions (acnAlignment : AcnGenericTypes.AcnAlignment option) (maxAlignment: AcnGenericTypes.AcnAlignment option) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (children : Asn1AcnAst.SeqChildInfo list): string list =
            generateSequenceSizeDefinitions acnAlignment maxAlignment acnMinSizeInBits acnMaxSizeInBits children

        override this.generateChoiceSizeDefinitions (acnAlignment : AcnGenericTypes.AcnAlignment option) (maxAlignment: AcnGenericTypes.AcnAlignment option)
                      (acnMinSizeInBits    : BigInteger)
                      (acnMaxSizeInBits    : BigInteger)
                      (typeDef : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) 
                      (children            : Asn1AcnAst.ChChildInfo list): string list =
            generateChoiceSizeDefinitions acnAlignment maxAlignment acnMinSizeInBits acnMaxSizeInBits typeDef children

        override this.generateSequenceOfSizeDefinitions (typeDef : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (maxSize : SIZE) (acnEncodingClass : Asn1AcnAst.SizeableAcnEncodingClass) (acnAlignment : AcnGenericTypes.AcnAlignment option) (maxAlignment: AcnGenericTypes.AcnAlignment option) (child : Asn1AcnAst.Asn1Type): string list * string list =
            generateSequenceOfSizeDefinitions typeDef  acnMinSizeInBits  acnMaxSizeInBits  maxSize  acnEncodingClass  acnAlignment maxAlignment child 

        override this.generateSequenceSubtypeDefinitions (dealiased: string) (typeDef:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) (children: Asn1AcnAst.Asn1Child list): string list =
            generateSequenceSubtypeDefinitions dealiased typeDef children

        override this.uper =
            {
                Uper_parts.createLv = (fun name -> Asn1SIntLocalVariable(name,None))
                requires_sBlockIndex  = true
                requires_sBLJ = false
                requires_charIndex = false
                requires_IA5String_i = true
                count_var            = Asn1SIntLocalVariable ("nCount", None)
                requires_presenceBit = false
                catd                 = false
                //createBitStringFunction = createBitStringFunction_funcBody_c
                seqof_lv              =
                  (fun id minSize maxSize -> [SequenceOfIndex (id.SequenceOfLevel + 1, None)])

                exprMethodCall = uperExprMethodCall
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
                createLocalVariableEnum =
                    (fun rtlIntType -> GenericLocalVariable {GenericLocalVariable.name = "intVal"; varType= rtlIntType; arrSize= None; isStatic = false; initExp= (Some("0L")) })
                choice_handle_always_absent_child = false
                choice_requires_tmp_decoding = false
            }
        override this.init =
            {
                Initialize_parts.zeroIA5String_localVars    = fun _ -> []
                choiceComponentTempInit                     = false
                initMethSuffix                              = initMethSuffix
            }
        override this.atc =
            {
                Atc_parts.uperPrefix = ""
                acnPrefix            = "ACN_"
                xerPrefix            = "XER_"
                berPrefix            = "BER_"
            }

        override this.CreateMakeFile (r:AstRoot)  (di:DirInfo) =
            ()

        override this.CreateAuxFiles (r:AstRoot)  (di:DirInfo) (arrsSrcTstFiles : string list, arrsHdrTstFiles:string list) =
            let CreateScalaMainFile (r:AstRoot)  outDir  =
                // Main file for test case
                let printMain =    test_cases_scala.PrintMain //match l with C -> test_cases_c.PrintMain | Ada -> test_cases_c.PrintMain
                let content = printMain "testsuite"
                let outFileName = Path.Combine(outDir, "mainprogram.scala")
                File.WriteAllText(outFileName, content.Replace("\r",""))

            CreateScalaMainFile r di.srcDir



        override this.getDirInfo (target:Targets option) rootDir =
            {
                rootDir = rootDir;
                srcDir=Path.Combine(rootDir, srcDirName);
                asn1rtlDir=Path.Combine(rootDir, asn1rtlDirName);
                boardsDir=rootDir
            }

        override this.getTopLevelDirs (target:Targets option) =
            [asn1rtlDirName; srcDirName; "lib"]
        override this.getChChildIsPresent   (arg:AccessPath) (chParent:string)  (pre_name:string) =
            sprintf "%s.isInstanceOf[%s.%s_PRESENT]" (arg.joined this) chParent pre_name
        override this.extractEnumClassName (prefix: String)(varName: String)(internalName: String): String =
            prefix + varName.Substring(0, max 0 (varName.Length - (internalName.Length + 1))) // TODO: check case where max is needed
