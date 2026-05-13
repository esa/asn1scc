module LangGeneric_python

open System.Linq
open AbstractMacros
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open CommonTypes
open System.Numerics
open DAst
open FsUtils
open Language
open System.IO
open System
// open ProofGen // TODO
// open ProofAst // TODO

let rec resolveReferenceType(t: Asn1TypeKind): Asn1TypeKind =
    match t with
    | ReferenceType rt -> resolveReferenceType rt.resolvedType.Kind
    | _ -> t

let isPythonPrimitive (t: Asn1TypeKind) =
    match resolveReferenceType t with
    | Integer _ | Real _ | NullType _ | Boolean _ -> true
    | _ -> false

let initMethSuffix k =
    match isPythonPrimitive k with
    | false ->
    match k with
        | BitString bitString -> ""
        | _ -> "()"
    | true -> ""

let isEnumForPythonelseFalse (k: Asn1TypeKind): bool =
    match ST.lang with
    | Python ->
        match resolveReferenceType k with
        | Enumerated e -> true
        | _ -> false
    | _ -> false

let isSequenceForPythonelseFalse (k: Asn1TypeKind): bool =
    match ST.lang with
    | Python ->
        match k with
        | Sequence s -> true
        | _ -> false
    | _ -> false

let isOctetStringForPythonelseFalse (k: Asn1TypeKind): bool =
    match ST.lang with
    | Python ->
        match k with
        | OctetString s -> true
        | _ -> false
    | _ -> false

let uperExprMethodCall (k: Asn1TypeKind) (sChildInitExpr: string) =
    let isSequence = isSequenceForPythonelseFalse k
    let isEnum = isEnumForPythonelseFalse k
    let isOctetString = isOctetStringForPythonelseFalse k

    match isSequence || sChildInitExpr.Equals("None") || isEnum || isOctetString with
    | true -> ""
    | false -> initMethSuffix k

type LangBasic_python() =
    inherit ILangBasic()
    
    override this.cmp (s1:string) (s2:string) = s1 = s2
    override this.isCaseSensitive = true
    override this.keywords = python_keywords
    override this.isKeyword (token) = python_keywords.Contains token
    override this.OnTypeNameConflictTryAppendModName = false
    override this.declare_IntegerNoRTL = "", "int", "INTEGER"
    override this.declare_PosIntegerNoRTL = "", "int", "INTEGER"
    override this.getRealRtlTypeName = "", "float", "REAL"
    override this.getObjectIdentifierRtlTypeName relativeId =
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
    override this.getNullRtlTypeName = "", "NullType", "NullType"
    override this.getBoolRtlTypeName = "", "bool", "bool"

let isClassVariable (receiverId: string) : bool =
        // True when the receiverId is an instance attribute that needs "self." prefix.
        // "self" itself is the receiver parameter — no prefix needed (would produce "self.self.field").
        receiverId <> "self" && receiverId.StartsWith("self")

// Helper type to store cross-sequence ACN dependency information
type CrossSequenceAcnDep = {
    acnChildId: ReferenceToType
    acnChildCName: string
    dependency: AcnDependency
}

// Helper function to find ACN children in a child sequence that have dependencies on fields in the parent sequence
// Takes the child sequence's ACN children and checks which ones depend on fields outside the child sequence
// Returns information about the ACN child ID and its dependency
let findCrossSequenceAcnDeps (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (parentSeqType:Asn1AcnAst.Asn1Type) (childSeqType:Asn1AcnAst.Asn1Type) (childSeqAcnChildren: Asn1AcnAst.AcnChild list) : CrossSequenceAcnDep list =
    // printfn "[DEBUG] findCrossSequenceAcnDeps: Checking child sequence %s (parent: %s) ACN children for cross-sequence dependencies" (childSeqType.id.AsString) (parentSeqType.id.AsString)
    // printfn "[DEBUG] findCrossSequenceAcnDeps: Child sequence has %d ACN children" childSeqAcnChildren.Length

    // For each ACN child in the child sequence, check if it has dependencies on fields in the parent sequence (outside the child sequence)
    let result =
        childSeqAcnChildren
        |> List.choose (fun acnChild ->
            // printfn "[DEBUG] findCrossSequenceAcnDeps: Checking ACN child %s (id: %s) for cross-sequence dependencies" acnChild.Name.Value (acnChild.id.AsString)

            // Find dependencies where this ACN child is the determinant
            let relevantDeps =
                deps.acnDependencies
                |> List.filter (fun d -> d.determinant.id = acnChild.id)

            // printfn "[DEBUG] findCrossSequenceAcnDeps: Found %d dependencies for ACN child %s" relevantDeps.Length acnChild.Name.Value

            // Check if any dependency references a field in the parent sequence (outside the child sequence)
            relevantDeps
            |> List.tryFind (fun dep ->
                // Check if the dependency's asn1Type (the field that determines the ACN child's value) is outside the child sequence
                let depFieldPath = dep.asn1Type.AsString
                let parentPath = parentSeqType.id.AsString
                let childPath = childSeqType.id.AsString

                // printfn "[DEBUG] findCrossSequenceAcnDeps: Checking dependency - ACN child uses field: %s" depFieldPath

                // The field is in the parent (outside child) if:
                // 1. It starts with the parent path
                // 2. It doesn't start with the child path (or is not nested inside child path)
                let isOutsideChild = depFieldPath.StartsWith(parentPath) && not (depFieldPath.StartsWith(childPath + "#") || depFieldPath.StartsWith(childPath + ".") || depFieldPath = childPath)

                // if isOutsideChild then
                    // printfn "[DEBUG] findCrossSequenceAcnDeps: >>> CROSS-SEQUENCE DEPENDENCY FOUND! ACN child %s (in %s) needs to access field %s (outside child sequence)" acnChild.Name.Value childPath depFieldPath

                isOutsideChild
            )
            |> Option.map (fun dep ->
                {
                    acnChildId = acnChild.id
                    acnChildCName = AcnCreateFromAntlr.getAcnDeterminantName acnChild.id
                    dependency = dep
                }
            )
        )

    // printfn "[DEBUG] findCrossSequenceAcnDeps: Returning %d cross-sequence ACN dependencies" result.Length
    result
    
type LangGeneric_python() =
    inherit ILangGeneric()
    
    override _.ArrayStartIndex = 0
    
    override _.intValueToString (i:BigInteger) (intClass:Asn1AcnAst.IntegerClass) =
        match intClass with
        | Asn1AcnAst.ASN1SCC_Int8     _ ->  sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_Int16    _ ->  sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_Int32    _ ->  sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_Int64    _ ->  sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_Int _ when
            i >= BigInteger System.Int32.MinValue &&
            i <= BigInteger System.Int32.MaxValue ->
                sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_Int      _ ->  sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_UInt8    _ ->  sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_UInt16   _ ->  sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_UInt32   _ ->  sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_UInt64   _ ->  sprintf "%s" (i.ToString())
        | Asn1AcnAst.ASN1SCC_UInt     _ ->  sprintf "%s" (i.ToString())

    override _.asn1SccIntValueToString (i: BigInteger) (unsigned: bool) =
        let iStr = i.ToString()
        if unsigned then iStr else iStr

    override _.doubleValueToString (v:double) =
        v.ToString(FsUtils.doubleParseString, System.Globalization.NumberFormatInfo.InvariantInfo)

    override _.initializeString (asciiCode:BigInteger option) (stringSize: int) =
        match asciiCode with
        | Some ac -> $"\"%c{char ac}\" * %d{stringSize} + \"\\x00\""
        | None -> $"\"\\x00\" * %d{stringSize}"


    override _.supportsInitExpressions = true

    override this.getPointer (sel: AccessPath) = sel.joined this

    override this.getValue (sel: AccessPath) = sel.joined this        
    override this.getValueUnchecked (sel: AccessPath) (kind: UncheckedAccessKind) = this.joinSelectionUnchecked sel kind
    override this.getPointerUnchecked (sel: AccessPath) (kind: UncheckedAccessKind) = this.joinSelectionUnchecked sel kind
    override _.joinSelectionUnchecked (sel: AccessPath) (kind: UncheckedAccessKind) =
        let len = sel.steps.Length
        let receiverPrefix = if isClassVariable sel.rootId then "self." else ""
        
        let fold =
            List.fold (fun str (ix, accessor) ->
                    let accStr =
                        match accessor with
                        | ValueAccess (id, _, isOpt) ->
                            if isOpt && (kind = FullAccess || ix < len - 1) then $".{id}" else $".{id}"
                        | PointerAccess (id, _, isOpt) ->
                            if isOpt && (kind = FullAccess || ix < len - 1) then $".{id}" else $".{id}"
                        | _ -> ""

                    $"{str}{accStr}"
                ) (receiverPrefix + sel.rootId) (List.indexed sel.steps)
        
        List.fold (fun str (ix, accessor) ->                       
                    let arrIndex =
                        match accessor with
                        | ArrayAccess (ix, _) -> $"[{ix}]"
                        | _ -> ""
                    $"{str}{arrIndex}"
                ) fold (List.indexed sel.steps)
    
    override _.asSelectionIdentifier (sel: AccessPath) =
        let fold =
            List.fold (fun str (ix, accessor) ->
                    let accStr =
                        match accessor with
                        | ValueAccess (id, _, isOpt) -> $"_{id}"
                        | PointerAccess (id, _, isOpt) -> $"_{id}"
                        | _ -> ""
                       
                    $"{str}{accStr}"
                ) sel.rootId (List.indexed sel.steps)
        
        if sel.steps.IsEmpty then
            fold
        else
            let last = (sel.steps.Reverse ()).First()
            let accStr =
                match last with
                | ArrayAccess (ix, _) -> $"[{ix}]"
                | _ -> ""
            $"{fold}{accStr}"

  
    
    override this.getAccess (sel: AccessPath) = "."

    override this.getAccess2 (acc: AccessStep) =
        match acc with
            | ValueAccess (sel, _, _) -> $".{sel}"
            | PointerAccess (sel, _, _) -> $".{sel}"
            | ArrayAccess (ix, _) -> $"[{ix}]"
            
    override this.getAccess3 (acc: AccessStep) =
        match acc with
            | ValueAccess (sel, _, _) -> $"_{sel}"
            | PointerAccess (sel, _, _) -> $"_{sel}"
            | ArrayAccess (ix, _) -> $"[{ix}]"

    override this.getPtrPrefix _ = ""

    override this.getPtrSuffix _ = ""

    override this.getStar _ = ""

    override _.real_annotations = []

    override this.getArrayItem (sel: AccessPath) (idx:string) (childTypeIsString: bool) = 
        (sel.appendSelection "arr" ArrayElem false).append (ArrayAccess (idx, if childTypeIsString then ArrayElem else ByValue))

    override this.getNamedItemBackendName (defOrRef: TypeDefinitionOrReference option) (nm: Asn1AcnAst.NamedItem) =
        // For Python, use the original name without the type prefix
        let itemName = ToC nm.Name.Value

        let itemname =
            match defOrRef with
            | Some (TypeDefinition td) ->
                // Subtypes (My2ndEnum ::= BaseEnum) have baseType pointing to the actual enum class.
                // Always use the base type's name for _Enum since only BaseEnum defines _Enum.
                match td.baseType with
                | Some bt ->
                    let prefix = match bt.programUnit with Some pu when pu <> "" -> pu + "." | _ -> ""
                    prefix + bt.typedefName + "_Enum." + itemName
                | None ->
                    td.typedefName + "_Enum." + itemName
            | Some (ReferenceToExistingDefinition rted) ->
                // For ReferenceToExistingDefinition, check if it has programUnit
                match rted.programUnit with
                | Some pu when pu <> "" ->
                    // Add module prefix: Module.TypeName_Enum.item
                    pu + "." + rted.typedefName + "_Enum." + itemName
                | _ ->
                    // No module prefix needed
                    rted.typedefName + "_Enum." + itemName
            | _ -> itemName
        itemname

    override this.getNamedItemBackendName0 (nm:Asn1Ast.NamedItem) =
        // For Python, use the original name without the type prefix
        ToC nm.Name.Value
    override this.setNamedItemBackendName0 (nm:Asn1Ast.NamedItem) (newValue:string) : Asn1Ast.NamedItem =
        {nm with python_name = newValue}

    override this.getNamedItemBackendName2 (_:string) (_:string) (nm:Asn1AcnAst.NamedItem) =
        ToC nm.python_name

    override this.decodeEmptySeq _ = None
    override this.decode_nullType _ = None

    override this.Length exp sAcc =
        isvalid_python.ArrayLen exp sAcc
    override this.FixedSizeSizableHasCount = true

    override this.typeDef (ptd:Map<ProgrammingLanguage, FE_PrimitiveTypeDefinition>) = ptd.[Python]
    override this.definitionOrRef (d:Map<ProgrammingLanguage, TypeDefinitionOrReference>) = d.[Python]
    override this.getTypeDefinition (td:Map<ProgrammingLanguage, FE_TypeDefinition>) = td.[Python]
    override this.getEnumTypeDefinition (td:Map<ProgrammingLanguage, FE_EnumeratedTypeDefinition>) = td.[Python]
    override this.getStrTypeDefinition (td:Map<ProgrammingLanguage, FE_StringTypeDefinition>) = td.[Python]
    override this.getChoiceTypeDefinition (td:Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) = td.[Python]
    override this.getSequenceTypeDefinition (td:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) = td.[Python]
    override this.getSizeableTypeDefinition (td:Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) = td.[Python]
    override this.getAsn1ChildBackendName (ch:Asn1Child) = ch._python_name
    override this.getAsn1ChChildBackendName (ch:ChChildInfo) = ch._python_name
    override _.getChildInfoName (ch:Asn1Ast.ChildInfo) = ch.python_name
    override _.setChildInfoName (ch:Asn1Ast.ChildInfo) (newValue:string) = {ch with python_name = newValue}
    override this.getAsn1ChildBackendName0 (ch:Asn1AcnAst.Asn1Child) = ch._python_name
    override this.getAsn1ChChildBackendName0 (ch:Asn1AcnAst.ChChildInfo) = ch._python_name
    override _.getChoiceChildPresentWhenName (ch:Asn1AcnAst.Choice) (c:Asn1AcnAst.ChChildInfo) (currentModule:string) : string =
        let typeDef = ch.typeDef[Python]
        // Normalize module name (ASN.1 uses hyphens, Python uses underscores)
        let normalizedCurrentModule = ToC currentModule
        let baseTypeName =
            match typeDef.programUnit with
            | "" -> typeDef.typeName
            | pu when pu = normalizedCurrentModule -> typeDef.typeName  // Same module - no prefix
            | pu -> pu + "." + typeDef.typeName                        // Different module - add prefix
        baseTypeName + "InUse." + (ToC c.present_when_name)

    override this.constructReferenceFuncName (baseTypeDefinitionName: string) (codecName: string) (methodSuffix: string): string =
        methodSuffix

    override this.constructFuncName (baseTypeDefinitionName: string) (codecName: string) (methodSuffix: string): string =
        // codecName is "" for UPER, "_ACN" for ACN; methodSuffix is "encode" or "decode" for Python
        let encodingSuffix = if codecName = "_ACN" then "_acn" else "_uper"
        let fullMethodName = methodSuffix + encodingSuffix
        if methodSuffix = "encode" then
            // encode is an instance method: return just method name, caller does p.methodName()
            fullMethodName
        else
            // decode is a classmethod: return Class.methodName, called directly
            baseTypeDefinitionName + "." + fullMethodName

    override this.getFuncNameGeneric (typeDefinition:TypeDefinitionOrReference) (nameSuffix: string): string option  =
        match typeDefinition with
        | ReferenceToExistingDefinition  refEx  -> None
        | TypeDefinition   td                   -> Some nameSuffix

    override this.getUPerFuncName (r:Asn1AcnAst.AstRoot) (codec:CommonTypes.Codec) (t: Asn1AcnAst.Asn1Type) (td:FE_TypeDefinition): option<string> =
        match codec with
        | CommonTypes.Encode -> Some "encode_uper"
        | CommonTypes.Decode -> Some "decode_uper"

    override this.getACNFuncName (r:Asn1AcnAst.AstRoot) (codec:CommonTypes.Codec) (t: Asn1AcnAst.Asn1Type) (td:FE_TypeDefinition): string option =
        match codec with
        | CommonTypes.Encode -> Some "encode_acn"
        | CommonTypes.Decode -> Some "decode_acn"
        // match t.acnParameters with
        // | []    ->
        //     match t.id.tasInfo with
        //     | None -> None
        //     | Some _ -> Some codec.suffix
        // | _     -> None
   
    // Analyze which ACN children from each Asn1 child SEQUENCE need to be returned
    // so that sibling children can reference them (deep field access pattern)
    override this.getAcnChildrenForDeepFieldAccess (asn1Children: Asn1Child list) (acnChildren: AcnChild list) (deps: AcnInsertedFieldDependencies) =
        let result = System.Collections.Generic.Dictionary<string, ResizeArray<string * AcnChild>>()

        // For each ASN.1 child in this sequence
        for i in 0 .. asn1Children.Length - 1 do
            let child = asn1Children.[i]
            let childName = this.getAsn1ChildBackendName child

            // ONLY process if child is a SEQUENCE type
            // Don't process other types like Enums, Integers, etc.
            let childActualType =
                match child.Type.Kind with
                | ReferenceType refType -> refType.resolvedType
                | _ -> child.Type

            match childActualType.Kind with
            | Sequence childSeq ->
                // Get ACN children from the child sequence
                let childSeqAcnChildren =
                    childSeq.children
                    |> List.choose (fun c ->
                        match c with
                        | AcnChild acnCh -> Some acnCh
                        | _ -> None)

                // For each ACN child inside this child sequence
                for nestedAcnChild in childSeqAcnChildren do
                    // Check if any later sibling depends on this nested ACN child
                    for j in (i+1) .. asn1Children.Length - 1 do
                        let siblingChild = asn1Children.[j]

                        // Find dependencies where the sibling depends on this nested ACN child
                        let hasDependency =
                            deps.acnDependencies
                            |> List.exists (fun d ->
                                d.asn1Type = siblingChild.Type.id &&
                                match d.determinant with
                                | AcnChildDeterminant acnCh when acnCh.id = nestedAcnChild.id -> true
                                | _ -> false
                            )

                        if hasDependency then
                            // This ASN.1 child sequence needs to return this nested ACN child
                            if not (result.ContainsKey(childName)) then
                                result.[childName] <- ResizeArray()
                            if not (result.[childName] |> Seq.exists (fun (_, ac) -> ac.id = nestedAcnChild.id)) then
                                result.[childName].Add((nestedAcnChild.c_name, nestedAcnChild))
            | _ -> ()

        result
        |> Seq.map (fun kvp -> (kvp.Key, kvp.Value |> Seq.toList))
        |> Map.ofSeq

    override this.isAcnInlineRequired (parentTypeId: Asn1AcnAst.Asn1Type) (childName: string) (deps: AcnInsertedFieldDependencies) =
        // Check if any dependency has a determinant (ACN child) where the determinant's ID starts with the child field's full type path
        // AND the dependency's asn1Type (field that depends on the determinant) is OUTSIDE the child field
        // Use the parent type's full path + child name to construct the full field path
        let childFieldPath = parentTypeId.id.AsString + "." + childName
        let shouldInlineAny =
            deps.acnDependencies
            |> List.exists (fun d ->
                // Does the determinant belong to the child field?
                let determinantPath = d.determinant.id.AsString
                // Normalize hyphens to underscores for comparison (ASN.1 names with hyphens become underscores in some contexts)
                // Can't use ToC because it replaces dots with underscores, which is not what we want here
                let normalizedDeterminantPath = determinantPath.Replace("-", "_")
                let normalizedChildFieldPath = childFieldPath.Replace("-", "_")
                let determinantBelongsToChild =
                    normalizedDeterminantPath.StartsWith(normalizedChildFieldPath + ".") ||
                    normalizedDeterminantPath = normalizedChildFieldPath

                // Does the dependent field belong OUTSIDE the child field?
                let dependentPath = d.asn1Type.AsString
                let normalizedDependentPath = dependentPath.Replace("-", "_")
                let dependentIsOutsideChild =
                    not (normalizedDependentPath.StartsWith(normalizedChildFieldPath + ".") ||
                         normalizedDependentPath = normalizedChildFieldPath)

                // Inline is required if: determinant is inside child, but dependent field is outside child
                let shouldInline = determinantBelongsToChild && dependentIsOutsideChild
                shouldInline
            )
        // Also require inlining when a sibling ACN field provides this child's size (AcnDepSizeDeterminant).
        // Only applies to OCTET STRING / BIT STRING — their standalone encode_acn writes a length prefix,
        // causing double-writing when the parent also writes the ACN size field.
        // SEQUENCE-OF encode_acn uses self.nCount and writes NO length prefix, so no double-counting;
        // forcing it inline would break the nLen parameter passing in decode.
        let hasExternalSizeDet =
            let normalizedChildFieldPath = childFieldPath.Replace("-", "_")
            let hasSizeDep =
                deps.acnDependencies
                |> List.exists (fun d ->
                    let normalizedDependentPath = d.asn1Type.AsString.Replace("-", "_")
                    let dependentIsThisChild =
                        normalizedDependentPath = normalizedChildFieldPath ||
                        normalizedDependentPath.StartsWith(normalizedChildFieldPath + ".")
                    let isSizeDep =
                        match d.dependencyKind with
                        | AcnDepSizeDeterminant _ -> true
                        | AcnDepSizeDeterminant_bit_oct_str_contain _ -> true
                        | _ -> false
                    dependentIsThisChild && isSizeDep
                )
            if not hasSizeDep then false
            else
                match parentTypeId.Kind with
                | Asn1AcnAst.Sequence sq ->
                    sq.children
                    |> List.exists (fun c ->
                        match c with
                        | Asn1AcnAst.Asn1Child ac when ac.Name.Value.Replace("-", "_") = childName ->
                            match ac.Type.Kind with
                            | Asn1AcnAst.OctetString _ | Asn1AcnAst.BitString _ -> true
                            | Asn1AcnAst.ReferenceType ref when ref.encodingOptions.IsSome -> true
                            | _ ->
                                match ac.Type.ActualType.Kind with
                                | Asn1AcnAst.OctetString _ | Asn1AcnAst.BitString _ -> true
                                | _ -> false
                        | _ -> false
                    )
                | _ -> false
        // Also require inlining when a sizable child has a fixed-size ACN encoding class.
        // WITH COMPONENTS narrowing (e.g. buf (SIZE(10))) gives SZ_EC_LENGTH_EMBEDDED 0I, which
        // means 0 bits for the length prefix (min==max). The named class method would call the
        // base type's encode_acn (e.g. Non_Generic_buf with SIZE(0..200)) which writes a non-zero
        // length prefix. Inlining the anonymous type's function avoids that mismatch.
        let isFixedSizeSizable =
            match parentTypeId.Kind with
            | Asn1AcnAst.Sequence sq ->
                sq.children
                |> List.exists (fun c ->
                    match c with
                    | Asn1AcnAst.Asn1Child ac when ac.Name.Value.Replace("-", "_") = childName ->
                        let isEffectivelyFixed (kind: Asn1AcnAst.Asn1TypeKind) =
                            match kind with
                            | Asn1AcnAst.OctetString os ->
                                os.acnEncodingClass = SZ_EC_FIXED_SIZE || os.acnEncodingClass = SZ_EC_LENGTH_EMBEDDED 0I
                            | Asn1AcnAst.BitString bs ->
                                bs.acnEncodingClass = SZ_EC_FIXED_SIZE || bs.acnEncodingClass = SZ_EC_LENGTH_EMBEDDED 0I
                            | Asn1AcnAst.SequenceOf sof ->
                                sof.acnEncodingClass = SZ_EC_FIXED_SIZE || sof.acnEncodingClass = SZ_EC_LENGTH_EMBEDDED 0I
                            | _ -> false
                        isEffectivelyFixed ac.Type.Kind ||
                        (match ac.Type.Kind with
                         | Asn1AcnAst.ReferenceType rt -> isEffectivelyFixed rt.resolvedType.Kind
                         | _ -> false)
                    | _ -> false
                )
            | _ -> false
        shouldInlineAny || hasExternalSizeDet || isFixedSizeSizable

    override this.getExternalField (getExternalField0: ((AcnDependency -> bool) -> string)) (relPath: RelativePath) (o: Asn1AcnAst.Sequence) (p: CodegenScope)=
        match relPath with
        | RelativePath  [] ->
            let filterDependency (d:AcnDependency) =
                match d.dependencyKind with
                | AcnDepPresenceBool   -> true
                | _                    -> false
            getExternalField0 filterDependency
        | RelativePath (_ ::_) ->
            // Build language-specific access path for deep field reference
            // This handles paths like "primaryHeader.secHeaderFlag" correctly for each language
            let rec getChildResult (seq:Asn1AcnAst.Sequence) (pSeq:CodegenScope) (RelativePath lp) : Choice<CodegenScope, string> option =
                match lp with
                | []    -> Some (Choice1Of2 pSeq)
                | x1::xs ->
                    match seq.children |> Seq.tryFind(fun (c: Asn1AcnAst.SeqChildInfo) -> c.Name = x1) with
                    | None -> None  // Not found in children - signal to caller to use dependency lookup
                    | Some ch ->
                        match ch with
                        | Asn1AcnAst.Asn1Child ch  ->
                            let newPath = this.getSeqChild pSeq.accessPath (this.getAsn1ChildBackendName0 ch) false ch.Optionality.IsSome
                            match ch.Type.ActualType.Kind, xs with
                            | Asn1AcnAst.Sequence s, _::_ ->
                                // Continue navigating for nested sequences
                                getChildResult s {pSeq with accessPath = newPath} (AcnGenericTypes.RelativePath xs)
                            | _, _ ->
                                // Reached the target field
                                Some (Choice1Of2 {pSeq with accessPath = newPath})
                        | Asn1AcnAst.AcnChild acnCh  -> Some (Choice2Of2 acnCh.c_name)

            match getChildResult o p relPath with
            | Some (Choice1Of2 resolvedPath) -> resolvedPath.accessPath.joined this
            | Some (Choice2Of2 varName) -> varName
            | None ->
                // Path not found in sequence children (e.g. ACN parameter) - use dependency lookup
                let filterDependency (d:AcnDependency) =
                    match d.dependencyKind with
                    | AcnDepPresenceBool -> true
                    | _ -> false
                getExternalField0 filterDependency
    
    override this.getAcnChildrenDictStatements (codec: Codec) (acnChildrenEncoded: (string * AcnChild) list) (p: CodegenScope) =
        // Check if this sequence has inline ACN children that need to be returned to parent
        let hasAcnChildrenToReturn =
            codec = Decode &&
            ProgrammingLanguage.ActiveLanguages.Head = Python &&
            not acnChildrenEncoded.IsEmpty
            
        // Build ACN children dictionary and tuple return for Python decode
        if hasAcnChildrenToReturn then
            // Build dictionary entries: {'acn_child_name': acn_child_var}
            let dictEntries =
                acnChildrenEncoded
                |> List.rev  // Reverse to get original order
                |> List.map (fun (varName, acnCh) ->
                    let varRef =
                        match acnCh.Type with
                        | Asn1AcnAst.AcnReferenceToIA5String _ ->
                            $"{p.accessPath.asIdentifier this}_{varName}"
                        | _ -> varName
                    $"'{acnCh.c_name}': {varRef}"
                )
                |> String.concat ", "

            let dictStmt = $"%s{p.accessPath.lastIdOrArr}_acn_children = {{%s{dictEntries}}}"
            let tupleReturnStmt = $"return %s{p.accessPath.asIdentifier this}, %s{p.accessPath.lastIdOrArr}_acn_children"
            [dictStmt], Some tupleReturnStmt
        else
            [], None
    
    override this.updateStateForCrossSequenceAcnParams (r: Asn1AcnAst.AstRoot) (state: State) (p: CodegenScope) (oChildren: Asn1AcnAst.SeqChildInfo list) (child: Asn1Child) (childNestingScope: NestingScope) (deps: AcnInsertedFieldDependencies) (t: Asn1AcnAst.Asn1Type) (codec: Codec) updateFncInEncoding getDeterminantTypeFunc initExpr =
        let childName = this.getAsn1ChildBackendName child
        
        // Check for cross-sequence ACN dependencies
        // Find the child's AST type from the parent sequence's children
        let childAstType =
            oChildren
            |> List.tryPick (fun c ->
                match c with
                | Asn1AcnAst.Asn1Child astChild when astChild.Name = child.Name -> Some astChild.Type
                | _ -> None)
            |> Option.defaultWith (fun () -> failwith (sprintf "Could not find AST child %s in parent sequence" child.Name.Value))
        
        // Extract ACN children from the child sequence type (SubPacket)
        // These ACN children are defined in the child's encoding, not the parent's
        // printfn "[DEBUG] handleChild: Parent sequence %s has %d children total" (t.id.AsString) oChildren.Length
        // printfn "[DEBUG] handleChild: Child sequence %s type" (childAstType.id.AsString)
        
        // Resolve reference types to get actual sequence
        let actualChildSeqType =
            match childAstType.Kind with
            | Asn1AcnAst.ReferenceType refType ->
                // printfn "[DEBUG] handleChild: Child is a ReferenceType, resolving to %s" (refType.resolvedType.id.AsString)
                refType.resolvedType
            | _ -> childAstType
        
        // Get ACN children from the child sequence
        let childSeqAcnChildren =
            match actualChildSeqType.Kind with
            | Asn1AcnAst.Sequence childSeq ->
                // printfn "[DEBUG] handleChild: Child sequence has %d children" childSeq.children.Length
                // childSeq.children |> List.iteri (fun i c ->
                //     match c with
                //     | Asn1AcnAst.Asn1Child a -> printfn "[DEBUG] handleChild:   Child %d: Asn1Child %s" i a.Name.Value
                //     | Asn1AcnAst.AcnChild a -> printfn "[DEBUG] handleChild:   Child %d: AcnChild %s (id: %s)" i a.Name.Value (a.id.AsString))
                childSeq.children
                |> List.choose (fun c ->
                    match c with
                    | Asn1AcnAst.AcnChild acnCh -> Some acnCh
                    | _ -> None)
            | _ ->
                // printfn "[DEBUG] handleChild: Child is not a sequence"
                []
        
        // printfn "[DEBUG] handleChild: Child sequence has %d ACN children" childSeqAcnChildren.Length
        
        let crossSeqAcnDeps = findCrossSequenceAcnDeps r deps t childAstType childSeqAcnChildren
        // printfn "[DEBUG] handleChild: Found %d cross-sequence ACN deps for child %s" crossSeqAcnDeps.Length childName
        
        // Get the module for this type
        let currentModule = r.Modules |> Seq.find(fun m -> m.Name.Value = t.moduleName)
        
        // Generate update code for cross-sequence ACN children in the parent context (current sequence)
        let crossSeqAcnUpdateStmts, crossSeqAcnParamsList, ns0 =
            match codec with
            | Encode when not crossSeqAcnDeps.IsEmpty ->
                // printfn "[DEBUG] handleChild: Generating update code for cross-sequence ACN children in parent context"
        
                crossSeqAcnDeps
                |> List.fold (fun (stmts, paramsList, state) crossDep ->
                    // printfn "[DEBUG] handleChild: Processing cross-sequence ACN child %s (id: %s)" crossDep.acnChildCName (crossDep.acnChildId.AsString)
        
                    // Get the update function for this ACN child
                    let funcUpdateStatement, newState = updateFncInEncoding currentModule crossDep.acnChildId state
        
                    match funcUpdateStatement with
                    | Some updateFunc ->
                        // printfn "[DEBUG] handleChild: Found update function, generating update code"
        
                        // Create a temporary AcnChild record to pass to the update function
                        // We need to construct this from the dependency information
                        let tempAcnChild = {
                            AcnChild.Name = StringLoc.ByValue crossDep.acnChildCName
                            id = crossDep.acnChildId
                            c_name = crossDep.acnChildCName
                            Type = match crossDep.dependency.determinant with
                                   | AcnChildDeterminant ch -> ch.Type
                                   | _ -> failwith "Expected AcnChildDeterminant"
                            typeDefinitionBodyWithinSeq = getDeterminantTypeFunc crossDep.dependency.determinant
                            funcBody = (fun _ _ _ _ _ -> None) // Dummy function body - not used for update (5 parameters)
                            funcUpdateStatement = Some updateFunc
                            Comments = [||] // Empty array
                            deps = { Asn1AcnAst.AcnInsertedFieldDependencies.acnDependencies = [crossDep.dependency] }
                            initExpression = initExpr currentModule (match crossDep.dependency.determinant with AcnChildDeterminant ch -> ch.Type | _ -> failwith "Expected AcnChildDeterminant")
                        }
        
                        // Generate the update code using the parent scope (current sequence)
                        let childP = {CodegenScope.modName = p.modName; accessPath= AccessPath.valueEmptyPath crossDep.acnChildCName}
                        let pRoot = this.getParamType t codec  // Use parent sequence type
                        let updateStatement = updateFunc.updateAcnChildFnc tempAcnChild childNestingScope childP pRoot
        
                        // printfn "[DEBUG] handleChild: Generated update statement for %s" crossDep.acnChildCName

                        // Create statement for this update
                        let stmt = {
                            SequenceChildStmt.body = Some updateStatement
                            lvs = updateFunc.localVariables
                            errCodes = updateFunc.errCodes
                            userDefinedFunctions = []
                            icdComments = updateFunc.icdComments
                        }

                        // Add parameter to pass to child sequence
                        let param = sprintf "%s=%s" crossDep.acnChildCName crossDep.acnChildCName

                        (stmt :: stmts, param :: paramsList, newState)
                    | None ->
                        // printfn "[DEBUG] handleChild: No update function found for ACN child %s" crossDep.acnChildCName
                        (stmts, paramsList, newState)
                ) ([], [], state)
                |> fun (stmts, paramsList, state) -> (List.rev stmts, List.rev paramsList, state)
            | _ -> ([], [], state)
        
        // printfn "[DEBUG] handleChild: Generated %d update statements and %d parameters for cross-sequence ACN children" crossSeqAcnUpdateStmts.Length crossSeqAcnParamsList.Length
        
        // // Merge cross-sequence ACN parameters with existing ACN parameters
        // let acnParamsForTemplate = acnParamsForTemplate @ crossSeqAcnParamsList
        // printfn "[DEBUG] handleChild: Total ACN parameters for template: %d" acnParamsForTemplate.Length
        crossSeqAcnUpdateStmts, crossSeqAcnParamsList, ns0
        
        
    override this.adjustTypedefWithFullPath (typeName: string) (moduleName: string) =
        if typeName = moduleName then moduleName + "." + typeName else typeName

    override this.getRtlFiles (encodings:Asn1Encoding list) (_ :string list) =
        let encRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN ) with true -> ["asn1crt_encoding"] | false -> []
        let uperRtl = match encodings |> Seq.exists(fun e -> e = UPER || e = ACN) with true -> ["asn1crt_encoding_uper"] | false -> []
        let acnRtl = match encodings |> Seq.exists(fun e -> e = ACN) with true -> ["asn1crt_encoding_acn"] | false -> []
        let xerRtl = match encodings |> Seq.exists(fun e -> e = XER) with true -> ["asn1crt_encoding_xer"] | false -> []
        encRtl@uperRtl@acnRtl@xerRtl

    override this.getEmptySequenceInitExpression sTypeDefName = $"{sTypeDefName}()"
    override this.callFuncWithNoArgs () = "()"
    override this.rtlModuleName = ""
    override this.AssignOperator = "="
    override this.TrueLiteral = "True"
    override this.FalseLiteral = "False"
    override this.emptyStatement = "pass"
    override this.bitStreamName = "BitStream"
    override this.unaryNotOperator = "not"
    override this.modOp = "%"
    override this.eqOp = "=="
    override this.neqOp = "!="
    override this.andOp = "and"
    override this.orOp = "or"
    override this.initMethod = InitMethod.Procedure
    override _.decodingKind = Copy
    override _.subtypeDecodeWrap pp currentTypeName isPrimitive =
        if isPrimitive then Some $"{pp} = {currentTypeName}({pp})"
        else Some $"{pp} = {currentTypeName}(**vars({pp}))"
    override _.ArrayInitByAppend = true
    override this.TempArrayItemSuffix = "_arr_elem"
    override _.usesWrappedOptional = false
    override this.castExpression (sExp:string) (sCastType:string) = sprintf "%s(%s)" sCastType sExp
    override this.createSingleLineComment (sText:string) = sprintf "#%s" sText

    // In case of Python, there is no Spec and Body file distinction. We use no Suffix and use Append in GenerateFiles.fs to merge the spec & body into the same file.
    override _.SpecNameSuffix = ""
    override _.SpecExtension = "py"
    override _.BodyExtension = "py"
    override _.isFilenameCaseSensitive = true
    
    override _.Keywords = CommonTypes.python_keywords

    override _.getValueAssignmentName (vas: ValueAssignment) = vas.python_name

    override this.hasModules = true
    override this.allowsSrcFilesWithNoFunctions = true
    override this.requiresValueAssignmentsInSrcFile = false
    override this.supportsStaticVerification = false
    override this.isObjectOriented = true
    override this.nullTerminatorByte = None
    override this.charToNumericValueExpression charValue = sprintf "ord(%s)" charValue
    override this.validationStringPrefix = "self.arr"
    override this.shouldRemoveModulePrefixFromTypedef = true
    override this.getEnumSelectionJoin path = this.joinSelectionEnum path
    override this.getAlignmentByteTypeName = "byte"
    override this.getAlignmentWordTypeName = "word"
    override this.getAlignmentDWordTypeName = "dword"
    override this.shouldAppendToBodyFile = true
    override this.shouldGenerateInitFiles = true
    override this.shouldAppendTestCaseFile = true
    override this.shouldWriteThenAppendTestSuite = true

    override this.getSeqChildIsPresent (sel: AccessPath) (childName: string) =
        sprintf "%s%s%s is not None" (sel.joined this) (this.getAccess sel) childName

    override this.getSeqChild (sel: AccessPath) (childName:string) (childTypeIsString: bool) (childIsOptional: bool) =
        sel.appendSelection childName (if childTypeIsString then ArrayElem else ByValue) childIsOptional
    
    override this.getSeqChildDependingOnChoiceParent (parents: (CodegenScope * Asn1AcnAst.Asn1Type) list) (p: AccessPath) (childName: string) (childTypeIsString: bool) (childIsOptional: bool) =
        // When a sequence is inlined as a choice child, p.accessPath is already self.data (the choice's data field).
        // We must still append the field name to get self.data.fieldName.
        this.getSeqChild p childName childTypeIsString childIsOptional
            
    override this.getChChild (sel: AccessPath) (childName:string) (childTypeIsString: bool) : AccessPath =
        sel.appendSelection "data" ByValue false

    override this.getChChildForKind (accessPath: AccessPath) (childName: string) (isString: bool) (kind: Asn1TypeKind) (codec: Codec) =
        let childPath = this.getChChild accessPath childName isString
        // Python enum wrappers store their value in a .val field; for encode we need self.data.val, not self.data
        match kind, codec with
        | Enumerated _, Encode -> childPath.appendSelection "val" ByValue false
        | _ -> childPath

    override this.choiceIDForNone (typeIdsSet:Map<string,int>) (id:ReferenceToType) = ""

    override this.presentWhenName (defOrRef:TypeDefinitionOrReference option) (ch:ChChildInfo) : string =
        let parentName =
            match defOrRef with
            | Some a -> match a with
                        | ReferenceToExistingDefinition b -> b.typedefName + "InUse."
                        | TypeDefinition c -> c.typedefName + "InUse."
            | None -> ""
        parentName + (ToC ch._present_when_name_private)

    override this.presentWhenName0 (defOrRef:TypeDefinitionOrReference option) (ch:Asn1AcnAst.ChChildInfo) : string =
        let parentName =
            match defOrRef with
            | Some a -> match a with
                        | ReferenceToExistingDefinition b -> b.typedefName + "."
                        | TypeDefinition c -> c.typedefName + "."
            | None -> ""
        parentName + (ToC ch.present_when_name)

    override this.getParamTypeSuffix (t:Asn1AcnAst.Asn1Type) (suf:string) (c:Codec) : CodegenScope =
        let p = this.getParamType t c
        {p with accessPath.rootId = p.accessPath.rootId + suf}
    
    override this.getParamTypeSuffixForEquals (t:Asn1AcnAst.Asn1Type) (s: string) (c:Codec) =
        let rec getRecvType (kind: Asn1AcnAst.Asn1TypeKind) =
            match kind with
            | Asn1AcnAst.NumericString _ | Asn1AcnAst.IA5String _ -> ArrayElem
            | Asn1AcnAst.ReferenceType r -> getRecvType r.resolvedType.Kind
            | _ -> ByPointer
        let recvId = 
            match s with
            | "1" -> "self"
            | "2" -> "other"
            | _ -> "param" + s
        {CodegenScope.modName = ToC t.id.ModName; accessPath = AccessPath.emptyPath recvId (getRecvType t.Kind) }
        // {p with accessPath.rootId = p.accessPath.rootId + s}
    
    override this.getParamType (t:Asn1AcnAst.Asn1Type) (c:Codec) : CodegenScope =
        let rec getRecvType (kind: Asn1AcnAst.Asn1TypeKind) =
            match kind with
            | Asn1AcnAst.NumericString _ | Asn1AcnAst.IA5String _ -> ArrayElem
            | Asn1AcnAst.ReferenceType r -> getRecvType r.resolvedType.Kind
            | _ -> ByPointer
        let recvId = match t.Kind, c with
                        | _, Decode -> "instance_" + t.FT_TypeDefinition[Python].asn1Name
                        | Asn1AcnAst.Enumerated _, Encode -> "self.val" // For enums, we encapsulate the inner value into a "val" object
                        | _, Encode -> "self"                           // For class methods, the receiver is always "self"

        {CodegenScope.modName = ToC t.id.ModName; accessPath = AccessPath.emptyPath recvId (getRecvType t.Kind) }
    
    override this.getParamTypeAtc (t:Asn1AcnAst.Asn1Type) (c:Codec) : CodegenScope =
        let res = this.getParamType t c
        {res with accessPath.rootId = "inputVal"}
            
    override this.getParamValue (t:Asn1AcnAst.Asn1Type) (p:AccessPath) (c:Codec) =
        p.joined this

    override this.getLocalVariableDeclaration (lv:LocalVariable) : string =
        match lv with
        | SequenceOfIndex (i,None)                  -> sprintf "i%d = 0" i
        | SequenceOfIndex (i,Some iv)               -> sprintf "i%d = %s" i iv
        | IntegerLocalVariable (name,None)          -> sprintf "%s = 0" name
        | IntegerLocalVariable (name,Some iv)       -> sprintf "%s = %s" name iv
        | Asn1SIntLocalVariable (name,None)         -> sprintf "%s = 0" name
        | Asn1SIntLocalVariable (name,Some iv)      -> sprintf "%s = %s" name iv
        | Asn1UIntLocalVariable (name,None)         -> sprintf "%s = 0" name
        | Asn1UIntLocalVariable (name,Some iv)      -> sprintf "%s = %s" name iv
        | FlagLocalVariable (name,None)             -> sprintf "%s = False" name
        | FlagLocalVariable (name,Some iv)          -> sprintf "%s = %s" name iv
        | BooleanLocalVariable (name,None)          -> sprintf "%s = False" name
        | BooleanLocalVariable (name,Some iv)       -> sprintf "%s = %s" name iv
        | AcnInsertedChild(name, vartype, initVal)  ->
            sprintf "%s = %s" name initVal
        | GenericLocalVariable lv                   ->
            sprintf "%s = %s" lv.name (if lv.initExp.IsNone then "NullType" else lv.initExp.Value)

    override this.getLongTypedefName (tdr:TypeDefinitionOrReference) : string =
        match tdr with
        | TypeDefinition  td -> td.typedefName
        | ReferenceToExistingDefinition ref ->
            match ref.programUnit with
            | Some pu ->
                match pu with
                | "" -> ref.typedefName
                | _ -> pu + "." + ref.typedefName
            | None    -> ref.typedefName

    override this.getObjectIdentifierIsValidExpr (p: CodegenScope) (isRelative: bool) : string =
        let ptr = this.getPointer p.accessPath
        if isRelative then ptr + ".is_roid_structurally_valid()"
        else ptr + ".is_structurally_valid()"

    override this.getQualifiedTypeName (tdr: TypeDefinitionOrReference) (modName: string) : string =
        match tdr with
        | TypeDefinition td -> modName + "." + td.typedefName
        | ReferenceToExistingDefinition ref ->
            match ref.programUnit with
            | Some pu when pu <> "" -> pu + "." + ref.typedefName
            | _ -> modName + "." + ref.typedefName

    override this.getLongTypedefNameBasedOnModule (tdr:FE_TypeDefinition) (currentModule: string) : string =
        if tdr.programUnit = ToC currentModule
        then
            tdr.typeName
        else
            (if tdr.programUnit.Length > 0 then tdr.programUnit + "." else "") + tdr.typeName
    
    override this.getLongTypedefNameFromReferenceToTypeAndCodegenScope (rf: ReferenceToType) (typeDefinition: TypeDefinitionOrReference) (p: CodegenScope) : string option =
        match typeDefinition with
        | TypeDefinition td ->
            // A new type definition exists - use its name (wrapper class exists)
            let typeName = td.typedefName
            // Check if we need module prefix
            match rf.topLevelTas with
            | Some k when p.modName <> (ToC k.modName) ->
                Some ((ToC k.modName) + "." + typeName)
            | _ ->
                Some typeName
        | ReferenceToExistingDefinition _ ->
            // No new type definition - check if we're referencing a top-level type
            match rf.tasInfo with
            | Some k ->
                // Direct reference to a top-level type assignment
                let tasName = ToC k.tasName
                let modName = ToC k.modName
                Some (if p.modName <> modName then modName + "." + tasName else tasName)
            | None ->
                // Inline field with no new type definition - use primitive type (None)
                None
        
    override this.longTypedefName2 (td: TypeDefinitionOrReference) (hasModules: bool) (moduleName: string) : string =
        let k =
            match td with
            | TypeDefinition  td ->
                // When defining a type within its own module, don't use module prefix
                td.typedefName// + "MINUSONETH"
            | ReferenceToExistingDefinition ref ->
                match ref.programUnit with
                | Some pu ->
                    match hasModules with
                    | true   ->
                        match pu with
                        | "" -> ref.typedefName// + "ZEROTH"
                        | k when k = moduleName -> ref.typedefName// + "FIRST"
                        | _ -> pu + "." + ref.typedefName// + "DEFAULT"
                    | false     -> ref.typedefName// + "THIRD"
                | None    -> ref.typedefName// + "FOURTH"
        k
    

    override this.toHex n = sprintf "0x%x" n

    override this.bitStringValueToByteArray (v : BitStringValue) = 
        FsUtils.bitStringValueToByteArray (StringLoc.ByValue v)

    override this.getTopLevelDirs (target:Targets option) = []

    override this.getDirInfo (target:Targets option) rootDir =
         let rootDir = Path.Combine(rootDir, "asn1pylib")
         let di = {
          rootDir = rootDir
          srcDir = Path.Combine(rootDir, "asn1src")
          asn1rtlDir = Path.Combine(rootDir, "asn1python")
          boardsDir = rootDir
         }
         Directory.CreateDirectory di.rootDir |> ignore         
         Directory.CreateDirectory di.srcDir |> ignore         
         Directory.CreateDirectory di.asn1rtlDir |> ignore         
         di

    override this.getChChildIsPresent (arg:AccessPath) (chParent:string) (pre_name:string) =
        sprintf "True"

    override this.CreateMakeFile (r:AstRoot) (di:DirInfo) =
        let printPyproject = aux_python.PrintMakeFile
        let content = printPyproject [""] false false false
        File.WriteAllText(Path.Combine(di.rootDir, "pyproject.toml"), content)

    override this.CreateAuxFiles (r:AstRoot) (di:DirInfo) (arrsSrcTstFiles : string list, arrsHdrTstFiles:string list) =
        let CreatePythonMainFile (r:AstRoot) outDir  =
            // Main file for test case
            let printMain = test_cases_python.PrintMain
            let content = printMain "testsuite"
            let outFileName = Path.Combine(outDir, "mainprogram.py")
            File.WriteAllText(outFileName, content.Replace("\r",""))

        CreatePythonMainFile r di.srcDir

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
                        GenericLocalVariable {GenericLocalVariable.name = "arr"; varType = "bytearray"; arrSize = Some sReqBytesForUperEncoding; isStatic = false; initExp = None}
                        GenericLocalVariable {GenericLocalVariable.name = "bitStrm"; varType = "BitStream"; arrSize = None; isStatic = false; initExp = None}
                    ]
            createLocalVariableEnum =
                (fun rtlIntType -> GenericLocalVariable {GenericLocalVariable.name = "intVal"; varType= rtlIntType; arrSize= None; isStatic = false; initExp= (Some("0")) })
            choice_handle_always_absent_child = false
            choice_requires_tmp_decoding = false
        }

    override this.init =
        {
            Initialize_parts.zeroIA5String_localVars    = fun _ -> []
            zeroOctetString_localVars                   = fun _ -> []
            zeroBitString_localVars                     = fun _ -> []
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

    override this.extractEnumClassName (prefix: String)(varName: String)(internalName: String): String =
        prefix + varName.Substring(0, max 0 (varName.Length - (internalName.Length + 1)))
        
    override _.getTypeBasedSuffix (fType: FunctionType) (kind: Asn1AcnAst.Asn1TypeKind) =
        match (kind, fType) with
        | Asn1AcnAst.Asn1TypeKind.Choice _, IsValidFunctionType -> ""
        | _ -> ""

    override _.getRealEncodingSuffix (fpWordSize: System.Numerics.BigInteger) (cls: Asn1AcnAst.RealClass) =
        match cls with
        | ASN1SCC_FP32 -> "_fp32"
        | ASN1SCC_FP64 -> ""
        | ASN1SCC_REAL -> if fpWordSize = 4I then "_fp32" else ""

    override this.castRealForEquality fpWordSize realClass pp realTypeName real32TypeName =
        match realClass with
        | ASN1SCC_REAL when fpWordSize = 4I -> this.castExpression pp real32TypeName
        | ASN1SCC_FP32                      -> this.castExpression pp realTypeName
        | _                                 -> pp

    // Placeholder methods for features not yet implemented in Python
    // override this.generateSequenceAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): string list =
    //     []

    // override this.generateIntegerAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (int: Asn1AcnAst.Integer) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): string list =
    //     []

    // override this.generateBooleanAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (boolean: Asn1AcnAst.Boolean) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): string list =
    //     []

    // override this.generateSequenceOfLikeAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (o: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): string list * string option =
    //     [], None

    // override this.generateOptionalAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (soc: SequenceOptionalChild) (codec: Codec): string list * string =
    //     [], ""

    // override this.generateChoiceAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (ch: Asn1AcnAst.Choice) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): string list =
    //     []

    // override this.generateNullTypeAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (nt: Asn1AcnAst.NullType) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): string list =
    //     []

    // override this.generateEnumAuxiliaries (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (enm: Asn1AcnAst.Enumerated) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): string list =
    //     []

    override this.adaptFuncBodyChoice (childType: Asn1TypeKind) (codec: Codec) (u: IUper) (enc: Asn1Encoding) (childContent_funcBody: string) (childTypeDef: string) (sChildName: string) =
        match childType with
        | Sequence _ | Enumerated _| IA5String _ ->
            let encodeFuncName = match enc with | ACN -> "encode_acn" | _ -> "encode_uper"
            let decodeFuncName = match enc with | ACN -> "decode_acn" | _ -> "decode_uper"
            match codec with
            | Encode -> u.call_base_type_func "self.data" encodeFuncName codec
            | Decode -> u.call_base_type_func ("instance_" + sChildName) (childTypeDef + "." + decodeFuncName) codec
        | _ ->
            let body = "# " + childType.GetType().ToString() + "unchanged funcBody \n" + childContent_funcBody
            match codec, childType with
            | Decode, NullType _ -> body  // NullType takes no constructor arguments
            | Decode, _ ->
                // Only wrap if the template didn't already wrap.
                // Two cases that already produce a typed instance:
                //   1. sType was set → template wraps: instance_X = TypeName(decoded_value)
                //   2. ReferenceType → call_superclass_func_decode: instance_X_decode = TypeName.decode_uper(...)
                let alreadyWrapped =
                    body.Contains(sprintf "instance_%s = %s(" sChildName childTypeDef) ||
                    body.Contains(sprintf "instance_%s_decode = " sChildName)
                if alreadyWrapped then body
                else body + "\ninstance_" + sChildName + " = " + childTypeDef + "(instance_" + sChildName + ")"
            | Encode, _ -> body

    override this.choiceChildDecodePath (sChildTypeDef: string) (sChildName: string) =
        Some (AccessPath.valueEmptyPath ("instance_" + sChildName))

    override this.assembleAllProcs (arrsEncConstBodies: string list) (arrsDecConstBodies: string list) (arrsFuncsAndOtherProcs: string list) (_: string list) =
        let makeConstantsClass (className: string) (bodies: string list) =
            let filtered = bodies |> List.filter (fun s -> not (System.String.IsNullOrWhiteSpace s))
            if filtered.IsEmpty then None
            else
                let lines =
                    filtered
                    |> List.collect (fun body -> body.Split('\n') |> Array.toList)
                    |> List.filter (fun l -> not (System.String.IsNullOrWhiteSpace l))
                    |> List.map (fun l -> "    " + l.Trim())
                Some (sprintf "class %s:\n%s" className (String.concat "\n" lines))
        [ makeConstantsClass "EncodeConstants" arrsEncConstBodies
          makeConstantsClass "DecodeConstants" arrsDecConstBodies ]
        |> List.choose id
        |> (@) (arrsFuncsAndOtherProcs |> List.filter (fun s -> not (System.String.IsNullOrWhiteSpace s)))

    // override this.adaptAcnFuncBody (r: Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (funcBody: AcnFuncBody) (isValidFuncName: string option) (t: Asn1AcnAst.Asn1Type) (codec: Codec): AcnFuncBody =
    //     funcBody

    // override this.generatePrecond (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (codec: Codec): string list =
    //     []

    // override this.generatePostcond (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (p: CallerScope) (t: Asn1AcnAst.Asn1Type) (codec: Codec) =
    //     None

    // override this.generateSequenceChildProof (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (stmts: string option list) (pg: SequenceProofGen) (codec: Codec): string list =
    //     []

    // override this.generateSequenceProof (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (t: Asn1AcnAst.Asn1Type) (sq: Asn1AcnAst.Sequence) (nestingScope: NestingScope) (sel: Selection) (codec: Codec): string list =
    //     []

    // override this.generateSequenceOfLikeProof (r: Asn1AcnAst.AstRoot) (enc: Asn1Encoding) (o: SequenceOfLike) (pg: SequenceOfLikeProofGen) (codec: Codec): SequenceOfLikeProofGenResult option =
    //     None

    // override this.generateIntFullyConstraintRangeAssert (topLevelTd: string) (p: CallerScope) (codec: Codec): string option =
    //     None

    // override this.generateOctetStringInvariants (minSize : SIZE) (maxSize : SIZE): string list =
    //     []

    // override this.generateBitStringInvariants (minSize : SIZE) (maxSize : SIZE): string list =
    //     []

    // override this.generateSequenceInvariants (children: Asn1AcnAst.Asn1Child list): string list =
    //     []

    // override this.generateSequenceOfInvariants (minSize : SIZE) (maxSize : SIZE) : string list =
    //     []

    // override this.generateSequenceSizeDefinitions (acnAlignment : AcnGenericTypes.AcnAlignment option) (maxAlignment: AcnGenericTypes.AcnAlignment option) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (children : Asn1AcnAst.SeqChildInfo list): string list =
    //     []

    // override this.generateChoiceSizeDefinitions (acnAlignment : AcnGenericTypes.AcnAlignment option) (maxAlignment: AcnGenericTypes.AcnAlignment option)
    //               (acnMinSizeInBits    : BigInteger)
    //               (acnMaxSizeInBits    : BigInteger)
    //               (typeDef : Map<ProgrammingLanguage, FE_ChoiceTypeDefinition>) 
    //               (children            : Asn1AcnAst.ChChildInfo list): string list =
    //     []

    // override this.generateSequenceOfSizeDefinitions (typeDef : Map<ProgrammingLanguage, FE_SizeableTypeDefinition>) (acnMinSizeInBits : BigInteger) (acnMaxSizeInBits : BigInteger) (maxSize : SIZE) (acnEncodingClass : Asn1AcnAst.SizeableAcnEncodingClass) (acnAlignment : AcnGenericTypes.AcnAlignment option) (maxAlignment: AcnGenericTypes.AcnAlignment option) (child : Asn1AcnAst.Asn1Type): string list * string list =
    //     [], []

    // override this.generateSequenceSubtypeDefinitions (dealiased: string) (typeDef:Map<ProgrammingLanguage, FE_SequenceTypeDefinition>) (children: Asn1AcnAst.Asn1Child list): string list =
    //     []