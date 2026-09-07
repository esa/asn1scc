module AcnSequence

open System.Globalization

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language

open AcnHelpers
open AcnExpression
open AcnExternalField
open AcnPrimitives


type private SequenceChildStmt = {
    body: string option
    lvs: LocalVariable list
    errCodes: ErrorCode list
    userDefinedFunctions : UserDefinedFunction list
    icdComments : string list
}
type private SequenceChildState = {
    us: State
    childIx: bigint
    uperAccBits: bigint
    acnAccBits: bigint
}
type private SequenceChildResult = {
    stmts: SequenceChildStmt list
    resultExpr: string option
    existVar: string option
    props: SequenceChildProps
    auxiliaries: string list
    icdResult : ((IcdRow list) * (IcdTypeAss list))
} with
    member this.joinedBodies (lm:LanguageMacros) (codec:CommonTypes.Codec): string option =
        this.stmts |> List.choose (fun s -> s.body) |> nestChildItems lm codec

// All the state that handleSequenceChild needs but is naturally constant
// across the foldMap over the SEQUENCE children.  Threading these through
// a record (instead of capturing 12+ outer bindings via a nested closure)
// makes the per-child handler readable as a top-level function.
type private SequenceChildCtx = {
    r: Asn1AcnAst.AstRoot
    deps: Asn1AcnAst.AcnInsertedFieldDependencies
    lm: LanguageMacros
    codec: CommonTypes.Codec
    t: Asn1AcnAst.Asn1Type
    o: Asn1AcnAst.Sequence
    p: CodegenScope
    nestingScope: NestingScope
    bitStreamPositionsLocalVar: string
    hasOwnPostEncoding: bool
    // Incoming acnArgs of the enclosing SEQUENCE function — forwarded to
    // children's funcBodyAsSeqComp so an inner ReferenceType caller can
    // resolve its acnArguments against the parent's formal params (matches
    // the behaviour of Choice and SequenceOf).
    acnArgs: (AcnGenericTypes.RelativePath * AcnGenericTypes.AcnParameter) list
    icdAsn1Child: Asn1Child -> string list -> ((IcdRow list) * (IcdTypeAss list))
    icdAcnChild: AcnChild -> string list -> ((IcdRow list) * (IcdTypeAss list))
}

// Build the NestingScope for one child of a SEQUENCE.  Same recipe used
// by both the Asn1Child and the AcnChild branches.
// childName identifies the child within ctx.o.children so that the
// 'size deduced' trailing constant (total ACN bits of all components that
// follow this child, see Docs/deduced-size-spec.md) can be accumulated on
// top of the parent's own trailing constant.
let private mkChildNestingScope (ctx:SequenceChildCtx) (s:SequenceChildState) (childName:string) : NestingScope =
    let trailingAfterChild =
        let rec sumAfter found acc (cs:Asn1AcnAst.SeqChildInfo list) =
            match cs with
            | [] -> acc
            | c::rest ->
                let name, szBits =
                    match c with
                    | Asn1AcnAst.Asn1Child a -> a.Name.Value, a.Type.acnMaxSizeInBits
                    | Asn1AcnAst.AcnChild  a -> a.Name.Value, a.Type.acnMaxSizeInBits
                match found with
                | true  -> sumAfter true (acc + szBits) rest
                | false -> sumAfter (name = childName) acc rest
        sumAfter false 0I ctx.o.children
    {ctx.nestingScope with
        nestingLevel = ctx.nestingScope.nestingLevel + 1I
        nestingIx = ctx.nestingScope.nestingIx + s.childIx
        uperRelativeOffset = s.uperAccBits
        uperOffset = ctx.nestingScope.uperOffset + s.uperAccBits
        acnRelativeOffset = s.acnAccBits
        acnOffset = ctx.nestingScope.acnOffset + s.acnAccBits
        parents = (ctx.p, ctx.t) :: ctx.nestingScope.parents
        deducedTrailingBits = ctx.nestingScope.deducedTrailingBits + trailingAfterChild
        parentSavePositionVar =
            match ctx.hasOwnPostEncoding with
            | true  -> Some ctx.bitStreamPositionsLocalVar
            | false -> ctx.nestingScope.parentSavePositionVar}

// Per-child handler for a regular ASN.1 SEQUENCE component.
let private handleAsn1Child
    (ctx:SequenceChildCtx)
    (s:SequenceChildState)
    (child:Asn1Child)
    (childInfo:SeqChildInfo)
    : SequenceChildResult * SequenceChildState =
    let lm = ctx.lm
    let codec = ctx.codec
    let p = ctx.p
    let t = ctx.t
    let o = ctx.o
    let r = ctx.r
    let deps = ctx.deps
    let us = s.us
    let soSaveBitStrmPosStatement = None
    let childNestingScope = mkChildNestingScope ctx s child.Name.Value

    let childTypeDef = child.Type.typeDefinitionOrReference.longTypedefName2 (Some lm.lg) lm.lg.hasModules p.modName
    let childName = lm.lg.getAsn1ChildBackendName child
    let chFunc = child.Type.getAcnFunction codec
    let childSel = lm.lg.getSeqChild p.accessPath childName child.Type.isIA5String child.Optionality.IsSome
    let childP =
        let newArg = if lm.lg.usesWrappedOptional && childSel.isOptional && codec = Encode then childSel.asLast else childSel
        {p with accessPath = newArg}
    let childContentResult, ns1 =
        match chFunc with
        | Some chFunc -> chFunc.funcBodyAsSeqComp us ctx.acnArgs childNestingScope childP childName ctx.bitStreamPositionsLocalVar
        | None -> None, us

    //handle present-when acn property
    let presentWhenStmts, presentWhenLvs, presentWhenErrs, existVar, ns2 =
        match child.Optionality with
        | Some (Optional opt) ->
            match opt.acnPresentWhen with
            | None ->
                match codec with
                | Encode ->
                    // We do not need the `exist` variable for encoding as we use the child `exist` bit
                    None, [], [], None, ns1
                | Decode ->
                    let existVar = ToC (child._c_name + "_exist")
                    let lv = FlagLocalVariable (existVar, None)
                    None, [lv], [], Some existVar, ns1
            | Some (PresenceWhenBool _) ->
                match codec with
                | Encode -> None, [], [], None, ns1
                | Decode ->
                    let getExternalField (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) asn1TypeIdWithDependency =
                        let filterDependency (d:AcnDependency) =
                            match d.dependencyKind with
                            | AcnDepPresenceBool   -> true
                            | _                    -> false
                        getExternalField0 lm r deps asn1TypeIdWithDependency filterDependency false
                    let extField = getExternalField r deps child.Type.id
                    let body (p: CodegenScope) (existVar: string option): string =
                        assert existVar.IsSome
                        lm.acn.sequence_presence_optChild_pres_bool (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName existVar.Value codec
                    Some body, [], [], Some extField, ns1
            | Some (PresenceWhenBoolExpression exp)    ->
                let _errCodeName = ToC ("ERR_ACN" + ((lm.lg.codecSuffix codec).ToUpper()) + "_" + ((child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_PRESENT_WHEN_EXP_FAILED")
                let errFieldPath = match child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((r.args.TypePrefix + first) :: rest)).Replace("#","elm")
                let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None errFieldPath
                let retExp = AcnExpression.acnExpressionToBackendExpression lm o p exp
                let existVar =
                    if codec = Decode then Some (ToC (child._c_name + "_exist"))
                    else None
                let lv = existVar |> Option.toList |> List.map (fun v -> FlagLocalVariable (v, None))
                let body (p: CodegenScope) (existVar: string option): string =
                    lm.acn.sequence_presence_optChild_pres_acn_expression (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName retExp existVar errCode.errCodeName codec
                Some body, lv, [errCode], existVar, ns1a
        | _ -> None, [], [], None, ns1

    // Build ACN parameters for child call sites (e.g. nLen=a3).
    // AcnDepRefTypeArgument deps map formal ACN parameters to their call-site values.
    // AcnDepChoiceDeterminant deps (decode only) pass the enum discriminant to CHOICE
    // decode functions so the correct branch can be selected.
    // Deduplicated by param name so TAS formal params are not emitted twice.
    let acnParamsForTemplate =
        deps.acnDependencies
        |> List.filter(fun d ->
            d.asn1Type = child.Type.id &&
            match d.dependencyKind with
            | AcnDepRefTypeArgument _ -> true
            | AcnDepChoiceDeterminant _ -> codec = Decode
            | _ -> false)
        |> List.choose(fun d ->
            let targetParamName =
                match d.dependencyKind with
                | AcnDepRefTypeArgument param -> param.c_name
                | AcnDepChoiceDeterminant _ ->
                    match d.determinant with
                    | AcnChildDeterminant acnCh -> getAcnDeterminantName acnCh.id
                    | AcnParameterDeterminant paramDet -> paramDet.c_name
                | _ ->
                    match d.determinant with
                    | AcnChildDeterminant acnCh -> acnCh.c_name
                    | AcnParameterDeterminant paramDet -> paramDet.c_name
            match d.determinant with
            | AcnChildDeterminant acnCh ->
                let varName = getAcnDeterminantName acnCh.id
                let valueExpr =
                    match codec with
                    | Decode ->
                        match acnCh.Type with
                        | AcnReferenceToIA5String _ ->
                            let parentId = p.accessPath.asIdentifier lm.lg
                            $"{parentId}_{varName}"
                        | _ -> varName
                    | Encode -> varName
                Some $"{targetParamName}={valueExpr}"
            | AcnParameterDeterminant paramDet ->
                let paramInScope = t.acnParameters |> List.exists (fun pp -> pp.c_name = paramDet.c_name)
                match codec, paramInScope with
                | Decode, true -> Some $"{targetParamName}={paramDet.c_name}"
                | _ -> None)
        |> List.distinctBy (fun s -> s.Split('=').[0])

    let bInlineRequired = lm.lg.isAcnInlineRequired t childName deps

    let childBody, childLvs, childUserDefFuncs, childErrs, childResultExpr, auxiliaries, ns3 =
        match childContentResult with
        | None ->
            // Copy-decoding expects to have a result expression (even if unused), so we pick the initExpression
            let childResultExpr =
                match codec, lm.lg.decodingKind with
                | Decode, Copy -> Some (child.Type.initFunction.initExpressionFnc ())
                | _ -> None
            match child.Optionality with
            | Some AlwaysPresent     ->
                let childBody (p: CodegenScope) (_: string option): string =
                    lm.acn.sequence_always_present_child (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName None childResultExpr childTypeDef soSaveBitStrmPosStatement true acnParamsForTemplate false None bInlineRequired codec
                Some childBody, [], [], [], childResultExpr, [], ns2
            | _ -> None, [], [], [], childResultExpr, [], ns2
        | Some childContent ->
            let isPrimitiveType =
                match (lm.lg.getTypeDefinition child.Type.FT_TypeDefinition) with
                | FE_PrimitiveTypeDefinition t -> t.kind.IsPrimitiveReference2RTL
                | _ -> false
            let childBody (p: CodegenScope) (existVar: string option): string =
                let pp, _ = joinedOrAsIdentifier lm codec p
                match child.Optionality with
                | None ->
                    lm.acn.sequence_mandatory_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody soSaveBitStrmPosStatement childTypeDef isPrimitiveType acnParamsForTemplate false None bInlineRequired codec
                | Some AlwaysAbsent ->
                    lm.acn.sequence_always_absent_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody childTypeDef soSaveBitStrmPosStatement isPrimitiveType codec
                | Some AlwaysPresent ->
                    lm.acn.sequence_always_present_child pp (lm.lg.getAccess p.accessPath) childName (Some childContent.funcBody) childContent.resultExpr childTypeDef soSaveBitStrmPosStatement isPrimitiveType acnParamsForTemplate false None bInlineRequired codec
                | Some (Optional opt)   ->
                    assert (codec = Encode || existVar.IsSome)
                    match opt.defaultValue with
                    | None ->
                        lm.acn.sequence_optional_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody existVar childContent.resultExpr childTypeDef soSaveBitStrmPosStatement isPrimitiveType acnParamsForTemplate false None bInlineRequired codec
                    | Some v ->
                        let defInit= child.Type.initFunction.initByAsn1Value childP (mapValue v).kind
                        lm.acn.sequence_default_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody defInit existVar childContent.resultExpr childTypeDef soSaveBitStrmPosStatement isPrimitiveType false None codec
            let lvs =
                match child.Optionality with
                | Some AlwaysAbsent -> []
                | _ -> childContent.localVariables
            // For non-primitive children in decode mode for Python, the template generates variables as
            // <parentId>_<childName>, so override resultExpr to match what the template generates.
            let adjustedResultExpr =
                let parentId = p.accessPath.asIdentifier lm.lg
                lm.lg.adjustChildDecodeResultExpr codec isPrimitiveType parentId childName childContent.resultExpr
            Some childBody, lvs, childContent.userDefinedFunctions, childContent.errCodes, adjustedResultExpr, childContent.auxiliaries, ns2

    let optAux, theCombinedBody =
        if presentWhenStmts.IsNone && childBody.IsNone then [], None
        else
            let combinedBody (p: CodegenScope) (existVar: string option): string =
                ((presentWhenStmts |> Option.toList) @ (childBody |> Option.toList) |> List.map (fun f -> f p existVar)) |> Seq.StrJoin "\n"
            let soc = {SequenceOptionalChild.t = t; sq = o; child = child; existVar = existVar; p = {p with accessPath = childSel}; nestingScope = childNestingScope; childBody = combinedBody}
            let optAux, theCombinedBody = lm.lg.generateOptionalAuxiliaries r ACN soc codec
            optAux, Some theCombinedBody

    let stmts = {body = theCombinedBody; lvs = presentWhenLvs @ childLvs;  userDefinedFunctions=childUserDefFuncs; errCodes = presentWhenErrs @ childErrs; icdComments = []}
    let props = {info=childInfo.toAsn1AcnAst; sel=childSel; uperMaxOffset=s.uperAccBits; acnMaxOffset=s.acnAccBits}
    let icdResult = ctx.icdAsn1Child child stmts.icdComments
    let res = {stmts=[stmts]; resultExpr=childResultExpr; existVar=existVar; props=props; auxiliaries=auxiliaries @ optAux; icdResult=icdResult}
    let newAcc = {us=ns3; childIx=s.childIx + 1I; uperAccBits=s.uperAccBits + child.uperMaxSizeInBits; acnAccBits=s.acnAccBits + child.acnMaxSizeInBits}
    res, newAcc

// Per-child handler for an ACN-inserted SEQUENCE component.
let private handleAcnChild
    (ctx:SequenceChildCtx)
    (s:SequenceChildState)
    (acnChild:AcnChild)
    (childInfo:SeqChildInfo)
    : SequenceChildResult * SequenceChildState =
    let lm = ctx.lm
    let codec = ctx.codec
    let p = ctx.p
    let us = s.us
    let soSaveBitStrmPosStatement = None
    let childNestingScope = mkChildNestingScope ctx s acnChild.Name.Value

    //handle updates
    let childP = {CodegenScope.modName = p.modName; accessPath= AccessPath.valueEmptyPath (getAcnDeterminantName acnChild.id)}

    let updateStatement, ns1 =
        match codec with
        | Encode ->
            let pRoot = p
            let updateStatement, lvs, errCodes, icdComments =
                match acnChild.funcUpdateStatement with
                | Some funcUpdateStatement -> Some (funcUpdateStatement.updateAcnChildFnc acnChild childNestingScope childP pRoot), funcUpdateStatement.localVariables, funcUpdateStatement.errCodes, funcUpdateStatement.icdComments
                | None                     -> None, [], [], []
            Some {body=updateStatement; lvs=lvs; errCodes=errCodes; userDefinedFunctions = [];  icdComments=icdComments}, us
        | Decode -> None, us

    //acn child encode/decode
    let childEncDecStatement, auxiliaries, ns2 =
        let chFunc = acnChild.funcBody codec
        let childContentResult = chFunc [] childNestingScope childP ctx.bitStreamPositionsLocalVar
        match childContentResult with
        | None              -> None, [], ns1
        | Some childContent ->
            let pp, _ = joinedOrAsIdentifier lm codec p
            let isPrimitiveType =
                match acnChild.Type with
                | AcnInteger _
                | AcnNullType _
                | AcnBoolean _ -> true
                | AcnReferenceToEnumerated _ -> true
                | _ -> false
            match codec with
            | Encode   ->
                match acnChild.Type with
                | AcnNullType _   ->
                    let childBody = Some (lm.acn.sequence_mandatory_child pp (lm.lg.getAccess p.accessPath) acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement "" isPrimitiveType [] false None false codec)
                    Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions; errCodes=childContent.errCodes;icdComments=[]}, childContent.auxiliaries, ns1

                | _             ->
                    let _errCodeName         = ToC ("ERR_ACN" + ((lm.lg.codecSuffix codec).ToUpper()) + "_" + ((acnChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_UNINITIALIZED")
                    let errFieldPath = match acnChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.toList with [] -> "" | first :: rest -> (String.concat "." ((ctx.r.args.TypePrefix + first) :: rest)).Replace("#","elm")
                    let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None errFieldPath
                    let childBody = Some (lm.acn.sequence_acn_child acnChild.c_name childContent.funcBody errCode.errCodeName soSaveBitStrmPosStatement isPrimitiveType codec)
                    Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions;  errCodes=errCode::childContent.errCodes; icdComments=[]}, childContent.auxiliaries, ns1a
            | Decode    ->
                let sType = acnChild.typeDefinitionBodyWithinSeq
                let childBody = Some (lm.acn.sequence_mandatory_child pp (lm.lg.getAccess p.accessPath) acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement sType isPrimitiveType [] false None false codec)
                Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions; errCodes=childContent.errCodes; icdComments=[]}, childContent.auxiliaries, ns1

    let stmts = (updateStatement |> Option.toList)@(childEncDecStatement |> Option.toList)
    let icdComments = stmts |> List.collect(fun z -> z.icdComments)
    // Note: uperMaxSizeBits and uperAccBits here do not make sense since we are in ACN
    let props = {info=childInfo.toAsn1AcnAst; sel=childP.accessPath; uperMaxOffset=s.uperAccBits; acnMaxOffset=s.acnAccBits}
    let icdResult = ctx.icdAcnChild acnChild icdComments
    let res =  {stmts=stmts; resultExpr=None; existVar=None; props=props; auxiliaries=auxiliaries; icdResult=icdResult}
    let newAcc = {us=ns2; childIx=s.childIx + 1I; uperAccBits=s.uperAccBits; acnAccBits=s.acnAccBits + acnChild.Type.acnMaxSizeInBits}
    res, newAcc

// Top-level dispatcher: hand the per-child work to the right helper based
// on whether the SEQUENCE component is an Asn1Child or an AcnChild.
let private handleSequenceChild
    (ctx:SequenceChildCtx)
    (s:SequenceChildState)
    (childInfo:SeqChildInfo)
    : SequenceChildResult * SequenceChildState =
    match childInfo with
    | Asn1Child child  -> handleAsn1Child ctx s child childInfo
    | AcnChild acnChild -> handleAcnChild ctx s acnChild childInfo

// Emit the uPER presence-bit statement for an optional ASN.1 child whose
// presence is *not* governed by an ACN `present-when` clause.  Returns
// `None` if the child has no uPER presence bit (mandatory, AlwaysPresent,
// AlwaysAbsent, or ACN-driven optional).
let private printPresenceBit
    (lm:LanguageMacros)
    (p:CodegenScope)
    (errCode:ErrorCode)
    (codec:CommonTypes.Codec)
    (child:Asn1Child)
    (existVar:string option) : string option =
    match child.Optionality with
    | Some (Optional opt) ->
        match opt.acnPresentWhen with
        | None ->
            assert (codec = Encode || existVar.IsSome)
            Some (lm.acn.sequence_presence_optChild
                    (p.accessPath.joined lm.lg)
                    (lm.lg.getAccess p.accessPath)
                    (lm.lg.getAsn1ChildBackendName child)
                    existVar
                    errCode.errCodeName
                    codec)
        | Some _ -> None
    | _ -> None

// `fallbackEpilogue` (deferred-patching only): an extra block of C/Ada code
// that the deferred dispatcher injects at the end of the encode body to
// patch determinants whose consumer never executed.  Threaded as plain
// text instead of via a synthetic AcnChild — see DAstACNDeferred for
// the rationale.
let createSequenceFunction_inline (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (_:DastAcnParameter list) (fallbackEpilogue:string option) (us:State)  =
    (*
        1. all Acn inserted children are declared as local variables in the encoded and decode functions (declaration step)
        2. all Acn inserted children must be initialized appropriately in the encoding phase
    *)
    // stg macros used only by the post-encoding / pre-decoding setup that
    // remains inside funcBody.  Per-child macros (sequence_mandatory_child,
    // sequence_optional_child, etc.) are now invoked via lm.acn.* directly
    // from handleAsn1Child / handleAcnChild.
    let sequence_call_post_encoding_function            = lm.acn.sequence_call_post_encoding_function
    let sequence_call_post_decoding_validator           = lm.acn.sequence_call_post_decoding_validator
    let sequence_save_bitStream_start                   = lm.acn.sequence_save_bitStream_start
    let bitStreamName                                   = lm.lg.bitStreamName
    let sequence_call_post_encoding_function_prototype  = lm.acn.sequence_call_post_encoding_function_prototype
    let sequence_call_post_decoding_validator_prototype = lm.acn.sequence_call_post_decoding_validator_prototype

    let acnChildren = children |>  List.choose(fun x -> match x with AcnChild z -> Some z | Asn1Child _ -> None)
    let asn1Children = children |>  List.choose(fun x -> match x with Asn1Child z -> Some z | AcnChild _ -> None)
    // Children covered by the uPER presence bit mask: only optionals whose
    // presence is NOT governed by an ACN present-when clause (same predicate
    // as nbPresenceBits in funcBody below). Bit numbers are 1-based, in wire order.
    let uperMaskChildren =
        asn1Children |>
        List.filter(fun c ->
            match c.Optionality with
            | Some(Optional opt) ->
                match opt.acnPresentWhen with
                | None   -> true
                | Some _ -> false
            | _ -> false)
    let sPresenceBitIndexMap  =
        uperMaskChildren |>
        List.mapi (fun i c -> (c.Name.Value, i + 1)) |>
        Map.ofList
    let uperPresenceMask =
        match uperMaskChildren with
        | [] -> []
        | _  ->
            let sBitToFieldMap = uperMaskChildren |> List.mapi (fun i c -> $"bit %d{i + 1} -> %s{c.Name.Value}") |> Seq.StrJoin ", "
            [{IcdRow.fieldName = "Presence Mask"; comments = [$"Presence bit mask (a set bit means the corresponding field is present): %s{sBitToFieldMap}"]; sPresent="always";sType=IcdPlainType "bit mask"; sConstraint=None; minLengthInBits = uperMaskChildren.Length.AsBigInt ;maxLengthInBits=uperMaskChildren.Length.AsBigInt;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}]

    let icd_asn1_child (c:Asn1Child) (extra_comments:string list) : ((IcdRow list) * (IcdTypeAss list)) =
        let optionality =
            match c.Optionality with
            | None                -> "always"
            | Some(AlwaysAbsent ) -> "never"
            | Some(AlwaysPresent) -> "always"
            | Some(Optional  opt) ->
                match opt.acnPresentWhen with
                | None                                      -> $"when bit %d{sPresenceBitIndexMap[c.Name.Value]} of the Presence Mask is set"
                | Some(PresenceWhenBool relPath)            -> $"when %s{relPath.AsString} is true"
                | Some(PresenceWhenBoolExpression acnExp)   ->
                    // Render the condition in ACN syntax (as the user wrote it),
                    // not in target-language syntax.
                    let _, sAcnExp = AcnGenericCreateFromAntlr.printDebug acnExp
                    $"when %s{sAcnExp}"
        let comments = (c.Comments |> Seq.toList)@extra_comments
        //let isRef = match c.Type.Kind with ReferenceType _ -> true | _ -> false
        match c.Type.icdTas with
        | Some childIcdTas ->
            match childIcdTas.canBeEmbedded   with
            | true  ->
                let chRows, _ = childIcdTas.createRowsFunc c.Name.Value optionality comments
                chRows, []
            | false ->
                let sType = TypeHash childIcdTas.hash
                let sConstraint = constraintsToIcdStr c.Type.ConstraintsAsn1Str
                [{IcdRow.fieldName = c.Name.Value; comments = comments; sPresent=optionality;sType=sType; sConstraint=sConstraint; minLengthInBits = c.Type.acnMinSizeInBits; maxLengthInBits=c.Type.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}], [childIcdTas]
        | None ->
            [], []

    let icd_acn_child (c:AcnChild) (extra_comments:string list) : ((IcdRow list) * (IcdTypeAss list))=
        let icdResult =
            let dummyNestingScope = NestingScope.init 0I 0I []
            let p : CodegenScope = {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath ""}
            let funcResult = c.funcBody Encode [] dummyNestingScope p "Dummy_body_bitstreamPositions"
            match funcResult with
            | None -> None
            | Some bodyResult -> bodyResult.icdResult
        match icdResult with
        | None -> [], []
        | Some icdArgAux ->
            icdArgAux.rowsFunc c.Name.Value "always" extra_comments



    let funcBody (us:State) (errCode:ErrorCode) (acnArgs: (AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list) (nestingScope: NestingScope) (p:CodegenScope) =
        let acnlocalVariablesCh =
            acnChildren |>
            List.filter(fun x -> match x.Type with AcnNullType _ -> false | _ -> true) |>
            List.collect(fun x ->
                let childLv =
                    match lm.lg.decodingKind with
                    | InPlace -> [AcnInsertedChild(x.c_name, x.typeDefinitionBodyWithinSeq, x.initExpression)]
                    | Copy -> []
                match codec with
                | Encode ->
                    let initLv =
                        if lm.lg.usesWrappedOptional then []
                        else [BooleanLocalVariable(x.c_name+"_is_initialized", Some lm.lg.FalseLiteral)]
                    childLv@initLv
                | Decode -> childLv)

        let acnlocalVariablesPrms =
            match t.id.tasInfo with
            | Some  _ -> [] // if the encoding type is a top level type (i.e. TAS) then the encoding parameters are transformed into function parameters and not local variables.
            | None    -> [] // acnPrms |>  List.map(fun x -> AcnInsertedChild(x.c_name, x.typeDefinitionBodyWithinSeq))
        let acnlocalVariables = acnlocalVariablesCh @ acnlocalVariablesPrms
        //let acnParams =  r.acnParameters |> List.filter(fun  prm -> prm.ModName )

        let localVariables = acnlocalVariables
        let td = lm.lg.getSequenceTypeDefinition o.typeDef

        let hasOwnPostEncoding =
            o.acnProperties.postEncodingFunction.IsSome || o.acnProperties.preDecodingFunction.IsSome
        let bitStreamPositionsLocalVar =
            match hasOwnPostEncoding with
            | true  -> sprintf "bitStreamPositions_%s_%d" p.accessPath.lastIdOrArr (p.accessPath.SequenceOfLevel + 1)
            | false ->
                match nestingScope.parentSavePositionVar with
                | Some parentVar -> parentVar
                | None -> sprintf "bitStreamPositions_%s_%d" p.accessPath.lastIdOrArr (p.accessPath.SequenceOfLevel + 1)

        let localVariables, post_encoding_function, _, soSaveInitialBitStrmStatement =
            let bsPosStart = sprintf "bitStreamPositions_%s_start%d" p.accessPath.lastIdOrArr (p.accessPath.SequenceOfLevel + 1)
            match o.acnProperties.postEncodingFunction with
            | Some (PostEncodingFunction (modFncName, fncName)) when codec = Encode  ->
                let actualFncName =
                    match lm.lg.hasModules with
                    | false ->  ToC fncName.Value
                    | true ->
                        match modFncName with
                        | None -> ToC (r.args.mappingFunctionsModule.orElse "") + "." + ToC fncName.Value
                        | Some modFncName -> ToC modFncName.Value + "." + ToC fncName.Value

                let fncCall = sequence_call_post_encoding_function (lm.lg.getPointer p.accessPath) actualFncName bsPosStart bitStreamPositionsLocalVar
                let fncCallPrototype = sequence_call_post_encoding_function_prototype td.typeName actualFncName td.extension_function_positions

                let initialBitStrmStatement = sequence_save_bitStream_start bsPosStart codec
                [AcnInsertedChild(bitStreamPositionsLocalVar, td.extension_function_positions, ""); AcnInsertedChild(bsPosStart, bitStreamName, "")]@localVariables, Some (fncCall,fncCallPrototype), Some bitStreamPositionsLocalVar, Some initialBitStrmStatement
            | _ ->
                match o.acnProperties.preDecodingFunction with
                | Some (PreDecodingFunction (modFncName, fncName)) when codec = Decode  ->
                    let actualFncName =
                        match lm.lg.hasModules with
                        | false -> (ToC fncName.Value)
                        | true ->
                            match modFncName with
                            | None -> (ToC (r.args.mappingFunctionsModule.orElse "")) + "." + (ToC fncName.Value)
                            | Some modFncName -> (ToC modFncName.Value) + "." + (ToC fncName.Value)
                    let fncCall = sequence_call_post_decoding_validator (lm.lg.getPointer p.accessPath) actualFncName bsPosStart  bitStreamPositionsLocalVar
                    let fncCallPrototype = sequence_call_post_decoding_validator_prototype td.typeName actualFncName td.extension_function_positions
                    let initialBitStrmStatement = sequence_save_bitStream_start bsPosStart codec
                    [AcnInsertedChild(bitStreamPositionsLocalVar, td.extension_function_positions, ""); AcnInsertedChild(bsPosStart, bitStreamName, "")]@localVariables, Some (fncCall,fncCallPrototype), Some bitStreamPositionsLocalVar, Some initialBitStrmStatement
                | _ ->  localVariables, None, None, None

        // find acn inserted fields, which are not NULL types and which have no dependency.
        // For those fields we should generated no anc encode/decode function
        // Otherwise, the encoding function is wrong since an uninitialized value is encoded.
        let existsAcnChildWithNoUpdates =
            if r.args.acnDeferred then []
            else
                acnChildren |>
                List.filter (fun acnChild -> match acnChild.Type with AcnNullType _ -> false | _ -> acnChild.funcUpdateStatement.IsNone)
        let saveInitialBitStrmStatements = soSaveInitialBitStrmStatement |> Option.toList
        let nbPresenceBits = asn1Children |> List.sumBy (fun c ->
            match c.Optionality with
            | Some (Optional opt) -> if opt.acnPresentWhen.IsNone then 1I else 0I
            | _ -> 0I
        )
        let childCtx : SequenceChildCtx = {
            r = r; deps = deps; lm = lm; codec = codec
            t = t; o = o; p = p; nestingScope = nestingScope
            bitStreamPositionsLocalVar = bitStreamPositionsLocalVar
            hasOwnPostEncoding = hasOwnPostEncoding
            acnArgs = acnArgs
            icdAsn1Child = icd_asn1_child
            icdAcnChild = icd_acn_child
        }
        let (childrenStatements00: SequenceChildResult list), scs = children |> foldMap (handleSequenceChild childCtx) {us=us; childIx=nbPresenceBits; uperAccBits=nbPresenceBits; acnAccBits=nbPresenceBits}
        let ns = scs.us
        let childrenStatements0 = childrenStatements00 |> List.collect (fun xs -> xs.stmts)

        let presenceBits = ((List.zip children childrenStatements00)
            |> List.choose (fun (child, res) ->
                match child with
                | Asn1Child asn1 -> printPresenceBit lm p errCode codec asn1 res.existVar
                | AcnChild _ -> None))
        let seqProofGen =
            let children = childrenStatements00 |> List.map (fun xs -> xs.props)
            {SequenceProofGen.t = t; sq = o; sel = p.accessPath; acnOuterMaxSize = nestingScope.acnOuterMaxSize; uperOuterMaxSize = nestingScope.uperOuterMaxSize;
            nestingLevel = nestingScope.nestingLevel; nestingIx = nestingScope.nestingIx;
            uperMaxOffset = nestingScope.uperOffset; acnMaxOffset = nestingScope.acnOffset;
            acnSiblingMaxSize = nestingScope.acnSiblingMaxSize; uperSiblingMaxSize = nestingScope.uperSiblingMaxSize;
            children = children}
        let allStmts =
            let presenceBits = presenceBits |> List.map Some
            let children = childrenStatements00 |> List.map (fun s -> s.joinedBodies lm codec)
            presenceBits @ children
        let childrenStatements = lm.lg.generateSequenceChildProof r ACN allStmts seqProofGen codec

        let childrenLocalvars = childrenStatements0 |> List.collect(fun s -> s.lvs)
        let childrenUserDefFuncs = (childrenStatements0 |> List.collect(fun s -> s.userDefinedFunctions))@(post_encoding_function |> Option.map (fun (_,f) -> UserPostEncodingFunction f) |> Option.toList)
        let childrenExistVar = childrenStatements00 |> List.choose(fun res -> res.existVar)
        let childrenResultExpr = childrenStatements00 |> List.choose(fun res -> res.resultExpr)
        let childrenErrCodes = childrenStatements0 |> List.collect(fun s -> s.errCodes)
        let childrenAuxiliaries = childrenStatements00 |> List.collect (fun s -> s.auxiliaries)

        let resultExpr, seqBuild=
            match codec, lm.lg.decodingKind with
            | Decode, Copy ->
                // If we are Decoding with Copy decoding kind, then all children `resultExpr`
                // must be defined as well (i.e. we must have the same number of `resultExpr` as children)
                // assert (childrenResultExpr.Length = asn1Children.Length)
                assert (childrenResultExpr.Length = asn1Children.Length)
                let existSeq =
                    if lm.lg.usesWrappedOptional || childrenExistVar.IsEmpty || not lm.lg.needsExistSequence then []
                    else
                        let existTd = (lm.lg.getSequenceTypeDefinition o.typeDef).exist
                        [lm.init.initSequenceExpr existTd childrenExistVar []]
                let resultExpr = p.accessPath.asIdentifier lm.lg
                Some resultExpr, [lm.uper.sequence_build resultExpr (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules p.modName) p.accessPath.isOptional (existSeq@childrenResultExpr)]
            | _ -> None, []
        let proof = lm.lg.generateSequenceProof r ACN t o nestingScope p.accessPath codec
        let aux = lm.lg.generateSequenceAuxiliaries r ACN t o nestingScope p.accessPath codec
        let fallbackStmts =
            match codec, fallbackEpilogue with
            | Encode, Some code -> [code]
            | _ -> []
        let seqContent =  (saveInitialBitStrmStatements@childrenStatements@fallbackStmts@(post_encoding_function |> Option.map fst |> Option.toList)@seqBuild@proof) |> nestChildItems lm codec

        let icdFnc _ _ _  =
            let chRows0, compositeChildren0 = childrenStatements00 |> List.map (fun s -> s.icdResult) |> List.unzip
            let chRows = chRows0 |> List.collect id
            let compositeChildren = compositeChildren0 |> List.collect id
            renumberIcdRows (uperPresenceMask@chRows), compositeChildren
        let icd = {IcdArgAux.canBeEmbedded = false; baseAsn1Kind = (getASN1Name t); rowsFunc = icdFnc; commentsForTas=[]; scope="type"; name= None}

        match existsAcnChildWithNoUpdates with
        | []     ->
            match seqContent with
            | None  ->
                match codec with
                | Encode -> None, ns
                | Decode ->
                    match lm.lg.decodeEmptySeq (p.accessPath.joined lm.lg) with
                    | None -> None, ns
                    | Some decodeEmptySeq ->
                        Some ({AcnFuncBodyResult.funcBody = decodeEmptySeq; errCodes = errCode::childrenErrCodes; userDefinedFunctions=childrenUserDefFuncs; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced= false; bBsIsUnReferenced=true; resultExpr=Some decodeEmptySeq; auxiliaries=childrenAuxiliaries @ aux; icdResult = Some icd}), ns
            | Some ret ->
                Some ({AcnFuncBodyResult.funcBody = ret; errCodes = errCode::childrenErrCodes; userDefinedFunctions=childrenUserDefFuncs; localVariables = localVariables@childrenLocalvars; bValIsUnReferenced= false; bBsIsUnReferenced=(o.acnMaxSizeInBits = 0I); resultExpr=resultExpr; auxiliaries=childrenAuxiliaries @ aux; icdResult = Some icd}), ns

        | errChild::_      ->
            let determinantUsage =
                match errChild.Type with
                | AcnInteger               _-> "length"
                | AcnNullType              _-> raise(BugErrorException "existsAcnChildWithNoUpdates")
                | AcnBoolean               _-> "presence"
                | AcnReferenceToEnumerated _-> "presence"
                | AcnReferenceToIA5String  _-> "presence"
            let errMessage = sprintf "Unused ACN inserted field.
                All fields inserted at ACN level (except NULL fields) must act as decoding determinants of other types.
                The field '%s' must either be removed or used as %s determinant of another ASN.1 type." errChild.Name.Value determinantUsage
            raise(SemanticError(errChild.Name.Location, errMessage))
            //let loc = errChild.Name.Location
            //Console.Out.WriteLine (FrontEntMain.formatSemanticWarning loc errMessage)
            //None, ns

    let isTestVaseValid (atc:AutomaticTestCase) =
        //an automatic test case value is valid
        //if all ach children can be update during the encoding from the value
        acnChildren |>
        List.filter (fun acnChild -> match acnChild.Type with AcnNullType _ -> false | _ -> true) |>
        Seq.forall(fun acnChild ->
            match acnChild.funcUpdateStatement with
            | Some funcUpdateStatement -> (funcUpdateStatement.testCaseFnc atc).IsSome
            | None                     -> false)
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 (Some lm.lg) lm.lg.hasModules t.moduleName) codec)

    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody isTestVaseValid soSparkAnnotations  [] us
