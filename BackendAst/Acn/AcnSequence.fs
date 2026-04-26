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

let createSequenceFunction_inline (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (codec:CommonTypes.Codec) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (typeDefinition:TypeDefinitionOrReference) (isValidFunc: IsValidFunction option) (children:SeqChildInfo list) (acnPrms:DastAcnParameter list) (us:State)  =
    (*
        1. all Acn inserted children are declared as local variables in the encoded and decode functions (declaration step)
        2. all Acn inserted children must be initialized appropriately in the encoding phase
    *)
    // stg macros
    let sequence_presence_optChild                      = lm.acn.sequence_presence_optChild
    let sequence_presence_optChild_pres_bool            = lm.acn.sequence_presence_optChild_pres_bool
    let sequence_presence_optChild_pres_acn_expression  = lm.acn.sequence_presence_optChild_pres_acn_expression
    let sequence_mandatory_child                        = lm.acn.sequence_mandatory_child
    let sequence_optional_child                         = lm.acn.sequence_optional_child
    let sequence_always_present_child                   = lm.acn.sequence_always_present_child
    let sequence_always_absent_child                    = lm.acn.sequence_always_absent_child
    let sequence_default_child                          = lm.acn.sequence_default_child
    let sequence_acn_child                              = lm.acn.sequence_acn_child
    let sequence_call_post_encoding_function            = lm.acn.sequence_call_post_encoding_function
    let sequence_call_post_decoding_validator           = lm.acn.sequence_call_post_decoding_validator
    let sequence_save_bitStream_start                   = lm.acn.sequence_save_bitStream_start
    let bitStreamName                                   = lm.lg.bitStreamName
    let sequence_call_post_encoding_function_prototype  = lm.acn.sequence_call_post_encoding_function_prototype
    let sequence_call_post_decoding_validator_prototype = lm.acn.sequence_call_post_decoding_validator_prototype

    let acnExpressionToBackendExpression (seq:Asn1AcnAst.Sequence) (pSeq:CodegenScope) (exp:AcnExpression) =
        let unaryNotOperator    = lm.lg.unaryNotOperator
        let modOp               = lm.lg.modOp
        let eqOp                = lm.lg.eqOp
        let neqOp               = lm.lg.neqOp
        let andOp               = lm.lg.andOp
        let orOp                = lm.lg.orOp

        let printUnary op chExpPriority expStr minePriority =
            minePriority, if chExpPriority >= minePriority then sprintf "%s(%s)" op expStr else sprintf "%s%s" op expStr
        let printBinary op (chExpPriority1, expStr1) (chExpPriority2, expStr2) minePriority =
            minePriority, (if chExpPriority1 >= minePriority then "(" + expStr1 + ")" else expStr1 ) + " " + op + " " + (if chExpPriority2 >= minePriority then "(" + expStr2 + ")" else expStr2 )


        let rec getChildResult (seq:Asn1AcnAst.Sequence) (pSeq:CodegenScope) (RelativePath lp) =
            match lp with
            | []    -> raise(BugErrorException "empty relative path")
            | x1::xs ->
                match seq.children |> Seq.tryFind(fun c -> c.Name = x1) with
                | None ->
                    raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin "."))))
                | Some ch ->
                    match ch with
                    | Asn1AcnAst.AcnChild ch  -> raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'. Expecting an ASN.1 child" (lp |> Seq.StrJoin "."))))
                    | Asn1AcnAst.Asn1Child ch  ->
                        match ch.Type.ActualType.Kind with
                        | Asn1AcnAst.Integer        _
                        | Asn1AcnAst.Real           _
                        | Asn1AcnAst.Boolean        _  -> {pSeq with accessPath = lm.lg.getSeqChild pSeq.accessPath (lm.lg.getAsn1ChildBackendName0 ch) false ch.Optionality.IsSome}
                        | Asn1AcnAst.Sequence s when xs.Length > 1 -> getChildResult s {pSeq with accessPath = lm.lg.getSeqChild pSeq.accessPath (lm.lg.getAsn1ChildBackendName0 ch) false ch.Optionality.IsSome} (RelativePath xs)
                        | _                 -> raise (SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin "."))))


        let ret =
            AcnGenericTypes.foldAcnExpression
                (fun i s -> ( (0, i.Value.ToString()) , 0))
                (fun i s -> ( (0,"") , 0))
                (fun i s -> ( (0, i.Value.ToString(FsUtils.doubleParseString, NumberFormatInfo.InvariantInfo)) , 0))
                (fun i s -> ( (0, i.Value.ToString().ToLower()) , 0))
                (fun lf s ->
                    let plf = getChildResult seq pSeq lf
                    (0, (plf.accessPath.joined lm.lg)) , 0)
                (fun loc (chExpPriority, expStr) s -> printUnary unaryNotOperator chExpPriority expStr 1, 0) //NotUnaryExpression
                (fun loc (chExpPriority, expStr) s -> printUnary "-" chExpPriority expStr 1, 0)//MinusUnaryExpression
                (fun l e1 e2  s -> printBinary "+" e1 e2 3, 0 )
                (fun l e1 e2  s -> printBinary "-" e1 e2 3, 0 )
                (fun l e1 e2  s -> printBinary "*" e1 e2 2, 0 )
                (fun l e1 e2  s -> printBinary "/" e1 e2 2, 0 )
                (fun l e1 e2  s -> printBinary modOp e1 e2 2, 0 )
                (fun l e1 e2  s -> printBinary "<=" e1 e2 4, 0 )
                (fun l e1 e2  s -> printBinary "<" e1 e2 4, 0 )
                (fun l e1 e2  s -> printBinary ">=" e1 e2 4, 0 )
                (fun l e1 e2  s -> printBinary ">" e1 e2 4, 0 )
                (fun l e1 e2  s -> printBinary eqOp e1 e2 5, 0 )
                (fun l e1 e2  s -> printBinary neqOp e1 e2 5, 0 )
                (fun lc e1 e2  s -> printBinary andOp  e1 e2 6, 0 )
                (fun lc e1 e2  s -> printBinary orOp e1 e2 6, 0 )
                exp 0 |> fst |> snd

        ret

    let acnChildren = children |>  List.choose(fun x -> match x with AcnChild z -> Some z | Asn1Child _ -> None)
    let asn1Children = children |>  List.choose(fun x -> match x with Asn1Child z -> Some z | AcnChild _ -> None)
    let sPresenceBitIndexMap  =
        asn1Children |>
        List.filter(fun c -> match c.Optionality with Some(Optional _) -> true | _ -> false) |>
        List.mapi (fun i c -> (c.Name.Value, i)) |>
        Map.ofList
    let uperPresenceMask =
        match sPresenceBitIndexMap.IsEmpty with
        | true -> []
        | false ->
            [{IcdRow.fieldName = "Presence Mask"; comments = [$"Presence bit mask"]; sPresent="always";sType=IcdPlainType "bit mask"; sConstraint=None; minLengthInBits = sPresenceBitIndexMap.Count.AsBigInt ;maxLengthInBits=sPresenceBitIndexMap.Count.AsBigInt;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}]

    let icd_asn1_child (c:Asn1Child) (extra_comments:string list) : ((IcdRow list) * (IcdTypeAss list)) =
        let optionality =
            match c.Optionality with
            | None                -> "always"
            | Some(AlwaysAbsent ) -> "never"
            | Some(AlwaysPresent) -> "always"
            | Some(Optional  opt) ->
                match opt.acnPresentWhen with
                | None                                      -> $"when bit %d{sPresenceBitIndexMap[c.Name.Value]} is set in the uPER bit mask"
                | Some(PresenceWhenBool relPath)            -> $"when %s{relPath.AsString} is true"
                | Some(PresenceWhenBoolExpression acnExp)   ->
                    let dummyScope = {CodegenScope.modName = ""; accessPath = AccessPath.valueEmptyPath "dummy"}
                    let retExp = acnExpressionToBackendExpression o dummyScope acnExp
                    $"when %s{retExp}"
        let comments = (c.Comments |> Seq.toList)@extra_comments
        let childIcdTas = c.Type.icdTas
        //let isRef = match c.Type.Kind with ReferenceType _ -> true | _ -> false
        match c.Type.icdTas with
        | Some childIcdTas ->
            match childIcdTas.canBeEmbedded   with
            | true  ->
                let chRows, _ = childIcdTas.createRowsFunc c.Name.Value optionality comments
                chRows, []
            | false ->
                let sType = TypeHash childIcdTas.hash
                [{IcdRow.fieldName = c.Name.Value; comments = comments; sPresent=optionality;sType=sType; sConstraint=None; minLengthInBits = c.Type.acnMinSizeInBits; maxLengthInBits=c.Type.acnMaxSizeInBits;sUnits=None; rowType = IcdRowType.LengthDeterminantRow; idxOffset = None}], [childIcdTas]
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
            List.filter(fun x -> match x.Type with Asn1AcnAst.AcnNullType _ -> false | _ -> true) |>
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

        let printPresenceBit (child:Asn1Child) (existVar: string option)=
            match child.Optionality with
            | Some (Asn1AcnAst.Optional opt)   ->
                match opt.acnPresentWhen with
                | None ->
                    assert (codec = Encode || existVar.IsSome)
                    Some (sequence_presence_optChild (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) (lm.lg.getAsn1ChildBackendName child) existVar errCode.errCodeName codec)
                | Some _ -> None
            | _ -> None

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

        let localVariables, post_encoding_function, soBitStreamPositionsLocalVar, soSaveInitialBitStrmStatement =
            let bsPosStart = sprintf "bitStreamPositions_%s_start%d" p.accessPath.lastIdOrArr (p.accessPath.SequenceOfLevel + 1)
            match o.acnProperties.postEncodingFunction with
            | Some (PostEncodingFunction (modFncName, fncName)) when codec = Encode  ->
                let actualFncName =
                    match lm.lg.hasModules with
                    | false ->  (ToC fncName.Value)
                    | true ->
                        match modFncName with
                        | None -> (ToC (r.args.mappingFunctionsModule.orElse "")) + "." + (ToC fncName.Value)
                        | Some modFncName -> (ToC modFncName.Value) + "." + (ToC fncName.Value)

                let fncCall = sequence_call_post_encoding_function (lm.lg.getPointer p.accessPath) (actualFncName) bsPosStart bitStreamPositionsLocalVar
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
                    let fncCall = sequence_call_post_decoding_validator (lm.lg.getPointer p.accessPath) (actualFncName) bsPosStart  bitStreamPositionsLocalVar
                    let fncCallPrototype = sequence_call_post_decoding_validator_prototype td.typeName actualFncName td.extension_function_positions
                    let initialBitStrmStatement = sequence_save_bitStream_start bsPosStart codec
                    [AcnInsertedChild(bitStreamPositionsLocalVar, td.extension_function_positions, ""); AcnInsertedChild(bsPosStart, bitStreamName, "")]@localVariables, Some (fncCall,fncCallPrototype), Some bitStreamPositionsLocalVar, Some initialBitStrmStatement
                | _ ->  localVariables, None, None, None

        let handleChild (s: SequenceChildState) (childInfo: SeqChildInfo): SequenceChildResult * SequenceChildState =
            let us = s.us
            let soSaveBitStrmPosStatement = None
            let childNestingScope =
                {nestingScope with
                    nestingLevel = nestingScope.nestingLevel + 1I
                    nestingIx = nestingScope.nestingIx + s.childIx
                    uperRelativeOffset = s.uperAccBits
                    uperOffset = nestingScope.uperOffset + s.uperAccBits
                    acnRelativeOffset = s.acnAccBits
                    acnOffset = nestingScope.acnOffset + s.acnAccBits
                    parents = (p, t) :: nestingScope.parents
                    parentSavePositionVar =
                        match hasOwnPostEncoding with
                        | true  -> Some bitStreamPositionsLocalVar
                        | false -> nestingScope.parentSavePositionVar}

            match childInfo with
            | Asn1Child child   ->
                let childTypeDef = child.Type.typeDefinitionOrReference.longTypedefName2 lm.lg.hasModules
                let childName = lm.lg.getAsn1ChildBackendName child
                let chFunc = child.Type.getAcnFunction codec
                let childSel = lm.lg.getSeqChild p.accessPath childName child.Type.isIA5String child.Optionality.IsSome
                let childP =
                    let newArg = if lm.lg.usesWrappedOptional && childSel.isOptional && codec = Encode then childSel.asLast else childSel
                    {p with accessPath = newArg}
                let childContentResult, ns1 =
                    match chFunc with
                    | Some chFunc -> chFunc.funcBodyAsSeqComp us [] childNestingScope childP childName bitStreamPositionsLocalVar
                    | None -> None, us

                //handle present-when acn property
                let presentWhenStmts, presentWhenLvs, presentWhenErrs, existVar, ns2 =
                    match child.Optionality with
                    | Some (Asn1AcnAst.Optional opt) ->
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
                                    getExternalField0 r deps asn1TypeIdWithDependency filterDependency
                                let extField = getExternalField r deps child.Type.id
                                let body (p: CodegenScope) (existVar: string option): string =
                                    assert existVar.IsSome
                                    sequence_presence_optChild_pres_bool (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName existVar.Value codec
                                Some body, [], [], Some extField, ns1
                        | Some (PresenceWhenBoolExpression exp)    ->
                            let _errCodeName = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((child.Type.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_PRESENT_WHEN_EXP_FAILED")
                            let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None
                            let retExp = acnExpressionToBackendExpression o p exp
                            let existVar =
                                if codec = Decode then Some (ToC (child._c_name + "_exist"))
                                else None
                            let lv = existVar |> Option.toList |> List.map (fun v -> FlagLocalVariable (v, None))
                            let body (p: CodegenScope) (existVar: string option): string =
                                sequence_presence_optChild_pres_acn_expression (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName retExp existVar errCode.errCodeName codec
                            Some body, lv, [errCode], existVar, ns1a
                    | _ -> None, [], [], None, ns1

                let childBody, childLvs, childUserDefFuncs, childErrs, childResultExpr, auxiliaries, ns3 =
                    match childContentResult with
                    | None ->
                        // Copy-decoding expects to have a result expression (even if unused), so we pick the initExpression
                        let childResultExpr =
                            match codec, lm.lg.decodingKind with
                            | Decode, Copy -> Some (child.Type.initFunction.initExpressionFnc ())
                            | _ -> None
                        match child.Optionality with
                        | Some Asn1AcnAst.AlwaysPresent     ->
                            let childBody (p: CodegenScope) (existVar: string option): string =
                                sequence_always_present_child (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName None childResultExpr childTypeDef soSaveBitStrmPosStatement codec
                            Some childBody, [], [], [], childResultExpr, [], ns2
                        | _ -> None, [], [], [], childResultExpr, [], ns2
                    | Some childContent ->
                        let childBody (p: CodegenScope) (existVar: string option): string =
                            match child.Optionality with
                            | None ->
                                sequence_mandatory_child childName childContent.funcBody soSaveBitStrmPosStatement codec
                            | Some Asn1AcnAst.AlwaysAbsent ->
                                sequence_always_absent_child (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName childContent.funcBody childTypeDef soSaveBitStrmPosStatement codec
                            | Some Asn1AcnAst.AlwaysPresent ->
                                sequence_always_present_child (p.accessPath.joined lm.lg) (lm.lg.getAccess p.accessPath) childName (Some childContent.funcBody) childContent.resultExpr childTypeDef soSaveBitStrmPosStatement codec
                            | Some (Asn1AcnAst.Optional opt)   ->
                                assert (codec = Encode || existVar.IsSome)
                                let pp, _ = joinedOrAsIdentifier lm codec p
                                match opt.defaultValue with
                                | None ->
                                    sequence_optional_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody existVar childContent.resultExpr childTypeDef soSaveBitStrmPosStatement codec
                                | Some v ->
                                    let defInit= child.Type.initFunction.initByAsn1Value childP (mapValue v).kind
                                    sequence_default_child pp (lm.lg.getAccess p.accessPath) childName childContent.funcBody defInit existVar childContent.resultExpr childTypeDef soSaveBitStrmPosStatement codec
                        let lvs =
                            match child.Optionality with
                            | Some Asn1AcnAst.AlwaysAbsent -> []
                            | _ -> childContent.localVariables
                        Some childBody, lvs, childContent.userDefinedFunctions, childContent.errCodes, childContent.resultExpr, childContent.auxiliaries, ns2

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
                let icdResult = icd_asn1_child child stmts.icdComments
                let res = {stmts=[stmts]; resultExpr=childResultExpr; existVar=existVar; props=props; auxiliaries=auxiliaries @ optAux; icdResult=icdResult}
                let newAcc = {us=ns3; childIx=s.childIx + 1I; uperAccBits=s.uperAccBits + child.uperMaxSizeInBits; acnAccBits=s.acnAccBits + child.acnMaxSizeInBits}
                res, newAcc
            | AcnChild acnChild ->
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
                    let childContentResult = chFunc [] childNestingScope childP bitStreamPositionsLocalVar
                    match childContentResult with
                    | None              -> None, [], ns1
                    | Some childContent ->
                        match codec with
                        | Encode   ->
                            match acnChild.Type with
                            | Asn1AcnAst.AcnNullType _   ->
                                let childBody = Some (sequence_mandatory_child acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement codec)
                                Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions; errCodes=childContent.errCodes;icdComments=[]}, childContent.auxiliaries, ns1

                            | _             ->
                                let _errCodeName         = ToC ("ERR_ACN" + (codec.suffix.ToUpper()) + "_" + ((acnChild.id.AcnAbsPath |> Seq.skip 1 |> Seq.StrJoin("-")).Replace("#","elm")) + "_UNINITIALIZED")
                                let errCode, ns1a = getNextValidErrorCode ns1 _errCodeName None
                                let childBody = Some (sequence_acn_child acnChild.c_name childContent.funcBody errCode.errCodeName soSaveBitStrmPosStatement codec)
                                Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions;  errCodes=errCode::childContent.errCodes; icdComments=[]}, childContent.auxiliaries, ns1a
                        | Decode    ->
                            let childBody = Some (sequence_mandatory_child acnChild.c_name childContent.funcBody soSaveBitStrmPosStatement codec)
                            Some {body=childBody; lvs=childContent.localVariables; userDefinedFunctions=childContent.userDefinedFunctions; errCodes=childContent.errCodes; icdComments=[]}, childContent.auxiliaries, ns1

                let stmts = (updateStatement |> Option.toList)@(childEncDecStatement |> Option.toList)
                let icdComments = stmts |> List.collect(fun z -> z.icdComments)
                // Note: uperMaxSizeBits and uperAccBits here do not make sense since we are in ACN
                let props = {info=childInfo.toAsn1AcnAst; sel=childP.accessPath; uperMaxOffset=s.uperAccBits; acnMaxOffset=s.acnAccBits}
                let icdResult = icd_acn_child acnChild icdComments
                let res =  {stmts=stmts; resultExpr=None; existVar=None; props=props; auxiliaries=auxiliaries; icdResult=icdResult}
                let newAcc = {us=ns2; childIx=s.childIx + 1I; uperAccBits=s.uperAccBits; acnAccBits=s.acnAccBits + acnChild.Type.acnMaxSizeInBits}
                res, newAcc




        // find acn inserted fields, which are not NULL types and which have no dependency.
        // For those fields we should generated no anc encode/decode function
        // Otherwise, the encoding function is wrong since an uninitialized value is encoded.
        let existsAcnChildWithNoUpdates =
            if r.args.acnDeferred then []
            else
                acnChildren |>
                List.filter (fun acnChild -> match acnChild.Type with Asn1AcnAst.AcnNullType _ -> false | _ -> acnChild.funcUpdateStatement.IsNone)
        let saveInitialBitStrmStatements = soSaveInitialBitStrmStatement |> Option.toList
        let nbPresenceBits = asn1Children |> List.sumBy (fun c ->
            match c.Optionality with
            | Some (Optional opt) -> if opt.acnPresentWhen.IsNone then 1I else 0I
            | _ -> 0I
        )
        let (childrenStatements00: SequenceChildResult list), scs = children |> foldMap handleChild {us=us; childIx=nbPresenceBits; uperAccBits=nbPresenceBits; acnAccBits=nbPresenceBits}
        let ns = scs.us
        let childrenStatements0 = childrenStatements00 |> List.collect (fun xs -> xs.stmts)

        let presenceBits = ((List.zip children childrenStatements00)
            |> List.choose (fun (child, res) ->
                match child with
                | Asn1Child asn1 -> printPresenceBit asn1 res.existVar
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
                    if lm.lg.usesWrappedOptional || childrenExistVar.IsEmpty then []
                    else
                        let existTd = (lm.lg.getSequenceTypeDefinition o.typeDef).exist
                        [lm.init.initSequenceExpr existTd childrenExistVar []]
                let resultExpr = p.accessPath.asIdentifier
                Some resultExpr, [lm.uper.sequence_build resultExpr (typeDefinition.longTypedefName2 lm.lg.hasModules) p.accessPath.isOptional (existSeq@childrenResultExpr)]
            | _ -> None, []
        let proof = lm.lg.generateSequenceProof r ACN t o nestingScope p.accessPath codec
        let aux = lm.lg.generateSequenceAuxiliaries r ACN t o nestingScope p.accessPath codec
        let seqContent =  (saveInitialBitStrmStatements@childrenStatements@(post_encoding_function |> Option.map fst |> Option.toList)@seqBuild@proof) |> nestChildItems lm codec

        let icdFnc fieldName sPresent comments  =
            let chRows0, compositeChildren0 = childrenStatements00 |> List.map (fun s -> s.icdResult) |> List.unzip
            let chRows = chRows0 |> List.collect id
            let compositeChildren = compositeChildren0 |> List.collect id
            uperPresenceMask@chRows |> List.mapi(fun i r -> {r with idxOffset = Some (i+1)}), compositeChildren
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
                | Asn1AcnAst.AcnInteger               _-> "length"
                | Asn1AcnAst.AcnNullType              _-> raise(BugErrorException "existsAcnChildWithNoUpdates")
                | Asn1AcnAst.AcnBoolean               _-> "presence"
                | Asn1AcnAst.AcnReferenceToEnumerated _-> "presence"
                | Asn1AcnAst.AcnReferenceToIA5String  _-> "presence"
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
        List.filter (fun acnChild -> match acnChild.Type with Asn1AcnAst.AcnNullType _ -> false | _ -> true) |>
        Seq.forall(fun acnChild ->
            match acnChild.funcUpdateStatement with
            | Some funcUpdateStatement -> (funcUpdateStatement.testCaseFnc atc).IsSome
            | None                     -> false)
    let soSparkAnnotations = Some(sparkAnnotations lm (typeDefinition.longTypedefName2 lm.lg.hasModules) codec)

    AcnFunctionWrapper.createAcnFunction r deps lm codec t typeDefinition  isValidFunc  funcBody isTestVaseValid soSparkAnnotations  [] us
