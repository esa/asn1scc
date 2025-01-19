module DAstConstruction
open System
open System.Numerics
open System.IO
open DAstTypeDefinition
open FsUtils
open CommonTypes
open DAst
open DAstUtilFunctions
open System.Collections.Generic
open Language

let foldMap = Asn1Fold.foldMap

let XER (r:Asn1AcnAst.AstRoot) func (us:State) =
    match r.args.hasXer with
    | true -> func ()
    | false -> XerFunctionDummy, us

let createAsn1ValueFromValueKind (t:Asn1AcnAst.Asn1Type) i (v:Asn1ValueKind)  =
    {Asn1Value.kind = v;  loc  = t.Location; id  = (ReferenceToValue (t.id.ToScopeNodeList,[IMG i]));moduleName    = t.moduleName}

let private mapAcnParameter (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (t:Asn1AcnAst.Asn1Type) (prm:AcnGenericTypes.AcnParameter) (us:State) =
    //let funcUpdateStatement, ns1 = DAstACN.getUpdateFunctionUsedInEncoding r deps l m prm.id us
    let ns1 = us
    {
        DastAcnParameter.asn1Type = prm.asn1Type;
        name = prm.name;
        loc = prm.loc
        id = prm.id
        c_name = DAstACN.getAcnDeterminantName prm.id
        typeDefinitionBodyWithinSeq = DAstACN.getDeterminantTypeDefinitionBodyWithinSeq r lm (Asn1AcnAst.AcnParameterDeterminant prm)

        //funcUpdateStatement00 = funcUpdateStatement
    }, ns1

let private createAcnChild (r:Asn1AcnAst.AstRoot) (icdStgFileName:string) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (ch:Asn1AcnAst.AcnChild) (us:State) =

    let acnAlignment =
        match ch.Type with
        | Asn1AcnAst.AcnInteger  a -> a.acnAlignment
        | Asn1AcnAst.AcnBoolean  a -> a.acnAlignment
        | Asn1AcnAst.AcnNullType a -> a.acnAlignment
        | Asn1AcnAst.AcnReferenceToEnumerated a -> a.acnAlignment
        | Asn1AcnAst.AcnReferenceToIA5String a -> a.acnAlignment

    let funcBodyEncode, ns1=
        match ch.Type with
        | Asn1AcnAst.AcnInteger  a -> DAstACN.createAcnIntegerFunction r deps lm Codec.Encode ch.id a us
        | Asn1AcnAst.AcnBoolean  a -> DAstACN.createAcnBooleanFunction r deps lm Codec.Encode ch.id a us
        | Asn1AcnAst.AcnNullType a -> DAstACN.createAcnNullTypeFunction r deps lm Codec.Encode ch.id a us
        | Asn1AcnAst.AcnReferenceToEnumerated a -> DAstACN.createAcnEnumeratedFunction r deps icdStgFileName lm Codec.Encode ch.id a (defOrRef r m a) us
        | Asn1AcnAst.AcnReferenceToIA5String a -> DAstACN.createAcnStringFunction r deps lm Codec.Encode ch.id a us

    let funcBodyDecode, ns2 =
        match ch.Type with
        | Asn1AcnAst.AcnInteger  a -> DAstACN.createAcnIntegerFunction r deps lm Codec.Decode ch.id a ns1
        | Asn1AcnAst.AcnBoolean  a -> DAstACN.createAcnBooleanFunction r deps lm Codec.Decode ch.id a ns1
        | Asn1AcnAst.AcnNullType a -> DAstACN.createAcnNullTypeFunction r deps lm Codec.Decode ch.id a ns1
        | Asn1AcnAst.AcnReferenceToEnumerated a -> DAstACN.createAcnEnumeratedFunction r deps icdStgFileName lm Codec.Decode ch.id a (defOrRef r m a) ns1
        | Asn1AcnAst.AcnReferenceToIA5String a -> DAstACN.createAcnStringFunction r deps lm Codec.Decode ch.id a ns1

    let funcUpdateStatement, ns3 = DAstACN.getUpdateFunctionUsedInEncoding r deps lm m ch.id ns2
    let c_name         = ch.c_name //DAstACN.getAcnDeterminantName ch.id

    let newFuncBody (codec:Codec) (prms:((AcnGenericTypes.RelativePath*AcnGenericTypes.AcnParameter) list)) (nestingScope: NestingScope) (p:CallerScope): AcnFuncBodyResult option=
        let funBodyWithState st errCode prms nestingScope p =
            let funcBody codec = match codec with Codec.Encode -> funcBodyEncode | Codec.Decode -> funcBodyDecode
            funcBody codec prms nestingScope p, st
        let retFunc = DAstACN.handleSavePosition funBodyWithState ch.Type.savePosition c_name ch.id lm codec
        retFunc emptyState {ErrorCode.errCodeName = ""; ErrorCode.errCodeValue=0; comment=None} prms nestingScope p |> fst

    let tdBodyWithinSeq = DAstACN.getDeterminantTypeDefinitionBodyWithinSeq r lm (Asn1AcnAst.AcnChildDeterminant ch)
    let initExpression =
        match ch.Type with
        | Asn1AcnAst.AcnInteger _ -> "0"
        | Asn1AcnAst.AcnBoolean _ -> lm.lg.FalseLiteral
        | Asn1AcnAst.AcnNullType _ -> "0"
        | Asn1AcnAst.AcnReferenceToEnumerated e ->
            lm.lg.getNamedItemBackendName (Some (defOrRef r m e)) e.enumerated.items.Head
        | Asn1AcnAst.AcnReferenceToIA5String s ->
            lm.lg.initializeString (int (s.str.maxSize.acn + 1I))

    let rec dealiasDeps (dep: Asn1AcnAst.AcnDependency): Asn1AcnAst.AcnDependency =
        match dep.dependencyKind with
        | Asn1AcnAst.AcnDepRefTypeArgument param ->
            let dealiased = dealiasDeps (deps.acnDependencies |> List.find (fun dep -> dep.determinant.id = param.id))
            {dep with dependencyKind = dealiased.dependencyKind}
        | _ -> dep

    let dealiasedDeps = deps.acnDependencies |> List.filter(fun d -> d.determinant.id = ch.id) |> List.map dealiasDeps
    let ret =
        {
            AcnChild.Name               = ch.Name
            id                          = ch.id
            c_name                      = c_name
            Type                        = ch.Type
            typeDefinitionBodyWithinSeq = tdBodyWithinSeq
            funcBody                    = DAstACN.handleAlignmentForAcnTypes r lm acnAlignment newFuncBody
            funcUpdateStatement         = funcUpdateStatement
            Comments                    = ch.Comments
            deps                        = { acnDependencies = dealiasedDeps }
            initExpression              = initExpression
        }
    AcnChild ret, ns3

type ParentInfoData = unit

let private createInteger (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (us:State) =
    //let defOrRef =  TL "DAstTypeDefinition" (fun () -> DAstTypeDefinition.createInteger_u r lm t o )
    let defOrRef =  lm.lg.definitionOrRef o.definitionOrRef
    let initFunction        = TL "DAstInitialize" (fun () -> DAstInitialize.createIntegerInitFunc r lm t o defOrRef)
    let equalFunction       = TL "DAstEqual" (fun () -> DAstEqual.createIntegerEqualFunction r lm t o defOrRef)
    let isValidFunction, s1     = TL "DastValidate2" (fun () -> DastValidate2.createIntegerFunction r lm t o defOrRef  us)
    let uperEncFunction, s2     = TL "DAstUPer" (fun () -> DAstUPer.createIntegerFunction r lm Codec.Encode t o defOrRef None isValidFunction s1)
    let uperDecFunction, s3     = TL "DAstUPer" (fun () -> DAstUPer.createIntegerFunction r lm Codec.Decode t o defOrRef None isValidFunction s2)
    let acnEncFunction, s4      = TL "DAstACN" (fun () -> DAstACN.createIntegerFunction r deps lm Codec.Encode t o defOrRef isValidFunction uperEncFunction s3)
    let acnDecFunction, s5      = TL "DAstACN" (fun () -> DAstACN.createIntegerFunction r deps lm Codec.Decode t o defOrRef isValidFunction uperDecFunction s4)

    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6


    let xerEncFunction, s8      = TL "DAstXer" (fun () -> XER r (fun () ->  DAstXer.createIntegerFunction  r lm Codec.Encode t o defOrRef isValidFunction s7) s7)
    let xerDecFunction, s9      = TL "DAstXer" (fun () -> XER r (fun () ->  DAstXer.createIntegerFunction  r lm Codec.Decode t o defOrRef isValidFunction s8) s8)
    let xerEncDecTestFunc,s10   = TL "DAstXer" (fun () -> EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9)

    let ret =
        {
            Integer.baseInfo    = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createIntegerFunction r lm t o
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            xerEncDecTestFunc   = xerEncDecTestFunc
            //automaticTestCasesValues     = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createIntegerFunction r t o
        }
    ((Integer ret),[]), s10

let private createReal (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Real) (us:State) =
    //let typeDefinition = DAstTypeDefinition.createReal  r l t o us
    let defOrRef            = lm.lg.definitionOrRef o.definitionOrRef  //DAstTypeDefinition.createReal_u r lm t o
    let equalFunction       = DAstEqual.createRealEqualFunction r lm t o defOrRef
    //let initialValue        = getValueByUperRange o.uperRange 0.0
    let initFunction            = DAstInitialize.createRealInitFunc r lm t o defOrRef
    let isValidFunction, s1     = DastValidate2.createRealFunction r lm t o defOrRef  us
    let uperEncFunction, s2     = DAstUPer.createRealFunction r lm Codec.Encode t o defOrRef None isValidFunction s1
    let uperDecFunction, s3     = DAstUPer.createRealFunction r lm Codec.Decode t o defOrRef None isValidFunction s2
    let acnEncFunction, s4      = DAstACN.createRealFunction r deps lm Codec.Encode t o defOrRef isValidFunction uperEncFunction s3
    let acnDecFunction, s5      = DAstACN.createRealFunction r deps lm Codec.Decode t o defOrRef isValidFunction uperDecFunction s4

    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6
    let automaticTestCasesValues      = EncodeDecodeTestCase.RealAutomaticTestCaseValues r t o |> List.mapi (fun i x -> createAsn1ValueFromValueKind t i (RealValue x))
    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createRealFunction  r lm Codec.Encode t o defOrRef isValidFunction s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createRealFunction  r lm Codec.Decode t o defOrRef isValidFunction s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9

    let ret =
        {
            Real.baseInfo = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createRealFunction r lm t o
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            ////automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createRealFunction r t o
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((Real ret),[]), s10



let private createStringType (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.StringType) (us:State) =
    let newPrms, us0 = t.acnParameters |> foldMap(fun ns p -> mapAcnParameter r deps lm m t p ns) us
    //let typeDefinition = DAstTypeDefinition.createString  r l t o us0

    let defOrRef            =  lm.lg.definitionOrRef o.definitionOrRef
    //let defOrRef            =  DAstTypeDefinition.createString_u r l t o us0

    let equalFunction       = DAstEqual.createStringEqualFunction r lm t o defOrRef
    let initFunction        = DAstInitialize.createIA5StringInitFunc r lm t o defOrRef
    let isValidFunction, s1     = DastValidate2.createStringFunction r lm t o defOrRef  us
    let uperEncFunction, s2     = DAstUPer.createIA5StringFunction r lm Codec.Encode t o  defOrRef None isValidFunction s1
    let uperDecFunction, s3     = DAstUPer.createIA5StringFunction r lm Codec.Decode t o  defOrRef None isValidFunction s2
    let acnEncFunction, s4      = DAstACN.createStringFunction r deps lm Codec.Encode t o defOrRef defOrRef isValidFunction uperEncFunction s3
    let acnDecFunction, s5      = DAstACN.createStringFunction r deps lm Codec.Decode t o defOrRef defOrRef isValidFunction uperDecFunction s4
    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6
    //let automaticTestCasesValues      = EncodeDecodeTestCase.StringAutomaticTestCaseValues r t o |> List.mapi (fun i x -> createAsn1ValueFromValueKind t i (StringValue x))
    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createIA5StringFunction  r lm Codec.Encode t o defOrRef isValidFunction s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createIA5StringFunction  r lm Codec.Decode t o defOrRef isValidFunction s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9

    let ret =
        {
            StringType.baseInfo = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createStringFunction r lm t o
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createStringFunction r t o
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    (ret,newPrms), s10


let private createOctetString (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.OctetString) (us:State) =
    let newPrms, us0 = t.acnParameters |> foldMap(fun ns p -> mapAcnParameter r deps lm m t p ns) us
    //let typeDefinition = DAstTypeDefinition.createOctet  r l t o us0
    let defOrRef            = lm.lg.definitionOrRef o.definitionOrRef
    let equalFunction       = DAstEqual.createOctetStringEqualFunction r lm t o defOrRef
    let printValue          = DAstVariables.createOctetStringFunction r lm t o defOrRef

    let isValidFunction, s1     = DastValidate2.createOctetStringFunction r lm t o defOrRef  equalFunction printValue us
    let initFunction            = DAstInitialize.createOctetStringInitFunc r lm t o defOrRef isValidFunction
    let uperEncFunction, s2     = DAstUPer.createOctetStringFunction r lm Codec.Encode t o  defOrRef None isValidFunction s1
    let uperDecFunction, s3     = DAstUPer.createOctetStringFunction r lm Codec.Decode t o  defOrRef None isValidFunction s2
    let acnEncFunction, s4      = DAstACN.createOctetStringFunction r deps lm Codec.Encode t o defOrRef isValidFunction uperEncFunction s3
    let acnDecFunction, s5      = DAstACN.createOctetStringFunction r deps lm Codec.Decode t o defOrRef isValidFunction uperDecFunction s4
    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6
    let automaticTestCasesValues      = EncodeDecodeTestCase.OctetStringAutomaticTestCaseValues r t o |> List.mapi (fun i x -> createAsn1ValueFromValueKind t i (OctetStringValue x))
    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createOctetStringFunction  r lm Codec.Encode t o defOrRef isValidFunction s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createOctetStringFunction  r lm Codec.Decode t o defOrRef isValidFunction s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9

    let ret =
        {
            OctetString.baseInfo = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = printValue
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createOctetStringFunction r t o
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((OctetString ret),newPrms), s10



let private createNullType (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.NullType) (us:State) =
    //let typeDefinition = DAstTypeDefinition.createNull  r l t o us
    let defOrRef            =  lm.lg.definitionOrRef o.definitionOrRef
    let equalFunction       = DAstEqual.createNullTypeEqualFunction r lm  t o defOrRef
    let initFunction        = DAstInitialize.createNullTypeInitFunc r lm t o defOrRef
    let uperEncFunction, s2     = DAstUPer.createNullTypeFunction r lm Codec.Encode t o defOrRef None None us
    let uperDecFunction, s3     = DAstUPer.createNullTypeFunction r lm Codec.Decode t o defOrRef None None s2
    let acnEncFunction, s4      = DAstACN.createNullTypeFunction r deps lm Codec.Encode t o defOrRef None  s3
    let acnDecFunction, s5      = DAstACN.createNullTypeFunction r deps lm Codec.Decode t o defOrRef None  s4
    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction None (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction None (Some acnEncFunction) (Some acnDecFunction) s6
    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createNullTypeFunction  r lm Codec.Encode t o defOrRef None s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createNullTypeFunction  r lm Codec.Decode t o defOrRef None s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction None xerEncFunction xerDecFunction s9
    let ret =
        {
            NullType.baseInfo   = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createNullTypeFunction r lm t o
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            constraintsAsn1Str = []
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((NullType ret),[]), s10



let private createBitString (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.BitString) (us:State) =
    let newPrms, us0 = t.acnParameters |> foldMap(fun ns p -> mapAcnParameter r deps lm m t p ns) us
    //let typeDefinition = DAstTypeDefinition.createBitString  r l t o us0
    let defOrRef            =  lm.lg.definitionOrRef o.definitionOrRef

    let equalFunction       = DAstEqual.createBitStringEqualFunction r lm t o defOrRef
    let printValue          = DAstVariables.createBitStringFunction r lm t o defOrRef
    let isValidFunction, s1     = DastValidate2.createBitStringFunction r lm t o defOrRef defOrRef equalFunction printValue us
    let initFunction        = DAstInitialize.createBitStringInitFunc r lm t o defOrRef isValidFunction

    let uperEncFunction, s2     = DAstUPer.createBitStringFunction r lm Codec.Encode t o  defOrRef None isValidFunction s1
    let uperDecFunction, s3     = DAstUPer.createBitStringFunction r lm Codec.Decode t o  defOrRef None isValidFunction s2
    let acnEncFunction, s4      = DAstACN.createBitStringFunction r deps lm Codec.Encode t o defOrRef isValidFunction uperEncFunction s3
    let acnDecFunction, s5      = DAstACN.createBitStringFunction r deps lm Codec.Decode t o defOrRef isValidFunction uperDecFunction s4
    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6
    let automaticTestCasesValues      = EncodeDecodeTestCase.BitStringAutomaticTestCaseValues r t o |> List.mapi (fun i x -> createAsn1ValueFromValueKind t i (BitStringValue x))
    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createBitStringFunction  r lm Codec.Encode t o defOrRef isValidFunction s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createBitStringFunction  r lm Codec.Decode t o defOrRef isValidFunction s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9
    let ret =
        {
            BitString.baseInfo  = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = printValue
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createBitStringFunction r t o
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((BitString ret),newPrms), s10


let private createBoolean (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Boolean) (us:State) =
    //let typeDefinition = DAstTypeDefinition.createBoolean  r l t o us
    let defOrRef            =  lm.lg.definitionOrRef o.definitionOrRef
    let equalFunction       = DAstEqual.createBooleanEqualFunction r lm t o defOrRef
    //let initialValue        = false
    let initFunction        = DAstInitialize.createBooleanInitFunc r lm t o defOrRef
    let isValidFunction, s1     = DastValidate2.createBoolFunction r lm t o defOrRef us
    let uperEncFunction, s2     = DAstUPer.createBooleanFunction r lm Codec.Encode t o defOrRef None isValidFunction s1
    let uperDecFunction, s3     = DAstUPer.createBooleanFunction r lm Codec.Decode t o defOrRef None isValidFunction s2
    let acnEncFunction, s4      = DAstACN.createBooleanFunction r deps lm Codec.Encode t o defOrRef None isValidFunction  s3
    let acnDecFunction, s5      = DAstACN.createBooleanFunction r deps lm Codec.Decode t o defOrRef None isValidFunction  s4
    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6
    let automaticTestCasesValues      = EncodeDecodeTestCase.BooleanAutomaticTestCaseValues r t o |> List.mapi (fun i x -> createAsn1ValueFromValueKind t i (BooleanValue x))
    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createBooleanFunction  r lm Codec.Encode t o defOrRef isValidFunction s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createBooleanFunction  r lm Codec.Decode t o defOrRef isValidFunction s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9
    let ret =
        {
            Boolean.baseInfo    = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createBooleanFunction r lm t o
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createBoolFunction r t o
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((Boolean ret),[]), s10


let private createEnumerated (r:Asn1AcnAst.AstRoot) (icdStgFileName:string) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Enumerated) (us:State) =
    let initialValue  =o.items.Head.Name.Value
    let defOrRef                        = lm.lg.definitionOrRef o.definitionOrRef
    let equalFunction                   = TL "EN_02" (fun () -> DAstEqual.createEnumeratedEqualFunction r lm t o defOrRef)
    let initFunction                    = TL "EN_03" (fun () -> DAstInitialize.createEnumeratedInitFunc r lm t o  defOrRef (EnumValue initialValue))
    let isValidFunction, s1             = TL "EN_04" (fun () -> DastValidate2.createEnumeratedFunction r lm t o defOrRef us)
    let uperEncFunction, s2             = TL "EN_05" (fun () -> DAstUPer.createEnumeratedFunction r lm Codec.Encode t o  defOrRef None isValidFunction s1)
    let uperDecFunction, s3             = TL "EN_06" (fun () -> DAstUPer.createEnumeratedFunction r lm Codec.Decode t o  defOrRef None isValidFunction s2)
    let acnEncFunction, s4              = TL "EN_07" (fun () -> DAstACN.createEnumeratedFunction r deps icdStgFileName lm Codec.Encode t o defOrRef defOrRef isValidFunction uperEncFunction s3)
    let acnDecFunction, s5              = TL "EN_08" (fun () -> DAstACN.createEnumeratedFunction r deps icdStgFileName lm Codec.Decode t o defOrRef defOrRef isValidFunction uperDecFunction s4)
    let uperEncDecTestFunc,s6           = TL "EN_09" (fun () -> EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5)
    let acnEncDecTestFunc ,s7           = TL "EN_10" (fun () -> EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6)
//    let automaticTestCasesValues        TL "EN_11" (fun () -> = EncodeDecodeTestCase.EnumeratedAutomaticTestCaseValues r t o |> List.mapi (fun i x -> createAsn1ValueFromValueKind t i (EnumValue x)))
    let xerEncFunction, s8              = TL "EN_12" (fun () -> XER r (fun () ->   DAstXer.createEnumeratedFunction  r lm Codec.Encode t o defOrRef isValidFunction s7) s7)
    let xerDecFunction, s9              = TL "EN_13" (fun () -> XER r (fun () ->   DAstXer.createEnumeratedFunction  r lm Codec.Decode t o defOrRef isValidFunction s8) s8)
    let xerEncDecTestFunc,s10           = TL "EN_14" (fun () -> EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9)
    let printValue                      = TL "EN_15" (fun () -> DAstVariables.createEnumeratedFunction r lm t o defOrRef)
    let ret =
        {
            Enumerated.baseInfo = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = printValue
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createEnumeratedFunction r t o
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((Enumerated ret),[]), s10








let private createObjectIdentifier (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ObjectIdentifier) (us:State) =
    //let typeDefinition = DAstTypeDefinition.createEnumerated  r l t o us
    let defOrRef            =  lm.lg.definitionOrRef o.definitionOrRef  //DAstTypeDefinition.createObjectIdentifier_u r lm t o
    let equalFunction       = DAstEqual.createObjectIdentifierEqualFunction r lm t o defOrRef

    let initialValue  = InternalObjectIdentifierValue([])
    let initFunction        = DAstInitialize.createObjectIdentifierInitFunc r lm t o  defOrRef (ObjOrRelObjIdValue initialValue)
    let isValidFunction, s1     = DastValidate2.createObjectIdentifierFunction r lm t o defOrRef us
    let uperEncFunction, s2     = DAstUPer.createObjectIdentifierFunction r lm Codec.Encode t o  defOrRef None isValidFunction s1
    let uperDecFunction, s3     = DAstUPer.createObjectIdentifierFunction r lm Codec.Decode t o  defOrRef None isValidFunction s2

    let acnEncFunction, s4      = DAstACN.createObjectIdentifierFunction r deps lm Codec.Encode t o defOrRef  isValidFunction uperEncFunction s3
    let acnDecFunction, s5      = DAstACN.createObjectIdentifierFunction r deps lm Codec.Decode t o defOrRef  isValidFunction uperDecFunction s4

    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6
    let automaticTestCasesValues      = EncodeDecodeTestCase.ObjectIdentifierAutomaticTestCaseValues r t o |> List.mapi (fun i x -> createAsn1ValueFromValueKind t i (ObjOrRelObjIdValue (InternalObjectIdentifierValue x)))
    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createObjectIdentifierFunction  r lm Codec.Encode t o defOrRef isValidFunction s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createObjectIdentifierFunction  r lm Codec.Decode t o defOrRef isValidFunction s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9

    let ret =
        {
            ObjectIdentifier.baseInfo = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createObjectIdentifierFunction r lm t o defOrRef
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createObjectIdentifierFunction r t o
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((ObjectIdentifier ret),[]), s10


let private createTimeType (r:Asn1AcnAst.AstRoot) (deps: Asn1AcnAst.AcnInsertedFieldDependencies) (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.TimeType) (us:State) =
    let defOrRef            =  lm.lg.definitionOrRef o.definitionOrRef  //DAstTypeDefinition.createTimeType_u r lm t o
    let equalFunction       = DAstEqual.createTimeTypeEqualFunction r lm t o defOrRef

    let initialValue  = (EncodeDecodeTestCase.TimeTypeAutomaticTestCaseValues r t o).Head
    let initFunction        = DAstInitialize.createTimeTypeInitFunc r lm t o  defOrRef (TimeValue initialValue)
    let isValidFunction, s1     = DastValidate2.createTimeTypeFunction r lm t o defOrRef us

    let uperEncFunction, s2     = DAstUPer.createTimeTypeFunction r lm Codec.Encode t o  defOrRef None isValidFunction s1
    let uperDecFunction, s3     = DAstUPer.createTimeTypeFunction r lm Codec.Decode t o  defOrRef None isValidFunction s2

    let acnEncFunction, s4      = DAstACN.createTimeTypeFunction r deps lm Codec.Encode t o defOrRef  isValidFunction uperEncFunction s3
    let acnDecFunction, s5      = DAstACN.createTimeTypeFunction r deps lm Codec.Decode t o defOrRef  isValidFunction uperDecFunction s4

    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6
    let automaticTestCasesValues      = EncodeDecodeTestCase.TimeTypeAutomaticTestCaseValues r t o |> List.mapi (fun i x -> createAsn1ValueFromValueKind t i (TimeValue x))
    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createTimeTypeFunction  r lm Codec.Encode t o defOrRef isValidFunction s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createTimeTypeFunction  r lm Codec.Decode t o defOrRef isValidFunction s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9

    let ret =
        {
            TimeType.baseInfo = o
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createTimeTypeFunction r lm t o defOrRef
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createTimeTypeFunction r t o
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((TimeType ret),[]), s10










let private createSequenceOf (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.SequenceOf) (childType:Asn1Type, us:State) =
    let newPrms, us0 = t.acnParameters |> foldMap(fun ns p -> mapAcnParameter r deps lm m t p ns) us
    let defOrRef            =  lm.lg.definitionOrRef o.definitionOrRef
    //let typeDefinition = DAstTypeDefinition.createSequenceOf r l t o childType.typeDefinition us0
    let equalFunction       = DAstEqual.createSequenceOfEqualFunction r lm t o defOrRef childType
    let initFunction        = DAstInitialize.createSequenceOfInitFunc r lm t o defOrRef childType
    let isValidFunction, s1     = DastValidate2.createSequenceOfFunction r lm t o defOrRef childType equalFunction us0
    let uperEncFunction, s2     = DAstUPer.createSequenceOfFunction r lm Codec.Encode t o  defOrRef None isValidFunction childType s1
    let uperDecFunction, s3     = DAstUPer.createSequenceOfFunction r lm Codec.Decode t o  defOrRef None isValidFunction childType s2
    let acnEncFunction, s4      = DAstACN.createSequenceOfFunction r deps lm Codec.Encode t o defOrRef isValidFunction childType s3
    let acnDecFunction, s5      = DAstACN.createSequenceOfFunction r deps lm Codec.Decode t o defOrRef isValidFunction childType s4
    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6
    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createSequenceOfFunction  r lm Codec.Encode t o defOrRef isValidFunction childType s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createSequenceOfFunction  r lm Codec.Decode t o defOrRef isValidFunction childType s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9
    let ret =
        {
            SequenceOf.baseInfo = o
            childType           = childType
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createSequenceOfFunction r lm t o defOrRef  childType
            //typeDefinition      = typeDefinition
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createSequenceOfFunction r t o
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((SequenceOf ret),newPrms), s10



let private createAsn1Child (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (ch:Asn1AcnAst.Asn1Child) (newChildType : Asn1Type, us:State) =
    let ret =
        {

            Asn1Child.Name     = ch.Name
            _c_name            = ch._c_name
            _scala_name        = ch._scala_name
            _ada_name          = ch._ada_name
            Type               = newChildType
            Optionality        = ch.Optionality
            // acnArgs            = ch.acnArgs
            Comments           = ch.Comments |> Seq.toArray
            isEqualBodyStats   = DAstEqual.isEqualBodySequenceChild lm ch newChildType
        }
    Asn1Child ret, us




let private createSequence (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Sequence) (children:SeqChildInfo list, us:State) =
    let newPrms, us0 = TL "SQ_mapAcnParameter" (fun () -> t.acnParameters |> foldMap(fun ns p -> mapAcnParameter r deps lm m t p ns) us)
    //let typeDefinition = DAstTypeDefinition.createSequence r l t o children us0
    let defOrRef            =  lm.lg.definitionOrRef o.definitionOrRef //TL "SQ_DAstTypeDefinition" (fun () -> DAstTypeDefinition.createSequence_u r lm t o  children )
    let equalFunction       = TL "SQ_DAstEqual" (fun () -> DAstEqual.createSequenceEqualFunction r lm t o defOrRef children)
    let initFunction        = TL "SQ_DAstInitialize" (fun () -> DAstInitialize.createSequenceInitFunc r lm t o defOrRef children )
    let isValidFunction, s1     =  TL "SQ_DastValidate2" (fun () -> DastValidate2.createSequenceFunction r lm t o defOrRef children  us)

    let uperEncFunction, s2     = TL "SQ_DAstUPer_encode" (fun () ->DAstUPer.createSequenceFunction r lm Codec.Encode t o defOrRef isValidFunction children s1)
    let uperDecFunction, s3     = TL "SQ_DAstUPer_decode" (fun () ->DAstUPer.createSequenceFunction r lm Codec.Decode t o defOrRef isValidFunction children s2)
    let acnEncFunction, s4      = TL "SQ_DAstACN_encode" (fun () ->DAstACN.createSequenceFunction r deps lm Codec.Encode t o defOrRef  isValidFunction children newPrms s3)
    let acnDecFunction, s5      = TL "SQ_DAstACN_decode" (fun () ->DAstACN.createSequenceFunction r deps lm Codec.Decode t o defOrRef  isValidFunction children newPrms s4)
    let uperEncDecTestFunc,s6         = TL "SQ_EncodeDecodeTestCase" (fun () ->EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5)
    let acnEncDecTestFunc ,s7         = TL "SQ_EncodeDecodeTestCase" (fun () ->EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6)
    let xerEncFunction, s8      = TL "SQ_DAstXer" (fun () ->XER r (fun () ->   DAstXer.createSequenceFunction  r lm Codec.Encode t o defOrRef isValidFunction children s7) s7)
    let xerDecFunction, s9      = TL "SQ_DAstXer" (fun () ->XER r (fun () ->   DAstXer.createSequenceFunction  r lm Codec.Decode t o defOrRef isValidFunction children s8) s8)
    let xerEncDecTestFunc,s10   = TL "SQ_EncodeDecodeTestCase" (fun () ->EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9)
    let ret =
        {
            Sequence.baseInfo   = o
            children            = children
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createSequenceFunction r lm t o defOrRef  children
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createSequenceFunction r t o children
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((Sequence ret),newPrms), s10

let private createChoice (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Choice) (children:ChChildInfo list, us:State) =
    let newPrms, us0 = t.acnParameters |> foldMap(fun ns p -> mapAcnParameter r deps lm m t p ns) us
    //let typeDefinition = DAstTypeDefinition.createChoice r l t o children us0
    let defOrRef            = lm.lg.definitionOrRef o.definitionOrRef 
    let equalFunction       = TL "DAstEqual" (fun () -> DAstEqual.createChoiceEqualFunction r lm t o defOrRef children)
    let initFunction        = TL "DAstInitialize" (fun () -> DAstInitialize.createChoiceInitFunc r lm t o defOrRef children)
    let isValidFunction, s1     = TL "DastValidate2" (fun () -> DastValidate2.createChoiceFunction r lm t o defOrRef defOrRef children None us)
    let uperEncFunction, s2     = TL "DAstUPer" (fun () -> DAstUPer.createChoiceFunction r lm Codec.Encode t o  defOrRef None isValidFunction children s1)
    let uperDecFunction, s3     = TL "DAstUPer" (fun () -> DAstUPer.createChoiceFunction r lm Codec.Decode t o  defOrRef None isValidFunction children s2)
    let (acnEncFunction, s4),ec      = TL "DAstACN" (fun () -> DAstACN.createChoiceFunction r deps lm Codec.Encode t o defOrRef defOrRef isValidFunction children newPrms  s3)
    let (acnDecFunction, s5),_      = TL "DAstACN" (fun () -> DAstACN.createChoiceFunction r deps lm Codec.Decode t o defOrRef defOrRef isValidFunction children newPrms  s4)
    let uperEncDecTestFunc,s6         = TL "EncodeDecodeTestCase" (fun () -> EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5)
    let acnEncDecTestFunc ,s7         = TL "EncodeDecodeTestCase" (fun () -> EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some acnEncFunction) (Some acnDecFunction) s6)
    let xerEncFunction, s8      = TL "DAstXer" (fun () -> XER r (fun () ->   DAstXer.createChoiceFunction  r lm Codec.Encode t o defOrRef isValidFunction children s7) s7)
    let xerDecFunction, s9      = TL "DAstXer" (fun () -> XER r (fun () ->   DAstXer.createChoiceFunction  r lm Codec.Decode t o defOrRef isValidFunction children s8) s8)
    let xerEncDecTestFunc,s10   = TL "EncodeDecodeTestCase" (fun () -> EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9)
    let ret =
        {
            Choice.baseInfo     = o
            children            = children
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createChoiceFunction r lm t o  defOrRef children
            //typeDefinition      = typeDefinition
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction
            acnDecFunction      = acnDecFunction
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            //automaticTestCasesValues = automaticTestCasesValues
            constraintsAsn1Str = DAstAsn1.createChoiceFunction r t o children
            xerEncDecTestFunc   = xerEncDecTestFunc
            ancEncClass         = ec
        }
    ((Choice ret),newPrms), s10

let private createChoiceChild (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (ch:Asn1AcnAst.ChChildInfo) (newChildType : Asn1Type, us:State) =
    let typeDefinitionName =
        let longName = newChildType.id.AcnAbsPath.Tail |> List.rev |> List.tail |> List.rev |> Seq.StrJoin "_"
        ToC2(r.args.TypePrefix + longName.Replace("#","elem"))
    let ret =
        {

            ChChildInfo.Name     = ch.Name
            _c_name             = ch._c_name
            _scala_name         = ch._scala_name
            _ada_name           = ch._ada_name
            _present_when_name_private  = ch.present_when_name
            acnPresentWhenConditions = ch.acnPresentWhenConditions
            chType              = newChildType
            Comments            = ch.Comments |> Seq.toArray
            isEqualBodyStats    = DAstEqual.isEqualBodyChoiceChild typeDefinitionName lm ch newChildType
            //isValidBodyStats    = DAstValidate.isValidChoiceChild l ch newChildType
            Optionality         = ch.Optionality
        }
    ret, us

let private createReferenceType (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (pi : Asn1Fold.ParentInfo<ParentInfoData> option) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.ReferenceType) (newResolvedType:Asn1Type, us:State) =
    let newPrms, us0 = t.acnParameters |> foldMap(fun ns p -> mapAcnParameter r deps lm m t p ns) us
    let defOrRef            =  lm.lg.definitionOrRef o.definitionOrRef   
    let equalFunction       = DAstEqual.createReferenceTypeEqualFunction r lm t o defOrRef newResolvedType
    let initFunction        = DAstInitialize.createReferenceType r lm t o defOrRef newResolvedType
    let isValidFunction, s1     = DastValidate2.createReferenceTypeFunction r lm t o defOrRef newResolvedType us
    let uperEncFunction, s2     = DAstUPer.createReferenceFunction r lm Codec.Encode t o defOrRef isValidFunction newResolvedType s1
    let uperDecFunction, s3     = DAstUPer.createReferenceFunction r lm Codec.Decode t o defOrRef isValidFunction newResolvedType s2
    let acnEncFunction, s4      = DAstACN.createReferenceFunction r deps lm Codec.Encode t o defOrRef  isValidFunction newResolvedType s3
    let acnDecFunction, s5      = DAstACN.createReferenceFunction r deps lm Codec.Decode t o defOrRef  isValidFunction newResolvedType s4

    let uperEncDecTestFunc,s6         = EncodeDecodeTestCase.createUperEncDecFunction r lm t defOrRef equalFunction isValidFunction (Some uperEncFunction) (Some uperDecFunction) s5
    let acnEncDecTestFunc ,s7         = EncodeDecodeTestCase.createAcnEncDecFunction r lm t defOrRef equalFunction isValidFunction acnEncFunction acnDecFunction s6

    let xerEncFunction, s8      = XER r (fun () ->   DAstXer.createReferenceFunction  r lm Codec.Encode t o defOrRef isValidFunction newResolvedType s7) s7
    let xerDecFunction, s9      = XER r (fun () ->   DAstXer.createReferenceFunction  r lm Codec.Decode t o defOrRef isValidFunction newResolvedType s8) s8
    let xerEncDecTestFunc,s10   = EncodeDecodeTestCase.createXerEncDecFunction r lm t defOrRef equalFunction isValidFunction xerEncFunction xerDecFunction s9

    let ret =
        {
            ReferenceType.baseInfo = o
            resolvedType        = newResolvedType
            //typeDefinition      = typeDefinition
            definitionOrRef     = defOrRef
            printValue          = DAstVariables.createReferenceTypeFunction r lm t o defOrRef newResolvedType
            //initialValue        = initialValue
            initFunction        = initFunction
            equalFunction       = equalFunction
            isValidFunction     = isValidFunction
            uperEncFunction     = uperEncFunction
            uperDecFunction     = uperDecFunction
            acnEncFunction      = acnEncFunction.Value
            acnDecFunction      = acnDecFunction.Value
            uperEncDecTestFunc  = uperEncDecTestFunc
            acnEncDecTestFunc   = acnEncDecTestFunc
            xerEncFunction      = xerEncFunction
            xerDecFunction      = xerDecFunction
            //automaticTestCasesValues = newResolvedType.automaticTestCasesValues
            constraintsAsn1Str = newResolvedType.ConstraintsAsn1Str
            xerEncDecTestFunc   = xerEncDecTestFunc
        }
    ((ReferenceType ret),newPrms), s10


let private createType (r:Asn1AcnAst.AstRoot) pi (t:Asn1AcnAst.Asn1Type) ((newKind, newPrms), (us:State )) =
    let newAsn1Type  =
        {
            Asn1Type.Kind = newKind
            id            = t.id
            acnAlignment  = t.acnAlignment
            acnParameters = newPrms
            Location      = t.Location
            moduleName    = t.moduleName
            inheritInfo = t.inheritInfo
            typeAssignmentInfo = t.typeAssignmentInfo
            unitsOfMeasure = t.unitsOfMeasure
            //newTypeDefName = DAstTypeDefinition2.getTypedefName r pi t
        }
    match us.newTypesMap.ContainsKey t.id with
    | false -> us.newTypesMap.Add(t.id, newAsn1Type)
    | true  -> us.newTypesMap[t.id] <- newAsn1Type
    newAsn1Type, us

let private mapType (r:Asn1AcnAst.AstRoot) (icdStgFileName:string) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (t:Asn1AcnAst.Asn1Type, us:State) =
    Asn1Fold.foldType2
        (fun pi t ti us -> TL "createInteger" (fun () -> createInteger r deps lm m pi t ti us))
        (fun pi t ti us -> TL "createReal" (fun () -> createReal r deps lm m pi t ti us))
        (fun pi t ti us ->
            let (strtype, prms), ns = TL "createStringType" (fun () -> createStringType r deps lm m pi t ti us)
            ((IA5String strtype),prms), ns)
        (fun pi t ti us ->
            let (strtype, prms), ns = TL "createStringType" (fun () -> createStringType r deps lm m pi t ti us)
            ((IA5String strtype),prms), ns)
        (fun pi t ti us -> TL "createOctetString" (fun () -> createOctetString r deps lm m pi t ti us))
        (fun pi t ti us -> TL "createTimeType" (fun () -> createTimeType r deps lm m pi t ti us))
        (fun pi t ti us -> TL "createNullType" (fun () -> createNullType r deps lm m pi t ti us))
        (fun pi t ti us -> TL "createBitString" (fun () -> createBitString r deps lm m pi t ti us))

        (fun pi t ti us -> TL "createBoolean" (fun () -> createBoolean r deps lm m pi t ti us))
        (fun pi t ti us -> TL "createEnumerated" (fun () -> createEnumerated r icdStgFileName deps lm m pi t ti us))
        (fun pi t ti us -> TL "createObjectIdentifier" (fun () -> createObjectIdentifier r deps lm  m pi t ti us))

        (fun pi t ti newChild -> TL "createSequenceOf" (fun () -> createSequenceOf r deps lm m pi t ti newChild))

        (fun pi t ti newChildren -> TL "createSequence" (fun () -> createSequence r deps lm m pi t ti newChildren))
        (fun ch newChild -> TL "createAsn1Child" (fun () -> createAsn1Child r lm m ch newChild))
        (fun ch us -> TL "createAcnChild" (fun () -> createAcnChild r icdStgFileName deps lm m ch us))


        (fun pi t ti newChildren -> TL "createChoice" (fun () -> createChoice r deps lm m pi t ti newChildren))
        (fun ch newChild -> TL "createChoiceChild" (fun () -> createChoiceChild r lm m ch newChild))

        (fun pi t ti newBaseType ->
            createReferenceType r deps lm m pi t ti newBaseType)

        (fun pi t ti us ->
            let refToType = ReferenceToType.createFromModAndTasName ti.modName.Value ti.tasName.Value
            let newType = us.newTypesMap[refToType] :?> Asn1Type
            //createReferenceType2 r deps lm m pi t ti (newType,us) 
            createReferenceType r deps lm m pi t ti (newType,us) 
            )
        (fun pi t ((newKind, newPrms),us)        -> TL "createType" (fun () -> createType r pi t ((newKind, newPrms),us)))

        (fun pi t ti us -> (),us)
        (fun pi t ti us -> (),us)
        (fun pi t ti us -> (),us)

        deps
        None
        t
        us
(*

let private mapTypeId (r:Asn1AcnAst.AstRoot)  (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (t:Asn1AcnAst.Asn1Type) =
    Asn1Fold.foldType2
        (fun pi t ti us -> [t.id], us)
        (fun pi t ti us -> [t.id], us)
        (fun pi t ti us -> [t.id], us)
        (fun pi t ti us -> [t.id], us)
        (fun pi t ti us -> [t.id], us)
        (fun pi t ti us -> [t.id], us)
        (fun pi t ti us -> [t.id], us)
        (fun pi t ti us -> [t.id], us)

        (fun pi t ti us -> [t.id], us)
        (fun pi t ti us -> [t.id], us)
        (fun pi t ti us -> [t.id], us)

        (fun pi t ti (newChild,_) -> newChild@[t.id],0)

        (fun pi t ti (newChildren,_) -> (newChildren|> List.collect id)@[t.id], 0)
        (fun ch (newChild,_) -> newChild, 0)
        (fun ch us -> [ch.id], us)


        (fun pi t ti (newChildren,_) -> (newChildren|> List.collect id)@[t.id], 0)
        (fun ch (newChild,_) -> newChild, 0)

        (fun pi t ti newBaseType -> [t.id], 0)
        (fun pi t ti us -> [t.id], 0)

        (fun pi t (newKind,_)        -> newKind, 0)

        (fun pi t ti us -> (),us)
        (fun pi t ti us -> (),us)
        (fun pi t ti us -> (),us)

        deps
        None
        t
        0 |> fst

*)


let private mapTas (r:Asn1AcnAst.AstRoot) (icdStgFileName:string) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (tas:Asn1AcnAst.TypeAssignment) (us:State)=
    let newType, ns = TL "mapType" (fun () -> mapType r icdStgFileName deps lm m (tas.Type, us))
    {
        TypeAssignment.Name = tas.Name
        c_name = tas.c_name
        scala_name = tas.scala_name
        ada_name = tas.ada_name
        Type = newType
        Comments = tas.Comments |> Seq.toArray
    },ns


let private mapVas (r:Asn1AcnAst.AstRoot) (icdStgFileName:string) (allNewTypeAssignments : (Asn1Module*TypeAssignment) list) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)   (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (vas:Asn1AcnAst.ValueAssignment) (us:State)=
    let newType, ns =
        match vas.Type.Kind with
        | Asn1AcnAst.ReferenceType ref ->
            //let aaa = allNewTypeAssignments |> Seq.map(fun tz -> sprintf "%s.%s")
            match allNewTypeAssignments |> Seq.tryFind(fun (tm, ts) -> ts.Name.Value = ref.tasName.Value && ref.modName.Value = tm.Name.Value) with
            | None          ->
                let oldType = Asn1AcnAstUtilFunctions.GetActualTypeByName r ref.modName ref.tasName
                mapType r icdStgFileName deps lm m (oldType, us)
            | Some (mt, newTas)   ->
                let (newKind, newPrms),ns = createReferenceType r deps lm m None vas.Type ref (newTas.Type, us)
                createType r None vas.Type ((newKind, newPrms),us)
                //newTas.Type, us
        | _     ->  mapType r icdStgFileName deps lm m (vas.Type, us)
    {
        ValueAssignment.Name = vas.Name
        c_name = vas.c_name
        scala_name = vas.scala_name
        ada_name = vas.ada_name
        Type = newType
        Value = mapValue vas.Value
    },ns



let internal sortTypes (r:Asn1AcnAst.AstRoot) =
    let rec getTypeDependencies2 (t:Asn1AcnAst.Asn1Type) : (TypeAssignmentInfo list )    =
        let prms = t.acnParameters |> List.choose(fun z -> match z.asn1Type with AcnGenericTypes.AcnPrmRefType (mdName,tsName) -> Some ({TypeAssignmentInfo.modName = mdName.Value; tasName = tsName.Value}) | _ -> None )
        match t.Kind with
        | Asn1AcnAst.Integer      _             -> prms
        | Asn1AcnAst.Real         _             -> prms
        | Asn1AcnAst.IA5String    _             -> prms
        | Asn1AcnAst.NumericString _            -> prms
        | Asn1AcnAst.OctetString  _             -> prms
        | Asn1AcnAst.NullType     _             -> prms
        | Asn1AcnAst.BitString    _             -> prms
        | Asn1AcnAst.Boolean      _             -> prms
        | Asn1AcnAst.Enumerated   _             -> prms
        | Asn1AcnAst.ObjectIdentifier _         -> prms
        | Asn1AcnAst.SequenceOf    sqof         -> prms@(getTypeDependencies2 sqof.child)
        | Asn1AcnAst.Sequence      sq            -> 
            let asn1Children = sq.children |> List.choose(fun c -> match c with Asn1AcnAst.Asn1Child q -> Some q | _ -> None)
            prms@(asn1Children |> List.collect (fun ch -> getTypeDependencies2 ch.Type))
        | Asn1AcnAst.Choice        children     -> prms@(children.children |> List.collect (fun ch -> getTypeDependencies2 ch.Type))
        | Asn1AcnAst.ReferenceType ref          -> {TypeAssignmentInfo.modName = ref.modName.Value; tasName = ref.tasName.Value}::prms
        | Asn1AcnAst.TimeType  _                -> prms


    let allNodes =
        seq {
            for f in r.Files do
                for m in f.Modules do
                    for tas in m.TypeAssignments do
                        let ts = {TypeAssignmentInfo.modName = m.Name.Value; tasName = tas.Name.Value}
                        let deps = getTypeDependencies2 tas.Type
                        yield (ts, deps)
        } |> List.ofSeq
        
    let independentNodes = allNodes |> List.filter(fun (_,list) -> List.isEmpty list) |> List.map(fun (n,l) -> n)
    let dependentNodes = allNodes |> List.filter(fun (_,list) -> not (List.isEmpty list) )
    let sortedTypeAss =
        DoTopologicalSort independentNodes dependentNodes
            (fun cyclicTasses ->
                match cyclicTasses with
                | []    -> BugErrorException "Impossible"
                | (m1,deps) ::_ ->
                    let printTas (md:TypeAssignmentInfo, deps: TypeAssignmentInfo list) =
                        sprintf "Type assignment '%s.%s' depends on : %s" md.modName md.tasName (deps |> List.map(fun z -> "'" + z.modName + "." + z.tasName + "'") |> Seq.StrJoin ", ")
                    let cycTasses = cyclicTasses |> List.map printTas |> Seq.StrJoin "\n\tand\n"
                    SemanticError(emptyLocation, sprintf "Cyclic Types detected:\n%s\n"  cycTasses)                    )
    sortedTypeAss


let private mapModule (r:Asn1AcnAst.AstRoot) (newTasMap : Map<TypeAssignmentInfo, TypeAssignment>) (icdStgFileName:string) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros)  (m:Asn1AcnAst.Asn1Module) (us:State) =
    //let newTases, ns1 = TL "mapTas" (fun () -> m.TypeAssignments |> foldMap (fun ns nt -> mapTas r icdStgFileName deps lm m nt ns) us)
    let allNewTasses = 
        m.TypeAssignments |> 
        List.choose(fun tas -> 
            let tsInfo = {TypeAssignmentInfo.modName = m.Name.Value; tasName = tas.Name.Value}
            newTasMap.TryFind tsInfo)

    {
        Asn1Module.Name = m.Name
        TypeAssignments = allNewTasses
        ValueAssignments = []
        Imports = m.Imports
        Exports = m.Exports
        Comments = m.Comments
    }, us

let private reMapModule (r:Asn1AcnAst.AstRoot) (icdStgFileName:string) (files0:Asn1File list) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1Module) (us:State) =
    let allNewTasses = files0 |> List.collect(fun f -> f.Modules) |> List.collect(fun m -> m.TypeAssignments |> List.map(fun ts -> (m,ts)))
    let oldModule = r.Files |> List.collect(fun f -> f.Modules) |> List.find(fun oldM -> oldM.Name.Value = m.Name.Value)
    let newVases, ns1 = oldModule.ValueAssignments |> foldMap (fun ns nt -> mapVas r icdStgFileName allNewTasses deps lm oldModule nt ns) us
    { m with ValueAssignments = newVases}, ns1

let private mapFile (r:Asn1AcnAst.AstRoot) (newTasMap : Map<TypeAssignmentInfo, TypeAssignment>) (icdStgFileName:string) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (f:Asn1AcnAst.Asn1File) (us:State) =
    let newModules, ns = TL "mapModules" (fun () -> f.Modules |> foldMap (fun cs m -> mapModule r newTasMap icdStgFileName deps lm m cs) us)
    {
        Asn1File.FileName = f.FileName
        Tokens = f.Tokens
        Modules = newModules
    }, ns

let private reMapFile (r:Asn1AcnAst.AstRoot) (icdStgFileName:string) (files0:Asn1File list) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (f:Asn1File) (us:State) =
    let newModules, ns = f.Modules |> foldMap (fun cs m -> reMapModule r icdStgFileName files0 deps lm m cs) us
    {f with Modules = newModules}, ns

let DoWork (r:Asn1AcnAst.AstRoot) (icdStgFileName:string) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (lang:CommonTypes.ProgrammingLanguage) (lm:LanguageMacros) (encodings: CommonTypes.Asn1Encoding list) : AstRoot=
    let l = lang

    let typeIdsSet = Map.empty  // become obsolete
        //r.Files |>
        //Seq.collect(fun f -> f.Modules) |>
        //Seq.collect(fun m -> m.TypeAssignments) |>
        //Seq.collect(fun tas -> mapTypeId r deps tas.Type) |>
        //Seq.map(fun tid -> ToC (tid.AcnAbsPath.Tail.StrJoin("_").Replace("#","elem"))) |>
        //Seq.groupBy id |>
        //Seq.map(fun (id, lst) -> (id, Seq.length lst)) |>
        //Map.ofSeq
    let initialState = {currErrorCode = 1; curErrCodeNames = Set.empty; (*allocatedTypeDefNames = []; allocatedTypeDefNameInTas = Map.empty;*) alphaIndex=0; alphaFuncs=[];typeIdsSet=typeIdsSet; newTypesMap = new Dictionary<ReferenceToType, System.Object>(); icdHashes = Map.empty; functionCalls=Map.empty}
    let sortedTypes = sortTypes r
    //let private mapTas (r:Asn1AcnAst.AstRoot) (icdStgFileName:string) (deps:Asn1AcnAst.AcnInsertedFieldDependencies)  (lm:LanguageMacros) (m:Asn1AcnAst.Asn1Module) (tas:Asn1AcnAst.TypeAssignment) (us:State)=

    let newTasses, ns0 = 
        sortedTypes |> 
        foldMap (fun ns tas -> 
            let oldTas = r.typeAssignmentsMap.[(tas.modName,tas.tasName)]
            let m = r.modulesMap.[tas.modName]
            mapTas r icdStgFileName deps lm m oldTas ns) initialState
    let newTasMap = newTasses |> Seq.map(fun tas -> (tas.Type.tasInfo.Value, tas)) |> Map.ofSeq
    //first map all type assignments and then value assignments
    let files0, ns = TL "mapFile" (fun () -> r.Files |> foldMap (fun cs f -> mapFile r newTasMap icdStgFileName deps lm f cs) ns0)
    let files, ns = TL "reMapFile" (fun () -> files0 |> foldMap (fun cs f -> reMapFile r icdStgFileName files0 deps lm f cs) ns)
    let icdTases = ns.icdHashes
    {
        AstRoot.Files = files
        acnConstants = r.acnConstants
        args = r.args
        programUnits = DAstProgramUnit.createProgramUnits r.args files lm
        lang = l
        acnParseResults = r.acnParseResults
        deps    = deps
        icdHashes   = ns.icdHashes
    }
