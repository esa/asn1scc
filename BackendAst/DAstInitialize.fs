module DAstInitialize
open System
open System.Numerics
open System.Globalization
open System.IO

open FsUtils
open CommonTypes
open RangeSets
open ValueSets
open SizeableSet
open Asn1AcnAstUtilFunctions
open Asn1Fold
open DAst
open DAstUtilFunctions
open Language




(*
create c and Ada procedures that initialize an ASN.1 type.



*)





let rangeConstraint2RangeSet (r:Asn1AcnAst.AstRoot)  (c:Asn1AcnAst.RangeTypeConstraint<'v,'v>) st =
    foldRangeTypeConstraint 
        (fun _ (e1:RangeSet<'v>) e2 b s -> e1.union e2, s) 
        (fun _ e1 e2 s -> e1.intersect e2, s) 
        (fun _ e s -> e.complement, s) 
        (fun _ e1 e2 s -> e1.difference e2, s) 
        (fun _ e s -> e,s) 
        (fun _ e1 e2 s -> e1.union e2, s) 
        (fun _ v  s         -> RangeSet<'v>.createFromSingleValue v ,s)
        (fun _ v1 v2  minIsIn maxIsIn s   -> RangeSet<'v>.createFromValuePair v1 v2  minIsIn maxIsIn, s)
        (fun _ v1 minIsIn s   -> RangeSet<'v>.createPosInfinite v1 minIsIn, s)
        (fun _ v2 maxIsIn s   -> RangeSet<'v>.createNegInfinite v2 maxIsIn, s)
        c
        st




let genericConstraint2ValueSet  (r:Asn1AcnAst.AstRoot) (c:Asn1AcnAst.GenericConstraint<'v>)  st =
    foldGenericConstraint 
        (fun _ (e1:ValueSet<'v>) e2 b s -> e1.union e2, s) 
        (fun _ e1 e2 s -> e1.intersect e2, s) 
        (fun _ e s -> e.complement, s) 
        (fun _ e1 e2 s -> e1.difference e2, s) 
        (fun _ e s -> e,s) 
        (fun _ e1 e2 s -> e1.union e2, s) 
        (fun _ v  s         -> ValueSet<'v>.createFromSingleValue v ,s)
        c
        st

//range types
let integerConstraint2BigIntSet r (c:Asn1AcnAst.IntegerTypeConstraint) = rangeConstraint2RangeSet r c
let realConstraint2DoubleSet r (c:Asn1AcnAst.RealTypeConstraint) = rangeConstraint2RangeSet r c

//single value types
let boolConstraint2BoolSet r (c:Asn1AcnAst.BoolConstraint) = genericConstraint2ValueSet r c
let enumConstraint2StringSet r (c:Asn1AcnAst.EnumConstraint) = genericConstraint2ValueSet r c
let objectIdConstraint2StringSet r (c:Asn1AcnAst.ObjectIdConstraint) = genericConstraint2ValueSet r c



let foldSizeRangeTypeConstraint (r:Asn1AcnAst.AstRoot)  (c:Asn1AcnAst.PosIntTypeConstraint) st = 
    rangeConstraint2RangeSet r c st

//SizeableSet
let foldSizableConstraint (r:Asn1AcnAst.AstRoot)  (c:Asn1AcnAst.SizableTypeConstraint<'v>) st =
    foldSizableTypeConstraint2 
        (fun _ (e1:SizeableSet<'v>) e2 b s -> e1.union e2, s) 
        (fun _ e1 e2 s -> e1.intersect e2, s) 
        (fun _ e s -> e.complement, s) 
        (fun _ e1 e2 s -> e1.difference e2, s) 
        (fun _ e s -> e,s) 
        (fun _ e1 e2 s -> e1.union e2, s) 
        (fun _ v  s         -> SizeableSet<'v>.createFromSingleValue v ,s)
        (fun _ intCon s       -> 
            let sizeRange, ns = foldSizeRangeTypeConstraint r intCon s
            SizeableSet<'v>.createFromSizeRange sizeRange, ns)
        c
        st



let octetConstraint2Set r (c:Asn1AcnAst.OctetStringConstraint) = foldSizableConstraint r c
let bitConstraint2Set r (c:Asn1AcnAst.BitStringConstraint) = foldSizableConstraint r c




let ia5StringConstraint2Set  (r:Asn1AcnAst.AstRoot)    (c:Asn1AcnAst.IA5StringConstraint) (us0:State) =
    foldStringTypeConstraint2 
        (fun _ (e1:SizeableSet<string>) e2 b s -> e1.union e2, s) 
        (fun _ e1 e2 s -> e1.intersect e2, s) 
        (fun _ e s -> e.complement, s) 
        (fun _ e1 e2 s -> e1.difference e2, s) 
        (fun _ e s -> e,s) 
        (fun _ e1 e2 s -> e1.union e2, s) 
        (fun _ v  s         -> SizeableSet<string>.createFromSingleValue v ,s)
        (fun _ intCon s       -> 
            let sizeRange, ns = foldSizeRangeTypeConstraint r intCon s
            SizeableSet<string>.createFromSizeRange sizeRange, ns)
        (fun _ alphcon s      -> 
            //currently the alphabet constraints are ignored ...
            Range2D ({sizeSet  = Range_Universe;  valueSet = SsUniverse}) , s) 
        c
        us0 


type AnySet =
    | IntSet of RangeSet<BigInteger>
    | RealSet of RangeSet<double>
    | StrSet of SizeableSet<string>
    | OctSet of SizeableSet<Asn1AcnAst.OctetStringValue * (ReferenceToValue * SrcLoc)>
    | BitSet of SizeableSet<Asn1AcnAst.BitStringValue * (ReferenceToValue * SrcLoc)>
    | NulSet
    | BoolSet of ValueSet<bool>
    | EnumSet of ValueSet<string>
    | ObjIdSet of ValueSet<Asn1AcnAst.ObjectIdenfierValue>
    | SeqOfSet  of SizeableSet<Asn1AcnAst.SeqOfValue>

and SequenceOfSet = {
    sizeableSet : SizeableSet<Asn1AcnAst.SeqOfValue>
    childSet    : AnySet option
}

type SequenceOfSet with
    member this.intersect (other:SequenceOfSet) =
        {this with sizeableSet = this.sizeableSet.intersect other.sizeableSet}


let rec anyConstraint2GenericSet (r:Asn1AcnAst.AstRoot)  (erLoc:SrcLoc) (t:Asn1Type) (ac:Asn1AcnAst.AnyConstraint) st =
    match t.ActualType.Kind, ac with
    | Integer o, Asn1AcnAst.IntegerTypeConstraint c        -> 
        let set, ns = integerConstraint2BigIntSet r c st
        IntSet set, ns
    | Real o, Asn1AcnAst.RealTypeConstraint   c            -> 
        let set, ns = realConstraint2DoubleSet r c st
        RealSet set, ns
    | IA5String  o, Asn1AcnAst.IA5StringConstraint c       -> 
        let set, ns = ia5StringConstraint2Set r c st
        StrSet set, ns
    | OctetString o, Asn1AcnAst.OctetStringConstraint c    -> 
        let set, ns = octetConstraint2Set r c st
        OctSet set, ns
    | BitString o, Asn1AcnAst.BitStringConstraint c        -> 
        let set, ns = bitConstraint2Set r c st
        BitSet set, ns
    | NullType o, Asn1AcnAst.NullConstraint                -> NulSet, st
    | Boolean o, Asn1AcnAst.BoolConstraint c               -> 
        let set, ns = boolConstraint2BoolSet r c st
        BoolSet set, ns
    | Enumerated o, Asn1AcnAst.EnumConstraint c            -> 
        let set, ns = enumConstraint2StringSet r c st
        EnumSet set, ns
    | ObjectIdentifier o, Asn1AcnAst.ObjectIdConstraint c  -> 
        let set, ns = objectIdConstraint2StringSet r c st
        ObjIdSet set, ns
//    | Sequence o, Asn1AcnAst.SeqConstraint c               -> 
//        let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.SeqValue) = VCBTrue //currently single value constraints are ignored.
//        sequenceConstraint2ValidationCodeBlock r l t.id o.Asn1Children valToStrFunc  c st
//    | SequenceOf o, Asn1AcnAst.SequenceOfConstraint c      -> sequenceOfConstraint2ValidationCodeBlock r l t.id o.baseInfo o.childType o.equalFunction c st
//    | Choice o, Asn1AcnAst.ChoiceConstraint c              -> 
//        let valToStrFunc (p:CallerScope) (v:Asn1AcnAst.ChValue) = VCBTrue //currently single value constraints are ignored.
//        choiceConstraint2ValidationCodeBlock r l t.id o.children valToStrFunc o.definitionOrRef c st
    | _                                         -> raise(SemanticError(erLoc, "Invalid combination of type/constraint type"))








//and sequenceOfConstraint2ValidationCodeBlock (r:Asn1AcnAst.AstRoot) (child:Asn1Type)  (c:Asn1AcnAst.SequenceOfConstraint) st =
//    foldSequenceOfTypeConstraint2 
//        (fun (e1:SizeableSet<'v>) e2 b s -> e1.union e2, s) 
//        (fun e1 e2 s -> e1.intersect e2, s) 
//        (fun e s -> e.complement, s) 
//        (fun e1 e2 s -> e1.difference e2, s) 
//        (fun e s -> e,s) 
//        (fun e1 e2 s -> e1.union e2, s) 
//        (fun v  s         -> SizeableSet<'v>.createFromSingleValue v ,s)
//        (fun intCon s       -> 
//            let sizeRange, ns = foldSizeRangeTypeConstraint r intCon s
//            SizeableSet<'v>.createFromSizeRange sizeRange, ns)
//        (fun c loc s -> 
//             let fnc, ns = anyConstraint2GenericSet r loc child c s
//
//             0, s) 
//        c
//        st


















let getFuncName2 (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (typeDefinition:TypeDefintionOrReference) =
    getFuncNameGeneric typeDefinition (lm.init.methodNameSuffix())


let createInitFunctionCommon (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (o:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) (defaultInitValue: String) initByAsn1Value (initTasFunction:CallerScope  -> InitFunctionResult) automaticTestCases (initExpression:string) (initExpressionGlobal:string) (nonEmbeddedChildrenFuncs:InitFunction list) (user_aux_functions:(string*string) list) =
    let funcName            = getFuncName2 r lm typeDefinition
    let globalName = getFuncNameGeneric typeDefinition "_constant"
    let p = lm.lg.getParamType o CommonTypes.Codec.Decode 
    let initTypeAssignment      = lm.init.initTypeAssignment
    let initTypeAssignment_def  = lm.init.initTypeAssignment_def
    let varName = p.arg.p
    let sPtrPrefix = lm.lg.getPtrPrefix p.arg
    let sPtrSuffix = lm.lg.getPtrSuffix p.arg
    let sStar = lm.lg.getStar p.arg
    let initDef = lm.init.initTypeConstant_def
    let initBody = lm.init.initTypeConstant_body
    let tdName = lm.lg.getLongTypedefName typeDefinition
    let initProcedure  = 
            match funcName  with
            | None              -> None
            | Some funcName     -> 
                match r.args.generateConstInitGlobals  &&  globalName.IsSome with
                | true ->
                    let funcBody = 
                        match o.isStringType with
                        | false -> lm.init.assignAny (lm.lg.getValue p.arg) globalName.Value
                        | true  -> lm.init.assignString p.arg.p  globalName.Value
                        //sprintf ("%s %s (%s)%s;")  (lm.lg.getValue p.arg) lm.lg.AssignOperator tdName globalName.Value
                    let func = initTypeAssignment varName sPtrPrefix sPtrSuffix funcName tdName funcBody [] defaultInitValue
                    let funcDef = initTypeAssignment_def varName sStar funcName  (lm.lg.getLongTypedefName typeDefinition)
                    Some {InitProcedure0.funcName = funcName; def = funcDef; body=func}
                | false ->
                    let res = initTasFunction p 
                    let lvars = res.localVariables |> List.map(fun (lv:LocalVariable) -> lm.lg.getLocalVariableDeclaration lv) |> List.distinct
                    let func = initTypeAssignment varName sPtrPrefix sPtrSuffix funcName tdName res.funcBody lvars defaultInitValue
                    let funcDef = initTypeAssignment_def varName sStar funcName (lm.lg.getLongTypedefName typeDefinition) 
                    Some {InitProcedure0.funcName = funcName; def = funcDef; body=func}

    {
        initExpression          = initExpression
        initExpressionGlobal    = initExpressionGlobal
        initProcedure           = initProcedure
        initFunction  = 
                funcName |> Option.map(fun n -> {InitProcedure0.funcName = n; def = initDef tdName n initExpression; body=initBody tdName n initExpression})
        initGlobal = 
                globalName |> Option.map(fun n -> {|globalName = n; def = initDef tdName n initExpressionGlobal; body=initBody tdName n initExpressionGlobal|})
        initTas                 = initTasFunction
        initByAsn1Value         = initByAsn1Value
        automaticTestCases      = automaticTestCases
        user_aux_functions      = user_aux_functions
        nonEmbeddedChildrenFuncs = nonEmbeddedChildrenFuncs
    }

let createIntegerInitFunc (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference)  =
    let initInteger = lm.init.initInteger
    
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | IntegerValue iv   -> iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initInteger (lm.lg.getValue p.arg) vl

    let integerVals = EncodeDecodeTestCase.IntegerAutomaticTestCaseValues r t o
    
    let allCons = DastValidate2.getIntSimplifiedConstraints r o.isUnsigned o.AllCons
    let isZeroAllowed = isValidValueRanged allCons 0I  
    let tasInitFunc (p:CallerScope)  = 
        match isZeroAllowed  with
        | false    -> 
            match integerVals with 
            |x::_ -> {InitFunctionResult.funcBody = initInteger (lm.lg.getValue p.arg) x;  localVariables=[]} 
            | [] -> {InitFunctionResult.funcBody = initInteger (lm.lg.getValue p.arg) 0I;  localVariables=[]}
        | true  -> {InitFunctionResult.funcBody = initInteger (lm.lg.getValue p.arg) 0I;  localVariables=[]}
    let constantInitExpression =
        match isZeroAllowed  with
        | false    -> 
            match integerVals with 
            |x::_ -> lm.lg.intValueToString x (o.intClass)
            | [] -> lm.lg.intValueToString 0I (o.intClass)
        | true  -> lm.lg.intValueToString 0I (o.intClass)
        

    let testCaseFuncs = 
        integerVals |> 
        List.map (fun vl -> 
            let initTestCaseFunc =
                (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initInteger (lm.lg.getValue p.arg) vl;  localVariables=[]} )
            {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] }        )

    createInitFunctionCommon r lm t typeDefinition o.defaultInitVal funcBody tasInitFunc testCaseFuncs constantInitExpression constantInitExpression [] []

let createRealInitFunc (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference)  = 
    let initReal = lm.init.initReal
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | RealValue iv   -> iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initReal (lm.lg.getValue p.arg) vl

    let realVals = EncodeDecodeTestCase.RealAutomaticTestCaseValues r t o
    let testCaseFuncs = 
        realVals |> 
        List.map (fun vl -> 
            let initTestCaseFunc = (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initReal (lm.lg.getValue p.arg) vl; localVariables=[]}) 
            {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] } )
    let isZeroAllowed = isValidValueRanged o.AllCons 0.0
    let tasInitFunc (p:CallerScope)  = 
        match isZeroAllowed with
        | false    -> 
            match realVals with 
            | x::_ -> {InitFunctionResult.funcBody = initReal (lm.lg.getValue p.arg) x;  localVariables=[]} 
            | [] -> {InitFunctionResult.funcBody = initReal (lm.lg.getValue p.arg) 0.0;  localVariables=[]}
        | true  -> {InitFunctionResult.funcBody = initReal (lm.lg.getValue p.arg) 0.0;  localVariables=[]}

    let constantInitExpression =
        match isZeroAllowed  with
        | false    -> 
            match realVals with 
            |x::_ -> lm.lg.doubleValueToString x
            | [] -> lm.lg.doubleValueToString 0.0
        | true  -> lm.lg.doubleValueToString 0.0

    createInitFunctionCommon r lm t typeDefinition o.defaultInitVal funcBody tasInitFunc testCaseFuncs constantInitExpression constantInitExpression [] []

let fragmentationCases seqOfCase maxSize =
    [
        seqOfCase (65535I + 1I)
        seqOfCase (min (65535I + 150I) maxSize)
        seqOfCase (49152I + 1I)
        seqOfCase (49152I + 150I)
        seqOfCase (32768I + 1I)
        seqOfCase (32768I + 150I)
        seqOfCase (16384I + 1I)
        seqOfCase (16384I + 150I)
    ]
let createIA5StringInitFunc (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.StringType   ) (typeDefinition:TypeDefintionOrReference)  = 
    let initIA5String           = lm.init.initIA5String
    let initTestCaseIA5String   = lm.init.initTestCaseIA5String
    let strTypeDef = lm.lg.getStrTypeDefinition o.typeDef

    
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | StringValue iv   -> 
                iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        let tlLit = DAstVariables.converStringValue2TargetLangStringLiteral lm (int o.maxSize.uper) vl
        initIA5String (lm.lg.getValue p.arg) tlLit

    let ii = t.id.SeqeuenceOfLevel + 1
    let i = sprintf "i%d" ii
    //let visibleChars = o.uperCharSet |> Seq.filter(fun c -> not (System.Char.IsControl c))
    let bAlpha = o.uperCharSet.Length < 128
    let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
    let testCaseFuncs = 
        let seqOfCase (nSize:BigInteger)  = 
            let initTestCaseFunc (p:CallerScope) = 
                let td = strTypeDef.longTypedefName2 (lm.lg.hasModules) (ToC p.modName)
                let funcBody = initTestCaseIA5String p.arg.p (lm.lg.getAccess p.arg) (nSize) ((o.maxSize.uper+1I)) i td bAlpha arrAsciiCodes (BigInteger arrAsciiCodes.Length) false
                {InitFunctionResult.funcBody = funcBody; localVariables=[SequenceOfIndex (ii, None)]}
            {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvSizeableTypeValue nSize)] }
        seq {
            match o.minSize.uper = o.maxSize.uper with
            | true  -> yield seqOfCase o.minSize.uper 
            | false -> 
                yield seqOfCase o.minSize.uper 
                yield seqOfCase o.maxSize.uper
                match o.maxSize.uper > 65536I with  //fragmentation cases
                | true ->
                      yield! fragmentationCases seqOfCase o.maxSize.uper
                | false -> ()
        } |> Seq.toList
    let zero (p:CallerScope) = 
        let td = strTypeDef.longTypedefName2 (lm.lg.hasModules) (ToC p.modName)
        let funcBody = initTestCaseIA5String p.arg.p (lm.lg.getAccess p.arg) ( (o.maxSize.uper+1I)) ( (o.maxSize.uper+1I)) i td bAlpha arrAsciiCodes (BigInteger arrAsciiCodes.Length) true
        let lvars = lm.lg.init.zeroIA5String_localVars ii
        {InitFunctionResult.funcBody = funcBody; localVariables=lvars}
    let constantInitExpression = lm.lg.initializeString (int o.maxSize.uper)
    createInitFunctionCommon r lm t typeDefinition o.defaultInitVal funcBody zero testCaseFuncs constantInitExpression constantInitExpression [] []

let createOctetStringInitFunc (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.OctetString ) (typeDefinition:TypeDefintionOrReference) (isValidFunction:IsValidFunction option) = 
    let initFixSizeBitOrOctString_bytei = lm.init.initFixSizeBitOrOctString_bytei
    let initFixSizeBitOrOctString       = lm.init.initFixSizeBitOrOctString
    let initFixVarSizeBitOrOctString    = lm.init.initFixVarSizeBitOrOctString
    let initTestCaseOctetString         = lm.init.initTestCaseOctetString

    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let bytes = 
            match v.ActualValue with
            | OctetStringValue iv -> iv
            | BitStringValue iv   -> bitStringValueToByteArray (StringLoc.ByValue iv) |> Seq.toList
            | _                 -> raise(BugErrorException "UnexpectedValue")
        let arrsBytes = bytes |> List.mapi(fun i b -> initFixSizeBitOrOctString_bytei p.arg.p (lm.lg.getAccess p.arg) ((i + lm.lg.ArrayStartIndex).ToString()) (sprintf "%x" b))
        match o.isFixedSize with
        | true  -> initFixSizeBitOrOctString p.arg.p (lm.lg.getAccess p.arg) arrsBytes
        | false -> initFixVarSizeBitOrOctString p.arg.p (lm.lg.getAccess p.arg) (BigInteger arrsBytes.Length) arrsBytes

    let constantInitExpression =
        match o.isFixedSize with
        | true   -> lm.init.initFixSizeOctetString o.maxSize.uper
        | false  -> lm.init.initVarSizeOctetString o.minSize.uper o.maxSize.uper

    let anonyms =
        o.AllCons |> 
        List.map DastFold.getValueFromSizeableConstraint |> 
        List.collect id |>
        List.map(fun (v,_) -> DAstVariables.printOctetStringValueAsCompoundLitteral lm "" o (v|>List.map(fun bl -> bl.Value)))

    let tdName = lm.lg.getLongTypedefName typeDefinition

    let testCaseFuncs, tasInitFunc =
        match anonyms with
        | []  ->
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = sprintf "i%d" ii
            let seqOfCase (nSize:BigInteger) = 
                let initTestCaseFunc (p:CallerScope) = 
                    let funcBody = initTestCaseOctetString p.arg.p (lm.lg.getAccess p.arg) tdName nSize i (o.minSize.uper = o.maxSize.uper) false o.minSize.uper
                    {InitFunctionResult.funcBody = funcBody; localVariables=[SequenceOfIndex (ii, None)]}
                {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvSizeableTypeValue nSize)] }
            let testCaseFuncs = 
                seq {
                    match o.minSize.acn = o.maxSize.acn with
                    | true  -> yield seqOfCase o.minSize.acn
                    | false -> 
                        yield seqOfCase o.minSize.acn 
                        yield seqOfCase o.maxSize.acn 
                        match o.maxSize.acn > 65536I with  //fragmentation cases
                        | true ->
                              yield! fragmentationCases seqOfCase o.maxSize.acn
                        | false -> ()
                } |> Seq.toList
            let zero (p:CallerScope) = 
                let isFixedSize =
                    match t.getBaseType r with
                    | None      -> o.isFixedSize
                    | Some bs  -> 
                        match bs.Kind with
                        | Asn1AcnAst.OctetString bo -> bo.isFixedSize
                        | _                        -> raise(BugErrorException "UnexpectedType")
                let funcBody = initTestCaseOctetString p.arg.p (lm.lg.getAccess p.arg) tdName o.maxSize.uper i (isFixedSize) true o.minSize.uper
                let lvars = lm.lg.init.zeroIA5String_localVars ii
                {InitFunctionResult.funcBody = funcBody; localVariables=lvars}

            testCaseFuncs, zero
        | _ ->
            let ret = 
                anonyms |> 
                List.map(fun (compLit) ->
                    let initTestCaseFunc (p:CallerScope) =
                        let ret = sprintf "%s%s%s;" (lm.lg.getValue p.arg) lm.lg.AssignOperator compLit
                        {InitFunctionResult.funcBody = ret; localVariables=[]}
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] })
            ret, ret.Head.initTestCaseFunc
    
    let maxArrLength = 
        match t.Kind with
        | Asn1AcnAst.OctetString oS -> oS.maxSize.ToString()
        | _ -> "0"

    createInitFunctionCommon r lm t typeDefinition "null" funcBody tasInitFunc testCaseFuncs constantInitExpression constantInitExpression [] []

let createNullTypeInitFunc (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.NullType) (typeDefinition:TypeDefintionOrReference)  = 
    let initNull = lm.init.initNull
    let funcBody (p:CallerScope) v = initNull (lm.lg.getValue p.arg) 
    let constantInitExpression = "0"
    let testCaseFuncs = [{AutomaticTestCase.initTestCaseFunc = (fun p -> {InitFunctionResult.funcBody = initNull (lm.lg.getValue p.arg); localVariables=[]}); testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)]} ]
    createInitFunctionCommon r lm t typeDefinition o.defaultInitVal funcBody testCaseFuncs.Head.initTestCaseFunc testCaseFuncs constantInitExpression constantInitExpression [] []

let createBitStringInitFunc (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.BitString   ) (typeDefinition:TypeDefintionOrReference) (isValidFunction:IsValidFunction option)= 
    let initFixSizeBitOrOctString_bytei = lm.init.initFixSizeBitOrOctString_bytei
    let initFixSizeBitOrOctString       = lm.init.initFixSizeBitOrOctString
    let initFixVarSizeBitOrOctString    = lm.init.initFixVarSizeBitOrOctString
    let initTestCaseBitString           = lm.init.initTestCaseBitString
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let bytes = 
            match v.ActualValue with
            | BitStringValue iv     -> bitStringValueToByteArray (StringLoc.ByValue iv) |> Seq.toList
            | OctetStringValue iv   -> iv
            | _                     -> raise(BugErrorException "UnexpectedValue")
        let arrsBytes = bytes |> List.mapi(fun i b -> initFixSizeBitOrOctString_bytei p.arg.p (lm.lg.getAccess p.arg) ((i + lm.lg.ArrayStartIndex).ToString()) (sprintf "%x" b))
        match o.minSize.uper = o.maxSize.uper with
        | true  -> initFixSizeBitOrOctString p.arg.p (lm.lg.getAccess p.arg) arrsBytes
        | false -> initFixVarSizeBitOrOctString p.arg.p (lm.lg.getAccess p.arg) (BigInteger arrsBytes.Length) arrsBytes

    let anonyms =
        o.AllCons |> 
        List.map DastFold.getValueFromSizeableConstraint |> 
        List.collect id |>
        List.map(fun (v,_) -> DAstVariables.printBitStringValueAsCompoundLitteral lm "" o v.Value)

    let tdName = lm.lg.getLongTypedefName typeDefinition

    let testCaseFuncs, tasInitFunc =
        match anonyms with
        | []  ->
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = sprintf "i%d" ii
            let seqOfCase (nSize:BigInteger) = 
                let initTestCaseFunc (p:CallerScope) = 
                    let nSizeCeiled =  if nSize % 8I = 0I then nSize else (nSize + (8I - nSize % 8I)) 
                    let funcBody = initTestCaseBitString p.arg.p (lm.lg.getAccess p.arg) tdName nSize (nSizeCeiled) i (o.minSize.uper = o.maxSize.uper) false o.minSize.uper
                    {InitFunctionResult.funcBody = funcBody; localVariables=[SequenceOfIndex (ii, None)]}
                {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvSizeableTypeValue nSize)] }

            let testCaseFuncs = 
                seq {
                    match o.minSize.acn = o.maxSize.acn with
                    | true  -> yield seqOfCase o.minSize.acn 
                    | false -> 
                        yield seqOfCase o.minSize.acn 
                        yield seqOfCase o.maxSize.acn 
                        match o.maxSize.acn > 65536I with  //fragmentation cases
                        | true ->
                              yield! fragmentationCases seqOfCase o.maxSize.acn
                        | false -> ()
                } |> Seq.toList
            let zero (p:CallerScope) = 
                let nSize = o.maxSize.uper
                let nSizeCeiled =  if nSize % 8I = 0I then nSize else (nSize + (8I - nSize % 8I)) 
                let isFixedSize =
                    match t.getBaseType r with
                    | None      -> o.isFixedSize
                    | Some bs  -> 
                        match bs.Kind with
                        | Asn1AcnAst.BitString bo -> bo.isFixedSize
                        | _                        -> raise(BugErrorException "UnexpectedType")

                let funcBody = initTestCaseBitString p.arg.p (lm.lg.getAccess p.arg) tdName nSize (nSizeCeiled) i (isFixedSize) true o.minSize.uper
                let lvars = lm.lg.init.zeroIA5String_localVars ii // match l with C -> [] | Ada -> [SequenceOfIndex (ii, None)]
                {InitFunctionResult.funcBody = funcBody; localVariables=lvars}
            testCaseFuncs, zero
        | _ ->
            let ret = 
                anonyms |> 
                List.map(fun compLit ->
                    let retFunc (p:CallerScope) =
                        let ret = sprintf "%s%s%s;" (lm.lg.getValue p.arg) lm.lg.AssignOperator compLit
                        {InitFunctionResult.funcBody = ret; localVariables=[]}
                    {AutomaticTestCase.initTestCaseFunc = retFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] })
            ret, ret.Head.initTestCaseFunc

    let user_aux_functions = 
        let funcName            = getFuncNameGeneric typeDefinition ""
        let p = lm.lg.getParamType t CommonTypes.Codec.Decode 
        let varName = p.arg.p
        let sStar = lm.lg.getStar p.arg
        let typeDefName = (lm.lg.getLongTypedefName typeDefinition)
        o.namedBitList |> 
        List.choose(fun z -> 
            match funcName with
            | Some funcName ->
                let nZeroBasedByteIndex = z.resolvedValue / 8I;
                let bitMask = 1 <<< (7 - ((int z.resolvedValue) % 8))
                let sf_body = lm.init.initBitStringAtPos varName sStar funcName typeDefName (ToC z.Name.Value) nZeroBasedByteIndex (bitMask.ToString("X")) z.resolvedValue
                let sf_if = lm.init.initBitStringAtPos_def varName sStar funcName typeDefName (ToC z.Name.Value)
                Some (sf_if, sf_body)
            | None -> None
        )
        
    let constantInitExpression =
        match o.isFixedSize with
        | true   -> lm.init.initFixSizeBitString o.maxSize.uper (BigInteger o.MaxOctets)
        | false  -> lm.init.initVarSizeBitString o.minSize.uper o.maxSize.uper (BigInteger o.MaxOctets)
    createInitFunctionCommon r lm t typeDefinition o.defaultInitVal funcBody tasInitFunc testCaseFuncs constantInitExpression constantInitExpression [] user_aux_functions

let createBooleanInitFunc (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Boolean     ) (typeDefinition:TypeDefintionOrReference)  = 
    let initBoolean = lm.init.initBoolean
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | BooleanValue iv   -> iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initBoolean (lm.lg.getValue p.arg) vl



    let testCaseFuncs = 
        EncodeDecodeTestCase.BooleanAutomaticTestCaseValues r t o |> 
        List.map (fun vl -> {AutomaticTestCase.initTestCaseFunc = (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initBoolean (lm.lg.getValue p.arg) vl; localVariables = []}); testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] })

    let tasInitFunc (p:CallerScope)  = 
        match isValidValueGeneric o.AllCons (=) false  with
        | true    -> {InitFunctionResult.funcBody = initBoolean (lm.lg.getValue p.arg) false; localVariables = []}
        | false     -> {InitFunctionResult.funcBody = initBoolean (lm.lg.getValue p.arg) true; localVariables = []}

    let constantInitExpression = lm.lg.FalseLiteral
    createInitFunctionCommon r lm t typeDefinition o.defaultInitVal funcBody tasInitFunc testCaseFuncs constantInitExpression constantInitExpression [] []







let createObjectIdentifierInitFunc (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.ObjectIdentifier     ) (typeDefinition:TypeDefintionOrReference) iv = 
    let initObjectIdentifier_vali   = lm.init.initObjectIdentifier_vali
    let initObjectIdentifier        = lm.init.initObjectIdentifier
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let bytes = 
            match v.ActualValue with
            | ObjOrRelObjIdValue iv   -> iv.Values |> List.map fst
            | _                 -> raise(BugErrorException "UnexpectedValue")
        let arrsBytes = bytes |> List.mapi(fun i b -> initObjectIdentifier_vali p.arg.p (lm.lg.getAccess p.arg) ((i+lm.lg.ArrayStartIndex).ToString()) b)
        initObjectIdentifier p.arg.p (lm.lg.getAccess p.arg) (BigInteger arrsBytes.Length) arrsBytes
    let testCaseFuncs = 
        EncodeDecodeTestCase.ObjectIdentifierAutomaticTestCaseValues r t o |> 
        List.map (fun vl -> 
            {AutomaticTestCase.initTestCaseFunc = (fun (p:CallerScope) -> 
                let arrsBytes = vl |> List.mapi(fun i b -> initObjectIdentifier_vali p.arg.p (lm.lg.getAccess p.arg) ((i+lm.lg.ArrayStartIndex).ToString()) b)
                {InitFunctionResult.funcBody = initObjectIdentifier (p.arg.p) (lm.lg.getAccess p.arg) (BigInteger vl.Length)  arrsBytes; localVariables = []}); testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] })

    let tasInitFunc (p:CallerScope)  = 
        {InitFunctionResult.funcBody = initObjectIdentifier (p.arg.p) (lm.lg.getAccess p.arg) 0I []; localVariables = []}

    let constantInitExpression = lm.init.initObjectIdentifierAsExpr ()
    createInitFunctionCommon r lm t typeDefinition "createObjectIdentifierInitFunc" funcBody tasInitFunc testCaseFuncs constantInitExpression constantInitExpression [] []

let createTimeTypeInitFunc (r:Asn1AcnAst.AstRoot)  (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.TimeType     ) (typeDefinition:TypeDefintionOrReference) iv = 
    let init_Asn1LocalTime                  = lm.init.init_Asn1LocalTime
    let init_Asn1UtcTime                    = lm.init.init_Asn1UtcTime
    let init_Asn1LocalTimeWithTimeZone      = lm.init.init_Asn1LocalTimeWithTimeZone
    let init_Asn1Date                       = lm.init.init_Asn1Date
    let init_Asn1Date_LocalTime             = lm.init.init_Asn1Date_LocalTime
    let init_Asn1Date_UtcTime               = lm.init.init_Asn1Date_UtcTime
    let init_Asn1Date_LocalTimeWithTimeZone = lm.init.init_Asn1Date_LocalTimeWithTimeZone

    let initByBalue (p:CallerScope) (iv:Asn1DateTimeValue) = 
        match iv with
        |Asn1LocalTimeValue                  tv        -> init_Asn1LocalTime p.arg.p (lm.lg.getAccess p.arg) tv
        |Asn1UtcTimeValue                    tv        -> init_Asn1UtcTime p.arg.p (lm.lg.getAccess p.arg) tv
        |Asn1LocalTimeWithTimeZoneValue      (tv,tz)   -> init_Asn1LocalTimeWithTimeZone p.arg.p (lm.lg.getAccess p.arg) tv tz
        |Asn1DateValue                       dt        -> init_Asn1Date p.arg.p (lm.lg.getAccess p.arg) dt
        |Asn1Date_LocalTimeValue             (dt,tv)   -> init_Asn1Date_LocalTime p.arg.p (lm.lg.getAccess p.arg) dt tv
        |Asn1Date_UtcTimeValue               (dt,tv)   -> init_Asn1Date_UtcTime  p.arg.p (lm.lg.getAccess p.arg) dt tv
        |Asn1Date_LocalTimeWithTimeZoneValue (dt,tv,tz)-> init_Asn1Date_LocalTimeWithTimeZone p.arg.p (lm.lg.getAccess p.arg) dt tv tz

    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        match v.ActualValue with
        | TimeValue iv   -> initByBalue p iv
        | _              -> raise(BugErrorException "UnexpectedValue")

    let atvs = EncodeDecodeTestCase.TimeTypeAutomaticTestCaseValues r t o
    let testCaseFuncs = 
        atvs |> 
        List.map (fun vl -> 
            {AutomaticTestCase.initTestCaseFunc = (fun (p:CallerScope) -> 
                {InitFunctionResult.funcBody = initByBalue p vl; localVariables = []}); testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] })

    let tasInitFunc (p:CallerScope)  = 
        {InitFunctionResult.funcBody = initByBalue p atvs.Head; localVariables = []}
    let constantInitExpression = 
        match o.timeClass with          
        |Asn1LocalTime                  _-> lm.init.init_Asn1LocalTimeExpr ()
        |Asn1UtcTime                    _-> lm.init.init_Asn1UtcTimeExpr ()
        |Asn1LocalTimeWithTimeZone      _-> lm.init.init_Asn1LocalTimeWithTimeZoneExpr ()
        |Asn1Date                       _-> lm.init.init_Asn1DateExpr ()
        |Asn1Date_LocalTime             _-> lm.init.init_Asn1Date_LocalTimeExpr ()
        |Asn1Date_UtcTime               _-> lm.init.init_Asn1Date_UtcTimeExpr ()
        |Asn1Date_LocalTimeWithTimeZone _-> lm.init.init_Asn1Date_LocalTimeWithTimeZoneExpr ()

    createInitFunctionCommon r lm t typeDefinition "createTimeTypeInitFunc" funcBody tasInitFunc testCaseFuncs constantInitExpression constantInitExpression [] []

let mergeMaps (m1:Map<'key,'value>) (m2:Map<'key,'value>) =
    Map.fold (fun (nm:Map<'key,'value>) key value -> match nm.ContainsKey key with false -> nm.Add(key,value) | true -> nm) m1 m2
    

let createEnumeratedInitFunc (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Enumerated  )  (typeDefinition:TypeDefintionOrReference) iv = 
    let initEnumerated = lm.init.initEnumerated
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | EnumValue iv      -> o.items |> Seq.find(fun x -> x.Name.Value = iv)
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initEnumerated (lm.lg.getValue p.arg) (lm.lg.getNamedItemBackendName (Some typeDefinition) vl)
    let testCaseFuncs = 
        EncodeDecodeTestCase.EnumeratedAutomaticTestCaseValues2 r t o |> 
        List.map (fun vl -> 
            {
                AutomaticTestCase.initTestCaseFunc = (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initEnumerated (lm.lg.getValue p.arg) (lm.lg.getNamedItemBackendName (Some typeDefinition) vl); localVariables=[]}); 
                testCaseTypeIDsMap = Map.ofList [(t.id, (TcvEnumeratedValue vl.Name.Value))] 
            })
    let constantInitExpression = lm.lg.getNamedItemBackendName  (Some typeDefinition) o.items.Head
    createInitFunctionCommon r lm t typeDefinition o.defaultInitVal funcBody testCaseFuncs.Head.initTestCaseFunc testCaseFuncs constantInitExpression constantInitExpression [] []

let getChildExpression (lm:LanguageMacros) (childType:Asn1Type) =
    match childType.initFunction.initFunction with
    | None -> childType.initFunction.initExpression
    | Some cn ->  
        match childType.isComplexType with
        | false -> childType.initFunction.initExpression
        | true -> sprintf "%s" cn.funcName

let getChildExpressionGlobal (lm:LanguageMacros) (childType:Asn1Type) =
    match childType.initFunction.initGlobal with
    | None -> childType.initFunction.initExpressionGlobal
    | Some cn ->  
        match childType.isComplexType with
        | false -> childType.initFunction.initExpressionGlobal
        | true -> sprintf "%s" cn.globalName

let createSequenceOfInitFunc (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.SequenceOf  ) (typeDefinition:TypeDefintionOrReference) (childType:Asn1Type)  = 
    let initFixedSequenceOf                     = lm.init.initFixedSequenceOf
    let initVarSizeSequenceOf                   = lm.init.initVarSizeSequenceOf
    let initTestCaseSizeSequenceOf_innerItem    = lm.init.initTestCaseSizeSequenceOf_innerItem
    let initTestCaseSizeSequenceOf              = lm.init.initTestCaseSizeSequenceOf
    let initChildWithInitFunc                   = lm.init.initChildWithInitFunc
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | SeqOfValue childVals ->
                childVals |> 
                List.mapi(fun i chv -> 
                    let new_arg = lm.lg.getArrayItem p.arg ((i+lm.lg.ArrayStartIndex).ToString()) childType.isIA5String
                    let ret = childType.initFunction.initByAsn1Value ({p with arg = new_arg}) chv.kind
                    match lm.lg.supportsStaticVerification with
                    | false     -> ret
                    | true   when i>0 -> ret
                    | true   -> 
                        // in the first array we have to emit a pragma Annotate false_positive, otherwise gnatprove emit an error
                        let pragma = lm.init.initSequence_pragma p.arg.p
                        ret + pragma
                    )
            | _               -> raise(BugErrorException "UnexpectedValue")
        match o.isFixedSize with
        | true  -> initFixedSequenceOf vl
        | false -> initVarSizeSequenceOf p.arg.p (lm.lg.getAccess p.arg) (BigInteger vl.Length) vl

    let ii = t.id.SeqeuenceOfLevel + 1
    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let testCaseFuncs =
        let seqOfCase (childTestCases : AutomaticTestCase list) (nSize:BigInteger)  = 
                //let len = childType.initFunction.automaticTestCases.Length
                //let childTestCases = childType.initFunction.automaticTestCases |> Seq.take (min (int nSize) len) |> Seq.toList //|>
                    //List.map(fun fnc -> fnc.initTestCaseFunc ({p with arg = p.arg.getArrayItem l i childType.isIA5String}))
                match childTestCases with
                | []    -> 
                    let initTestCaseFunc (p:CallerScope) = 
                        {InitFunctionResult.funcBody = ""; localVariables = []}
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvSizeableTypeValue nSize)] }
                | atc::[] -> 
                    let initTestCaseFunc (p:CallerScope) = 
                        let childCase = atc.initTestCaseFunc ({p with arg = lm.lg.getArrayItem p.arg i childType.isIA5String})
                        let funcBody = initTestCaseSizeSequenceOf p.arg.p (lm.lg.getAccess p.arg) None nSize (o.minSize.uper = o.maxSize.uper) [childCase.funcBody] false i
                        {InitFunctionResult.funcBody = funcBody; localVariables= (SequenceOfIndex (ii, None))::childCase.localVariables }
                    let combinedTestCase = atc.testCaseTypeIDsMap.Add(t.id, TcvSizeableTypeValue nSize)
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = combinedTestCase }
                | _             ->
                    let initTestCaseFunc (p:CallerScope) = 
                        let arrsInnerItems, childLocalVars = 
                            childTestCases |> 
                            List.mapi(fun idx atc -> 
                                let sChildItem = atc.initTestCaseFunc ({p with arg = lm.lg.getArrayItem p.arg  i childType.isIA5String})
                                let funcBody = initTestCaseSizeSequenceOf_innerItem (idx=0) (idx = childTestCases.Length-1) idx.AsBigInt sChildItem.funcBody i (BigInteger childTestCases.Length)
                                (funcBody, (SequenceOfIndex (ii, None))::sChildItem.localVariables)) |>
                            List.unzip
                        let funcBody = initTestCaseSizeSequenceOf p.arg.p (lm.lg.getAccess p.arg) None  nSize (o.minSize.uper = o.maxSize.uper) arrsInnerItems true i 
                        {InitFunctionResult.funcBody = funcBody; localVariables= (SequenceOfIndex (ii, None))::(childLocalVars |> List.collect id)}
                    let combinedTestCase =
                        let thisCase = Map.ofList [(t.id, TcvSizeableTypeValue nSize)]
                        childTestCases |> List.fold(fun (newMap:Map<ReferenceToType, TestCaseValue>) atc -> mergeMaps newMap atc.testCaseTypeIDsMap) thisCase
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = combinedTestCase }
        match r.args.generateAutomaticTestCases with
        | true  ->    
            let seqOfCase_aux (nSize:BigInteger) =
                match nSize > 0I with
                | true ->
                    let totalChildAtcs = childType.initFunction.automaticTestCases.Length
                    let childTestCases = childType.initFunction.automaticTestCases 
                    let test_case_bundles = int (totalChildAtcs.AsBigInt / nSize)
                    let last_test_case_bundle_size = int (totalChildAtcs.AsBigInt % nSize)
                    seq {
                        for i in [1..test_case_bundles] do
                            let bund_cases = childTestCases |> Seq.skip ((i-1) * (int nSize)) |> Seq.take (int nSize) |> Seq.toList
                            yield seqOfCase bund_cases nSize
                        
                        if (last_test_case_bundle_size > 0) then
                            let last_bund_cases = childTestCases |> Seq.skip (test_case_bundles * (int nSize)) |> Seq.take (last_test_case_bundle_size) |> Seq.toList
                            yield seqOfCase last_bund_cases nSize

                    } |> Seq.toList
                | false -> []
        
            seq {
                match o.minSize.acn = o.maxSize.acn with
                | true  -> yield! seqOfCase_aux o.minSize.acn
                | false -> 
                    yield! seqOfCase_aux o.maxSize.acn 
                    yield! seqOfCase_aux o.minSize.acn 
                    match o.maxSize.acn > 65536I with  //fragmentation cases
                    | true ->
                            yield! (fragmentationCases seqOfCase_aux o.maxSize.acn |> List.collect id)
                    | false -> ()
            } |> Seq.toList
        | fase  -> []

    let initTasFunction, nonEmbeddedChildrenFuncs =
        let initTasFunction (p:CallerScope) =
            let initCountValue = Some o.minSize.uper
            let chp = {p with arg = lm.lg.getArrayItem p.arg i childType.isIA5String}
            let childInitRes_funcBody, childInitRes_localVariables = 
                match childType.initFunction.initProcedure with
                | None  ->
                    let childInitRes = childType.initFunction.initTas chp
                    childInitRes.funcBody, childInitRes.localVariables
                | Some initProc  ->
                    initChildWithInitFunc (lm.lg.getPointer chp.arg) initProc.funcName, []
            let isFixedSize =
                match t.getBaseType r with
                | None      -> o.isFixedSize
                | Some bs  -> 
                    match bs.Kind with
                    | Asn1AcnAst.SequenceOf bo -> bo.isFixedSize
                    | _                        -> raise(BugErrorException "UnexpectedType")

            let funcBody = initTestCaseSizeSequenceOf p.arg.p (lm.lg.getAccess p.arg) initCountValue o.maxSize.uper (isFixedSize) [childInitRes_funcBody] false i
            {InitFunctionResult.funcBody = funcBody; localVariables= (SequenceOfIndex (ii, None))::childInitRes_localVariables }
        let nonEmbeddedChildrenFuncs =
            match childType.initFunction.initProcedure with
            | None  -> []
            | Some _ when r.args.generateConstInitGlobals  -> []
            | Some _  -> [childType.initFunction]
        
        initTasFunction, nonEmbeddedChildrenFuncs

    
    let childInitExpr = getChildExpression lm childType
    let childInitGlobal = getChildExpressionGlobal lm childType
    let constantInitExpression childExpr = 
        match o.isFixedSize with
        | true  -> lm.init.initFixSizeSequenceOfExpr o.maxSize.uper childExpr
        | false -> lm.init.initVarSizeSequenceOfExpr o.minSize.uper o.maxSize.uper childExpr
    createInitFunctionCommon r lm t typeDefinition "createSequenceOfInitFunc" funcBody initTasFunction testCaseFuncs (constantInitExpression childInitExpr) (constantInitExpression childInitGlobal) nonEmbeddedChildrenFuncs []

let extractDefaultInitValue (childType: Asn1TypeKind): String = 
        match childType with
        | Integer i -> i.baseInfo.defaultInitVal
        | Real r -> r.baseInfo.defaultInitVal
        | NullType n -> n.baseInfo.defaultInitVal
        | Boolean b -> "false"
        | _ -> "null"

let createSequenceInitFunc (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (children:SeqChildInfo list) = 
    let initSequence                        = lm.init.initSequence
    let initSequence_optionalChild          = lm.init.initSequence_optionalChild
    let initTestCase_sequence_child         = lm.init.initTestCase_sequence_child
    let initTestCase_sequence_child_opt     = lm.init.initTestCase_sequence_child_opt
    let initChildWithInitFunc               = lm.init.initChildWithInitFunc
    let initSequence_emptySeq               = lm.init.initSequence_emptySeq
    let initByAsn1ValueFnc (p:CallerScope) (v:Asn1ValueKind) = 

        let childrenRet = 
            match v.ActualValue with
            | SeqValue iv     -> 
                children |>
                List.choose(fun seqChild ->
                    match seqChild with
                    | Asn1Child seqChild   ->
                        match iv |> Seq.tryFind(fun chv -> chv.name = seqChild.Name.Value) with
                        | None  ->
                            match seqChild.Optionality with
                            | None      -> None
                            | Some _    -> Some (initSequence_optionalChild p.arg.p (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName seqChild) "0" "")
                        | Some chv  ->
                            let chContent = seqChild.Type.initFunction.initByAsn1Value ({p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName seqChild) seqChild.Type.isIA5String}) chv.Value.kind
                            match seqChild.Optionality with
                            | None      -> Some chContent
                            | Some _    -> Some (initSequence_optionalChild p.arg.p (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName seqChild) "1" chContent)
                    | AcnChild _     -> None)

            | _               -> raise(BugErrorException "UnexpectedValue")
        initSequence childrenRet

    let testCaseFuncs = 
        let asn1Children = 
            children |> 
            List.choose(fun c -> match c with Asn1Child x -> Some x | _ -> None) |> 
            List.filter(fun z -> match z.Type.Kind with NullType _ -> false | _ -> true) |>
            List.filter(fun z -> match z.Optionality with Some Asn1AcnAst.AlwaysAbsent -> false | _ -> true)
        

        let optChildCount = 
            children |> 
            List.filter(fun c -> 
                match c.Optionality with
                | Some (Asn1AcnAst.Optional opt) when opt.acnPresentWhen.IsSome -> true
                | _                                                             -> false
            ) |> Seq.length 

        let handleChild  (ch:Asn1Child)  = 
            let len = ch.Type.initFunction.automaticTestCases.Length
            ch.Type.initFunction.automaticTestCases |> 
            List.collect(fun atc -> 
                let presentFunc  = 
                    let initTestCaseFunc (p:CallerScope) = 
                        let chContent =  atc.initTestCaseFunc {p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String} 
                        let funcBody = initTestCase_sequence_child p.arg.p (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName ch) chContent.funcBody ch.Optionality.IsSome
                        {InitFunctionResult.funcBody = funcBody; localVariables = chContent.localVariables }
                    let combinedTestCase =
                        match atc.testCaseTypeIDsMap.ContainsKey ch.Type.id with
                        | true      -> atc.testCaseTypeIDsMap
                        | false     -> atc.testCaseTypeIDsMap.Add(ch.Type.id, TcvAnyValue)
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = combinedTestCase }
                let nonPresenceFunc =  
                    let initTestCaseFunc (p:CallerScope) = 
                        let funcBody = initTestCase_sequence_child_opt p.arg.p (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName ch)
                        {InitFunctionResult.funcBody = funcBody; localVariables = [] }
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.empty }
                match ch.Optionality with
                | None                              -> [presentFunc]
                | Some (Asn1AcnAst.Optional opt)    -> [presentFunc; nonPresenceFunc] 
                | Some (Asn1AcnAst.AlwaysAbsent)    -> [nonPresenceFunc] 
                | Some (Asn1AcnAst.AlwaysPresent)   -> [presentFunc] )

        let generateCases   (children : Asn1Child list) : AutomaticTestCase list=
            let childrenATCs =
                children |> 
                List.map(fun c -> 
                    let childAtcs = handleChild c |> Seq.toArray
                    (c, childAtcs, childAtcs.Length)) |>
                List.filter(fun (_,_,ln) -> ln > 0)
            match childrenATCs with
            | []    -> []
            | _     ->
                let (_,_,mxAtcs) = childrenATCs |> List.maxBy(fun (_,_,len) -> len) 
                let tesCases =
                    [0 .. mxAtcs - 1] |>
                    List.map(fun seqTestCaseIndex ->
                        let children_ith_testCase = 
                            childrenATCs  |> 
                            List.map(fun (c,childCases,ln) -> childCases.[seqTestCaseIndex % ln]) 
                        match children_ith_testCase with
                        | []        -> raise(BugErrorException "")
                        | c1::[]    -> c1
                        | c1::cs    ->
                            cs |> List.fold(fun (st:AutomaticTestCase) (cur:AutomaticTestCase) ->
                                    let combineFnc (p:CallerScope) = 
                                        let partA = st.initTestCaseFunc p
                                        let partB = cur.initTestCaseFunc p
                                        let funcBody = [partA.funcBody; partB.funcBody] |> Seq.StrJoin "\n"
                                        {InitFunctionResult.funcBody = funcBody; localVariables = partA.localVariables@partB.localVariables }
                                    let combinedTestCases = mergeMaps st.testCaseTypeIDsMap cur.testCaseTypeIDsMap
                                    {AutomaticTestCase.initTestCaseFunc = combineFnc; testCaseTypeIDsMap = combinedTestCases } ) c1 )

                tesCases

        match r.args.generateAutomaticTestCases with
        | true  ->  generateCases  asn1Children 
        | false ->  [] 
    let initTasFunction, nonEmbeddedChildrenFuncs = 
        let handleChild  (p:CallerScope) (ch:Asn1Child) : (InitFunctionResult*InitFunction option) = 
            let nonEmbeddedChildrenFunc = 
                match r.args.generateConstInitGlobals with
                | true -> None
                | false -> Some ch.Type.initFunction
            let presentFunc (defaultValue  : Asn1AcnAst.Asn1Value option) = 
                match defaultValue with
                | None  ->
                    match ch.Type.initFunction.initProcedure with
                    | None  ->
                        match ch.Type.typeDefintionOrReference with
                        | ReferenceToExistingDefinition    rf   when (not rf.definedInRtl) ->
                            let fncName = (ch.Type.typeDefintionOrReference.longTypedefName2 lm.lg.hasModules) + (lm.init.methodNameSuffix())
                            let chP = {p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String} 
                            let chContent =  initChildWithInitFunc (lm.lg.getPointer chP.arg) fncName
                            let funcBody = initTestCase_sequence_child p.arg.p (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName ch) chContent ch.Optionality.IsSome
                            {InitFunctionResult.funcBody = funcBody; localVariables = [] }, nonEmbeddedChildrenFunc
                        | _       ->
                            let fnc = ch.Type.initFunction.initTas
                            let chContent =  fnc {p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String} 
                            let funcBody = initTestCase_sequence_child p.arg.p (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName ch) chContent.funcBody ch.Optionality.IsSome
                            {InitFunctionResult.funcBody = funcBody; localVariables = chContent.localVariables }, None
                        
                    | Some initProc  ->
                        let chP = {p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String} 
                        let chContent =  initChildWithInitFunc (lm.lg.getPointer chP.arg) initProc.funcName
                        let funcBody = initTestCase_sequence_child p.arg.p (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName ch) chContent ch.Optionality.IsSome
                        {InitFunctionResult.funcBody = funcBody; localVariables = [] }, nonEmbeddedChildrenFunc
                | Some dv    ->
                    let fnc = ch.Type.initFunction.initByAsn1Value
                    let chContent =  fnc {p with arg = lm.lg.getSeqChild p.arg (lm.lg.getAsn1ChildBackendName ch) ch.Type.isIA5String} (mapValue dv).kind
                    let funcBody = initTestCase_sequence_child p.arg.p (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName ch) chContent ch.Optionality.IsSome
                    {InitFunctionResult.funcBody = funcBody; localVariables = [] }, nonEmbeddedChildrenFunc
                    
                    
            let nonPresenceFunc () =  
                let funcBody = initTestCase_sequence_child_opt p.arg.p (lm.lg.getAccess p.arg) (lm.lg.getAsn1ChildBackendName ch)
                {InitFunctionResult.funcBody = funcBody; localVariables = [] }, None
            match ch.Optionality with
            | None                              -> presentFunc None
            | Some (Asn1AcnAst.Optional opt)    -> presentFunc opt.defaultValue
            | Some (Asn1AcnAst.AlwaysAbsent)    -> nonPresenceFunc ()
            | Some (Asn1AcnAst.AlwaysPresent)   -> presentFunc None
        let asn1Children = children |> List.choose(fun c -> match c with Asn1Child x -> Some x | _ -> None)
        let initTasFunction (p:CallerScope) =
            match asn1Children with
            | []    -> 
                let initEmpytSeq = initSequence_emptySeq p.arg.p
                {InitFunctionResult.funcBody = initEmpytSeq; localVariables = []}
            | _     ->
                asn1Children |> 
                List.fold(fun (cr) ch ->
                        let chResult, _ = handleChild p ch
                        let newFuncBody = cr.funcBody + "\n" + chResult.funcBody
                        {InitFunctionResult.funcBody = newFuncBody; localVariables = cr.localVariables@chResult.localVariables}
                    ) {InitFunctionResult.funcBody = ""; localVariables = []}
        let dummyScope = {CallerScope.modName = ""; arg = VALUE "dummy"}
        let nonEmbeddedChildrenFuncs = asn1Children |> List.choose(fun ch -> handleChild dummyScope ch |> snd)
        initTasFunction, nonEmbeddedChildrenFuncs
    let constantInitExpression getChildExpr = 
        let nonEmptyChildren = 
            children |> 
            List.choose(fun c -> match c with Asn1Child x -> Some x | _ -> None) |>
            List.map (fun c -> 
                let childName = lm.lg.getAsn1ChildBackendName c
                let childExp = getChildExpr lm c.Type 
                lm.init.initSequenceChildExpr childName childExp) 
        let arrsOptionalChildren =
            children |> 
            List.choose(fun c -> match c with Asn1Child x -> Some x | _ -> None) |>
            List.choose (fun c -> 
                match c.Optionality with 
                | None -> None
                | Some (Asn1AcnAst.Optional opt)    -> Some (lm.init.initSequenceOptionalChildExpr (lm.lg.getAsn1ChildBackendName c) 1I)
                | Some (Asn1AcnAst.AlwaysAbsent)    -> Some (lm.init.initSequenceOptionalChildExpr (lm.lg.getAsn1ChildBackendName c) 0I)
                | Some (Asn1AcnAst.AlwaysPresent)   -> Some (lm.init.initSequenceOptionalChildExpr (lm.lg.getAsn1ChildBackendName c) 1I)                )
        match nonEmptyChildren with
        | [] -> lm.lg.getEmptySequenceInitExpression ()
        | _  -> lm.init.initSequenceExpr nonEmptyChildren arrsOptionalChildren
    
    let optChildrenString: String = // opt children will be first in scala
        let hasOptional =
            children |>
            List.choose(fun c -> match c with Asn1Child x -> Some x | _ -> None) |>
            List.map(fun c -> 
                match c.Optionality with
                | Some (Asn1AcnAst.Optional opt) -> true
                | Some (Asn1AcnAst.AlwaysPresent) -> true
                | _ -> false            
            ) |>
            List.exists((=) true)
        if hasOptional then "null, " else ""

    let rec resolveReferenceType(t: Asn1TypeKind): Asn1TypeKind = 
        match t with
        | ReferenceType rt -> resolveReferenceType rt.resolvedType.Kind
        | _ -> t

    let extractAsn1Types: Asn1TypeKind list = 
        children |> 
        List.choose(fun c -> match c with Asn1Child x -> Some x.Type.Kind | _ -> None) |>
        List.map(fun k -> resolveReferenceType k)

    let defaultParamListForScala: String = 
        match typeDefinition with
        | TypeDefinition td -> 
            td.typedefName + "(" + optChildrenString + (extractAsn1Types |> List.map (fun c -> extractDefaultInitValue c) |> List.reduce (fun c1 c2 -> c1 + ", " + c2)) + ")"
        | ReferenceToExistingDefinition r -> "TODO"

    createInitFunctionCommon r lm t typeDefinition defaultParamListForScala initByAsn1ValueFnc 
        initTasFunction testCaseFuncs (constantInitExpression getChildExpression)  
        (constantInitExpression getChildExpressionGlobal)  nonEmbeddedChildrenFuncs []

let createChoiceInitFunc (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference) (children:ChChildInfo list)  =     
    let initTestCase_choice_child   = lm.init.initTestCase_choice_child
    let initChildWithInitFunc       = lm.init.initChildWithInitFunc
    let initChoice                  = lm.init.initChoice

    let typeDefinitionName = typeDefinition.longTypedefName2 lm.lg.hasModules 
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let childrenOut = 
            match v.ActualValue with
            | ChValue iv     -> 
                children |> 
                List.choose(fun chChild -> 
                    match chChild.Name.Value = iv.name with
                    | false -> None
                    | true  ->
                        let sChildTypeName = chChild.chType.typeDefintionOrReference.longTypedefName2 lm.lg.hasModules
                        let sChoiceTypeName = typeDefinition.longTypedefName2 lm.lg.hasModules
                        let sChildName = (lm.lg.getAsn1ChChildBackendName chChild)
                        let sChildTempVarName = (ToC chChild.chType.id.AsString) + "_tmp"

                        let chContent = 
                            match lm.lg.init.choiceComponentTempInit with
                            | false ->
                                chChild.chType.initFunction.initByAsn1Value ({p with arg = lm.lg.getChChild p.arg (lm.lg.getAsn1ChChildBackendName chChild) chChild.chType.isIA5String}) iv.Value.kind
                            | true ->
                                chChild.chType.initFunction.initByAsn1Value ({CallerScope.modName = t.id.ModName; arg = VALUE sChildTempVarName}) iv.Value.kind
                        Some (initChoice p.arg.p (lm.lg.getAccess p.arg) chContent (lm.lg.presentWhenName (Some typeDefinition) chChild) sChildTempVarName sChildTypeName sChoiceTypeName sChildName lm.lg.init.choiceComponentTempInit) 
                        ) 

            | _               -> raise(BugErrorException "UnexpectedValue")
        childrenOut |> Seq.head


    let testCaseFuncs = 
        let handleChild  (ch:ChChildInfo)  = 
            let sChildID (p:CallerScope) = 
                (ToC ch._present_when_name_private) + "_PRESENT"


            let len = ch.chType.initFunction.automaticTestCases.Length
            let sChildName = (lm.lg.getAsn1ChChildBackendName ch)
            let sChildTypeDef = ch.chType.typeDefintionOrReference.longTypedefName2 lm.lg.hasModules 
            ch.chType.initFunction.automaticTestCases (*|> Seq.take (min 5 len)*) |> Seq.toList |>
            List.map(fun atc -> 
                let fnc = atc.initTestCaseFunc
                let presentFunc (p:CallerScope) = 
                    let childContent =  
                        match lm.lg.init.choiceComponentTempInit with
                        | false  -> fnc {p with arg = lm.lg.getChChild p.arg sChildName ch.chType.isIA5String} 
                        | true   -> fnc {p with arg = VALUE (sChildName + "_tmp")} 
                    let funcBody = initTestCase_choice_child p.arg.p (lm.lg.getAccess p.arg) (sChildID p) childContent.funcBody sChildName  sChildTypeDef typeDefinitionName
                    {InitFunctionResult.funcBody = funcBody; localVariables = childContent.localVariables}
                let combinedTestCase =
                    match atc.testCaseTypeIDsMap.ContainsKey ch.chType.id with
                    | true      -> atc.testCaseTypeIDsMap
                    | false     -> atc.testCaseTypeIDsMap.Add(ch.chType.id, TcvAnyValue)
                {AutomaticTestCase.initTestCaseFunc = presentFunc; testCaseTypeIDsMap = combinedTestCase } )

        match r.args.generateAutomaticTestCases with
        | true  ->          
            children |>
            //if some alternatives have restricted to always ABSENT (via WITH COMPONENTS constraint) then do not produce a test case for them.
            List.filter (fun c -> c.Optionality.IsNone || c.Optionality = (Some Asn1AcnAst.Asn1ChoiceOptionality.ChoiceAlwaysPresent)) |>
            List.collect handleChild
        | false -> []


    let initTasFunction (p:CallerScope) =
        let handleChild  (ch:ChChildInfo)  = 
            let sChildName = (lm.lg.getAsn1ChChildBackendName ch)
            let sChildTypeDef = ch.chType.typeDefintionOrReference.longTypedefName2 lm.lg.hasModules 
            let chp = {p with arg = lm.lg.getChChild p.arg sChildName ch.chType.isIA5String} 
            let childContent_funcBody, childContent_localVariables = 
                match ch.chType.initFunction.initProcedure with
                | None  ->
                    let fnc = ch.chType.initFunction.initTas
                    let childContent =  
                        match lm.lg.init.choiceComponentTempInit with
                        | false     -> fnc chp
                        | true   -> fnc {p with arg = VALUE (sChildName + "_tmp")} 
                    childContent.funcBody, childContent.localVariables
                | Some initProc  ->
                    match lm.lg.init.choiceComponentTempInit with
                    | false  -> initChildWithInitFunc (lm.lg.getPointer chp.arg) initProc.funcName, []
                    | true   -> initChildWithInitFunc (sChildName + "_tmp") initProc.funcName, []
            let funcBody = initTestCase_choice_child p.arg.p (lm.lg.getAccess p.arg) (lm.lg.presentWhenName (Some typeDefinition) ch) childContent_funcBody sChildName  sChildTypeDef typeDefinitionName

            {InitFunctionResult.funcBody = funcBody; localVariables = childContent_localVariables}
        match children with
        | x::_  -> handleChild x
        | _     -> {InitFunctionResult.funcBody = ""; localVariables = []}
  
    let nonEmbeddedChildrenFuncs = 
        children |> List.choose(fun ch -> 
            match ch.chType.initFunction.initProcedure with
            | None -> None
            | Some _ when r.args.generateConstInitGlobals -> None
            | Some _ -> Some ch.chType.initFunction)

    let constantInitExpression getChildExp = 
        children |> 
        List.map (fun c -> 
            let childName = lm.lg.getAsn1ChChildBackendName c
            let presentWhenName = lm.lg.presentWhenName (Some typeDefinition) c
             
            let childExp = getChildExp lm c.chType
            lm.init.initChoiceExpr childName presentWhenName childExp ) |>
        List.head
    createInitFunctionCommon r lm t typeDefinition o.defaultInitVal funcBody initTasFunction testCaseFuncs (constantInitExpression getChildExpression) (constantInitExpression getChildExpressionGlobal) nonEmbeddedChildrenFuncs []

let createReferenceType (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (baseType:Asn1Type) =
    let initChildWithInitFunc = lm.init.initChildWithInitFunc
    let nonEmbeddedChildrenFuncs = []

    (*
    let moduleName, typeDefinitionName = 
        let t1 = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        let typeDef = lm.lg.getTypeDefinition t1.FT_TypeDefintion
        typeDef.programUnit, typeDef.typeName
*)
    let moduleName, typeDefinitionName = 
        match typeDefinition with
        | ReferenceToExistingDefinition refToExist   ->
            match refToExist.programUnit with
            | Some md -> md, refToExist.typedefName
            | None    -> ToC t.id.ModName, refToExist.typedefName
        | TypeDefinition                tdDef        ->
            match tdDef.baseType with
            | None -> ToC t.id.ModName, tdDef.typedefName
            | Some refToExist -> 
                match refToExist.programUnit with
                | Some md -> md, refToExist.typedefName
                | None    -> ToC t.id.ModName, refToExist.typedefName

    

    let t1              = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
    let t1WithExtensios = o.resolvedType;

    let bs = baseType.initFunction
    match TypesEquivalence.uperEquivalence t1 t1WithExtensios with
    | false -> 
        createInitFunctionCommon r lm t typeDefinition (extractDefaultInitValue baseType.Kind) bs.initByAsn1Value bs.initTas bs.automaticTestCases bs.initExpression bs.initExpressionGlobal  bs.nonEmbeddedChildrenFuncs []
    | true  ->
        match t.isComplexType with
        | true ->
            let baseFncName, baseGlobaleName = 
                let funcName = typeDefinitionName + (lm.init.methodNameSuffix())
                let globalName = typeDefinitionName + "_constant"
                match lm.lg.hasModules with
                | false     -> funcName, globalName
                | true   -> 
                    match t.id.ModName = o.modName.Value with
                    | true  -> funcName, globalName
                    | false -> moduleName + "." + funcName, moduleName + "." + globalName
            let constantInitExpression = baseFncName
            let constantInitExpressionGlobal = baseGlobaleName
            let initTasFunction (p:CallerScope) =
                let funcBody = initChildWithInitFunc (lm.lg.getPointer p.arg) baseFncName
                {InitFunctionResult.funcBody = funcBody; localVariables = []}
            createInitFunctionCommon r lm t typeDefinition (extractDefaultInitValue baseType.Kind) bs.initByAsn1Value  initTasFunction bs.automaticTestCases constantInitExpression constantInitExpressionGlobal nonEmbeddedChildrenFuncs []
        | false ->
            createInitFunctionCommon r lm t typeDefinition (extractDefaultInitValue baseType.Kind) bs.initByAsn1Value bs.initTas bs.automaticTestCases bs.initExpression bs.initExpressionGlobal bs.nonEmbeddedChildrenFuncs []
