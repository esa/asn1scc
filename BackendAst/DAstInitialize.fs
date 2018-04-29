module DAstInitialize
open System
open System.Numerics
open System.Globalization
open System.IO

open FsUtils
open CommonTypes
open Asn1AcnAstUtilFunctions
open Asn1Fold
open DAst
open DAstUtilFunctions


#if ZZZZ

type TestCaseValue =
    | TypeValueAny
    | OctetTypeValue of Asn1Type*BigInteger      //size of byte array, actual value is not important
    | BitTypeValue of Asn1Type*BigInteger            //size of bit array, actual value is not important
    | SequenceOfTypeValue of Asn1Type*(TestCaseValue list)     //size of SequenceOf value, actual child type value not important
    | SequenceTypeValue of Asn1Type*((string*TestCaseValue) list)
    
type Asn1Type
with
    member this.isValidAsn1AcnVal (vRoot:TestCaseValue) =
        match this.Kind with
        | ReferenceType t-> t.resolvedType.isValidAsn1AcnVal vRoot
        | Integer      _ -> true
        | Real         _ -> true
        | IA5String    _ -> true
        | OctetString  _ -> true
        | NullType     _ -> true
        | BitString    _ -> true
        | Boolean      _ -> true
        | Enumerated   _ -> true
        | SequenceOf   _ -> true
        | Sequence     _ -> true
        | Choice       _ -> true

let getSizeValue (ReferenceToType nodes)   (pVal : TestCaseValue) =
    0I

let getSubCaseFromPath (ReferenceToType nodes)  (pVal : TestCaseValue) =
    let handleNode zeroBasedSeqeuenceOfLevel (tcVal : TestCaseValue) (n:ScopeNode)  = 
        match n with
        | MD _
        | TA _
        | PRM _
        | VA _              -> raise(BugErrorException "getSubCaseFromPath")
        | SEQ_CHILD chName  -> [], 
            match tcVal with
            | 
        | CH_CHILD (chName,pre_name)  -> 
            
            [pVal.arg.getChChildIsPresent l pre_name], {pVal with arg = pVal.arg.getChChild l (ToC chName) childTypeIsString}
        | SQF               -> 
            let curIdx = sprintf "i%d" (zeroBasedSeqeuenceOfLevel + 1)

            [], {pVal with arg = pVal.arg.getArrayItem l curIdx childTypeIsString}

    match nodes with
    | (MD md)::(TA tas)::(PRM prm)::[]  -> ({CallerScope.modName = pVal.modName; arg = VALUE (ToC (md + "_" + tas + "_" + prm))}, [])
    | (MD md)::(TA tas):: xs            ->
        let length = Seq.length xs
        let ret = 
            xs |> 
            List.fold(fun (curPath, curCheckExp, zeroBasedSeqeuenceOfLevel, idx) n -> 
                let chekPath, newPath = handleNode zeroBasedSeqeuenceOfLevel curPath n (childTypeIsString && idx=length)
                let zeroBasedSeqeuenceOfLevel = match n with SQF -> zeroBasedSeqeuenceOfLevel + 1 | _ -> zeroBasedSeqeuenceOfLevel
                (newPath, chekPath@curCheckExp, zeroBasedSeqeuenceOfLevel, idx+1)) (pVal,[], 0, 1) |> (fun (a,chekPath,_,_) -> a, chekPath)
        ret 
    | _                                 -> raise(BugErrorException "getAccessFromScopeNodeList")

let rec handleSingleUpdateDependency (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (m:Asn1AcnAst.Asn1Module) (d:Asn1AcnAst.AcnDependency)   =
    match d.dependencyKind with
    | Asn1AcnAst.AcnDepSizeDeterminant         -> 
        let updateFunc (vRoot:TestCaseValue)  = 
            let nSize = getSizeValue d.asn1Type vRoot
            nSize
        updateFunc

and getUpdateFunctionUsedInEncoding (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (m:Asn1AcnAst.Asn1Module) (acnChildOrAcnParameterId)  (*: ((TestCaseValue -> BigInteger)option)*)=
    (*
    match deps.acnDependencies |> List.filter(fun d -> d.determinant.id = acnChildOrAcnParameterId) with
    | []  -> None
    | d1::[]    -> 
        let ret= handleSingleUpdateDependency r deps  m d1 
        ret
        *)
    deps.acnDependencies |> List.filter(fun d -> d.determinant.id = acnChildOrAcnParameterId) |> List.map(fun d -> handleSingleUpdateDependency r deps  m d)

let isValidAsn1SeqValue (r:Asn1AcnAst.AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (m:Asn1AcnAst.Asn1Module) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Sequence)  (children:SeqChildInfo list) =
    let acnChildren = children |> List.choose (fun c -> match c with AcnChild c -> Some c | Asn1Child _ -> None)
    let asn1Children = children |> List.choose (fun c -> match c with AcnChild _ -> None | Asn1Child c -> Some c)
    let retFunc (seqVal: TestCaseValue) =
        let isValidAcnChild (acnChild :AcnChild ) =
            let values = getUpdateFunctionUsedInEncoding r deps m acnChild.id |> List.map(fun fnc -> fnc seqVal)
            match values with
            | [] | [_]  -> true
            | v1::restValues    -> restValues |> List.forall ((=) v1)
            
        let foo = acnChildren |> List.forall isValidAcnChild

        let childrenResult = 
            match seqVal with
            | SequenceTypeValue (_,chVals)  ->
                chVals |> List.fold(fun curRes (chnm, chval) ->
                    let chChild = asn1Children |> Seq.find(fun z -> z.Name.Value = chnm)
                    curRes &&  chChild.Type.isValidAsn1AcnVal chval
                ) true
            | _         -> false
        childrenResult
    retFunc 
#endif

(*
create c and Ada procedures that initialize an ASN.1 type.
Currently this code is not used since it is no longer required (it was originally written to handle the 'data might not be initialized' errors of spark
However, now with the 'pragma Annotate (GNATprove, False_Positive)' we can handle this case.
*)


let nameSuffix l = match l with C -> "_Initialize" | Ada -> "_Init"

let getFuncName2 (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (tasInfo:TypeAssignmentInfo option) (inhInfo: InheritanceInfo option) (typeKind:Asn1AcnAst.Asn1TypeKind) (typeDefinition:TypeDefintionOrReference) =
    //getFuncNameGeneric r nameSuffix tasInfo inhInfo typeKind typeDefinition
    getFuncNameGeneric typeDefinition (nameSuffix l)

(*
let getFuncName (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (tasInfo:TypeAssignmentInfo option) =
    match l with
    | C     -> tasInfo |> Option.map (fun x -> ToC2(r.args.TypePrefix + x.tasName + "_Initialize"))
    | Ada   -> tasInfo |> Option.map (fun x -> ToC2(r.args.TypePrefix + x.tasName + "_Init"))
*)

let createInitFunctionCommon (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)   (o:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) initByAsn1Value (iv:Asn1ValueKind) (initTasFunction:CallerScope  -> InitFunctionResult) automaticTestCases =
//    if o.id.AsString = "TEST-CASE.T-POS.anInt" then
//        printfn "%s" o.id.AsString

    //let aaaa = o.id.AsString
    let funcName            = getFuncName2 r l o.id.tasInfo o.inheritInfo o.Kind typeDefinition
    //let funcName            = getFuncName r l o.id.tasInfo 
    let p = o.getParamType l CommonTypes.Codec.Decode
    let initTypeAssignment = match l with C -> init_c.initTypeAssignment | Ada -> init_a.initTypeAssignment
    let initTypeAssignment_def = match l with C -> init_c.initTypeAssignment_def | Ada -> init_a.initTypeAssignment_def
    let varName = p.arg.p
    let sStar = p.arg.getStar l

    let  func, funcDef  = 
            match funcName  with
            | None              -> None, None
            | Some funcName     -> 
                let res = initTasFunction p 
                let lvars = res.localVariables |> List.map(fun (lv:LocalVariable) -> lv.GetDeclaration l) |> Seq.distinct
                Some(initTypeAssignment varName sStar funcName  (typeDefinition.longTypedefName l) res.funcBody lvars), Some(initTypeAssignment_def varName sStar funcName  (typeDefinition.longTypedefName l))


    {
        initFuncName            = funcName
        initFunc                = func
        initFuncDef             = funcDef
        initTas                 = initTasFunction
        initByAsn1Value         = initByAsn1Value
        automaticTestCases      = automaticTestCases
    }

let createIntegerInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o:Asn1AcnAst.Integer) (typeDefinition:TypeDefintionOrReference) iv =
    let initInteger = match l with C -> init_c.initInteger | Ada -> init_a.initInteger
    let funcBody (p:CallerScope) v = 
        let vl = 
            match v with
            | IntegerValue iv   -> iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initInteger (p.arg.getValue l) vl

    let integerVals = EncodeDecodeTestCase.IntegerAutomaticTestCaseValues r t o
    
    let allCons = DAstValidate.getIntSimplifiedConstraints r o.isUnsigned o.AllCons
    let tasInitFunc (p:CallerScope)  = 
        match isValidValueRanged allCons 0I  with
        | false    -> 
            match integerVals with 
            |x::_ -> {InitFunctionResult.funcBody = initInteger (p.arg.getValue l) x;  localVariables=[]} 
            | [] -> {InitFunctionResult.funcBody = initInteger (p.arg.getValue l) 0I;  localVariables=[]}
        | true  -> {InitFunctionResult.funcBody = initInteger (p.arg.getValue l) 0I;  localVariables=[]}

    let testCaseFuncs = 
        integerVals |> 
        List.map (fun vl -> 
            let initTestCaseFunc =
                (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initInteger (p.arg.getValue l) vl;  localVariables=[]} )
            {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = Map.ofList [(t.id, TcvAnyValue)] }        )

    createInitFunctionCommon r l t  typeDefinition funcBody iv tasInitFunc testCaseFuncs

let createRealInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference) iv = 
    let initReal = match l with C -> init_c.initReal | Ada -> init_a.initReal
    let funcBody (p:CallerScope) v = 
        let vl = 
            match v with
            | RealValue iv   -> iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initReal (p.arg.getValue l) vl

    let realVals = EncodeDecodeTestCase.RealAutomaticTestCaseValues r t o
    let testCaseFuncs = 
        realVals |> 
        List.map (fun vl -> 
            let initTestCaseFunc = (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initReal (p.arg.getValue l) vl; localVariables=[]}) 
            {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = Map.ofList [(t.id, TcvAnyValue)] } )

    let tasInitFunc (p:CallerScope)  = 
        match isValidValueRanged o.AllCons 0.0  with
        | false    -> 
            match realVals with 
            | x::_ -> {InitFunctionResult.funcBody = initReal (p.arg.getValue l) x;  localVariables=[]} 
            | [] -> {InitFunctionResult.funcBody = initReal (p.arg.getValue l) 0.0;  localVariables=[]}
        | true  -> {InitFunctionResult.funcBody = initReal (p.arg.getValue l) 0.0;  localVariables=[]}

    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs

let createIA5StringInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.StringType   ) (typeDefinition:TypeDefintionOrReference) iv = 
    let initIA5String = match l with C -> init_c.initIA5String | Ada -> init_a.initIA5String
    let initTestCaseIA5String = match l with C -> init_c.initTestCaseIA5String | Ada -> init_a.initTestCaseIA5String

    
    let funcBody (p:CallerScope) v = 
        let vl = 
            match v with
            | StringValue iv   -> 
                iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        let arrNuls = [0 .. (int o.maxSize- vl.Length)]|>Seq.map(fun x -> variables_a.PrintStringValueNull())
        initIA5String (p.arg.getValue l) (vl.Replace("\"","\"\"")) arrNuls

    let ii = t.id.SeqeuenceOfLevel + 1
    let i = sprintf "i%d" ii
    //let visibleChars = o.uperCharSet |> Seq.filter(fun c -> not (System.Char.IsControl c))
    let bAlpha = o.uperCharSet.Length < 128
    let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
    let testCaseFuncs = 
        let seqOfCase (nSize:BigInteger)  = 
            let initTestCaseFunc (p:CallerScope) = 
                let funcBody = initTestCaseIA5String p.arg.p (p.arg.getAcces l) (nSize) ((o.maxSize+1I)) i (typeDefinition.longTypedefName l) bAlpha arrAsciiCodes (BigInteger arrAsciiCodes.Length) false
                {InitFunctionResult.funcBody = funcBody; localVariables=[SequenceOfIndex (ii, None)]}
            {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = Map.ofList [(t.id, TcvSizeableTypeValue nSize)] }
        seq {
            match o.minSize = o.maxSize with
            | true  -> yield seqOfCase o.minSize 
            | false -> 
                yield seqOfCase o.minSize 
                yield seqOfCase o.maxSize 
        } |> Seq.toList
    let zero (p:CallerScope) = 
        let funcBody = initTestCaseIA5String p.arg.p (p.arg.getAcces l) ( (o.maxSize+1I)) ( (o.maxSize+1I)) i (typeDefinition.longTypedefName l) bAlpha arrAsciiCodes (BigInteger arrAsciiCodes.Length) true
        let lvars = match l with C -> [] | Ada -> [SequenceOfIndex (ii, None)]
        {InitFunctionResult.funcBody = funcBody; localVariables=lvars}
    createInitFunctionCommon r l t typeDefinition funcBody iv zero testCaseFuncs

let createOctetStringInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.OctetString ) (typeDefinition:TypeDefintionOrReference) iv (isValidFunction:IsValidFunction option) = 
    let initFixSizeBitOrOctString_bytei = match l with C -> init_c.initFixSizeBitOrOctString_bytei  | Ada -> init_a.initFixSizeBitOrOctString_bytei
    let initFixSizeBitOrOctString       = match l with C -> init_c.initFixSizeBitOrOctString        | Ada -> init_a.initFixSizeBitOrOctString
    let initFixVarSizeBitOrOctString    = match l with C -> init_c.initFixVarSizeBitOrOctString     | Ada -> init_a.initFixVarSizeBitOrOctString
    let initTestCaseOctetString         = match l with C -> init_c.initTestCaseOctetString     | Ada -> init_a.initTestCaseOctetString
    let funcBody (p:CallerScope) v = 
        let bytes = 
            match v with
            | OctetStringValue iv -> iv
            | BitStringValue iv   -> bitStringValueToByteArray (StringLoc.ByValue iv) |> Seq.toList
            | _                 -> raise(BugErrorException "UnexpectedValue")
        let arrsBytes = bytes |> List.mapi(fun i b -> initFixSizeBitOrOctString_bytei p.arg.p (p.arg.getAcces l) ((i+l.ArrayStartIndex).ToString()) (sprintf "%x" b))
        match o.minSize = o.maxSize with
        | true  -> initFixSizeBitOrOctString p.arg.p (p.arg.getAcces l) arrsBytes
        | false -> initFixVarSizeBitOrOctString p.arg.p (p.arg.getAcces l) (BigInteger arrsBytes.Length) arrsBytes
    let anonymousValues =
        match isValidFunction with
        | None  -> []
        | Some isV -> 
            isV.anonymousVariables |> 
                List.choose(fun iv ->
                    match iv.valKind with
                    | OctetStringValue bytes    -> Some(iv, bytes)
                    | _                         -> None ) 

    let testCaseFuncs, tasInitFunc =
        match anonymousValues with
        | []  ->
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = sprintf "i%d" ii
            let seqOfCase (nSize:BigInteger) = 
                let initTestCaseFunc (p:CallerScope) = 
                    let funcBody = initTestCaseOctetString p.arg.p (p.arg.getAcces l) nSize i (o.minSize = o.maxSize) false
                    {InitFunctionResult.funcBody = funcBody; localVariables=[SequenceOfIndex (ii, None)]}
                {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = Map.ofList [(t.id, TcvSizeableTypeValue nSize)] }
            let testCaseFuncs = 
                seq {
                    match o.minSize = o.maxSize with
                    | true  -> yield seqOfCase o.minSize 
                    | false -> 
                        yield seqOfCase o.minSize 
                        yield seqOfCase o.maxSize 
                } |> Seq.toList
            let zero (p:CallerScope) = 
                let funcBody = initTestCaseOctetString p.arg.p (p.arg.getAcces l) o.maxSize i (o.minSize = o.maxSize) true
                let lvars = match l with C -> [] | Ada -> [SequenceOfIndex (ii, None)]
                {InitFunctionResult.funcBody = funcBody; localVariables=lvars}

            testCaseFuncs, zero
        | _ ->
            let ret = 
                anonymousValues |> 
                List.map(fun (iv, bytes) ->
                    let initTestCaseFunc (p:CallerScope) =
                        let ret = sprintf "%s%s%s;" (p.arg.getValue l) l.AssignOperator iv.valueName
                        {InitFunctionResult.funcBody = ret; localVariables=[]}
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = Map.ofList [(t.id, TcvSizeableTypeValue bytes.Length.AsBigInt)] })
            ret, ret.Head.initTestCaseFunc
    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs

let createNullTypeInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.NullType    ) (typeDefinition:TypeDefintionOrReference) iv = 
    let initNull = match l with C -> init_c.initNull | Ada -> init_a.initNull
    let funcBody (p:CallerScope) v = initNull (p.arg.getValue l) 
    let testCaseFuncs = [{AutomaticTestCase.initTestCaseFunc = (fun p -> {InitFunctionResult.funcBody = initNull (p.arg.getValue l); localVariables=[]}); testCase = Map.ofList [(t.id, TcvAnyValue)]} ]
    createInitFunctionCommon r l t typeDefinition funcBody iv testCaseFuncs.Head.initTestCaseFunc testCaseFuncs

let createBitStringInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.BitString   ) (typeDefinition:TypeDefintionOrReference) iv (isValidFunction:IsValidFunction option)= 
    let initFixSizeBitOrOctString_bytei = match l with C -> init_c.initFixSizeBitOrOctString_bytei  | Ada -> init_a.initFixSizeBitOrOctString_bytei
    let initFixSizeBitOrOctString       = match l with C -> init_c.initFixSizeBitOrOctString        | Ada -> init_a.initFixSizeBitOrOctString
    let initFixVarSizeBitOrOctString    = match l with C -> init_c.initFixVarSizeBitOrOctString     | Ada -> init_a.initFixVarSizeBitOrOctString
    let initTestCaseBitString           = match l with C -> init_c.initTestCaseBitString     | Ada -> init_a.initTestCaseBitString
    let funcBody (p:CallerScope) v = 
        let bytes = 
            match v with
            | BitStringValue iv     -> bitStringValueToByteArray (StringLoc.ByValue iv) |> Seq.toList
            | OctetStringValue iv   -> iv
            | _                     -> raise(BugErrorException "UnexpectedValue")
        let arrsBytes = bytes |> List.mapi(fun i b -> initFixSizeBitOrOctString_bytei p.arg.p (p.arg.getAcces l) ((i+l.ArrayStartIndex).ToString()) (sprintf "%x" b))
        match o.minSize = o.maxSize with
        | true  -> initFixSizeBitOrOctString p.arg.p (p.arg.getAcces l) arrsBytes
        | false -> initFixVarSizeBitOrOctString p.arg.p (p.arg.getAcces l) (BigInteger arrsBytes.Length) arrsBytes

    let anonymousValues =
        match isValidFunction with
        | None  -> []
        | Some isV -> 
            isV.anonymousVariables |>
                List.choose(fun iv ->
                    match iv.valKind with
                    | BitStringValue bitVal    -> Some(iv, bitVal)
                    | _                         -> None )
    let testCaseFuncs, tasInitFunc =
        match anonymousValues with
        | []  ->
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = sprintf "i%d" ii
            let seqOfCase (nSize:BigInteger) = 
                let initTestCaseFunc (p:CallerScope) = 
                    let nSizeCeiled =  if nSize % 8I = 0I then nSize else (nSize + (8I - nSize % 8I)) 
                    let funcBody = initTestCaseBitString p.arg.p (p.arg.getAcces l) nSize (nSizeCeiled) i (o.minSize = o.maxSize) false
                    {InitFunctionResult.funcBody = funcBody; localVariables=[SequenceOfIndex (ii, None)]}
                {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = Map.ofList [(t.id, TcvSizeableTypeValue nSize)] }

            let testCaseFuncs = 
                seq {
                    match o.minSize = o.maxSize with
                    | true  -> yield seqOfCase o.minSize 
                    | false -> 
                        yield seqOfCase o.minSize 
                        yield seqOfCase o.maxSize 
                } |> Seq.toList
            let zero (p:CallerScope) = 
                let nSize = o.maxSize
                let nSizeCeiled =  if nSize % 8I = 0I then nSize else (nSize + (8I - nSize % 8I)) 
                let funcBody = initTestCaseBitString p.arg.p (p.arg.getAcces l) nSize (nSizeCeiled) i (o.minSize = o.maxSize) true
                let lvars = match l with C -> [] | Ada -> [SequenceOfIndex (ii, None)]
                {InitFunctionResult.funcBody = funcBody; localVariables=lvars}
            testCaseFuncs, zero
        | _ ->
            let ret = 
                anonymousValues |> 
                List.map(fun (iv, bitVal) ->
                    let retFunc (p:CallerScope) =
                        let ret = sprintf "%s%s%s;" (p.arg.getValue l) l.AssignOperator iv.valueName
                        {InitFunctionResult.funcBody = ret; localVariables=[]}
                    {AutomaticTestCase.initTestCaseFunc = retFunc; testCase = Map.ofList [(t.id, TcvSizeableTypeValue bitVal.Length.AsBigInt)] })
            ret, ret.Head.initTestCaseFunc
    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs

let createBooleanInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Boolean     ) (typeDefinition:TypeDefintionOrReference) iv = 
    let initBoolean = match l with C -> init_c.initBoolean | Ada -> init_a.initBoolean
    let funcBody (p:CallerScope) v = 
        let vl = 
            match v with
            | BooleanValue iv   -> iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initBoolean (p.arg.getValue l) vl
    let testCaseFuncs = 
        EncodeDecodeTestCase.BooleanAutomaticTestCaseValues r t o |> 
        List.map (fun vl -> {AutomaticTestCase.initTestCaseFunc = (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initBoolean (p.arg.getValue l) vl; localVariables = []}); testCase = Map.ofList [(t.id, TcvAnyValue)] })

    let tasInitFunc (p:CallerScope)  = 
        match isValidValueGeneric o.AllCons (=) false  with
        | true    -> {InitFunctionResult.funcBody = initBoolean (p.arg.getValue l) false; localVariables = []}
        | false     -> {InitFunctionResult.funcBody = initBoolean (p.arg.getValue l) true; localVariables = []}

    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs

let mergeMaps (m1:Map<'key,'value>) (m2:Map<'key,'value>) =
    Map.fold (fun (nm:Map<'key,'value>) key value -> match nm.ContainsKey key with false -> nm.Add(key,value) | true -> nm) m1 m2
    

let createEnumeratedInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Enumerated  )  (typeDefinition:TypeDefintionOrReference) iv = 
    let initEnumerated = match l with C -> init_c.initEnumerated | Ada -> init_a.initEnumerated
    let funcBody (p:CallerScope) v = 
        let vl = 
            match v with
            | EnumValue iv      -> o.items |> Seq.find(fun x -> x.Name.Value = iv)
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initEnumerated (p.arg.getValue l) (vl.getBackendName (Some typeDefinition) l)
    let testCaseFuncs = 
        EncodeDecodeTestCase.EnumeratedAutomaticTestCaseValues2 r t o |> 
        List.map (fun vl -> 
            {
                AutomaticTestCase.initTestCaseFunc = (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initEnumerated (p.arg.getValue l) (vl.getBackendName (Some typeDefinition) l); localVariables=[]}); 
                testCase = Map.ofList [(t.id, (TcvEnumeratedValue vl.Name.Value))] 
            })
    createInitFunctionCommon r l t typeDefinition funcBody iv testCaseFuncs.Head.initTestCaseFunc testCaseFuncs

let createSequenceOfInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.SequenceOf  ) (typeDefinition:TypeDefintionOrReference) (childType:Asn1Type) iv = 
    let initFixedSequenceOf = match l with C -> init_c.initFixedSequenceOf | Ada -> init_a.initFixedSequenceOf
    let initVarSizeSequenceOf = match l with C -> init_c.initVarSizeSequenceOf | Ada -> init_a.initVarSizeSequenceOf
    let initTestCaseSizeSequenceOf_innerItem = match l with C -> init_c.initTestCaseSizeSequenceOf_innerItem | Ada -> init_a.initTestCaseSizeSequenceOf_innerItem
    let initTestCaseSizeSequenceOf = match l with C -> init_c.initTestCaseSizeSequenceOf | Ada -> init_a.initTestCaseSizeSequenceOf
    let initChildWithInitFunc       = match l with C -> init_c.initChildWithInitFunc | Ada -> init_a.initChildWithInitFunc
    let funcBody (p:CallerScope) v = 
        let vl = 
            match v with
            | SeqOfValue childVals ->
                childVals |> 
                List.mapi(fun i chv -> 
                    let ret = childType.initFunction.initByAsn1Value ({p with arg = p.arg.getArrayItem l ((i+l.ArrayStartIndex).ToString()) childType.isIA5String}) chv.kind
                    match l with
                    | C     -> ret
                    | Ada   when i>0 -> ret
                    | Ada   -> 
                        // in the first array we have to emit a pragma Annotate false_positive, otherwise gnatprove emit an error
                        let pragma = init_a.initSequence_pragma p.arg.p
                        ret + pragma
                    )
            | _               -> raise(BugErrorException "UnexpectedValue")
        match o.minSize = o.maxSize with
        | true  -> initFixedSequenceOf vl
        | false -> initVarSizeSequenceOf p.arg.p (p.arg.getAcces l) (BigInteger vl.Length) vl

    let ii = t.id.SeqeuenceOfLevel + 1
    let i = sprintf "i%d" (t.id.SeqeuenceOfLevel + 1)
    let testCaseFuncs =
        let seqOfCase (nSize:BigInteger)  = 
                let len = childType.initFunction.automaticTestCases.Length
                let childTestCases = 
                    childType.initFunction.automaticTestCases |> Seq.take (min 5 len) |> Seq.toList //|>
                    //List.map(fun fnc -> fnc.initTestCaseFunc ({p with arg = p.arg.getArrayItem l i childType.isIA5String}))
                match childTestCases with
                | []    -> 
                    let initTestCaseFunc (p:CallerScope) = 
                        {InitFunctionResult.funcBody = ""; localVariables = []}
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = Map.ofList [(t.id, TcvSizeableTypeValue nSize)] }
                | atc::[] -> 
                    let initTestCaseFunc (p:CallerScope) = 
                        let childCase = atc.initTestCaseFunc ({p with arg = p.arg.getArrayItem l i childType.isIA5String})
                        let funcBody = initTestCaseSizeSequenceOf p.arg.p (p.arg.getAcces l) None nSize (o.minSize = o.maxSize) [childCase.funcBody] false i
                        {InitFunctionResult.funcBody = funcBody; localVariables= (SequenceOfIndex (ii, None))::childCase.localVariables }
                    let combinedTestCase = atc.testCase.Add(t.id, TcvSizeableTypeValue nSize)
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = combinedTestCase }
                | _             ->
                    let initTestCaseFunc (p:CallerScope) = 
                        let arrsInnerItems, childLocalVars = 
                            childTestCases |> 
                            List.mapi(fun idx atc -> 
                                let sChildItem = atc.initTestCaseFunc ({p with arg = p.arg.getArrayItem l i childType.isIA5String})
                                let funcBody = initTestCaseSizeSequenceOf_innerItem (idx=0) (idx = childTestCases.Length-1) idx.AsBigInt sChildItem.funcBody i (BigInteger childTestCases.Length)
                                (funcBody, (SequenceOfIndex (ii, None))::sChildItem.localVariables)) |>
                            List.unzip
                        let funcBody = initTestCaseSizeSequenceOf p.arg.p (p.arg.getAcces l) None  nSize (o.minSize = o.maxSize) arrsInnerItems true i 
                        {InitFunctionResult.funcBody = funcBody; localVariables= (SequenceOfIndex (ii, None))::(childLocalVars |> List.collect id)}
                    let combinedTestCase =
                        let thisCase = Map.ofList [(t.id, TcvSizeableTypeValue nSize)]
                        childTestCases |> List.fold(fun (newMap:Map<ReferenceToType, TestCaseValue>) atc -> mergeMaps newMap atc.testCase) thisCase
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = combinedTestCase }

        seq {
            match o.minSize = o.maxSize with
            | true  -> yield seqOfCase o.minSize 
            | false -> 
                yield seqOfCase o.maxSize 
                yield seqOfCase o.minSize 
        } |> Seq.toList
    let initTasFunction (p:CallerScope) =
        let initCountValue = Some o.minSize
        let chp = {p with arg = p.arg.getArrayItem l i childType.isIA5String}
        let childInitRes_funcBody, childInitRes_localVariables = 
            match childType.initFunction.initFuncName with
            | None  ->
                let childInitRes = childType.initFunction.initTas chp
                childInitRes.funcBody, childInitRes.localVariables
            | Some fncName  ->
                initChildWithInitFunc (chp.arg.getPointer l) fncName, []
        let funcBody = initTestCaseSizeSequenceOf p.arg.p (p.arg.getAcces l) initCountValue o.maxSize (o.minSize = o.maxSize) [childInitRes_funcBody] false i
        {InitFunctionResult.funcBody = funcBody; localVariables= (SequenceOfIndex (ii, None))::childInitRes_localVariables }
        
    createInitFunctionCommon r l t typeDefinition funcBody iv  initTasFunction testCaseFuncs

let createSequenceInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (children:SeqChildInfo list) iv = 
    let initSequence                = match l with C -> init_c.initSequence | Ada -> init_a.initSequence
    let initSequence_optionalChild  = match l with C -> init_c.initSequence_optionalChild | Ada -> init_a.initSequence_optionalChild
    let initTestCase_sequence_child = match l with C -> init_c.initTestCase_sequence_child | Ada -> init_a.initTestCase_sequence_child
    let initTestCase_sequence_child_opt = match l with C -> init_c.initTestCase_sequence_child_opt | Ada -> init_a.initTestCase_sequence_child_opt
    let initChildWithInitFunc       = match l with C -> init_c.initChildWithInitFunc | Ada -> init_a.initChildWithInitFunc
    let dummy =
        let aaa = typeDefinition.longTypedefName l
        match aaa = "MySuperSeqOf_elem" with
        | true  -> 1
        | false -> 0

    let funcBody (p:CallerScope) v = 

        let childrenRet = 
            match v with
            | SeqValue iv     -> 
                children |>
                List.choose(fun seqChild ->
                    match seqChild with
                    | Asn1Child seqChild   ->
                        match iv |> Seq.tryFind(fun chv -> chv.name = seqChild.Name.Value) with
                        | None  ->
                            match seqChild.Optionality with
                            | None      -> None
                            | Some _    -> Some (initSequence_optionalChild p.arg.p (p.arg.getAcces l) seqChild.c_name "0" "")
                        | Some chv  ->
                            let chContent = seqChild.Type.initFunction.initByAsn1Value ({p with arg = p.arg.getSeqChild l seqChild.c_name seqChild.Type.isIA5String}) chv.Value.kind
                            match seqChild.Optionality with
                            | None      -> Some chContent
                            | Some _    -> Some (initSequence_optionalChild p.arg.p (p.arg.getAcces l) seqChild.c_name "1" chContent)
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

        let maxCasesPerChild = 
            match asn1Children.Length with
            | _ when asn1Children.Length <= 3 -> 5   //max 3^5 --> 125 cases
            | _ when asn1Children.Length <= 6 -> 2   // max 2^6 = 128 cases
            | _                               -> 1   
        let handleChild  (ch:Asn1Child)  = 
            let len = ch.Type.initFunction.automaticTestCases.Length

            ch.Type.initFunction.automaticTestCases |> 
            Seq.take (min maxCasesPerChild len) |> Seq.toList |>
            List.collect(fun atc -> 
                let presentFunc  = 
                    let initTestCaseFunc (p:CallerScope) = 
                        let chContent =  atc.initTestCaseFunc {p with arg = p.arg.getSeqChild l ch.c_name ch.Type.isIA5String} 
                        let funcBody = initTestCase_sequence_child p.arg.p (p.arg.getAcces l) ch.c_name chContent.funcBody ch.Optionality.IsSome
                        {InitFunctionResult.funcBody = funcBody; localVariables = chContent.localVariables }
                    let combinedTestCase =
                        match atc.testCase.ContainsKey ch.Type.id with
                        | true      -> atc.testCase
                        | false     -> atc.testCase.Add(ch.Type.id, TcvAnyValue)
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = combinedTestCase }
                let nonPresenceFunc =  
                    let initTestCaseFunc (p:CallerScope) = 
                        let funcBody = initTestCase_sequence_child_opt p.arg.p (p.arg.getAcces l) ch.c_name
                        {InitFunctionResult.funcBody = funcBody; localVariables = [] }
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCase = Map.empty }
                match ch.Optionality with
                | None                              ->  [presentFunc]
//                | Some (Asn1AcnAst.Optional opt) when optChildCount > 1 && opt.acnPresentWhen.IsSome ->
//                       [nonPresenceFunc] //if child is optional with present-when conditions then no test case is generated for this component because we might generated wrong test cases 
                | Some (Asn1AcnAst.Optional opt)    -> 
                    
                    [presentFunc; nonPresenceFunc] 
                    
                | Some (Asn1AcnAst.AlwaysAbsent)    -> [nonPresenceFunc] 
                | Some (Asn1AcnAst.AlwaysPresent)   -> [presentFunc] )

        let rec generateCases   (children : Asn1Child list) : AutomaticTestCase list=
            match children with
            | []        -> []
            | x1::xs    -> 
                // generate this component test cases (x1) and the rest and the join them.
                let rest = generateCases  xs
                let childCases  = 
                    let ths =  handleChild  x1 
                    match ths with
                    | []    -> rest
                    | _     ->
                        seq {
                            for i1 in ths do   
                                match rest with
                                | []    ->  yield i1
                                | _     ->
                                    for lst in rest do
                                        let ret = 
                                            let combineFnc (p:CallerScope) = 
                                                let partA = i1.initTestCaseFunc p
                                                let partB = lst.initTestCaseFunc p
                                                let funcBody = [partA.funcBody; partB.funcBody] |> Seq.StrJoin "\n"
                                                {InitFunctionResult.funcBody = funcBody; localVariables = partA.localVariables@partB.localVariables }
                                            let combinedTestCases = mergeMaps i1.testCase lst.testCase
                                            {AutomaticTestCase.initTestCaseFunc = combineFnc; testCase = combinedTestCases }
                                        yield ret
                            } |> Seq.toList
                childCases
        let tesCases = generateCases  asn1Children 
        tesCases 

    let initTasFunction (p:CallerScope) =
        let handleChild  (ch:Asn1Child)  = 
            let presentFunc () = 
                match ch.Type.initFunction.initFuncName with
                | None  ->
                    match ch.Type.typeDefintionOrReference with
                    | ReferenceToExistingDefinition    rf   when (not rf.definedInRtl) ->
                        let fncName = (ch.Type.typeDefintionOrReference.longTypedefName l) + (nameSuffix l)
                        let chP = {p with arg = p.arg.getSeqChild l ch.c_name ch.Type.isIA5String} 
                        let chContent =  initChildWithInitFunc (chP.arg.getPointer l) fncName
                        let funcBody = initTestCase_sequence_child p.arg.p (p.arg.getAcces l) ch.c_name chContent ch.Optionality.IsSome
                        {InitFunctionResult.funcBody = funcBody; localVariables = [] }
                    | _       ->
                        let fnc = ch.Type.initFunction.initTas
                        let chContent =  fnc {p with arg = p.arg.getSeqChild l ch.c_name ch.Type.isIA5String} 
                        let funcBody = initTestCase_sequence_child p.arg.p (p.arg.getAcces l) ch.c_name chContent.funcBody ch.Optionality.IsSome
                        {InitFunctionResult.funcBody = funcBody; localVariables = chContent.localVariables }
                        
                | Some fncName  ->
                    let chP = {p with arg = p.arg.getSeqChild l ch.c_name ch.Type.isIA5String} 
                    let chContent =  initChildWithInitFunc (chP.arg.getPointer l) fncName
                    let funcBody = initTestCase_sequence_child p.arg.p (p.arg.getAcces l) ch.c_name chContent ch.Optionality.IsSome
                    {InitFunctionResult.funcBody = funcBody; localVariables = [] }
                    
            let nonPresenceFunc () =  
                let funcBody = initTestCase_sequence_child_opt p.arg.p (p.arg.getAcces l) ch.c_name
                {InitFunctionResult.funcBody = funcBody; localVariables = [] }
            match ch.Optionality with
            | None                              -> presentFunc ()
            | Some (Asn1AcnAst.Optional opt)    -> presentFunc ()
            | Some (Asn1AcnAst.AlwaysAbsent)    -> nonPresenceFunc ()
            | Some (Asn1AcnAst.AlwaysPresent)   -> presentFunc ()
        let asn1Children = children |> List.choose(fun c -> match c with Asn1Child x -> Some x | _ -> None)
        let ret = 
            match asn1Children with
            | []    -> 
                match l with
                | Ada   -> 
                    let initEmpytSeq = init_a.initSequence_emptySeq p.arg.p
                    {InitFunctionResult.funcBody = initEmpytSeq; localVariables = [] }
                | C     -> {InitFunctionResult.funcBody = ""; localVariables = [] }
            | _     ->
                asn1Children |> 
                List.fold(fun (cr:InitFunctionResult) ch ->
                        let chResult = handleChild ch
                        let newFuncBody = cr.funcBody + "\n" + chResult.funcBody
                        {InitFunctionResult.funcBody = newFuncBody; localVariables = cr.localVariables@chResult.localVariables }
                    ) {InitFunctionResult.funcBody = ""; localVariables = [] }
        ret
        
    createInitFunctionCommon r l t typeDefinition funcBody iv initTasFunction testCaseFuncs

let createChoiceInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference) (children:ChChildInfo list) iv =     
    //let initChoice = match l with C -> init_c.initChoice | Ada -> init_a.initChoice
    let initTestCase_choice_child = match l with C -> init_c.initTestCase_choice_child | Ada -> init_a.initTestCase_choice_child
    let initChildWithInitFunc       = match l with C -> init_c.initChildWithInitFunc | Ada -> init_a.initChildWithInitFunc
    let typeDefinitionName = typeDefinition.longTypedefName l 
    let funcBody (p:CallerScope) v = 
        let childrenOut = 
            match v with
            | ChValue iv     -> 
                children |> 
                List.choose(fun chChild -> 
                    match chChild.Name.Value = iv.name with
                    | false -> None
                    | true  ->
                        match l with
                        | C ->
                            let chContent = chChild.chType.initFunction.initByAsn1Value ({p with arg = p.arg.getChChild l chChild.c_name chChild.chType.isIA5String}) iv.Value.kind
                            Some (init_c.initChoice p.arg.p (p.arg.getAcces l) chContent (chChild.presentWhenName (Some typeDefinition) l)) 
                        | Ada ->
                            let sChildTypeName = chChild.chType.typeDefintionOrReference.longTypedefName l
                            let sChildTempVarName = (ToC chChild.chType.id.AsString) + "_tmp"
                            let sChoiceTypeName = typeDefinition.longTypedefName l
                            let sChildName = chChild.c_name
                            let chContent = chChild.chType.initFunction.initByAsn1Value ({CallerScope.modName = t.id.ModName; arg = VALUE sChildTempVarName}) iv.Value.kind
                            Some (init_a.initChoice p.arg.p (p.arg.getAcces l) chContent (chChild.presentWhenName (Some typeDefinition) l) sChildTempVarName sChildTypeName sChoiceTypeName sChildName) 
                        ) 

            | _               -> raise(BugErrorException "UnexpectedValue")
        childrenOut |> Seq.head


    let testCaseFuncs = 
        let handleChild  (ch:ChChildInfo)  = 
            let sChildID (p:CallerScope) = 
                match l with
                | C     -> (ToC ch._present_when_name_private) + "_PRESENT"
                | Ada   ->
        //                    let modName = 
        //                        match ch.chType.typeDefintionOrReference with
        //                        | TypeDefinition  _ -> ToC ch.chType.id.ModName
        //                        | ReferenceToExistingDefinition ref -> 
        //                            match ref.programUnit with
        //                            | Some pu       -> pu
        //                            | None          -> ToC ch.chType.id.ModName
        //                    modName + "." + ((ToC ch._present_when_name_private) + "_PRESENT")
                    (ToC ch._present_when_name_private) + "_PRESENT"


            let len = ch.chType.initFunction.automaticTestCases.Length
            let sChildName = ch.c_name
            let sChildTypeDef = ch.chType.typeDefintionOrReference.longTypedefName l 
            ch.chType.initFunction.automaticTestCases |> Seq.take (min 5 len) |> Seq.toList |>
            List.map(fun atc -> 
                let fnc = atc.initTestCaseFunc
                let presentFunc (p:CallerScope) = 
                    let childContent =  
                        match l with
                        | C     -> fnc {p with arg = p.arg.getChChild l sChildName ch.chType.isIA5String} 
                        | Ada   -> fnc {p with arg = VALUE (sChildName + "_tmp")} 
                    let funcBody = initTestCase_choice_child p.arg.p (p.arg.getAcces l) (sChildID p) childContent.funcBody sChildName  sChildTypeDef typeDefinitionName
                    {InitFunctionResult.funcBody = funcBody; localVariables = childContent.localVariables}
                let combinedTestCase =
                    match atc.testCase.ContainsKey ch.chType.id with
                    | true      -> atc.testCase
                    | false     -> atc.testCase.Add(ch.chType.id, TcvAnyValue)
                {AutomaticTestCase.initTestCaseFunc = presentFunc; testCase = combinedTestCase } )

        children |>
        //if some alternatives have restricted to always ABSENT (via WITH COMPONENTS constraint) then do not produce a test case for them.
        List.filter (fun c -> c.Optionality.IsNone || c.Optionality = (Some Asn1AcnAst.Asn1ChoiceOptionality.ChoiceAlwaysPresent)) |>
        List.collect handleChild


    let initTasFunction (p:CallerScope) =
        let handleChild  (ch:ChChildInfo)  = 
            let sChildName = ch.c_name
            let sChildTypeDef = ch.chType.typeDefintionOrReference.longTypedefName l 
            let chp = {p with arg = p.arg.getChChild l sChildName ch.chType.isIA5String} 
            let childContent_funcBody, childContent_localVariables = 
                match ch.chType.initFunction.initFuncName with
                | None  ->
                    let fnc = ch.chType.initFunction.initTas
                    let childContent =  
                        match l with
                        | C     -> fnc chp
                        | Ada   -> fnc {p with arg = VALUE (sChildName + "_tmp")} 
                    childContent.funcBody, childContent.localVariables
                | Some fncName  ->
                    initChildWithInitFunc (chp.arg.getPointer l) fncName, []
            let funcBody = initTestCase_choice_child p.arg.p (p.arg.getAcces l) (ch.presentWhenName (Some typeDefinition) l) childContent_funcBody sChildName  sChildTypeDef typeDefinitionName

            {InitFunctionResult.funcBody = funcBody; localVariables = childContent_localVariables}
        match children with
        | x::_  -> handleChild x
        | _     -> {InitFunctionResult.funcBody = ""; localVariables = []}

        
    createInitFunctionCommon r l t typeDefinition funcBody iv initTasFunction testCaseFuncs

let createReferenceType (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (baseType:Asn1Type) =
    let initChildWithInitFunc       = match l with C -> init_c.initChildWithInitFunc | Ada -> init_a.initChildWithInitFunc
    let typeDefinitionName = ToC2(r.args.TypePrefix + o.tasName.Value)
    
    let t1              = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
    let t1WithExtensios = o.resolvedType;

    let bs = baseType.initFunction
    match TypesEquivalence.uperEquivalence t1 t1WithExtensios with
    | false -> 
        createInitFunctionCommon r l t typeDefinition bs.initByAsn1Value baseType.initialValue bs.initTas bs.automaticTestCases
    | true  ->
        let baseFncName = 
            match l with
            | C     -> typeDefinitionName + (nameSuffix l)
            | Ada   -> 
                match t.id.ModName = o.modName.Value with
                | true  -> typeDefinitionName + (nameSuffix l)
                | false -> (ToC o.modName.Value) + "." + (typeDefinitionName + (nameSuffix l))
        let initTasFunction (p:CallerScope) =
            let funcBody = initChildWithInitFunc (p.arg.getPointer l) baseFncName
            {InitFunctionResult.funcBody = funcBody; localVariables = []}
        createInitFunctionCommon r l t typeDefinition bs.initByAsn1Value baseType.initialValue initTasFunction bs.automaticTestCases
