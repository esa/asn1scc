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
            let b = true
            match b with true -> ()
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

















let nameSuffix l = match l with C -> "_Initialize" | Ada -> "_Init"

let getFuncName2 (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)  (typeDefinition:TypeDefintionOrReference) =
    getFuncNameGeneric typeDefinition (nameSuffix l)


let createInitFunctionCommon (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage)   (o:Asn1AcnAst.Asn1Type) (typeDefinition:TypeDefintionOrReference) initByAsn1Value (iv:Asn1ValueKind) (initTasFunction:CallerScope  -> InitFunctionResult) automaticTestCases =
    let funcName            = 
        //match o.id.tasInfo with
        //| None -> None
        //| Some _ -> 
            getFuncName2 r l typeDefinition
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
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | IntegerValue iv   -> iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initInteger (p.arg.getValue l) vl

    let integerVals = EncodeDecodeTestCase.IntegerAutomaticTestCaseValues r t o
    
    let allCons = DastValidate2.getIntSimplifiedConstraints r o.isUnsigned o.AllCons
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
            {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] }        )

    createInitFunctionCommon r l t  typeDefinition funcBody iv tasInitFunc testCaseFuncs

let createRealInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Real) (typeDefinition:TypeDefintionOrReference) iv = 
    let initReal = match l with C -> init_c.initReal | Ada -> init_a.initReal
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | RealValue iv   -> iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initReal (p.arg.getValue l) vl

    let realVals = EncodeDecodeTestCase.RealAutomaticTestCaseValues r t o
    let testCaseFuncs = 
        realVals |> 
        List.map (fun vl -> 
            let initTestCaseFunc = (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initReal (p.arg.getValue l) vl; localVariables=[]}) 
            {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] } )

    let tasInitFunc (p:CallerScope)  = 
        match isValidValueRanged o.AllCons 0.0  with
        | false    -> 
            match realVals with 
            | x::_ -> {InitFunctionResult.funcBody = initReal (p.arg.getValue l) x;  localVariables=[]} 
            | [] -> {InitFunctionResult.funcBody = initReal (p.arg.getValue l) 0.0;  localVariables=[]}
        | true  -> {InitFunctionResult.funcBody = initReal (p.arg.getValue l) 0.0;  localVariables=[]}

    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs

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
let createIA5StringInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.StringType   ) (typeDefinition:TypeDefintionOrReference) iv = 
    let initIA5String = match l with C -> init_c.initIA5String | Ada -> init_a.initIA5String
    let initTestCaseIA5String = match l with C -> init_c.initTestCaseIA5String | Ada -> init_a.initTestCaseIA5String

    
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | StringValue iv   -> 
                iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        let arrNuls = [0 .. (int o.maxSize.uper- vl.Length)]|>Seq.map(fun x -> variables_a.PrintStringValueNull())
        initIA5String (p.arg.getValue l) (vl.Replace("\"","\"\"")) arrNuls

    let ii = t.id.SeqeuenceOfLevel + 1
    let i = sprintf "i%d" ii
    //let visibleChars = o.uperCharSet |> Seq.filter(fun c -> not (System.Char.IsControl c))
    let bAlpha = o.uperCharSet.Length < 128
    let arrAsciiCodes = o.uperCharSet |> Array.map(fun x -> BigInteger (System.Convert.ToInt32 x))
    let testCaseFuncs = 
        let seqOfCase (nSize:BigInteger)  = 
            let initTestCaseFunc (p:CallerScope) = 
                let td = (o.typeDef.[l]).longTypedefName l (ToC p.modName)
                let funcBody = initTestCaseIA5String p.arg.p (p.arg.getAcces l) (nSize) ((o.maxSize.uper+1I)) i td bAlpha arrAsciiCodes (BigInteger arrAsciiCodes.Length) false
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
        let td = (o.typeDef.[l]).longTypedefName l (ToC p.modName)
        let funcBody = initTestCaseIA5String p.arg.p (p.arg.getAcces l) ( (o.maxSize.uper+1I)) ( (o.maxSize.uper+1I)) i td bAlpha arrAsciiCodes (BigInteger arrAsciiCodes.Length) true
        let lvars = match l with C -> [] | Ada -> [SequenceOfIndex (ii, None)]
        {InitFunctionResult.funcBody = funcBody; localVariables=lvars}
    createInitFunctionCommon r l t typeDefinition funcBody iv zero testCaseFuncs

let createOctetStringInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.OctetString ) (typeDefinition:TypeDefintionOrReference) iv (isValidFunction:IsValidFunction option) = 
    let initFixSizeBitOrOctString_bytei = match l with C -> init_c.initFixSizeBitOrOctString_bytei  | Ada -> init_a.initFixSizeBitOrOctString_bytei
    let initFixSizeBitOrOctString       = match l with C -> init_c.initFixSizeBitOrOctString        | Ada -> init_a.initFixSizeBitOrOctString
    let initFixVarSizeBitOrOctString    = match l with C -> init_c.initFixVarSizeBitOrOctString     | Ada -> init_a.initFixVarSizeBitOrOctString
    let initTestCaseOctetString         = match l with C -> init_c.initTestCaseOctetString     | Ada -> init_a.initTestCaseOctetString

    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let bytes = 
            match v.ActualValue with
            | OctetStringValue iv -> iv
            | BitStringValue iv   -> bitStringValueToByteArray (StringLoc.ByValue iv) |> Seq.toList
            | _                 -> raise(BugErrorException "UnexpectedValue")
        let arrsBytes = bytes |> List.mapi(fun i b -> initFixSizeBitOrOctString_bytei p.arg.p (p.arg.getAcces l) ((i+l.ArrayStartIndex).ToString()) (sprintf "%x" b))
        match o.isFixedSize with
        | true  -> initFixSizeBitOrOctString p.arg.p (p.arg.getAcces l) arrsBytes
        | false -> initFixVarSizeBitOrOctString p.arg.p (p.arg.getAcces l) (BigInteger arrsBytes.Length) arrsBytes

    let anonyms =
        o.AllCons |> 
        List.map DastFold.getValueFromSizeableConstraint |> 
        List.collect id |>
        List.map(fun (v,_) -> DAstVariables.printOctetStringValueAsCompoundLitteral l "" o (v|>List.map(fun bl -> bl.Value)))

    let testCaseFuncs, tasInitFunc =
        match anonyms with
        | []  ->
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = sprintf "i%d" ii
            let seqOfCase (nSize:BigInteger) = 
                let initTestCaseFunc (p:CallerScope) = 
                    let funcBody = initTestCaseOctetString p.arg.p (p.arg.getAcces l) nSize i (o.minSize.uper = o.maxSize.uper) false o.minSize.uper
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
                let funcBody = initTestCaseOctetString p.arg.p (p.arg.getAcces l) o.maxSize.uper i (isFixedSize) true o.minSize.uper
                let lvars = match l with C -> [] | Ada -> [SequenceOfIndex (ii, None)]
                {InitFunctionResult.funcBody = funcBody; localVariables=lvars}

            testCaseFuncs, zero
        | _ ->
            let ret = 
                anonyms |> 
                List.map(fun (compLit) ->
                    let initTestCaseFunc (p:CallerScope) =
                        let ret = sprintf "%s%s%s;" (p.arg.getValue l) l.AssignOperator compLit
                        {InitFunctionResult.funcBody = ret; localVariables=[]}
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] })
            ret, ret.Head.initTestCaseFunc
    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs

let createNullTypeInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.NullType    ) (typeDefinition:TypeDefintionOrReference) iv = 
    let initNull = match l with C -> init_c.initNull | Ada -> init_a.initNull
    let funcBody (p:CallerScope) v = initNull (p.arg.getValue l) 
    let testCaseFuncs = [{AutomaticTestCase.initTestCaseFunc = (fun p -> {InitFunctionResult.funcBody = initNull (p.arg.getValue l); localVariables=[]}); testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)]} ]
    createInitFunctionCommon r l t typeDefinition funcBody iv testCaseFuncs.Head.initTestCaseFunc testCaseFuncs

let createBitStringInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.BitString   ) (typeDefinition:TypeDefintionOrReference) iv (isValidFunction:IsValidFunction option)= 
    let initFixSizeBitOrOctString_bytei = match l with C -> init_c.initFixSizeBitOrOctString_bytei  | Ada -> init_a.initFixSizeBitOrOctString_bytei
    let initFixSizeBitOrOctString       = match l with C -> init_c.initFixSizeBitOrOctString        | Ada -> init_a.initFixSizeBitOrOctString
    let initFixVarSizeBitOrOctString    = match l with C -> init_c.initFixVarSizeBitOrOctString     | Ada -> init_a.initFixVarSizeBitOrOctString
    let initTestCaseBitString           = match l with C -> init_c.initTestCaseBitString     | Ada -> init_a.initTestCaseBitString
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let bytes = 
            match v.ActualValue with
            | BitStringValue iv     -> bitStringValueToByteArray (StringLoc.ByValue iv) |> Seq.toList
            | OctetStringValue iv   -> iv
            | _                     -> raise(BugErrorException "UnexpectedValue")
        let arrsBytes = bytes |> List.mapi(fun i b -> initFixSizeBitOrOctString_bytei p.arg.p (p.arg.getAcces l) ((i+l.ArrayStartIndex).ToString()) (sprintf "%x" b))
        match o.minSize.uper = o.maxSize.uper with
        | true  -> initFixSizeBitOrOctString p.arg.p (p.arg.getAcces l) arrsBytes
        | false -> initFixVarSizeBitOrOctString p.arg.p (p.arg.getAcces l) (BigInteger arrsBytes.Length) arrsBytes

    let anonyms =
        o.AllCons |> 
        List.map DastFold.getValueFromSizeableConstraint |> 
        List.collect id |>
        List.map(fun (v,_) -> DAstVariables.printBitStringValueAsCompoundLitteral l "" o v.Value)

    let testCaseFuncs, tasInitFunc =
        match anonyms with
        | []  ->
            let ii = t.id.SeqeuenceOfLevel + 1
            let i = sprintf "i%d" ii
            let seqOfCase (nSize:BigInteger) = 
                let initTestCaseFunc (p:CallerScope) = 
                    let nSizeCeiled =  if nSize % 8I = 0I then nSize else (nSize + (8I - nSize % 8I)) 
                    let funcBody = initTestCaseBitString p.arg.p (p.arg.getAcces l) nSize (nSizeCeiled) i (o.minSize.uper = o.maxSize.uper) false o.minSize.uper
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

                let funcBody = initTestCaseBitString p.arg.p (p.arg.getAcces l) nSize (nSizeCeiled) i (isFixedSize) true o.minSize.uper
                let lvars = match l with C -> [] | Ada -> [SequenceOfIndex (ii, None)]
                {InitFunctionResult.funcBody = funcBody; localVariables=lvars}
            testCaseFuncs, zero
        | _ ->
            let ret = 
                anonyms |> 
                List.map(fun compLit ->
                    let retFunc (p:CallerScope) =
                        let ret = sprintf "%s%s%s;" (p.arg.getValue l) l.AssignOperator compLit
                        {InitFunctionResult.funcBody = ret; localVariables=[]}
                    {AutomaticTestCase.initTestCaseFunc = retFunc; testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] })
            ret, ret.Head.initTestCaseFunc
    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs

let createBooleanInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Boolean     ) (typeDefinition:TypeDefintionOrReference) iv = 
    let initBoolean = match l with C -> init_c.initBoolean | Ada -> init_a.initBoolean
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | BooleanValue iv   -> iv
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initBoolean (p.arg.getValue l) vl



    let testCaseFuncs = 
        EncodeDecodeTestCase.BooleanAutomaticTestCaseValues r t o |> 
        List.map (fun vl -> {AutomaticTestCase.initTestCaseFunc = (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initBoolean (p.arg.getValue l) vl; localVariables = []}); testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] })

    let tasInitFunc (p:CallerScope)  = 
        match isValidValueGeneric o.AllCons (=) false  with
        | true    -> {InitFunctionResult.funcBody = initBoolean (p.arg.getValue l) false; localVariables = []}
        | false     -> {InitFunctionResult.funcBody = initBoolean (p.arg.getValue l) true; localVariables = []}

    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs







let createObjectIdentifierInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.ObjectIdentifier     ) (typeDefinition:TypeDefintionOrReference) iv = 
    let initObjectIdentifier_vali = match l with C -> init_c.initObjectIdentifier_vali | Ada -> init_a.initObjectIdentifier_vali
    let initObjectIdentifier = match l with C -> init_c.initObjectIdentifier | Ada -> init_a.initObjectIdentifier
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let bytes = 
            match v.ActualValue with
            | ObjOrRelObjIdValue iv   -> iv.Values |> List.map fst
            | _                 -> raise(BugErrorException "UnexpectedValue")
        let arrsBytes = bytes |> List.mapi(fun i b -> initObjectIdentifier_vali p.arg.p (p.arg.getAcces l) ((i+l.ArrayStartIndex).ToString()) b)
        initObjectIdentifier p.arg.p (p.arg.getAcces l) (BigInteger arrsBytes.Length) arrsBytes
    let testCaseFuncs = 
        EncodeDecodeTestCase.ObjectIdentifierAutomaticTestCaseValues r t o |> 
        List.map (fun vl -> 
            {AutomaticTestCase.initTestCaseFunc = (fun (p:CallerScope) -> 
                let arrsBytes = vl |> List.mapi(fun i b -> initObjectIdentifier_vali p.arg.p (p.arg.getAcces l) ((i+l.ArrayStartIndex).ToString()) b)
                {InitFunctionResult.funcBody = initObjectIdentifier (p.arg.p) (p.arg.getAcces l) (BigInteger vl.Length)  arrsBytes; localVariables = []}); testCaseTypeIDsMap = Map.ofList [(t.id, TcvAnyValue)] })

    let tasInitFunc (p:CallerScope)  = 
        {InitFunctionResult.funcBody = initObjectIdentifier (p.arg.p) (p.arg.getAcces l) 0I []; localVariables = []}

    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs

let createTimeTypeInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.TimeType     ) (typeDefinition:TypeDefintionOrReference) iv = 
    let init_Asn1LocalTime                  = match l with C -> init_c.init_Asn1LocalTime                   | Ada -> init_a.init_Asn1LocalTime
    let init_Asn1UtcTime                    = match l with C -> init_c.init_Asn1UtcTime                     | Ada -> init_a.init_Asn1UtcTime
    let init_Asn1LocalTimeWithTimeZone      = match l with C -> init_c.init_Asn1LocalTimeWithTimeZone       | Ada -> init_a.init_Asn1LocalTimeWithTimeZone
    let init_Asn1Date                       = match l with C -> init_c.init_Asn1Date                        | Ada -> init_a.init_Asn1Date
    let init_Asn1Date_LocalTime             = match l with C -> init_c.init_Asn1Date_LocalTime              | Ada -> init_a.init_Asn1Date_LocalTime
    let init_Asn1Date_UtcTime               = match l with C -> init_c.init_Asn1Date_UtcTime                | Ada -> init_a.init_Asn1Date_UtcTime
    let init_Asn1Date_LocalTimeWithTimeZone = match l with C -> init_c.init_Asn1Date_LocalTimeWithTimeZone  | Ada -> init_a.init_Asn1Date_LocalTimeWithTimeZone

    let initByBalue (p:CallerScope) (iv:Asn1DateTimeValue) = 
        match iv with
        |Asn1LocalTimeValue                  tv        -> init_Asn1LocalTime p.arg.p (p.arg.getAcces l) tv
        |Asn1UtcTimeValue                    tv        -> init_Asn1UtcTime p.arg.p (p.arg.getAcces l) tv
        |Asn1LocalTimeWithTimeZoneValue      (tv,tz)   -> init_Asn1LocalTimeWithTimeZone p.arg.p (p.arg.getAcces l) tv tz
        |Asn1DateValue                       dt        -> init_Asn1Date p.arg.p (p.arg.getAcces l) dt
        |Asn1Date_LocalTimeValue             (dt,tv)   -> init_Asn1Date_LocalTime p.arg.p (p.arg.getAcces l) dt tv
        |Asn1Date_UtcTimeValue               (dt,tv)   -> init_Asn1Date_UtcTime  p.arg.p (p.arg.getAcces l) dt tv
        |Asn1Date_LocalTimeWithTimeZoneValue (dt,tv,tz)-> init_Asn1Date_LocalTimeWithTimeZone p.arg.p (p.arg.getAcces l) dt tv tz

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

    createInitFunctionCommon r l t typeDefinition funcBody iv tasInitFunc testCaseFuncs











let mergeMaps (m1:Map<'key,'value>) (m2:Map<'key,'value>) =
    Map.fold (fun (nm:Map<'key,'value>) key value -> match nm.ContainsKey key with false -> nm.Add(key,value) | true -> nm) m1 m2
    

let createEnumeratedInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Enumerated  )  (typeDefinition:TypeDefintionOrReference) iv = 
    let initEnumerated = match l with C -> init_c.initEnumerated | Ada -> init_a.initEnumerated
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
            | EnumValue iv      -> o.items |> Seq.find(fun x -> x.Name.Value = iv)
            | _                 -> raise(BugErrorException "UnexpectedValue")
        initEnumerated (p.arg.getValue l) (vl.getBackendName (Some typeDefinition) l)
    let testCaseFuncs = 
        EncodeDecodeTestCase.EnumeratedAutomaticTestCaseValues2 r t o |> 
        List.map (fun vl -> 
            {
                AutomaticTestCase.initTestCaseFunc = (fun (p:CallerScope) -> {InitFunctionResult.funcBody = initEnumerated (p.arg.getValue l) (vl.getBackendName (Some typeDefinition) l); localVariables=[]}); 
                testCaseTypeIDsMap = Map.ofList [(t.id, (TcvEnumeratedValue vl.Name.Value))] 
            })
    createInitFunctionCommon r l t typeDefinition funcBody iv testCaseFuncs.Head.initTestCaseFunc testCaseFuncs

let createSequenceOfInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.SequenceOf  ) (typeDefinition:TypeDefintionOrReference) (childType:Asn1Type) iv = 
    let initFixedSequenceOf = match l with C -> init_c.initFixedSequenceOf | Ada -> init_a.initFixedSequenceOf
    let initVarSizeSequenceOf = match l with C -> init_c.initVarSizeSequenceOf | Ada -> init_a.initVarSizeSequenceOf
    let initTestCaseSizeSequenceOf_innerItem = match l with C -> init_c.initTestCaseSizeSequenceOf_innerItem | Ada -> init_a.initTestCaseSizeSequenceOf_innerItem
    let initTestCaseSizeSequenceOf = match l with C -> init_c.initTestCaseSizeSequenceOf | Ada -> init_a.initTestCaseSizeSequenceOf
    let initChildWithInitFunc       = match l with C -> init_c.initChildWithInitFunc | Ada -> init_a.initChildWithInitFunc
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let vl = 
            match v.ActualValue with
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
        match o.isFixedSize with
        | true  -> initFixedSequenceOf vl
        | false -> initVarSizeSequenceOf p.arg.p (p.arg.getAcces l) (BigInteger vl.Length) vl

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
                        let childCase = atc.initTestCaseFunc ({p with arg = p.arg.getArrayItem l i childType.isIA5String})
                        let funcBody = initTestCaseSizeSequenceOf p.arg.p (p.arg.getAcces l) None nSize (o.minSize.uper = o.maxSize.uper) [childCase.funcBody] false i
                        {InitFunctionResult.funcBody = funcBody; localVariables= (SequenceOfIndex (ii, None))::childCase.localVariables }
                    let combinedTestCase = atc.testCaseTypeIDsMap.Add(t.id, TcvSizeableTypeValue nSize)
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = combinedTestCase }
                | _             ->
                    let initTestCaseFunc (p:CallerScope) = 
                        let arrsInnerItems, childLocalVars = 
                            childTestCases |> 
                            List.mapi(fun idx atc -> 
                                let sChildItem = atc.initTestCaseFunc ({p with arg = p.arg.getArrayItem l i childType.isIA5String})
                                let funcBody = initTestCaseSizeSequenceOf_innerItem (idx=0) (idx = childTestCases.Length-1) idx.AsBigInt sChildItem.funcBody i (BigInteger childTestCases.Length)
                                (funcBody, (SequenceOfIndex (ii, None))::sChildItem.localVariables)) |>
                            List.unzip
                        let funcBody = initTestCaseSizeSequenceOf p.arg.p (p.arg.getAcces l) None  nSize (o.minSize.uper = o.maxSize.uper) arrsInnerItems true i 
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

    let initTasFunction (p:CallerScope) =
        let initCountValue = Some o.minSize.uper
        let chp = {p with arg = p.arg.getArrayItem l i childType.isIA5String}
        let childInitRes_funcBody, childInitRes_localVariables = 
            match childType.initFunction.initFuncName with
            | None  ->
                let childInitRes = childType.initFunction.initTas chp
                childInitRes.funcBody, childInitRes.localVariables
            | Some fncName  ->
                initChildWithInitFunc (chp.arg.getPointer l) fncName, []
        let isFixedSize =
            match t.getBaseType r with
            | None      -> o.isFixedSize
            | Some bs  -> 
                match bs.Kind with
                | Asn1AcnAst.SequenceOf bo -> bo.isFixedSize
                | _                        -> raise(BugErrorException "UnexpectedType")

        let funcBody = initTestCaseSizeSequenceOf p.arg.p (p.arg.getAcces l) initCountValue o.maxSize.uper (isFixedSize) [childInitRes_funcBody] false i
        {InitFunctionResult.funcBody = funcBody; localVariables= (SequenceOfIndex (ii, None))::childInitRes_localVariables }
        
    createInitFunctionCommon r l t typeDefinition funcBody iv  initTasFunction testCaseFuncs

let createSequenceInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Sequence) (typeDefinition:TypeDefintionOrReference) (children:SeqChildInfo list) iv = 
    let initSequence                = match l with C -> init_c.initSequence | Ada -> init_a.initSequence
    let initSequence_optionalChild  = match l with C -> init_c.initSequence_optionalChild | Ada -> init_a.initSequence_optionalChild
    let initTestCase_sequence_child = match l with C -> init_c.initTestCase_sequence_child | Ada -> init_a.initTestCase_sequence_child
    let initTestCase_sequence_child_opt = match l with C -> init_c.initTestCase_sequence_child_opt | Ada -> init_a.initTestCase_sequence_child_opt
    let initChildWithInitFunc       = match l with C -> init_c.initChildWithInitFunc | Ada -> init_a.initChildWithInitFunc

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
                            | Some _    -> Some (initSequence_optionalChild p.arg.p (p.arg.getAcces l) (seqChild.getBackendName l) "0" "")
                        | Some chv  ->
                            let chContent = seqChild.Type.initFunction.initByAsn1Value ({p with arg = p.arg.getSeqChild l (seqChild.getBackendName l) seqChild.Type.isIA5String}) chv.Value.kind
                            match seqChild.Optionality with
                            | None      -> Some chContent
                            | Some _    -> Some (initSequence_optionalChild p.arg.p (p.arg.getAcces l) (seqChild.getBackendName l) "1" chContent)
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
                        let chContent =  atc.initTestCaseFunc {p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String} 
                        let funcBody = initTestCase_sequence_child p.arg.p (p.arg.getAcces l) (ch.getBackendName l) chContent.funcBody ch.Optionality.IsSome
                        {InitFunctionResult.funcBody = funcBody; localVariables = chContent.localVariables }
                    let combinedTestCase =
                        match atc.testCaseTypeIDsMap.ContainsKey ch.Type.id with
                        | true      -> atc.testCaseTypeIDsMap
                        | false     -> atc.testCaseTypeIDsMap.Add(ch.Type.id, TcvAnyValue)
                    {AutomaticTestCase.initTestCaseFunc = initTestCaseFunc; testCaseTypeIDsMap = combinedTestCase }
                let nonPresenceFunc =  
                    let initTestCaseFunc (p:CallerScope) = 
                        let funcBody = initTestCase_sequence_child_opt p.arg.p (p.arg.getAcces l) (ch.getBackendName l)
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

    let initTasFunction (p:CallerScope) =
        let handleChild  (ch:Asn1Child)  = 
            let presentFunc (defaultValue  : Asn1AcnAst.Asn1Value option) = 
                match defaultValue with
                | None  ->
                    match ch.Type.initFunction.initFuncName with
                    | None  ->
                        match ch.Type.typeDefintionOrReference with
                        | ReferenceToExistingDefinition    rf   when (not rf.definedInRtl) ->
                            let fncName = (ch.Type.typeDefintionOrReference.longTypedefName l) + (nameSuffix l)
                            let chP = {p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String} 
                            let chContent =  initChildWithInitFunc (chP.arg.getPointer l) fncName
                            let funcBody = initTestCase_sequence_child p.arg.p (p.arg.getAcces l) (ch.getBackendName l) chContent ch.Optionality.IsSome
                            {InitFunctionResult.funcBody = funcBody; localVariables = [] }
                        | _       ->
                            let fnc = ch.Type.initFunction.initTas
                            let chContent =  fnc {p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String} 
                            let funcBody = initTestCase_sequence_child p.arg.p (p.arg.getAcces l) (ch.getBackendName l) chContent.funcBody ch.Optionality.IsSome
                            {InitFunctionResult.funcBody = funcBody; localVariables = chContent.localVariables }
                        
                    | Some fncName  ->
                        let chP = {p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String} 
                        let chContent =  initChildWithInitFunc (chP.arg.getPointer l) fncName
                        let funcBody = initTestCase_sequence_child p.arg.p (p.arg.getAcces l) (ch.getBackendName l) chContent ch.Optionality.IsSome
                        {InitFunctionResult.funcBody = funcBody; localVariables = [] }
                | Some dv    ->
                    let fnc = ch.Type.initFunction.initByAsn1Value
                    let chContent =  fnc {p with arg = p.arg.getSeqChild l (ch.getBackendName l) ch.Type.isIA5String} (mapValue dv).kind
                    let funcBody = initTestCase_sequence_child p.arg.p (p.arg.getAcces l) (ch.getBackendName l) chContent ch.Optionality.IsSome
                    {InitFunctionResult.funcBody = funcBody; localVariables = [] }
                    
                    
            let nonPresenceFunc () =  
                let funcBody = initTestCase_sequence_child_opt p.arg.p (p.arg.getAcces l) (ch.getBackendName l)
                {InitFunctionResult.funcBody = funcBody; localVariables = [] }
            match ch.Optionality with
            | None                              -> presentFunc None
            | Some (Asn1AcnAst.Optional opt)    -> presentFunc opt.defaultValue
            | Some (Asn1AcnAst.AlwaysAbsent)    -> nonPresenceFunc ()
            | Some (Asn1AcnAst.AlwaysPresent)   -> presentFunc None
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
        
    createInitFunctionCommon r l t typeDefinition initByAsn1ValueFnc iv initTasFunction testCaseFuncs

let createChoiceInitFunc (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.Choice) (typeDefinition:TypeDefintionOrReference) (children:ChChildInfo list) iv =     
    //let initChoice = match l with C -> init_c.initChoice | Ada -> init_a.initChoice
    let initTestCase_choice_child = match l with C -> init_c.initTestCase_choice_child | Ada -> init_a.initTestCase_choice_child
    let initChildWithInitFunc       = match l with C -> init_c.initChildWithInitFunc | Ada -> init_a.initChildWithInitFunc
    let typeDefinitionName = typeDefinition.longTypedefName l 
    let funcBody (p:CallerScope) (v:Asn1ValueKind) = 
        let childrenOut = 
            match v.ActualValue with
            | ChValue iv     -> 
                children |> 
                List.choose(fun chChild -> 
                    match chChild.Name.Value = iv.name with
                    | false -> None
                    | true  ->
                        match l with
                        | C ->
                            let chContent = chChild.chType.initFunction.initByAsn1Value ({p with arg = p.arg.getChChild l (chChild.getBackendName l) chChild.chType.isIA5String}) iv.Value.kind
                            Some (init_c.initChoice p.arg.p (p.arg.getAcces l) chContent (chChild.presentWhenName (Some typeDefinition) l)) 
                        | Ada ->
                            let sChildTypeName = chChild.chType.typeDefintionOrReference.longTypedefName l
                            let sChildTempVarName = (ToC chChild.chType.id.AsString) + "_tmp"
                            let sChoiceTypeName = typeDefinition.longTypedefName l
                            let sChildName = (chChild.getBackendName l)
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
                    (ToC ch._present_when_name_private) + "_PRESENT"


            let len = ch.chType.initFunction.automaticTestCases.Length
            let sChildName = (ch.getBackendName l)
            let sChildTypeDef = ch.chType.typeDefintionOrReference.longTypedefName l 
            ch.chType.initFunction.automaticTestCases (*|> Seq.take (min 5 len)*) |> Seq.toList |>
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
            let sChildName = (ch.getBackendName l)
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
                    match l with
                    | C     -> initChildWithInitFunc (chp.arg.getPointer l) fncName, []
                    | Ada   -> initChildWithInitFunc (sChildName + "_tmp") fncName, []
            let funcBody = initTestCase_choice_child p.arg.p (p.arg.getAcces l) (ch.presentWhenName (Some typeDefinition) l) childContent_funcBody sChildName  sChildTypeDef typeDefinitionName

            {InitFunctionResult.funcBody = funcBody; localVariables = childContent_localVariables}
        match children with
        | x::_  -> handleChild x
        | _     -> {InitFunctionResult.funcBody = ""; localVariables = []}

        
    createInitFunctionCommon r l t typeDefinition funcBody iv initTasFunction testCaseFuncs

let createReferenceType (r:Asn1AcnAst.AstRoot) (l:ProgrammingLanguage) (t:Asn1AcnAst.Asn1Type) (o :Asn1AcnAst.ReferenceType) (typeDefinition:TypeDefintionOrReference) (baseType:Asn1Type) =
    let initChildWithInitFunc       = match l with C -> init_c.initChildWithInitFunc | Ada -> init_a.initChildWithInitFunc
    let moduleName, typeDefinitionName = //ToC2(r.args.TypePrefix + o.tasName.Value)
        let t1 = Asn1AcnAstUtilFunctions.GetActualTypeByName r o.modName o.tasName
        t1.FT_TypeDefintion.[l].programUnit, t1.FT_TypeDefintion.[l].typeName
    
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
                | false -> moduleName + "." + (typeDefinitionName + (nameSuffix l))
        let initTasFunction (p:CallerScope) =
            let funcBody = initChildWithInitFunc (p.arg.getPointer l) baseFncName
            {InitFunctionResult.funcBody = funcBody; localVariables = []}
        createInitFunctionCommon r l t typeDefinition bs.initByAsn1Value baseType.initialValue initTasFunction bs.automaticTestCases
    