module ConstraintsMapping
open System
open System.Numerics
open FsUtils
open Asn1AcnAst


let rec getBaseValue (v: Asn1Value) = 
    match v.kind with
    | RefValue (_, rv)  -> getBaseValue rv
    | _                 -> v



let foldBConstraint 
    singleValueContraintFunc
    rangeContraintFunc
    rangeContraint_val_MAXFunc
    rangeContraint_MIN_valFunc
    sizeContraintFunc         
    alphabetContraintFunc     
    unionConstraintFunc       
    intersectionConstraintFunc
    allExceptConstraintFunc   
    exceptConstraintFunc      
    rootConstraintFunc        
    rootConstraint2Func       
    (c:Asn1Ast.Asn1Constraint) =
    match c with
    | Asn1Ast.SingleValueContraint       rv                -> singleValueContraintFunc rv 
    | Asn1Ast.RangeContraint             (rv1,rv2,b1,b2)   -> rangeContraintFunc rv1 rv2 b1 b2 
    | Asn1Ast.RangeContraint_val_MAX     (rv,b)            -> rangeContraint_val_MAXFunc rv b 
    | Asn1Ast.RangeContraint_MIN_val     (rv,b)            -> rangeContraint_MIN_valFunc rv b 
    | Asn1Ast.SizeContraint              c                 -> sizeContraintFunc c 
    | Asn1Ast.AlphabetContraint          c                 -> alphabetContraintFunc c 
    | Asn1Ast.UnionConstraint            (c1,c2,b)         -> unionConstraintFunc c1 c2  b 
    | Asn1Ast.IntersectionConstraint     (c1,c2)           -> intersectionConstraintFunc c1 c2 
    | Asn1Ast.AllExceptConstraint        c                 -> allExceptConstraintFunc c 
    | Asn1Ast.ExceptConstraint           (c1,c2)           -> exceptConstraintFunc c1 c2 
    | Asn1Ast.RootConstraint             c1                -> rootConstraintFunc c1    
    | Asn1Ast.RootConstraint2            (c1,c2)           -> rootConstraint2Func c1 c2      
    | Asn1Ast.RangeContraint_MIN_MAX            _          -> raise(BugErrorException "Unexpected constraint type")
    | Asn1Ast.TypeInclusionConstraint           _          -> raise(BugErrorException "Unexpected constraint type")   
    | Asn1Ast.WithComponentConstraint           _          -> raise(BugErrorException "Unexpected constraint type")
    | Asn1Ast.WithComponentsConstraint          _          -> raise(BugErrorException "Unexpected constraint type")




let rec private getValueAsBigInteger (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | IntegerValue x -> x.Value
    | _                  -> raise(BugErrorException "Value is not of expected type")

let private getValueAsDouble (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | RealValue x -> x.Value
    | _                  -> raise(BugErrorException "Value is not of expected type")

let private posIntValGetter (r:Asn1Ast.AstRoot) (v:Asn1Ast.Asn1Value) =
    let sizeIntegerType = 
        {
            Asn1Ast.Asn1Type.Kind = Asn1Ast.Integer
            Asn1Ast.Asn1Type.Constraints = []
            Asn1Ast.Asn1Type.Location = FsUtils.emptyLocation
            Asn1Ast.Asn1Type.parameterizedTypeInstance = false

        }
    let newValue = ValuesMapping.mapValue r sizeIntegerType v
    match (getBaseValue newValue).kind with
    | IntegerValue x when x.Value >= 0I && x.Value <= BigInteger UInt32.MaxValue  -> uint32 x.Value
    | IntegerValue x when x.Value > BigInteger UInt32.MaxValue     -> raise(SemanticError(v.Location, (sprintf "Constant value '%A' is too large" x.Value)))
    | _                                 -> raise(SemanticError(v.Location, "Value is not of expected type"))

let private charGetter (r:Asn1Ast.AstRoot)  (v:Asn1Ast.Asn1Value) =
    let charType = 
        {
            Asn1Ast.Asn1Type.Kind = Asn1Ast.IA5String
            Asn1Ast.Asn1Type.Constraints = []
            Asn1Ast.Asn1Type.Location = FsUtils.emptyLocation
            Asn1Ast.Asn1Type.parameterizedTypeInstance = false

        }
    let newValue = ValuesMapping.mapValue r charType v
    match (getBaseValue newValue).kind with
    | StringValue vl    when vl.Value.Length = 1    -> vl.Value.ToCharArray().[0]
    | _                                         -> raise(SemanticError (v.Location, "Expecting a string with just one character"))

let private strGetter (r:Asn1Ast.AstRoot)  (v:Asn1Ast.Asn1Value) =
    let charType = 
        {
            Asn1Ast.Asn1Type.Kind = Asn1Ast.IA5String
            Asn1Ast.Asn1Type.Constraints = []
            Asn1Ast.Asn1Type.Location = FsUtils.emptyLocation
            Asn1Ast.Asn1Type.parameterizedTypeInstance = false
        }
    let newValue = ValuesMapping.mapValue r charType v
    match (getBaseValue newValue).kind with
    | StringValue vl            -> vl.Value
    | _                        -> raise(BugErrorException "Value is not of expected type")

let private strValueGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | StringValue vl            -> vl.Value
    | _                        -> raise(BugErrorException "Value is not of expected type")

let private octGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    |OctetStringValue vl            -> (vl, (v.id, v.Location)) 
    | _                             -> raise(BugErrorException "Value is not of expected type")

let private bitGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | BitStringValue vl            -> (vl, (v.id, v.Location))
    | _                             -> raise(BugErrorException "Value is not of expected type")

let private boolGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | BooleanValue vl            -> vl.Value
    | _                             -> raise(BugErrorException "Value is not of expected type")

let private enumGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type)  (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | EnumValue vl         -> vl.Value
    | _                             -> raise(BugErrorException "Value is not of expected type")

let private seqOfValueGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | SeqOfValue vl            -> vl
    | _                             -> raise(BugErrorException "Value is not of expected type")

let private seqValueGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | SeqValue vl            -> vl
    | _                             -> raise(BugErrorException "Value is not of expected type")

let private chValueGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    |ChValue v           -> v
    | _                             -> raise(BugErrorException "Value is not of expected type")


let rec private getRecursiveTypeConstraint valueGetter   (c:Asn1Ast.Asn1Constraint)   =
    foldBConstraint
        (fun rv                 -> SingleValueConstraint (valueGetter rv )) 
        (fun rv1 rv2 b1 b2      -> raise(BugErrorException "range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "range constraint is not expected here"))
        (fun c                  -> raise(BugErrorException "SizeContraint is not expected here"))
        (fun c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun c1 c2 b            -> 
            let c1 = getRecursiveTypeConstraint valueGetter c1 
            let c2 = getRecursiveTypeConstraint valueGetter c2 
            UnionConstraint (c1,c2,b))           
        (fun c1 c2             -> 
            let c1 = getRecursiveTypeConstraint valueGetter c1 
            let c2 = getRecursiveTypeConstraint valueGetter c2 
            IntersectionConstraint (c1,c2))           
        (fun c             -> 
            let c = getRecursiveTypeConstraint valueGetter c 
            AllExceptConstraint c)           
        (fun c1 c2             -> 
            let c1 = getRecursiveTypeConstraint valueGetter c1 
            let c2 = getRecursiveTypeConstraint valueGetter c2 
            ExceptConstraint (c1,c2))           
        (fun c             -> 
            let c = getRecursiveTypeConstraint valueGetter c 
            RootConstraint c)           
        (fun c1 c2             -> 
            let c1 = getRecursiveTypeConstraint valueGetter c1 
            let c2 = getRecursiveTypeConstraint valueGetter c2 
            RootConstraint2 (c1,c2))           
        c


let rec private getRangeTypeConstraint valueGetter  valueGetter2 (c:Asn1Ast.Asn1Constraint)   =
    foldBConstraint 
        (fun rv                 -> RangeSingleValueConstraint (valueGetter2 rv )) 
        (fun rv1 rv2 b1 b2      -> RangeContraint (valueGetter rv1 ,valueGetter rv2, b1,b2) )
        (fun rv b               -> RangeContraint_val_MAX (valueGetter rv, b))
        (fun rv b               -> RangeContraint_MIN_val (valueGetter rv, b))
        (fun c                  -> raise(BugErrorException "SizeContraint is not expected here"))
        (fun c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun c1 c2 b            -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeUnionConstraint (c1,c2,b))           
        (fun c1 c2             -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeIntersectionConstraint (c1,c2))           
        (fun c                 -> 
            let c = getRangeTypeConstraint valueGetter valueGetter2 c 
            RangeAllExceptConstraint c)           
        (fun c1 c2             -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeExceptConstraint (c1,c2))
        (fun c                 -> 
            let c = getRangeTypeConstraint valueGetter valueGetter2 c 
            RangeRootConstraint c)
        (fun c1 c2             -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeRootConstraint2 (c1,c2))           
        c


let rec private getSizeTypeConstraint (r:Asn1Ast.AstRoot) valueGetter  (c:Asn1Ast.Asn1Constraint)   =
    foldBConstraint 
        (fun rv                 -> SizeSingleValueConstraint (valueGetter rv)) 
        (fun rv1 rv2 b1 b2      -> raise(BugErrorException "Range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun c                  -> 
            let posIntCon = getRangeTypeConstraint (posIntValGetter r) (posIntValGetter r)  c
            SizeContraint posIntCon)
        (fun c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun c1 c2 b            -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2 
            SizeUnionConstraint (c1,c2,b))           
        (fun c1 c2             -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2
            SizeIntersectionConstraint (c1,c2))           
        (fun c                 -> 
            let c = getSizeTypeConstraint r valueGetter c 
            SizeAllExceptConstraint c)           
        (fun c1 c2             -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2
            SizeExceptConstraint (c1,c2))
        (fun c                 -> 
            let c = getSizeTypeConstraint r valueGetter c 
            SizeRootConstraint c)
        (fun c1 c2             -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2
            SizeRootConstraint2 (c1,c2))           
        c


let rec private getStringTypeConstraint (r:Asn1Ast.AstRoot) valueGetter  (c:Asn1Ast.Asn1Constraint)   =
    foldBConstraint 
        (fun rv                 -> StrSingleValueConstraint (valueGetter rv)) 
        (fun rv1 rv2 b1 b2      -> raise(SemanticError(rv1.Location, "Range constraint is not expected here in string types")))
        (fun rv b               -> raise(SemanticError(rv.Location, "Range constraint is not expected here in string types")))
        (fun rv b               -> raise(SemanticError(rv.Location, "Range constraint is not expected here in string types")))
        (fun c                  -> 
            let posIntCon = getRangeTypeConstraint (posIntValGetter r) (posIntValGetter r) c
            StrSizeContraint posIntCon)
        (fun c                  -> 
            let charCons = getRangeTypeConstraint (charGetter r) (strGetter r) c
            AlphabetContraint charCons)
        (fun c1 c2 b            -> 
            let c1 = getStringTypeConstraint r valueGetter c1
            let c2 = getStringTypeConstraint r valueGetter c2
            StrUnionConstraint (c1,c2,b))           
        (fun c1 c2             -> 
            let c1 = getStringTypeConstraint r valueGetter c1
            let c2 = getStringTypeConstraint r valueGetter c2
            StrIntersectionConstraint (c1,c2))           
        (fun c                 -> 
            let c = getStringTypeConstraint r valueGetter c
            StrAllExceptConstraint c)           
        (fun c1 c2             -> 
            let c1 = getStringTypeConstraint r valueGetter c1
            let c2 = getStringTypeConstraint r valueGetter c2
            StrExceptConstraint (c1,c2))
        (fun c                 -> 
            let c = getStringTypeConstraint r valueGetter c
            StrRootConstraint c)
        (fun c1 c2             -> 
            let c1 = getStringTypeConstraint r valueGetter c1
            let c2 = getStringTypeConstraint r valueGetter c2
            StrRootConstraint2 (c1,c2))           
        c


let getIntegerTypeConstraint (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRangeTypeConstraint (getValueAsBigInteger r t) (getValueAsBigInteger r t)
let getRealTypeConstraint    (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRangeTypeConstraint (getValueAsDouble r t) (getValueAsDouble r t)
let getIA5StringConstraint   (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getStringTypeConstraint r (strValueGetter r t)
let getOctetStringConstraint (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getSizeTypeConstraint r (octGetter r t)
let getBitStringConstraint   (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getSizeTypeConstraint r (bitGetter r t)
let getBoolConstraint        (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (boolGetter r t)
let getEnumConstraint        (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (enumGetter r t)
let getSequenceOfConstraint  (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getSizeTypeConstraint r (seqOfValueGetter r t)
let getSequenceConstraint    (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (seqValueGetter r t)
let getChoiceConstraint      (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (chValueGetter r t)


