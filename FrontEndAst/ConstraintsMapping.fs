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
    withComponentConstraintFunc     
    withComponentsConstraintFunc     
    (c:Asn1Ast.Asn1Constraint) =
    match c with
    | Asn1Ast.SingleValueContraint       (s, rv)              -> singleValueContraintFunc s rv 
    | Asn1Ast.RangeContraint             (s, rv1,rv2,b1,b2)   -> rangeContraintFunc s rv1 rv2 b1 b2 
    | Asn1Ast.RangeContraint_val_MAX     (s, rv,b)            -> rangeContraint_val_MAXFunc s rv b 
    | Asn1Ast.RangeContraint_MIN_val     (s, rv,b)            -> rangeContraint_MIN_valFunc s rv b 
    | Asn1Ast.SizeContraint              (s, c)                 -> sizeContraintFunc s c 
    | Asn1Ast.AlphabetContraint          (s, c)                 -> alphabetContraintFunc s c 
    | Asn1Ast.UnionConstraint            (s, c1,c2,b)         -> unionConstraintFunc s c1 c2  b 
    | Asn1Ast.IntersectionConstraint     (s, c1,c2)           -> intersectionConstraintFunc s c1 c2 
    | Asn1Ast.AllExceptConstraint        (s, c)                 -> allExceptConstraintFunc s c 
    | Asn1Ast.ExceptConstraint           (s, c1,c2)           -> exceptConstraintFunc s c1 c2 
    | Asn1Ast.RootConstraint             (s, c1)                -> rootConstraintFunc s c1    
    | Asn1Ast.RootConstraint2            (s, c1,c2)           -> rootConstraint2Func s c1 c2      
    | Asn1Ast.RangeContraint_MIN_MAX            _          -> raise(BugErrorException "Unexpected constraint type")
    | Asn1Ast.TypeInclusionConstraint           _          -> raise(BugErrorException "Unexpected constraint type")   
    | Asn1Ast.WithComponentConstraint    (s, c,l)             -> withComponentConstraintFunc  s c l //raise(BugErrorException "Unexpected constraint type")
    | Asn1Ast.WithComponentsConstraint   (s, ncs)               -> withComponentsConstraintFunc s ncs   //raise(BugErrorException "Unexpected constraint type")




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
            Asn1Ast.Asn1Type.acnInfo = None
            Asn1Ast.Asn1Type.unitsOfMeasure = None
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
            Asn1Ast.Asn1Type.acnInfo = None
            Asn1Ast.Asn1Type.unitsOfMeasure = None

        }
    let newValue = ValuesMapping.mapValue r charType v
    match (getBaseValue newValue).kind with
    | StringValue (vl, _)    when (CommonTypes.StringValue2String vl).Length = 1    -> (CommonTypes.StringValue2String vl).ToCharArray().[0]
    | _                                         -> raise(SemanticError (v.Location, "Expecting a string with just one character"))

let private strGetter (r:Asn1Ast.AstRoot)  (v:Asn1Ast.Asn1Value) =
    let charType = 
        {
            Asn1Ast.Asn1Type.Kind = Asn1Ast.IA5String
            Asn1Ast.Asn1Type.Constraints = []
            Asn1Ast.Asn1Type.Location = FsUtils.emptyLocation
            Asn1Ast.Asn1Type.parameterizedTypeInstance = false
            Asn1Ast.Asn1Type.acnInfo = None
            Asn1Ast.Asn1Type.unitsOfMeasure = None
        }
    let newValue = ValuesMapping.mapValue r charType v
    match (getBaseValue newValue).kind with
    | StringValue (vl,_)            -> (CommonTypes.StringValue2String vl)
    | _                        -> raise(BugErrorException "Value is not of expected type")

let private strValueGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | StringValue (vl,_)            -> (CommonTypes.StringValue2String vl)
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

let private timeGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | TimeValue vl            -> vl.Value
    | _                             -> raise(BugErrorException "Value is not of expected type")

let private objectIdentifierGetter (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (v:Asn1Ast.Asn1Value) =
    let newValue = ValuesMapping.mapValue r t v
    match (getBaseValue newValue).kind with
    | ObjOrRelObjIdValue z            -> z
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
        (fun s rv                 -> SingleValueConstraint (s, valueGetter rv )) 
        (fun s rv1 rv2 b1 b2      -> raise(BugErrorException "range constraint is not expected here"))
        (fun s rv b               -> raise(BugErrorException "range constraint is not expected here"))
        (fun s rv b               -> raise(BugErrorException "range constraint is not expected here"))
        (fun s c                  -> raise(BugErrorException "SizeContraint is not expected here"))
        (fun s c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun s c1 c2 b            -> 
            let c1 = getRecursiveTypeConstraint valueGetter c1 
            let c2 = getRecursiveTypeConstraint valueGetter c2 
            UnionConstraint (s, c1,c2,b))           
        (fun s c1 c2             -> 
            let c1 = getRecursiveTypeConstraint valueGetter c1 
            let c2 = getRecursiveTypeConstraint valueGetter c2 
            IntersectionConstraint (s, c1,c2))           
        (fun s c             -> 
            let c = getRecursiveTypeConstraint valueGetter c 
            AllExceptConstraint (s, c))           
        (fun s c1 c2             -> 
            let c1 = getRecursiveTypeConstraint valueGetter c1 
            let c2 = getRecursiveTypeConstraint valueGetter c2 
            ExceptConstraint (s, c1,c2))           
        (fun s c             -> 
            let c = getRecursiveTypeConstraint valueGetter c 
            RootConstraint (s,c))           
        (fun s c1 c2             -> 
            let c1 = getRecursiveTypeConstraint valueGetter c1 
            let c2 = getRecursiveTypeConstraint valueGetter c2 
            RootConstraint2 (s, c1,c2))  
        (fun s c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun s c  -> raise(BugErrorException "Unexpected constraint type"))         
        c


let rec private getRangeTypeConstraint valueGetter  valueGetter2 (c:Asn1Ast.Asn1Constraint)   =
    foldBConstraint 
        (fun s rv                 -> RangeSingleValueConstraint (s, valueGetter2 rv )) 
        (fun s rv1 rv2 b1 b2      -> RangeContraint (s, valueGetter rv1 ,valueGetter rv2, b1,b2) )
        (fun s rv b               -> RangeContraint_val_MAX (s, valueGetter rv, b))
        (fun s rv b               -> RangeContraint_MIN_val (s, valueGetter rv, b))
        (fun s c                  -> raise(BugErrorException "SizeContraint is not expected here"))
        (fun s c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun s c1 c2 b            -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeUnionConstraint (s, c1,c2,b))           
        (fun s c1 c2             -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeIntersectionConstraint (s, c1,c2))           
        (fun s c                 -> 
            let c = getRangeTypeConstraint valueGetter valueGetter2 c 
            RangeAllExceptConstraint (s,c))           
        (fun s c1 c2             -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeExceptConstraint (s, c1,c2))
        (fun s c                 -> 
            let c = getRangeTypeConstraint valueGetter valueGetter2 c 
            RangeRootConstraint (s,c))
        (fun s c1 c2             -> 
            let c1 = getRangeTypeConstraint valueGetter valueGetter2 c1 
            let c2 = getRangeTypeConstraint valueGetter valueGetter2 c2 
            RangeRootConstraint2 (s, c1,c2))           
        (fun s c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun s c  -> raise(BugErrorException "Unexpected constraint type"))         
        c


let rec private getSizeTypeConstraint (r:Asn1Ast.AstRoot) valueGetter  (c:Asn1Ast.Asn1Constraint)   =
    foldBConstraint 
        (fun s rv                 -> SizeSingleValueConstraint (s, valueGetter rv)) 
        (fun s rv1 rv2 b1 b2      -> raise(BugErrorException "Range constraint is not expected here"))
        (fun s rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun s rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun s c                  -> 
            let posIntCon = getRangeTypeConstraint (posIntValGetter r) (posIntValGetter r)  c
            SizeContraint (s, posIntCon))
        (fun s c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun s c1 c2 b            -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2 
            SizeUnionConstraint (s, c1,c2,b))           
        (fun s c1 c2             -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2
            SizeIntersectionConstraint (s, c1,c2))           
        (fun s c                 -> 
            let c = getSizeTypeConstraint r valueGetter c 
            SizeAllExceptConstraint (s, c))           
        (fun s c1 c2             -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2
            SizeExceptConstraint (s, c1,c2))
        (fun s c                 -> 
            let c = getSizeTypeConstraint r valueGetter c 
            SizeRootConstraint (s, c))
        (fun s c1 c2             -> 
            let c1 = getSizeTypeConstraint r valueGetter c1
            let c2 = getSizeTypeConstraint r valueGetter c2
            SizeRootConstraint2 (s, c1,c2))           
        (fun _  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun _  -> raise(BugErrorException "Unexpected constraint type"))         
        c


let rec private getStringTypeConstraint (r:Asn1Ast.AstRoot) valueGetter  (c:Asn1Ast.Asn1Constraint)   =
    foldBConstraint 
        (fun s rv                 -> StrSingleValueConstraint (s, valueGetter rv)) 
        (fun s rv1 rv2 b1 b2      -> raise(SemanticError(rv1.Location, "Range constraint is not expected here in string types")))
        (fun s rv b               -> raise(SemanticError(rv.Location, "Range constraint is not expected here in string types")))
        (fun s rv b               -> raise(SemanticError(rv.Location, "Range constraint is not expected here in string types")))
        (fun s c                  -> 
            let posIntCon = getRangeTypeConstraint (posIntValGetter r) (posIntValGetter r) c
            StrSizeContraint (s, posIntCon))
        (fun s c                  -> 
            let charCons = getRangeTypeConstraint (charGetter r) (strGetter r) c
            AlphabetContraint (s, charCons))
        (fun s c1 c2 b            -> 
            let c1 = getStringTypeConstraint r valueGetter c1
            let c2 = getStringTypeConstraint r valueGetter c2
            StrUnionConstraint (s, c1,c2,b))           
        (fun s c1 c2             -> 
            let c1 = getStringTypeConstraint r valueGetter c1
            let c2 = getStringTypeConstraint r valueGetter c2
            StrIntersectionConstraint (s, c1,c2))           
        (fun s c                 -> 
            let c = getStringTypeConstraint r valueGetter c
            StrAllExceptConstraint (s, c))           
        (fun s c1 c2             -> 
            let c1 = getStringTypeConstraint r valueGetter c1
            let c2 = getStringTypeConstraint r valueGetter c2
            StrExceptConstraint (s, c1,c2))
        (fun s c                 -> 
            let c = getStringTypeConstraint r valueGetter c
            StrRootConstraint (s, c))
        (fun s c1 c2             -> 
            let c1 = getStringTypeConstraint r valueGetter c1
            let c2 = getStringTypeConstraint r valueGetter c2
            StrRootConstraint2 (s, c1,c2))           
        (fun _  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun _  -> raise(BugErrorException "Unexpected constraint type"))         
        c


let getIntegerTypeConstraint (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRangeTypeConstraint (getValueAsBigInteger r t) (getValueAsBigInteger r t)
let getRealTypeConstraint    (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRangeTypeConstraint (getValueAsDouble r t) (getValueAsDouble r t)
let getIA5StringConstraint   (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getStringTypeConstraint r (strValueGetter r t)
let getOctetStringConstraint (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getSizeTypeConstraint r (octGetter r t)
let getBitStringConstraint   (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getSizeTypeConstraint r (bitGetter r t)
let getBoolConstraint        (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (boolGetter r t)
let getTimeConstraint        (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (timeGetter r t)
let getEnumConstraint        (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (enumGetter r t)
//let getSequenceOfConstraint  (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getSizeTypeConstraint r (seqOfValueGetter r t)
let getObjectIdConstraint    (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (objectIdentifierGetter r t)
let getSequenceConstraint    (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (seqValueGetter r t)
//let getChoiceConstraint      (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) = getRecursiveTypeConstraint (chValueGetter r t)

let rec getAnyConstraint (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (c:Asn1Ast.Asn1Constraint) =
    match t.Kind with
    |Asn1Ast.Integer             -> IntegerTypeConstraint (getIntegerTypeConstraint r t c)
    |Asn1Ast.Real                -> RealTypeConstraint (getRealTypeConstraint r t c)
    |Asn1Ast.IA5String                  -> IA5StringConstraint (getIA5StringConstraint r t c)
    |Asn1Ast.NumericString              -> IA5StringConstraint (getIA5StringConstraint r t c)
    |Asn1Ast.OctetString                -> OctetStringConstraint (getOctetStringConstraint r t c)
    |Asn1Ast.NullType                   -> NullConstraint
    |Asn1Ast.TimeType        _          -> TimeConstraint (getTimeConstraint r t c)
    |Asn1Ast.BitString   _              -> BitStringConstraint (getBitStringConstraint r t c)
    |Asn1Ast.Boolean                    -> BoolConstraint (getBoolConstraint r t c)
    |Asn1Ast.ObjectIdentifier           -> ObjectIdConstraint(getObjectIdConstraint r t c)
    |Asn1Ast.RelativeObjectIdentifier   -> ObjectIdConstraint(getObjectIdConstraint r t c)
    |Asn1Ast.Enumerated        (_)      -> EnumConstraint(getEnumConstraint r t c)
    |Asn1Ast.SequenceOf        (ch)      -> SequenceOfConstraint(getSequenceOfConstraint r t ch c)
    |Asn1Ast.Sequence      children     -> SeqConstraint(getSeqConstraint r t children c)
    |Asn1Ast.Choice        children     -> ChoiceConstraint(getChoiceConstraint r t children c)
    |Asn1Ast.ReferenceType     (_)      -> getAnyConstraint r (Asn1Ast.GetActualType t r) c

and getSequenceOfConstraint  (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (child : Asn1Ast.Asn1Type) =
    let rec  getSizeTypeConstraint (r:Asn1Ast.AstRoot) valueGetter  (c:Asn1Ast.Asn1Constraint)   =
        foldBConstraint 
            (fun s rv                 -> SeqOfSizeSingleValueConstraint (s, valueGetter rv)) 
            (fun s rv1 rv2 b1 b2      -> raise(BugErrorException "Range constraint is not expected here"))
            (fun s rv b               -> raise(BugErrorException "Range constraint is not expected here"))
            (fun s rv b               -> raise(BugErrorException "Range constraint is not expected here"))
            (fun s c                  -> 
                let posIntCon = getRangeTypeConstraint (posIntValGetter r) (posIntValGetter r)  c
                SeqOfSizeContraint (s,posIntCon))
            (fun s c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
            (fun s c1 c2 b            -> 
                let c1 = getSizeTypeConstraint r valueGetter c1
                let c2 = getSizeTypeConstraint r valueGetter c2 
                SeqOfSizeUnionConstraint (s, c1,c2,b))           
            (fun s c1 c2             -> 
                let c1 = getSizeTypeConstraint r valueGetter c1
                let c2 = getSizeTypeConstraint r valueGetter c2
                SeqOfSizeIntersectionConstraint (s, c1,c2))           
            (fun s c                 -> 
                let c = getSizeTypeConstraint r valueGetter c 
                SeqOfSizeAllExceptConstraint (s, c))           
            (fun s c1 c2             -> 
                let c1 = getSizeTypeConstraint r valueGetter c1
                let c2 = getSizeTypeConstraint r valueGetter c2
                SeqOfSizeExceptConstraint (s, c1,c2))
            (fun s c                 -> 
                let c = getSizeTypeConstraint r valueGetter c 
                SeqOfSizeRootConstraint (s, c))
            (fun s c1 c2             -> 
                let c1 = getSizeTypeConstraint r valueGetter c1
                let c2 = getSizeTypeConstraint r valueGetter c2
                SeqOfSizeRootConstraint2 (s, c1,c2))           
            (fun s c l -> SeqOfSeqWithComponentConstraint(s, (getAnyConstraint r child c),l) )         
            (fun _  -> raise(BugErrorException "Unexpected constraint type"))         
            c
    getSizeTypeConstraint r (seqOfValueGetter r t)




and getRecursiveTypeSeqOrConstraint (r:Asn1Ast.AstRoot) valueGetter (children:Asn1Ast.ChildInfo list)  (c:Asn1Ast.Asn1Constraint)   =
    foldBConstraint
        (fun s rv                 -> SeqOrChSingleValueConstraint (s, valueGetter rv )) 
        (fun s rv1 rv2 b1 b2      -> raise(BugErrorException "range constraint is not expected here"))
        (fun s rv b               -> raise(BugErrorException "range constraint is not expected here"))
        (fun s rv b               -> raise(BugErrorException "range constraint is not expected here"))
        (fun s c                  -> raise(BugErrorException "SizeContraint is not expected here"))
        (fun s c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun s c1 c2 b            -> 
            let c1 = getRecursiveTypeSeqOrConstraint r valueGetter children c1 
            let c2 = getRecursiveTypeSeqOrConstraint r valueGetter children c2 
            SeqOrChUnionConstraint (s, c1,c2,b))           
        (fun s c1 c2             -> 
            let c1 = getRecursiveTypeSeqOrConstraint r valueGetter children c1 
            let c2 = getRecursiveTypeSeqOrConstraint r valueGetter children c2 
            SeqOrChIntersectionConstraint (s, c1,c2))           
        (fun s c             -> 
            let c = getRecursiveTypeSeqOrConstraint r valueGetter children c 
            SeqOrChAllExceptConstraint (s, c))           
        (fun s c1 c2             -> 
            let c1 = getRecursiveTypeSeqOrConstraint r valueGetter children c1 
            let c2 = getRecursiveTypeSeqOrConstraint r valueGetter children c2 
            SeqOrChExceptConstraint (s, c1,c2))           
        (fun s c             -> 
            let c = getRecursiveTypeSeqOrConstraint r valueGetter children c 
            SeqOrChRootConstraint (s, c))           
        (fun s c1 c2             -> 
            let c1 = getRecursiveTypeSeqOrConstraint r valueGetter children c1 
            let c2 = getRecursiveTypeSeqOrConstraint r valueGetter children c2 
            SeqOrChRootConstraint2 (s, c1,c2))  
        (fun s c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun s ncs  -> 
            let newItems = 
                ncs |> 
                List.map(fun nc -> 
                    let ch = children |> Seq.find(fun x -> x.Name.Value = nc.Name.Value)
                    let newCon = nc.Contraint |> Option.map (getAnyConstraint r ch.Type)
                    {NamedConstraint.Name = nc.Name; Mark = nc.Mark; Contraint = newCon})
            SeqOrChWithComponentsConstraint (s, newItems) )         
        c

and getSeqConstraint (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (children:Asn1Ast.ChildInfo list)   =
    getRecursiveTypeSeqOrConstraint r (seqValueGetter r t) children



and getChoiceConstraint (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (children:Asn1Ast.ChildInfo list)   =
    getRecursiveTypeSeqOrConstraint r (chValueGetter r t) children


