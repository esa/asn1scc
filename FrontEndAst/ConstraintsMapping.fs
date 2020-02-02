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
    | Asn1Ast.WithComponentConstraint    (c,l)             -> withComponentConstraintFunc  c l //raise(BugErrorException "Unexpected constraint type")
    | Asn1Ast.WithComponentsConstraint   ncs               -> withComponentsConstraintFunc ncs   //raise(BugErrorException "Unexpected constraint type")




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
            Asn1Ast.Asn1Type.acnInfo = None
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
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
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
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
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
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
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
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
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
            (fun rv                 -> SeqOfSizeSingleValueConstraint (valueGetter rv)) 
            (fun rv1 rv2 b1 b2      -> raise(BugErrorException "Range constraint is not expected here"))
            (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
            (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
            (fun c                  -> 
                let posIntCon = getRangeTypeConstraint (posIntValGetter r) (posIntValGetter r)  c
                SeqOfSizeContraint posIntCon)
            (fun c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
            (fun c1 c2 b            -> 
                let c1 = getSizeTypeConstraint r valueGetter c1
                let c2 = getSizeTypeConstraint r valueGetter c2 
                SeqOfSizeUnionConstraint (c1,c2,b))           
            (fun c1 c2             -> 
                let c1 = getSizeTypeConstraint r valueGetter c1
                let c2 = getSizeTypeConstraint r valueGetter c2
                SeqOfSizeIntersectionConstraint (c1,c2))           
            (fun c                 -> 
                let c = getSizeTypeConstraint r valueGetter c 
                SeqOfSizeAllExceptConstraint c)           
            (fun c1 c2             -> 
                let c1 = getSizeTypeConstraint r valueGetter c1
                let c2 = getSizeTypeConstraint r valueGetter c2
                SeqOfSizeExceptConstraint (c1,c2))
            (fun c                 -> 
                let c = getSizeTypeConstraint r valueGetter c 
                SeqOfSizeRootConstraint c)
            (fun c1 c2             -> 
                let c1 = getSizeTypeConstraint r valueGetter c1
                let c2 = getSizeTypeConstraint r valueGetter c2
                SeqOfSizeRootConstraint2 (c1,c2))           
            (fun c l -> SeqOfSeqWithComponentConstraint((getAnyConstraint r child c),l) )         
            (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
            c
    getSizeTypeConstraint r (seqOfValueGetter r t)




and getRecursiveTypeSeqOrConstraint (r:Asn1Ast.AstRoot) valueGetter (children:Asn1Ast.ChildInfo list)  (c:Asn1Ast.Asn1Constraint)   =
    foldBConstraint
        (fun rv                 -> SeqOrChSingleValueConstraint (valueGetter rv )) 
        (fun rv1 rv2 b1 b2      -> raise(BugErrorException "range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "range constraint is not expected here"))
        (fun c                  -> raise(BugErrorException "SizeContraint is not expected here"))
        (fun c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun c1 c2 b            -> 
            let c1 = getRecursiveTypeSeqOrConstraint r valueGetter children c1 
            let c2 = getRecursiveTypeSeqOrConstraint r valueGetter children c2 
            SeqOrChUnionConstraint (c1,c2,b))           
        (fun c1 c2             -> 
            let c1 = getRecursiveTypeSeqOrConstraint r valueGetter children c1 
            let c2 = getRecursiveTypeSeqOrConstraint r valueGetter children c2 
            SeqOrChIntersectionConstraint (c1,c2))           
        (fun c             -> 
            let c = getRecursiveTypeSeqOrConstraint r valueGetter children c 
            SeqOrChAllExceptConstraint c)           
        (fun c1 c2             -> 
            let c1 = getRecursiveTypeSeqOrConstraint r valueGetter children c1 
            let c2 = getRecursiveTypeSeqOrConstraint r valueGetter children c2 
            SeqOrChExceptConstraint (c1,c2))           
        (fun c             -> 
            let c = getRecursiveTypeSeqOrConstraint r valueGetter children c 
            SeqOrChRootConstraint c)           
        (fun c1 c2             -> 
            let c1 = getRecursiveTypeSeqOrConstraint r valueGetter children c1 
            let c2 = getRecursiveTypeSeqOrConstraint r valueGetter children c2 
            SeqOrChRootConstraint2 (c1,c2))  
        (fun c  -> raise(BugErrorException "Unexpected constraint type"))         
        (fun ncs  -> 
            let newItems = 
                ncs |> 
                List.map(fun nc -> 
                    let ch = children |> Seq.find(fun x -> x.Name.Value = nc.Name.Value)
                    let newCon = nc.Contraint |> Option.map (getAnyConstraint r ch.Type)
                    {NamedConstraint.Name = nc.Name; Mark = nc.Mark; Contraint = newCon})
            SeqOrChWithComponentsConstraint newItems )         
        c

and getSeqConstraint (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (children:Asn1Ast.ChildInfo list)   =
    getRecursiveTypeSeqOrConstraint r (seqValueGetter r t) children



and getChoiceConstraint (r:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (children:Asn1Ast.ChildInfo list)   =
    getRecursiveTypeSeqOrConstraint r (chValueGetter r t) children


