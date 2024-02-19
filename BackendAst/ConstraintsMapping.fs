module ConstraintsMapping
open Constraints
open System
open System.Numerics
open FsUtils



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
    (c:Asn1AnyConstraint) =
    match c with
    | AnySingleValueContraint       rv                -> singleValueContraintFunc rv 
    | AnyRangeContraint             (rv1,rv2,b1,b2)   -> rangeContraintFunc rv1 rv2 b1 b2 
    | AnyRangeContraint_val_MAX     (rv,b)            -> rangeContraint_val_MAXFunc rv b 
    | AnyRangeContraint_MIN_val     (rv,b)            -> rangeContraint_MIN_valFunc rv b 
    | AnySizeContraint              c                 -> sizeContraintFunc c 
    | AnyAlphabetContraint          c                 -> alphabetContraintFunc c 
    | AnyUnionConstraint            (c1,c2,b)         -> unionConstraintFunc c1 c2  b 
    | AnyIntersectionConstraint     (c1,c2)           -> intersectionConstraintFunc c1 c2 
    | AnyAllExceptConstraint        c                 -> allExceptConstraintFunc c 
    | AnyExceptConstraint           (c1,c2)           -> exceptConstraintFunc c1 c2 
    | AnyRootConstraint             c1                -> rootConstraintFunc c1    
    | AnyRootConstraint2            (c1,c2)           -> rootConstraint2Func c1 c2      




let getValueAsBigInteger (v:Asn1GenericValue) =
    match v with
    |IntegerValue x -> x.Value, x.litOrRef
    | _                  -> raise(BugErrorException "Value is not of expected type")

let getValueAsDouble (v:Asn1GenericValue) =
    match v with
    |RealValue x -> x.Value, x.litOrRef
    | _                  -> raise(BugErrorException "Value is not of expected type")

let posIntValGetter (v:Asn1GenericValue) =
    match v with
    |IntegerValue x when x.Value >= 0I   -> uint32 x.Value, x.litOrRef
    | _                                 -> raise(BugErrorException "Value is not of expected type")

let charGetter (v:Asn1GenericValue) =
    match v with
    |StringValue vl    when vl.Value.Length>0    -> vl.Value.ToCharArray().[0], vl.litOrRef
    | _                                         -> raise(BugErrorException "Value is not of expected type")

let strGetter (v:Asn1GenericValue) =
    match v with
    |StringValue vl            -> vl.Value, vl.litOrRef
    | _                        -> raise(BugErrorException "Value is not of expected type")

let octGetter (v:Asn1GenericValue) =
    match v with
    |OctetStringValue vl            -> vl, vl.litOrRef
    | _                             -> raise(BugErrorException "Value is not of expected type")

let bitGetter (v:Asn1GenericValue) =
    match v with
    |BitStringValue vl            -> vl, vl.litOrRef
    | _                             -> raise(BugErrorException "Value is not of expected type")

let boolGetter (v:Asn1GenericValue) =
    match v with
    |BooleanValue vl            -> vl.Value, vl.litOrRef
    | _                             -> raise(BugErrorException "Value is not of expected type")

let enumGetter (v:Asn1GenericValue) =
    match v with
    |EnumValue vl            -> vl.Value, vl.litOrRef
    | _                             -> raise(BugErrorException "Value is not of expected type")

let seqOfValueGetter (v:Asn1GenericValue) =
    match v with
    |SeqOfValue vl            -> vl, vl.litOrRef
    | _                             -> raise(BugErrorException "Value is not of expected type")

let seqValueGetter (v:Asn1GenericValue) =
    match v with
    |SeqValue vl            -> vl, vl.litOrRef
    | _                             -> raise(BugErrorException "Value is not of expected type")

let chValueGetter (v:Asn1GenericValue) =
    match v with
    |ChValue vl           -> vl, vl.litOrRef
    | _                             -> raise(BugErrorException "Value is not of expected type")


let rec getRecursiveTypeConstraint valueGetter   (c:Asn1AnyConstraint)   =
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


let rec getRangeTypeConstraint valueGetter  valueGetter2 (c:Asn1AnyConstraint)   =
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


let rec getSizeTypeConstraint valueGetter  (c:Asn1AnyConstraint)   =
    foldBConstraint 
        (fun rv                 -> SizeSingleValueConstraint (valueGetter rv)) 
        (fun rv1 rv2 b1 b2      -> raise(BugErrorException "Range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun c                  -> 
            let posIntCon = getRangeTypeConstraint posIntValGetter posIntValGetter c
            SizeContraint posIntCon)
        (fun c                  -> raise(BugErrorException "AlphabetContraint is not expected here"))
        (fun c1 c2 b            -> 
            let c1 = getSizeTypeConstraint valueGetter c1
            let c2 = getSizeTypeConstraint valueGetter c2 
            SizeUnionConstraint (c1,c2,b))           
        (fun c1 c2             -> 
            let c1 = getSizeTypeConstraint valueGetter c1
            let c2 = getSizeTypeConstraint valueGetter c2
            SizeIntersectionConstraint (c1,c2))           
        (fun c                 -> 
            let c = getSizeTypeConstraint valueGetter c 
            SizeAllExceptConstraint c)           
        (fun c1 c2             -> 
            let c1 = getSizeTypeConstraint valueGetter c1
            let c2 = getSizeTypeConstraint valueGetter c2
            SizeExceptConstraint (c1,c2))
        (fun c                 -> 
            let c = getSizeTypeConstraint valueGetter c 
            SizeRootConstraint c)
        (fun c1 c2             -> 
            let c1 = getSizeTypeConstraint valueGetter c1
            let c2 = getSizeTypeConstraint valueGetter c2
            SizeRootConstraint2 (c1,c2))           
        c


let rec getStringTypeConstraint valueGetter  (c:Asn1AnyConstraint)   =
    foldBConstraint 
        (fun rv                 -> StrSingleValueConstraint (valueGetter rv)) 
        (fun rv1 rv2 b1 b2      -> raise(BugErrorException "Range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun rv b               -> raise(BugErrorException "Range constraint is not expected here"))
        (fun c                  -> 
            let posIntCon = getRangeTypeConstraint posIntValGetter posIntValGetter c
            StrSizeContraint posIntCon)
        (fun c                  -> 
            let charCons = getRangeTypeConstraint charGetter strGetter c
            AlphabetContraint charCons)
        (fun c1 c2 b            -> 
            let c1 = getStringTypeConstraint valueGetter c1
            let c2 = getStringTypeConstraint valueGetter c2
            StrUnionConstraint (c1,c2,b))           
        (fun c1 c2             -> 
            let c1 = getStringTypeConstraint valueGetter c1
            let c2 = getStringTypeConstraint valueGetter c2
            StrIntersectionConstraint (c1,c2))           
        (fun c                 -> 
            let c = getStringTypeConstraint valueGetter c
            StrAllExceptConstraint c)           
        (fun c1 c2             -> 
            let c1 = getStringTypeConstraint valueGetter c1
            let c2 = getStringTypeConstraint valueGetter c2
            StrExceptConstraint (c1,c2))
        (fun c                 -> 
            let c = getStringTypeConstraint valueGetter c
            StrRootConstraint c)
        (fun c1 c2             -> 
            let c1 = getStringTypeConstraint valueGetter c1
            let c2 = getStringTypeConstraint valueGetter c2
            StrRootConstraint2 (c1,c2))           
        c


let getIntegerTypeConstraint = getRangeTypeConstraint getValueAsBigInteger  getValueAsBigInteger 
let getRealTypeConstraint = getRangeTypeConstraint getValueAsDouble  getValueAsDouble
let getIA5StringConstraint = getStringTypeConstraint strGetter
let getOctetStringConstraint = getSizeTypeConstraint octGetter
let getBitStringConstraint = getSizeTypeConstraint bitGetter
let getBoolConstraint = getRecursiveTypeConstraint boolGetter
let getEnumConstraint = getRecursiveTypeConstraint enumGetter
let getSequenceOfConstraint = getSizeTypeConstraint seqOfValueGetter

let getSequenceConstraint = getRecursiveTypeConstraint seqValueGetter
let getChoiceConstraint = getRecursiveTypeConstraint chValueGetter
