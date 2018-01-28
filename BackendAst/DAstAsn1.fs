module DAstAsn1

open System
open System.Numerics
open System.Globalization
open System.IO

open FsUtils
open CommonTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open Asn1Fold
open DAst
open DAstUtilFunctions


let foldGenericCon valToStrFunc  (c:GenericConstraint<'v>)  =
    foldGenericConstraint
        (fun e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun v  s           -> stg_asn1.Print_SingleValueContraint (valToStrFunc v) ,s)
        c
        0 |> fst

let foldRangeCon valToStrFunc1 valToStrFunc2  (c:RangeTypeConstraint<'v1,'v2>)  =
    foldRangeTypeConstraint        
        (fun e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun v  s           -> stg_asn1.Print_SingleValueContraint (valToStrFunc2 v) ,s)
        (fun v1 v2  minIsIn maxIsIn s   -> stg_asn1.Print_RangeContraint (valToStrFunc1 v1) (valToStrFunc1 v2) minIsIn  maxIsIn , s)
        (fun v1 minIsIn s   -> stg_asn1.Print_RangeContraint_val_MAX  (valToStrFunc1 v1) minIsIn, s)
        (fun v2 maxIsIn s   -> stg_asn1.Print_RangeContraint_MIN_val (valToStrFunc1 v2) maxIsIn, s)
        c
        0 |> fst


let foldSizeRangeTypeConstraint getSizeFunc  (c:PosIntTypeConstraint) = 
    foldRangeCon getSizeFunc getSizeFunc c

let foldSizableConstraint compareSingValueFunc  getSizeFunc (c:SizableTypeConstraint<'v>) =
    foldSizableTypeConstraint2
        (fun e1 e2 b s      -> stg_asn1.Print_UnionConstraint e1 e2, s)
        (fun e1 e2 s        -> stg_asn1.Print_IntersectionConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_AllExceptConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_ExceptConstraint e1 e2, s)
        (fun e s            -> stg_asn1.Print_RootConstraint e, s)
        (fun e1 e2 s        -> stg_asn1.Print_RootConstraint2 e1 e2, s)
        (fun v  s           -> (compareSingValueFunc v) ,s)
        (fun intCon s       -> foldSizeRangeTypeConstraint getSizeFunc intCon, s)
        c
        0 |> fst
