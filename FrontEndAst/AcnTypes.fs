(*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*)

module AcnTypes

open System.Numerics
open FsUtils
open Antlr.Runtime.Tree
open Antlr.Runtime


type LongFieldLoc = list<StringLoc>

type AbsPathLoc = LongFieldLoc
type RelPath = list<string>
type AbsPath = list<string>




let GetLastItemLocation (lf:LongFieldLoc) =
    (lf |> List.rev).Head.Location

type AcnIntExpr =
    | IntegerExpr       of acnIntegerConstant
    | SumExpr           of AcnIntExpr*AcnIntExpr
    | MinExpr           of AcnIntExpr*AcnIntExpr
    | MulExpr           of AcnIntExpr*AcnIntExpr
    | DivExpr           of AcnIntExpr*AcnIntExpr
    | ModExpr           of AcnIntExpr*AcnIntExpr
    | PowExpr           of AcnIntExpr*AcnIntExpr
    | UnMinExp          of AcnIntExpr
and acnIntegerConstant =
    | IntConst of IntLoc
    | RefConst of StringLoc       //reference to other constant




type AcnConstant = {
    Name  : StringLoc
    Value : AcnIntExpr
}

type AcnProperty = 
    | Encoding          of encoding                     // used by int, real, enum
    | SizeProperty      of sizeProperty                 // used by int, real, and all sizeable types
    | Aligment          of aligment                     // *
    | EncodeValues                                      // used by enums => values will be encoded and not indexes
    | BooleanEncoding   of booleanEncoding              // bool
    | NullValue         of StringLoc                    // null
    | Endianness        of endianness                   // used by int, real, enum
    | EnumeratorResetValue of string*BigInteger        // used by enum children to redefine values
    | MappingFunction   of StringLoc                    // used by int
with
    override this.ToString() =  
        match this with
        | Encoding          enc                       -> sprintf "encoding %A" enc
        | SizeProperty      sz                        -> sprintf "size %A" sz
        | Aligment          al                        -> sprintf "aligment %A" al
        | EncodeValues                                -> "encode-values"
        | BooleanEncoding   ben                       -> sprintf "pattern '%A'" ben
        | NullValue         pattern                   -> sprintf "pattern '%s'" pattern.Value
        | Endianness        endi                -> sprintf "endianness %A" endi
        | EnumeratorResetValue (enChildName,vl)       -> enChildName + vl.ToString()
        | MappingFunction   funcName                  -> funcName.Value

and aligment = 
    | NextByte
    | NextWord
    | NextDWord

and sizeProperty =
    | Fixed             of acnIntegerConstant
    | NullTerminated    of byte      //termination character
    | SizeDeterminant0  of (RelPath*SrcLoc)

and endianness =
    | LittleEndianness
    | BigEndianness            // Default

and encoding =
    | PosInt
    | TwosComplement
    | Ascii
    | BCD
    | IEEE754_32
    | IEEE754_64

and booleanEncoding =
    | TrueValue    of StringLoc    //Default '1'
    | FalseValue   of StringLoc

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
type ParamMode =
    | DecodeMode
    | EncodeDecodeMode

type AcnType = {
    TypeID     : AbsPath
    ImpMode    : AcnTypeImplMode
    Properties : list<AcnProperty>      //does not contain the properties with long fields 
    Location : SrcLoc
    Comments: string array
    acnParameters   : AcnParameter list
}
with 
  override x.ToString() =  x.TypeID |> Seq.StrJoin "."
and AcnTempType = {                // this type is not encoded decoded. It is declared locally at the tas level
                                    // and it is used for passing values
    ModName     : string
    TasName     : string
    Name        : string
    Asn1Type    : AcnAsn1Type
}
and AcnTypeImplMode =
    | RecordField                           // normal ASN.1 type
    | LocalVariable of AcnAsn1Type          // ACN inserted type
    | FunctionParameter of AcnAsn1Type

and AcnParameter = {
    Name        : string
    Asn1Type    : AcnAsn1Type
    Location    : SrcLoc
}

and AcnAsn1Type =
    | Integer
    | Boolean
    | NullType
    | RefTypeCon of StringLoc*StringLoc



type LongReference = {
    TypeID  : AbsPath           // the type that has a property with the LongReference path
    LongRef : RelPath           // the relative long Reference path
    Kind : LongReferenceKind
    Location : SrcLoc
}
with 
  override x.ToString() =  
    let decType = x.TypeID |> Seq.StrJoin "."
    let determnant = x.LongRef |> Seq.StrJoin "."
    sprintf "%s %s %s" decType (x.Kind.ToString() ) determnant


and Point =
    | TypePoint  of AbsPath              // point is an encoded/decoded AcnType
    | ParamPoint of AbsPath              // point is parameter
    | TempPoint  of AbsPath              // point is an AcnTempType
    member x.AbsPath =
        match x with  TypePoint(a) | ParamPoint(a) | TempPoint(a)   -> a
    member x.ReplacePath newPath =
        match x with  
        | TypePoint(_)      -> TypePoint newPath
        | ParamPoint(_)     -> ParamPoint newPath
        | TempPoint(a)      -> TempPoint newPath



and LongReferenceKind = 
    //| SizeDeterminant                   // points to an integer type that acts as a size determinant to a SEQUENCE OF, BIT STRINT, OCTET STRING etc
    | RefTypeArgument of string         // string is the param name
    | PresenceBool                             // points to a SEQEUNCE or Choice child
    | PresenceInt of acnIntegerConstant        // points to a SEQEUNCE or Choice child
    | PresenceStr of string
    | ChoiceDeteterminant       // points to Enumerated type acting as CHOICE determinant.
with
    override x.ToString() =  
        match x with
        //| SizeDeterminant                   -> "size"
        | RefTypeArgument argName           -> sprintf "RefArg<%s>" argName
        | PresenceBool                      -> "present-when-bool"
        | PresenceInt  vl                   -> sprintf "present-when-int %A" vl
        | PresenceStr stVal                 -> sprintf "present-when-str %s" stVal
        | ChoiceDeteterminant               -> "choice-determinant"

type AcnAst = {
    Constants : list<AcnConstant>
    Types     : list<AcnType>
    References : list<LongReference>
    Files      : list<string*(IToken array)>
}


let rec EvaluateConstant (constants:list<AcnConstant>) intConstant =
    let rec EvaluateConstantAux = function 
    | IntegerExpr(consta)   -> EvaluateConstant constants consta
    | SumExpr(exp1,exp2)    -> (EvaluateConstantAux exp1) + (EvaluateConstantAux exp2)
    | MinExpr(exp1,exp2)    -> (EvaluateConstantAux exp1) - (EvaluateConstantAux exp2)
    | MulExpr(exp1,exp2)    -> (EvaluateConstantAux exp1) * (EvaluateConstantAux exp2)
    | DivExpr(exp1,exp2)    -> (EvaluateConstantAux exp1) / (EvaluateConstantAux exp2)
    | ModExpr(exp1,exp2)    -> (EvaluateConstantAux exp1) % (EvaluateConstantAux exp2)
    | PowExpr(exp1,exp2)    -> 
        System.Numerics.BigInteger.Pow(EvaluateConstantAux exp1, int (EvaluateConstantAux exp2))
    | UnMinExp(exp1)        -> -(EvaluateConstantAux exp1) 
    match intConstant with
    | IntConst(a)   -> a.Value
    | RefConst(consLookUp)  ->
        match constants |> Seq.tryFind(fun c-> c.Name.Value = consLookUp.Value) with
        |None       -> raise(SemanticError(consLookUp.Location, (sprintf "Unknown symbol '%s'" consLookUp.Value)))
        |Some(cn)   -> EvaluateConstantAux cn.Value
        

        




