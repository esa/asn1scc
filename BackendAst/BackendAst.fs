module BackendAst

open System.Numerics
open FsUtils
open Ast
open System.IO
//open VisitTree
//open uPER
//open CloneTree
//open c_utils

type LOCAL_VARIABLE =
    | SEQUENCE_OF_INDEX of int
    | LENGTH
    | EXTENSION_BIT
    | ENUM_IDX of Ast.INTTYPE
    | CHOICE_IDX
    | SEQUENCE_BitMask  of string*BigInteger
    | REF_TYPE_PARAM   of string*string
    | CHAR_VAL
    | NCOUNT
    | CUR_BLOCK_SIZE
    | CUR_ITEM
    | LEN2


type PathToRoot = 
    PathToRoot of string list
type AccessPath = 
    | AccessPath of string

(*
Aphabet constraints e.g.
e.g. FROM "A" .. "Z" | "abcd"
*)
type CharComparisonBoolExp =
    | AlphaOr                   of CharComparisonBoolExp*CharComparisonBoolExp
    | AlphaAnd                  of CharComparisonBoolExp*CharComparisonBoolExp
    | AlphaNot                  of CharComparisonBoolExp
    | AlphaEq                   of AccessPath*char      
    | AlphaGreaterThan          of AccessPath*char     
    | AlphaGreaterThanOrEq      of AccessPath*char     
    | AlphaLessThan             of AccessPath*char     
    | AlphaLessThanOrEq         of AccessPath*char     
    | AlphaStringContainsChar   of AccessPath*string           
    


type AlphabetCheckFunc = {
    funcName : string
    con : CharComparisonBoolExp
}


type UnnamedVariableDeclaration =
    {
        privateName : string
        typereference : string*string
        value         : string  //needs to defined
    }
    

type BackendPrimaryNumericExpression =
    | IntegerLiteral            of BigInteger*string        //int value, prefix e.g L, UL etc
    | RealLiteral               of double
    | ReferenceToRangeVas       of (string*string)

(*
A BackendPrimaryExpression is an expression in the target language (C or Ada) 
It is either a constant (literal) or an identifier
*)
type BackendPrimaryExpression =
    | BackendPrimaryNumericExpression          of BackendPrimaryNumericExpression
    | StringLiteral             of string
    | CharLiteral               of char
    | EnumeratedTypeLiteral     of string
    | BooleanLiteral            of bool
    | ReferenceToVas            of (string*string)
    | NullTypeLiteral           

    

type BackendBooleanExpression =
    | OrConstraintExp            of BackendBooleanExpression*BackendBooleanExpression
    | AndConstraintExp           of BackendBooleanExpression*BackendBooleanExpression
    | NotConstraintExp           of BackendBooleanExpression
    | EqExp                      of AccessPath*BackendPrimaryExpression
    | GreaterThanExp             of AccessPath*BackendPrimaryNumericExpression
    | GreaterThanOrEqExp         of AccessPath*BackendPrimaryNumericExpression
    | LessThanExp                of AccessPath*BackendPrimaryNumericExpression
    | LessThanOrEqExp            of AccessPath*BackendPrimaryNumericExpression
    | CallAlphaFunc              of AccessPath*AlphabetCheckFunc
    | FixSizeBitStringEq         of AccessPath*UnnamedVariableDeclaration       
    | VarSizeBitStringEq         of AccessPath*UnnamedVariableDeclaration
    | FixSizeOctStringEq         of AccessPath*UnnamedVariableDeclaration
    | VarSizeOctStringEq         of AccessPath*UnnamedVariableDeclaration


type TypeIsValidFuncBody =
    | SequenceIsValidFuncBody       of SeqeunceComponentCheckBody list
    | SequenceOfIsValidFuncBody     of SeqeunceOfCheck
    | BaseTypeIsValidFuncBody       of BaseTypeIsValidFuncBody
with 
    member this.hasValidateFunc = false

and SeqeunceComponentCheckBody = 
    | CheckComponent            of PathToRoot*TypeIsValidFuncBody
    | CheckOptionalComponent    of PathToRoot*TypeIsValidFuncBody
    | CheckComponentIsPresent   of PathToRoot*AlwaysPresentOrAbsentCheck
    | CheckComponentIsAbsent    of PathToRoot*AlwaysPresentOrAbsentCheck

and AlwaysPresentOrAbsentCheck = {
    childExitsPath : string
    errorCode      : string
}    

and SeqeunceOfCheck = {
    path:PathToRoot
    childCheckBody:TypeIsValidFuncBody
    sIndex : string
    lengthCheckBody : TypeIsValidFuncBody
    nMax : BigInteger
    bFixed : bool
}

and BaseTypeIsValidFuncBody = {
    t:ConstraintType 
    path:PathToRoot
    alphaFuncName : string //= ToC ((path |> Seq.skip 1).StrJoin("_").Replace("#","elem"))
    errCodeName : string option //GetTypeConstraintsErrorCode t.Type.Constraints path r
    consCheck      : BackendBooleanExpression list
}

type TasIsConstraintValid = {
    funcName :string
    //sContent : TypeIsValidFuncBody
} with 
    member 
        this.LocalVars : LOCAL_VARIABLE list = 
            []
    member 
        this.AlphaCheckFunctions : AlphabetCheckFunc list = 
            []

type TasIsEqual = {
    funcName :string
} 

type TasInititalize = {
    funcName :string
}

(*
type EncodingPrototype = 
    | BER_EncPrototype of string*string
    | BER_DecPrototype of string*string
    | XER_EncPrototype of string*string
    | XER_DecPrototype of string*string
    | UPR_EncPrototype of string*string
    | UPR_DecPrototype of string*string
    | ACN_EncPrototype of string*string
    | ACN_DecPrototype of string*string
*)

// represents a .h or .ads file
type BitStringTypeInfo = {
    nBitLen     : BigInteger
    nBytesLen   : BigInteger
}

type TypeDefition =
    | INTEGER
    | POS_INTEGER
    | BOOLEAN
    | REAL
    | STRING
    | VOID_TYPE
    | FIX_SIZE_BIT_STRING   of BitStringTypeInfo
    | VAR_SIZE_BIT_STRING   of BitStringTypeInfo
    | FIX_SIZE_OCT_STRING   of BigInteger
    | VAR_SIZE_OCT_STRING   of BigInteger
    | ENUM                  of (string*BigInteger) list
    | CHOICE                of ChoiceTypeInfo
    | SEQUENCE              of SequenceTypeInfo
    | FIX_SIZE_SEQUENCE_OF  of SequenceOfTypeInfo
    | VAR_SIZE_SEQUENCE_OF  of SequenceOfTypeInfo
    | LOCAL_REFENCED_TYPE   of string

and ChoiceTypeInfo = {
    choiceIDForNone : string
    enmItems        : string list
    children    : ChoiceOrSeqChild list
}

and SequenceTypeInfo = {
    children    : ChoiceOrSeqChild list
    optChildren : string list
}

and ChoiceOrSeqChild = {
    name : string
    typeDef : TypeDefition
    arrayPostfix : string
}

and SequenceOfTypeInfo = {
    typeDef       : TypeDefition
    length       : BigInteger
    arrayPostfix : string
}



type TasDefition = {
    sTypeDecl               : TypeDefition
    sarrPostfix             : string
    sName                   : string
    nMaxBitsInPER           : BigInteger
    nMaxBytesInPER          : BigInteger
    nMaxBitsInACN           : BigInteger
    nMaxBytesInACN          : BigInteger
    nMaxBytesInXER          : int
    sStar                   : string
    errorCodes              : string list
    isConstraintValidFnc    : TasIsConstraintValid
    isEqualFnc              : TasIsEqual
    inititalizeFnc          : TasInititalize
}

type DefinitionsFile = {
    fileName : string
    fileNameNoExtUpper : string
    tases : TasDefition list
    //vases 
    //protos 
    //enumAndChoiceUtils
}

let TypeItemsCount (t:Asn1Type) (r:AstRoot)=  
    match (GetTypeUperRange t.Kind t.Constraints r) with
    |Concrete(_, max)        -> max
    |_                       -> raise(BugErrorException "SEQUENCE OF or OCTET STRING etc has no constraints")

let rec TypeCArrayItemsCount (t:Asn1Type) (r:AstRoot)=  
    match t.Kind with
    | BitString(_)  -> BigInteger(System.Math.Ceiling(float(TypeItemsCount t r)/8.0))
    | IA5String | NumericString     -> (TypeItemsCount t r) + 1I
    | SequenceOf(_) | OctetString   -> TypeItemsCount t r
    | ReferenceType(_)  ->
        let basetype = Ast.GetBaseTypeConsIncluded t r
        TypeCArrayItemsCount basetype r
    |_                       -> raise(BugErrorException "TypeCArrayItemsCount called for a non sizeable type")


let TypeArrayPostfix (t:Asn1Type) (r:AstRoot)= 
    match t.Kind with
    | IA5String | NumericString     -> "[" + (TypeCArrayItemsCount t r).ToString() + "]"
    | ReferenceType(refCon, _, _)      -> "" 
    | _                             -> ""

let rec TypeStar (t:Asn1Type) (r:AstRoot)= 
    match t.Kind with
    | IA5String | NumericString -> ""
    | ReferenceType(_)   -> TypeStar (Ast.GetActualType t r) r
    | _                  -> "*"



let TypeLongName (p:list<string>) =
    let keyAsStr = p.Tail
    ToC (keyAsStr.StrJoin("_").Replace("#","elem"))

    
