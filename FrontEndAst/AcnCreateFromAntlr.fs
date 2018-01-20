﻿(*
* Copyright (c) 2008-2012 Semantix and (c) 2012-2015 Neuropublic
*
* This file is part of the ASN1SCC tool.
*
* Licensed under the terms of GNU General Public Licence as published by
* the Free Software Foundation.
*
*  For more informations see License.txt file
*)

module AcnCreateFromAntlr

open System.Linq
open System.Numerics
open Antlr.Acn
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open FsUtils


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN PRIVATE TYPES (NOT EXPOSED TO OTHER MODULES) /////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type private GenericAcnPresentWhenCondition =
    | GP_PresenceBool  of Asn1AcnAst.RelativePath                         
    | GP_PresenceInt   of Asn1AcnAst.RelativePath*IntLoc
    | GP_PresenceStr   of Asn1AcnAst.RelativePath*StringLoc          
    
type private GenAcnEncodingProp =
    | GP_PosInt
    | GP_TwosComplement
    | GP_Ascii
    | GP_BCD
    | GP_IEEE754_32
    | GP_IEEE754_64

type private GenSizeProperty = 
    | GP_Fixed                 of IntLoc
    | GP_NullTerminated        
    | GP_SizeDeterminant       of Asn1AcnAst.RelativePath



type private GenericAcnProperty = 
    | ENCODING          of GenAcnEncodingProp
    | SIZE              of GenSizeProperty
    | ALIGNTONEXT       of Asn1AcnAst.AcnAligment
    | ENCODE_VALUES   
    | PRESENT_WHEN      of GenericAcnPresentWhenCondition list
    | TRUE_VALUE        of StringLoc
    | FALSE_VALUE       of StringLoc
    | PATTERN           of StringLoc
    | CHOICE_DETERMINANT of Asn1AcnAst.RelativePath
    | ENDIANNES         of Asn1AcnAst.AcnEndianness
    | ENUM_SET_VALUE    of IntLoc
    | TERMINATION_PATTERN of byte
    | MAPPING_FUNCTION  of StringLoc



type private AcnTypeEncodingSpec = {
    acnProperties   : GenericAcnProperty list
    children        : ChildSpec list
    loc             : SrcLoc
}

and private ChildSpec = {
    name            : StringLoc
    childEncodingSpec : AcnTypeEncodingSpec
    asn1Type        : Asn1AcnAst.AcnParamType option    // if present then it indicates an ACN inserted type
    argumentList    : Asn1AcnAst.RelativePath list
}

type private AcnTypeAssignment = {
    name            : StringLoc
    acnParameters   : Asn1AcnAst.AcnParameter list
    typeEncodingSpec: AcnTypeEncodingSpec
}

type private AcnModule = {
    name            : StringLoc
    typeAssignments : AcnTypeAssignment list
}


type private AcnFile = {
    antlrResult : ParameterizedAsn1Ast.AntlrParserResult
    modules     : AcnModule list
}

type private AcnAst = {
    files : AcnFile list
    acnConstants : Map<string, BigInteger>
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN CONSTANTS DEFINITION /////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

type private AcnIntExpr =
    | IntegerExpr       of AcnIntegerConstant
    | SumExpr           of AcnIntExpr*AcnIntExpr
    | MinExpr           of AcnIntExpr*AcnIntExpr
    | MulExpr           of AcnIntExpr*AcnIntExpr
    | DivExpr           of AcnIntExpr*AcnIntExpr
    | ModExpr           of AcnIntExpr*AcnIntExpr
    | PowExpr           of AcnIntExpr*AcnIntExpr
    | UnMinExp          of AcnIntExpr

and private AcnIntegerConstant =
    | IntConst of IntLoc
    | RefConst of StringLoc       //reference to other constant

type private AcnConstant = {
    Name  : StringLoc
    Value : AcnIntExpr
}

let rec private EvaluateConstant (constants:AcnConstant list) intConstant =
    match intConstant with
    | IntConst(a)   -> a.Value
    | RefConst(consLookUp)  ->
        match constants |> Seq.tryFind(fun c-> c.Name.Value = consLookUp.Value) with
        |None       -> raise(SemanticError(consLookUp.Location, (sprintf "Unknown symbol '%s'" consLookUp.Value)))
        |Some(cn)   -> EvaluateAcnIntExpression constants cn.Value

and private  EvaluateAcnIntExpression (constants:AcnConstant list) acnExpr = 
    match  acnExpr with
    | IntegerExpr(consta)   -> EvaluateConstant constants consta
    | SumExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) + (EvaluateAcnIntExpression constants exp2)
    | MinExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) - (EvaluateAcnIntExpression constants exp2)
    | MulExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) * (EvaluateAcnIntExpression constants exp2)
    | DivExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) / (EvaluateAcnIntExpression constants exp2)
    | ModExpr(exp1,exp2)    -> (EvaluateAcnIntExpression constants exp1) % (EvaluateAcnIntExpression constants exp2)
    | PowExpr(exp1,exp2)    -> 
        System.Numerics.BigInteger.Pow(EvaluateAcnIntExpression constants exp1, int (EvaluateAcnIntExpression constants exp2))
    | UnMinExp(exp1)        -> -(EvaluateAcnIntExpression constants exp1) 


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// CONVERT ACN ANTLR TO ACN PRIVATE TYPEA ///////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



let private CreateLongField(t:ITree) = t.Children |> List.map(fun x -> x.TextL) |> Asn1AcnAst.RelativePath

let private CreateAcnParamType (t:ITree) =
    match t.Type with
    | acnParser.INTEGER         -> Asn1AcnAst.AcnParamType.AcnPrmInteger t.Location
    | acnParser.BOOLEAN         -> Asn1AcnAst.AcnParamType.AcnPrmBoolean t.Location
    | acnParser.NULL            -> Asn1AcnAst.AcnParamType.AcnPrmNullType t.Location
    | acnParser.REFERENCED_TYPE -> 
        let mdName, tsName = 
            match t.Children with
            | first::[]             -> first.GetAncestor(acnParser.MODULE_DEF).GetChild(0).TextL,first.TextL
            | first::sec::[]        -> first.TextL,sec.TextL
            | _                     -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnAsn1Type 1"))
        Asn1AcnAst.AcnParamType.AcnPrmRefType(mdName, tsName)
    | _                         -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnAsn1Type 2"))



//Search in the antlr acn AST for a specific module, typeassigment and
//return the ACN parameters as a string list
let private GetParams (files:ParameterizedAsn1Ast.AntlrParserResult list) modName tasName  =
    let GetParamsAux (asn1File:ITree) =
        match  asn1File.Children |> Seq.tryFind(fun x -> x.GetChild(0).Text = modName)   with
        | None      -> []
        | Some(md)  ->
            match md.GetChildrenByType(acnParser.TYPE_ENCODING) |> Seq.tryFind(fun x -> x.GetChild(0).Text = tasName)   with
            | None  -> []
            | Some(tas) ->
                match tas.GetOptChild(acnParser.PARAM_LIST) with
                | None          -> []
                | Some(prmLst)  -> prmLst.Children |> List.map(fun p -> p.GetChild(1).Text)
    files |>  List.map (fun pr -> GetParamsAux pr.rootItem) |> List.collect(fun x -> x)


let private CreateNamedExpression (t:ITree) : AcnConstant= 
    let CreateAcnIntegerConstant  (t:ITree) = 
        match t.Type with
        | acnParser.INT                 -> IntConst(t.BigIntL)
        | acnParser.UID                 -> RefConst(t.TextL)
        | _                             -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnIntegerConstant"))
    let rec CreateExpression  (t:ITree) = 
        match t.Type with
        | acnParser.INT | acnParser.UID -> IntegerExpr(CreateAcnIntegerConstant  t)
        | acnParser.PLUS                -> SumExpr(CreateExpression  (t.GetChild(0)), CreateExpression  (t.GetChild(1)))
        | acnParser.MINUS               -> MinExpr(CreateExpression  (t.GetChild(0)), CreateExpression  (t.GetChild(1)))
        | acnParser.MULTIPLICATION      -> MulExpr(CreateExpression  (t.GetChild(0)), CreateExpression  (t.GetChild(1)))
        | acnParser.DIVISION            -> DivExpr(CreateExpression  (t.GetChild(0)), CreateExpression  (t.GetChild(1)))
        | acnParser.MODULO              -> ModExpr(CreateExpression  (t.GetChild(0)), CreateExpression  (t.GetChild(1)))
        | acnParser.POWER_SYMBOL        -> PowExpr(CreateExpression  (t.GetChild(0)), CreateExpression  (t.GetChild(1)))
        | acnParser.UNARY_MINUS         -> UnMinExp(CreateExpression (t.GetChild(0)))
        | _                             -> raise( BugErrorException("AcnCreateFromAntlr::CreateExpression Unsupported operator"))
    {AcnConstant.Name = t.GetChild(0).TextL;  Value = CreateExpression  (t.GetChild(1)) }


let private CheckCircularDependenciesInAcnConstants (constants : List<ITree>) =
    let HandleConstant (t:ITree) =
        let rec GetNamesFromExpr (t:ITree) =
            seq {
                match t.Type with
                | acnParser.INT                 -> ()
                | acnParser.UID                 -> yield t.TextL
                | acnParser.PLUS |acnParser.MINUS | acnParser.MULTIPLICATION | acnParser.DIVISION | acnParser.MODULO | acnParser.POWER_SYMBOL        -> 
                    yield! GetNamesFromExpr (t.GetChild(0))
                    yield! GetNamesFromExpr (t.GetChild(1))
                | acnParser.UNARY_MINUS         -> yield! GetNamesFromExpr (t.GetChild(0)) 
                | _                             -> raise(BugErrorException("CheckCircularDependenciesInAcnConstants.HandleConstant.GetNamesFromExpr Unsupported operator"))
                } |> Seq.toList
        (t.GetChild(0).TextL, GetNamesFromExpr (t.GetChild(1)))
    let constantsExpanded = constants |> List.map HandleConstant
    let independentConstants = constantsExpanded |> List.filter(fun (nm, lst) -> lst.IsEmpty ) |> List.map fst
    let dependentConstansts = constantsExpanded |> List.filter(fun (nm, lst) -> not (lst.IsEmpty) )
    let comparer (s1:StringLoc) (s2:StringLoc) = s1.Value = s2.Value
    let ExToThrow (cyclicDepds:List<StringLoc*List<StringLoc>>) = 
        match cyclicDepds with
        | []        -> raise(BugErrorException(""))
        | (x,_)::xs -> 
            let printConstant (md:StringLoc, deps: StringLoc list) = 
                sprintf "Anc constant '%s' depends on : %s" md.Value (deps |> List.map(fun z -> "'" + z.Value + "'") |> Seq.StrJoin ", ")
            let names = cyclicDepds |> List.map printConstant |> Seq.StrJoin "\n\tand\n"
            //let names = cyclicDepds |> Seq.map (fun (n,_) -> n.Value) |> Seq.StrJoin ", "
            SemanticError(x.Location, sprintf "Cyclic dependencies in ACN constants: %s" names)
    DoTopologicalSort2 independentConstants dependentConstansts comparer ExToThrow |> ignore


let private creareAcnProperty (acnConstants : Map<string, BigInteger>) (t:ITree) =
    let CreateAcnIntegerConstant  (t:ITree) = 
        match t.Type with
        | acnParser.INT                 -> t.BigIntL
        | acnParser.UID                 -> 
            match acnConstants.TryFind t.Text with
            | Some ret -> {IntLoc.Location = t.Location; Value=ret}
            | None     -> raise (SemanticError(t.Location, (sprintf "No ACN constant is defined with name '%s'" t.Text)))
        | _                             -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnIntegerConstant"))
    let GetActualString (str:string) = 
        let strVal = str.Substring(1)
        strVal.Remove(strVal.Length-2).Replace("\r", "").Replace("\n", "").Replace("\t", "").Replace(" ", "")

    match t.Type with
    | acnParser.ENCODING    ->
        match t.GetChild(0).Type with
        | acnParser.POS_INT             -> ENCODING GP_PosInt
        | acnParser.TWOSCOMPLEMENT      -> ENCODING GP_TwosComplement
        | acnParser.BCD                 -> ENCODING GP_BCD
        | acnParser.ASCII               -> ENCODING GP_Ascii
        | acnParser.IEEE754_1985_32     -> ENCODING GP_IEEE754_32
        | acnParser.IEEE754_1985_64     -> ENCODING GP_IEEE754_64
        | _                             -> raise(BugErrorException("creareAcnProperty_ENCODING"))
    | acnParser.SIZE    ->
        match t.GetChild(0).Type with
        | acnParser.NULL_TERMINATED     -> SIZE GP_NullTerminated
        | acnParser.INT                 
        | acnParser.UID                 -> SIZE (GP_Fixed (CreateAcnIntegerConstant (t.GetChild 0)))    
        | acnParser.LONG_FIELD          -> SIZE (GP_SizeDeterminant (CreateLongField (t.GetChild 0)))
        | _                             -> raise(BugErrorException("creareAcnProperty_SIZE"))
    | acnParser.ALIGNTONEXT ->
        match t.GetChild(0).Type with 
        | acnParser.BYTE                -> ALIGNTONEXT Asn1AcnAst.NextByte
        | acnParser.WORD                -> ALIGNTONEXT Asn1AcnAst.NextWord
        | acnParser.DWORD               -> ALIGNTONEXT Asn1AcnAst.NextDWord
        | _                             -> raise(BugErrorException("creareAcnProperty_ALIGNTONEXT"))
    | acnParser.ENCODE_VALUES           -> ENCODE_VALUES
    | acnParser.PRESENT_WHEN            -> 
        let CreateAcnPresenseCondition(t:ITree) = 
            match t.Type with
            | acnParser.LONG_FIELD  -> GP_PresenceBool(CreateLongField t)
            | acnParser.EQUAL       -> GP_PresenceInt ((CreateLongField(t.GetChild 0)), CreateAcnIntegerConstant (t.GetChild 1))
            | acnParser.PRESENT_WHEN_STR_EQUAL -> 
                let txt = (t.GetChild 1).Text.Replace("\"","")
                let txtL = { StringLoc.Value = txt; Location = (t.GetChild 1).Location}
                GP_PresenceStr ((CreateLongField(t.GetChild 0)), txtL )
            | _                     -> raise(BugErrorException("creareAcnProperty_PRESENT_WHEN"))
        PRESENT_WHEN (t.Children |> List.map CreateAcnPresenseCondition )

    | acnParser.TRUE_VALUE              -> 
        let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
        TRUE_VALUE v
    | acnParser.FALSE_VALUE             -> 
        let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
        FALSE_VALUE v
    | acnParser.PATTERN                 -> 
        let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
        PATTERN v
    | acnParser.DETERMINANT             -> CHOICE_DETERMINANT (CreateLongField(t.GetChild 0))
    | acnParser.ENDIANNES               -> 
        match t.GetChild(0).Type with 
        | acnParser.BIG                 -> ENDIANNES Asn1AcnAst.BigEndianness
        | acnParser.LITTLE              -> ENDIANNES Asn1AcnAst.LittleEndianness
        | _                             -> raise(BugErrorException("creareAcnProperty_ENDIANNES"))
    | acnParser.MAPPING_FUNCTION        -> MAPPING_FUNCTION (t.GetChild(0).TextL)
    | acnParser.INT                     -> ENUM_SET_VALUE t.BigIntL
    | acnParser.TERMINATION_PATTERN     -> 
        let tp = t
        let bitPattern = GetActualString (tp.GetChild(0).Text)
        match tp.GetChild(0).Type with
        | acnParser.BitStringLiteral    ->
            match bitPattern.Length <> 8 with
            | true  -> raise(SemanticError(tp.Location, sprintf "ternination-patern value must be a byte"  ))
            | false ->
                let byteVal = 
                    bitPattern.ToCharArray() |> 
                    Seq.fold(fun (p,cs) c -> if c='0' then (p/2,cs) else (p/2,p+cs) ) (128, 0) 
                    |> snd |> byte
                TERMINATION_PATTERN byteVal
        | acnParser.OctectStringLiteral ->
            match bitPattern.Length <> 2 with
            | true  -> raise(SemanticError(tp.Location, sprintf "ternination-patern value must be a byte"  ))
            | false ->
                TERMINATION_PATTERN (System.Byte.Parse(bitPattern, System.Globalization.NumberStyles.AllowHexSpecifier))
        | _     ->  raise(BugErrorException("creareAcnProperty_TERMINATION_PATTERN"))
    | _                             -> raise(BugErrorException("creareAcnProperty"))

let rec  private createTypeEncodingSpec (allAcnFiles: ParameterizedAsn1Ast.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: ParameterizedAsn1Ast.AntlrParserResult)  (encSpecITree:ITree) : AcnTypeEncodingSpec =
    let acnProperties = 
        match encSpecITree.GetOptChild(acnParser.ENCODING_PROPERTIES) with
        | None              -> []
        | Some(propList)    -> propList.Children |> List.map (creareAcnProperty acnConstants)
    
    let children = 
        match encSpecITree.GetOptChild(acnParser.CHILDREN_ENC_SPEC) with
        | None              -> []
        | Some childrenList ->
            let createChild (t:ITree) =
                let name  = 
                    match t.GetOptChild(acnParser.LID) with
                    | None          -> StringLoc.ByValue "#"
                    | Some(lid)     -> lid.TextL
                let argumentList    =
                        match t.GetOptChild(acnParser.ARGUMENTS) with
                        | None            -> []
                        | Some(argList)   -> argList.Children |> List.map CreateLongField
                let childEncodingSpec = createTypeEncodingSpec allAcnFiles acnConstants thisAcnFile (t.GetChildByType acnParser.ENCODING_SPEC) 
                let asn1Type  =
                    match t.Type with
                    | acnParser.CHILD       -> None
                    | acnParser.CHILD_NEW   -> Some (CreateAcnParamType (t.GetChild 1) ) 
                    | _     ->  raise(BugErrorException("createTypeEncodingSpec_CHILD"))

                {ChildSpec.name = name; childEncodingSpec= childEncodingSpec; asn1Type=asn1Type; argumentList=argumentList}
            childrenList.Children |> List.map createChild
    {AcnTypeEncodingSpec.acnProperties = acnProperties; children = children; loc = encSpecITree.Location}

let private CreateTypeAssignment (allAcnFiles: ParameterizedAsn1Ast.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: ParameterizedAsn1Ast.AntlrParserResult)  (tasTree:ITree) : AcnTypeAssignment =
    let tasNameL = tasTree.GetChildByType(acnParser.UID).TextL

    let encSpecITree = tasTree.GetChildByType(acnParser.ENCODING_SPEC)
    let prms = 
        match tasTree.GetOptChild(acnParser.PARAM_LIST) with
        | None -> []
        | Some(paramList) -> 
            let CreateParam (x:ITree) =
                let prmName = x.GetChild(1).Text
                let loc = x.GetChild(1).Location
                //check that all parameters are used
                let refs = encSpecITree.AllChildren |> List.filter(fun x -> x.Type = acnParser.LONG_FIELD && x.ChildCount=1) |> List.map(fun x -> x.GetChild(0).Text)
                match refs |> Seq.tryFind(fun x -> x = prmName) with
                | Some(_)   -> 
                    //parameter id is initially set to an invalid value.
                    //It takes the correct value when the ASN.1 is constructed.
                    {Asn1AcnAst.AcnParameter.name = prmName; Asn1AcnAst.AcnParameter.asn1Type=CreateAcnParamType (x.GetChild(0)) ; Asn1AcnAst.AcnParameter.loc = loc; Asn1AcnAst.id = ReferenceToType([]) }
                | None      -> raise(SemanticError(loc, sprintf "unreferenced parameter '%s'" prmName))
            paramList.Children |> List.map CreateParam

    let typeEncodingSpec = createTypeEncodingSpec allAcnFiles acnConstants thisAcnFile encSpecITree
    {AcnTypeAssignment.name = tasNameL; acnParameters = prms; typeEncodingSpec = typeEncodingSpec}


let private CreateModule (allAcnFiles: ParameterizedAsn1Ast.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: ParameterizedAsn1Ast.AntlrParserResult)   (modTree : ITree) : AcnModule =
    let modNameL = modTree.GetChildByType(acnParser.UID).TextL

    let tasITreeList = modTree.GetChildrenByType(acnParser.TYPE_ENCODING)
    
    //check for duplicate type assignments in the ACN module
    tasITreeList |> List.map(fun x -> x.GetChildByType(acnParser.UID).TextL) |> CheckForDuplicates

    let newTasses = tasITreeList |> List.map(fun tasTree -> CreateTypeAssignment allAcnFiles acnConstants thisAcnFile tasTree) 
    
    {AcnModule.name = modNameL; typeAssignments = newTasses}


let private LoadAcnFile (allAcnFiles: ParameterizedAsn1Ast.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: ParameterizedAsn1Ast.AntlrParserResult)   : AcnFile = 
    let modules = thisAcnFile.rootItem.Children |> List.map (CreateModule allAcnFiles acnConstants thisAcnFile)
    {AcnFile.antlrResult = thisAcnFile; modules = modules}

let private CreateAcnAst (allAcnFiles: ParameterizedAsn1Ast.AntlrParserResult list) : AcnAst =  
    ITree.RegisterFiles(allAcnFiles|> Seq.map(fun pr -> (pr.rootItem, pr.fileName)))
    let constants = seq {
        for acnFile in allAcnFiles do
            for m in acnFile.rootItem.Children do
                for c in m.GetChildrenByType(acnParser.CONSTANT) do
                    yield c } |> Seq.toList

    let constantNames = constants |> List.map(fun c -> c.GetChild(0).TextL)

    // check that all constant names are unique
    constantNames |> CheckForDuplicates 

    CheckCircularDependenciesInAcnConstants constants

    let constantValues = constants |> List.map CreateNamedExpression  
    let acnConstantsMap = constantValues |> List.map(fun c -> c.Name.Value, EvaluateAcnIntExpression constantValues c.Value) |> Map.ofList

    let acnFiles = allAcnFiles |> List.map (LoadAcnFile allAcnFiles acnConstantsMap)
    {AcnAst.files = acnFiles; acnConstants = acnConstantsMap}


let private tryFindAcnTypeByName modName tasName (r:AcnAst) =
    match r.files |> List.collect (fun f -> f.modules) |> Seq.tryFind(fun m -> m.name = modName) with
    | None  -> None
    | Some m-> m.typeAssignments |> Seq.tryFind (fun t -> t.name = tasName)


open Asn1AcnAst
open Asn1AcnAstUtilFunctions

let private tryGetProp (props:GenericAcnProperty list) fnPropKind = 
    match props |> List.choose fnPropKind  with
    | e::_  -> Some e
    | _     -> None

let private  getEndianessProperty (props:GenericAcnProperty list) = 
    tryGetProp props (fun x -> match x with ENDIANNES e -> Some e | _ -> None)  


let private getIntSizeProperty  errLoc (props:GenericAcnProperty list) = 
    match tryGetProp props (fun x -> match x with SIZE e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_Fixed           v)   -> Some(Fixed (int v.Value))
    | Some (GP_NullTerminated   )   -> 
        match tryGetProp props (fun x -> match x with TERMINATION_PATTERN e -> Some e | _ -> None) with
        | Some b    -> Some(IntNullTerminated b)
        | None      -> Some(IntNullTerminated (byte 0))
    | Some (GP_SizeDeterminant _)   -> raise(SemanticError(errLoc ,"Expecting an Integer value or an ACN constant as value for the size property"))

let private getStringSizeProperty minSize maxSize errLoc (props:GenericAcnProperty list) = 
    match tryGetProp props (fun x -> match x with SIZE e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_Fixed           v)   -> 
        match minSize = maxSize with
        | false ->
            let errMessage = sprintf "The size constraints of the ASN.1  allows variable items (%d .. %d). Therefore, you should either remove the size property (in which case the size determinant will be encoded automatically exactly like uPER), or use a an Integer field as size determinant"  minSize maxSize
            raise(SemanticError(errLoc ,errMessage))
        | true  ->
            match minSize = int v.Value with
            | true  -> None
            | false -> raise(SemanticError(errLoc , (sprintf "The size property must either be ommited or have the fixed value %d" minSize)))
    | Some (GP_NullTerminated   )   -> 
        match tryGetProp props (fun x -> match x with TERMINATION_PATTERN e -> Some e | _ -> None) with
        | Some b    -> Some(StrNullTerminated b)
        | None      -> Some(StrNullTerminated (byte 0))
    | Some (GP_SizeDeterminant fld)   -> (Some (StrExternalField fld))

let private getSizeableSizeProperty minSize maxSize errLoc (props:GenericAcnProperty list) = 
    match tryGetProp props (fun x -> match x with SIZE e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_NullTerminated   )        -> raise(SemanticError(errLoc ,"Acn proporty 'size null-terminated' is supported only in IA5String and NumericString string types and in Integer types and when encoding is ASCII"))
    | Some (GP_Fixed           v)   ->
        match minSize = maxSize with
        | false ->
            let errMessage = sprintf "The size constraints of the ASN.1  allows variable items (%d .. %d). Therefore, you should either remove the size property (in which case the size determinant will be encoded automatically exactly like uPER), or use a an Integer field as size determinant"  minSize maxSize
            raise(SemanticError(errLoc ,errMessage))
        | true  ->
            match minSize = int v.Value with
            | true  -> None
            | false -> raise(SemanticError(errLoc , (sprintf "The size property must either be ommited or have the fixed value %d" minSize)))
    | Some (GP_SizeDeterminant fld)   -> (Some fld)

let private getIntEncodingProperty errLoc (props:GenericAcnProperty list) = 
    match tryGetProp props (fun x -> match x with ENCODING e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_PosInt         ) ->  Some (PosInt)
    | Some (GP_TwosComplement ) ->  Some (TwosComplement)
    | Some (GP_Ascii          ) ->  Some (IntAscii)
    | Some (GP_BCD            ) ->  Some (BCD)        
    | Some (GP_IEEE754_32     ) 
    | Some (GP_IEEE754_64     ) ->   raise(SemanticError(errLoc ,"The encoding property was expected to be one of 'pos-int','twos-complement','BCD' or 'ASCII' "))

let private getRealEncodingProperty errLoc (props:GenericAcnProperty list) = 
    match tryGetProp props (fun x -> match x with ENCODING e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_PosInt         ) 
    | Some (GP_TwosComplement ) 
    | Some (GP_Ascii          ) 
    | Some (GP_BCD            ) ->  raise(SemanticError(errLoc ,"Invalid encoding property value. Expecting 'IEEE754-1985-32' or 'IEEE754-1985-64'"))
    | Some (GP_IEEE754_32     ) ->  Some (IEEE754_32)
    | Some (GP_IEEE754_64     ) ->  Some (IEEE754_64) 

let private getStringEncodingProperty errLoc (props:GenericAcnProperty list) = 
    match tryGetProp props (fun x -> match x with ENCODING e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_PosInt         ) 
    | Some (GP_TwosComplement ) 
    | Some (GP_IEEE754_32     ) 
    | Some (GP_IEEE754_64     ) 
    | Some (GP_BCD            ) ->  raise(SemanticError(errLoc ,"The encoding property was expected to be 'ASCII' or empty"))
    | Some (GP_Ascii          ) -> Some (StrAscii)

let private mergeInteger (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons =
    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let rootCons = cons |> List.filter(fun c -> match c with RangeRootConstraint _  | RangeRootConstraint2 _ -> true | _ -> false)
    let uperRange    = uPER.getIntTypeConstraintUperRange cons  loc
    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                IntegerAcnProperties.encodingProp    = getIntEncodingProperty acnErrLoc props
                sizeProp                             = getIntSizeProperty acnErrLoc props
                endiannessProp                       = getEndianessProperty props
            }
        | None  -> {IntegerAcnProperties.encodingProp = None; sizeProp = None; endiannessProp = None }
    let uperMinSizeInBits, uperMaxSizeInBits = 
        match rootCons with
        | []  -> uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes uperRange
        | _   -> 
            let mn,mx = uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes uperRange
            1 + mn, 1 + mx

    let isUnsigned =
        match uperRange with
        | Concrete  (a,b) when a >= 0I && rootCons.IsEmpty-> true      //[a, b]
        | Concrete  _                  -> false
        | NegInf    _                  -> false    //(-inf, b]
        | PosInf   a when a >= 0I  && rootCons.IsEmpty    -> true     //[a, +inf)
        | PosInf  _                    -> false    //[a, +inf)
        | Full    _                    -> false    // (-inf, +inf)

    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetIntEncodingClass aligment acnErrLoc0 acnProperties uperMinSizeInBits uperMaxSizeInBits isUnsigned

    let asn1Min, asn1Max =
        match uperRange with
        | Concrete  (a,b)                   -> (a,b)
        | NegInf    b                       -> (asn1.args.SIntMin, b)    //(-inf, b]
        | PosInf   a    when a >= 0I        -> (a, asn1.args.UIntMax)     //[a, +inf)
        | PosInf   a                        -> (a, asn1.args.SIntMax)
        | Full    _                         -> (asn1.args.SIntMin, asn1.args.SIntMax)

    let checkEnoughSpace =
        let check_ (minEnc : BigInteger) (maxEnc:BigInteger) =
            match minEnc <= asn1Min && asn1Max <= maxEnc with
            | true                              -> ()
            | false  when not (asn1Max <= maxEnc)     -> 
                let errMessage = sprintf "The applied ACN encoding does not allow values larger than %A to be encoded, while the corresponding ASN.1 type allows values up to %A" maxEnc asn1Max
                raise(SemanticError(acnErrLoc0, errMessage))
            | false                             -> 
                let errMessage = sprintf "The applied ACN encoding does not allow values smaller than %A to be encoded, while the corresponding ASN.1 type allows values up to %A" minEnc asn1Min
                raise(SemanticError(acnErrLoc0, errMessage))
        let checkBinary isUnsigned lengthInBiths =
            match isUnsigned  with
            | true              -> check_ 0I (BigInteger.Pow(2I, lengthInBiths) - 1I) 
            | false             -> check_ (-BigInteger.Pow(2I, lengthInBiths - 1)) ((BigInteger.Pow(2I, lengthInBiths - 1)) - 1I)
        let checkAscii isUnsigned lengthInBiths =
            let digits = lengthInBiths / 8
            match isUnsigned  with
            | true              -> check_ 0I (BigInteger.Pow(10I, digits) - 1I) 
            | false             -> check_ (-BigInteger.Pow(10I, digits - 1)) ((BigInteger.Pow(10I, digits - 1)) - 1I)
        let checkBCD lengthInBiths =
            let digits = lengthInBiths / 4
            check_ 0I (BigInteger.Pow(10I, digits) - 1I) 


        match acnEncodingClass with
        |Integer_uPER                                   -> ()
        |PositiveInteger_ConstSize_8                    -> checkBinary true 8
        |PositiveInteger_ConstSize_big_endian_16        -> checkBinary true 16
        |PositiveInteger_ConstSize_little_endian_16     -> checkBinary true 16
        |PositiveInteger_ConstSize_big_endian_32        -> checkBinary true 32
        |PositiveInteger_ConstSize_little_endian_32     -> checkBinary true 32
        |PositiveInteger_ConstSize_big_endian_64        -> checkBinary true 64
        |PositiveInteger_ConstSize_little_endian_64     -> checkBinary true 64
        |PositiveInteger_ConstSize nBits                -> checkBinary true nBits
        |TwosComplement_ConstSize_8                     -> checkBinary false 8
        |TwosComplement_ConstSize_big_endian_16         -> checkBinary false 16
        |TwosComplement_ConstSize_little_endian_16      -> checkBinary false 16
        |TwosComplement_ConstSize_big_endian_32         -> checkBinary false 32
        |TwosComplement_ConstSize_little_endian_32      -> checkBinary false 32
        |TwosComplement_ConstSize_big_endian_64         -> checkBinary false 64
        |TwosComplement_ConstSize_little_endian_64      -> checkBinary false 64
        |TwosComplement_ConstSize nBits                 -> checkBinary false nBits
        |ASCII_ConstSize nBits                          -> checkAscii false nBits
        |ASCII_VarSize_NullTerminated _                 -> ()
        |ASCII_UINT_ConstSize nBits                     -> checkAscii false nBits
        |ASCII_UINT_VarSize_NullTerminated _            -> ()
        |BCD_ConstSize nBits                            -> checkBCD nBits
        |BCD_VarSize_NullTerminated _                   -> ()


    {Integer.acnProperties = acnProperties; cons = cons; withcons = withcons; uperRange = uperRange;uperMinSizeInBits=uperMinSizeInBits; uperMaxSizeInBits=uperMaxSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; isUnsigned= isUnsigned}

let private mergeReal (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons =
    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    
    //check for invalid properties
    props |> 
    Seq.iter(fun pr -> 
        match pr with
        | SIZE  _   -> raise(SemanticError(acnErrLoc0, "Acn property 'size' cannot be applied to REAL types"))
        | _         -> ())

    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                RealAcnProperties.encodingProp    = getRealEncodingProperty acnErrLoc props
                endiannessProp                    = getEndianessProperty props
            }
        | None  -> {RealAcnProperties.encodingProp = None; endiannessProp = None }
    let uperRange    = uPER.getRealTypeConstraintUperRange cons loc
    let uperMaxSizeInBits=(5+asn1.args.integerSizeInBytes)*8
    let uperMinSizeInBits=8
    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetRealEncodingClass aligment loc acnProperties uperMinSizeInBits uperMaxSizeInBits 
    {Real.acnProperties = acnProperties; cons = cons; withcons = withcons; uperRange=uperRange; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits}

let private mergeStringType (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons defaultCharSet =
    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let sizeUperRange = uPER.getSrtingSizeUperRange cons loc
    let uperCharSet = uPER.getSrtingAlphaUperRange cons defaultCharSet loc
    let minSize, maxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange
    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                StringAcnProperties.encodingProp    = getStringEncodingProperty acnErrLoc props
                sizeProp                            = getStringSizeProperty minSize maxSize acnErrLoc props
            }
        | None  -> {StringAcnProperties.encodingProp = None; sizeProp = None }
    
    let charSize =  int (GetNumberOfBitsForNonNegativeInteger (BigInteger (uperCharSet.Length-1)))
    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize maxSize charSize
    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)

    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetStringEncodingClass aligment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize maxSize uperCharSet

    match acnEncodingClass with                                          
    | Acn_Enc_String_uPER                                                -> ()
    | Acn_Enc_String_uPER_Ascii                                          -> ()
    | Acn_Enc_String_Ascii_Null_Teminated                   nullChar     -> 
        match uperCharSet |> Seq.exists ((=) (System.Convert.ToChar nullChar)) with
        | true  when nullChar <> (byte 0) -> raise(SemanticError(acnErrLoc0, "The termination-pattern defines a character which belongs to the allowed values of the ASN.1 type. Use another value in the termination-pattern or apply different constraints in the ASN.1 type."))
        | _ -> ()
    | Acn_Enc_String_Ascii_External_Field_Determinant       relativePath -> ()
    | Acn_Enc_String_CharIndex_External_Field_Determinant   relativePath -> ()




    {StringType.acnProperties = acnProperties; cons = cons; withcons = withcons; minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; uperCharSet=uperCharSet; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits}

let private mergeOctetStringType (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons =
    let sizeUperRange = uPER.getOctetStringUperRange cons loc
    let minSize, maxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange
    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize maxSize 8
    let fixAsn1Size = match minSize = maxSize with true -> Some minSize | false -> None

    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    -> {SizeableAcnProperties.sizeProp = getSizeableSizeProperty minSize maxSize acnErrLoc props}
        | None              -> {SizeableAcnProperties.sizeProp = None }


    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetOctetStringEncodingClass aligment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize maxSize 

    {OctetString.acnProperties = acnProperties; cons = cons; withcons = withcons; minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits}

let private mergeBitStringType (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons =
    let sizeUperRange = uPER.getBitStringUperRange cons loc
    let minSize, maxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange
    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize maxSize 8
    let fixAsn1Size = match minSize = maxSize with true -> Some minSize | false -> None

    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    -> { SizeableAcnProperties.sizeProp  = getSizeableSizeProperty minSize maxSize acnErrLoc props}
        | None              -> {SizeableAcnProperties.sizeProp = None }

    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetBitStringEncodingClass aligment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize maxSize 

    {BitString.acnProperties = acnProperties; cons = cons; withcons = withcons; minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits}

let private mergeNullType (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) =
    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    -> { NullTypeAcnProperties.encodingPattern  = tryGetProp props (fun x -> match x with PATTERN e -> Some e | _ -> None)}
        | None              -> {NullTypeAcnProperties.encodingPattern = None }

    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetNullEncodingClass aligment loc acnProperties
    {NullType.acnProperties = acnProperties; uperMaxSizeInBits = 0; uperMinSizeInBits=0;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits}

let private mergeBooleanType (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons =
    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    -> 
            match tryGetProp props (fun x -> match x with TRUE_VALUE e -> Some e | _ -> None) with
            | Some tv   ->  {BooleanAcnProperties.encodingPattern  = Some (TrueValue tv)}
            | None      ->
                match tryGetProp props (fun x -> match x with FALSE_VALUE e -> Some e | _ -> None) with
                | Some tv   ->  {BooleanAcnProperties.encodingPattern  = Some (FalseValue tv)}
                | None      ->  {BooleanAcnProperties.encodingPattern  = None}
        | None              -> {BooleanAcnProperties.encodingPattern = None }
    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetBooleanEncodingClass aligment loc acnProperties
    {Boolean.acnProperties = acnProperties; cons = cons; withcons = withcons;uperMaxSizeInBits = 1; uperMinSizeInBits=1; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits}




let private mergeEnumerated (asn1:Asn1Ast.AstRoot) (items: Asn1Ast.NamedItem list) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (acnType:AcnTypeEncodingSpec option) (props:GenericAcnProperty list) cons withcons  =
    let endodeValues = 
        match tryGetProp props (fun x -> match x with ENCODE_VALUES -> Some true | _ -> None) with
        | Some  true    -> true
        | _             -> false

    let allocatedValuesToAllEnumItems (namedItems:Asn1Ast.NamedItem list) = 
        let createAsn1ValueByBigInt biVal = {Asn1Ast.Asn1Value.Kind = Asn1Ast.IntegerValue (IntLoc.ByValue biVal); Asn1Ast.Location = emptyLocation; Asn1Ast.id = ReferenceToValue([],[])}
        let allocated   = namedItems |> List.filter(fun x -> x._value.IsSome)
        let unallocated = namedItems |> List.filter(fun x -> x._value.IsNone)
        let rec allocateItems (unallocated:Asn1Ast.NamedItem list) (allocated:Asn1Ast.NamedItem list) potentialValue: Asn1Ast.NamedItem list =
            match unallocated with
            | []    -> allocated
            |x::xs  -> 
                let rec getValue (allocated:Asn1Ast.NamedItem list) (vl:BigInteger) =
                    match allocated |> Seq.exists(fun ni -> match ni._value with Some value -> vl = (Asn1Ast.GetValueAsInt value asn1) | None -> false) with
                    | false -> vl
                    | true  -> getValue allocated (vl + 1I)
                let vl = getValue allocated potentialValue
                let newAllocatedItems = allocated@[{x with _value = Some (createAsn1ValueByBigInt vl)} ]
                allocateItems xs newAllocatedItems (vl + 1I)
        let newItems = allocateItems unallocated allocated 0I |> List.sortBy(fun ni -> namedItems |> Seq.findIndex(fun x -> x.Name.Value = ni.Name.Value) )
        newItems
    let mapItem (i:int) (itm:Asn1Ast.NamedItem) =
        let definitionValue = Asn1Ast.GetValueAsInt itm._value.Value asn1
        match acnType with
        | None  ->
            let acnEncodeValue = (BigInteger i)
            {NamedItem.Name = itm.Name; Comments = itm.Comments; c_name = itm.c_name; ada_name = itm.ada_name; py_name = itm.py_name; definitionValue = definitionValue; acnEncodeValue = acnEncodeValue}        
        | Some acnType ->
            let acnEncodeValue = 
                match tryGetProp props (fun x -> match x with ENCODE_VALUES -> Some true | _ -> None) with
                | Some _    -> 
                    match acnType.children |> Seq.tryFind(fun z -> z.name.Value = itm.Name.Value) with
                    | Some acnNameItem    -> 
                        match tryGetProp acnNameItem.childEncodingSpec.acnProperties (fun x -> match x with ENUM_SET_VALUE newIntVal -> Some newIntVal | _ -> None) with
                        | Some intVal   -> intVal.Value
                        | None          -> definitionValue
                    | None      -> definitionValue
                | None      -> (BigInteger i)
            {NamedItem.Name = itm.Name; Comments = itm.Comments; c_name = itm.c_name; ada_name = itm.ada_name; py_name = itm.py_name; definitionValue = definitionValue; acnEncodeValue = acnEncodeValue}        

    let items0, userDefinedValues = 
        match items |> Seq.exists (fun nm -> nm._value.IsSome) with
        | false -> allocatedValuesToAllEnumItems items, false 
        | true -> allocatedValuesToAllEnumItems items, true
    let uperSizeInBits = int32(GetNumberOfBitsForNonNegativeInteger(BigInteger((Seq.length items) - 1)))
    let items = items0|> List.mapi mapItem
    
    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                IntegerAcnProperties.encodingProp    = getIntEncodingProperty acnErrLoc props
                sizeProp                             = 
                    getIntSizeProperty  acnErrLoc props
                endiannessProp                       = getEndianessProperty props
            }
        | None  -> {IntegerAcnProperties.encodingProp = None; sizeProp = None; endiannessProp = None }
    
    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetEnumeratedEncodingClass items aligment loc acnProperties uperSizeInBits uperSizeInBits endodeValues
    {Enumerated.acnProperties = acnProperties; items=items; cons = cons; withcons = withcons;uperMaxSizeInBits = uperSizeInBits; uperMinSizeInBits=uperSizeInBits;encodeValues=endodeValues; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits;userDefinedValues=userDefinedValues}

let rec private mergeAcnEncodingSpecs (thisType:AcnTypeEncodingSpec option) (baseType:AcnTypeEncodingSpec option) =
    match thisType, baseType with
    | None, None    -> None
    | None, Some x -> Some x
    | Some x, None -> Some x
    | Some thisType, Some baseType ->
        let mergedProperties = thisType.acnProperties@baseType.acnProperties
        let mergedChildren =
            let thisTypeNames = thisType.children |> List.map(fun x -> x.name)
            let baseTypeNames = 
                baseType.children |> 
                List.filter(fun x -> 
                    match thisType.children with
                    | []    -> true                             //if this type has children specification then use this type children
                    | _     -> false (*x.asn1Type.IsNone*)) |>  //otherwise use base 
                List.map(fun x -> x.name)   
            let combinedNames = thisTypeNames@baseTypeNames |> Seq.distinctBy(fun x -> x.Value) |> Seq.toList
            combinedNames |> 
                List.choose(fun nm ->
                    let e1 = thisType.children |> Seq.tryFind(fun x -> x.name = nm)
                    let e2 = baseType.children |> Seq.tryFind(fun x -> x.name = nm)
                    match e1, e2 with
                    | None, None    -> None
                    | None, Some x  -> Some x
                    | Some x, None  -> Some x
                    | Some thisChild, Some baseChild    ->
                        match mergeAcnEncodingSpecs (Some thisChild.childEncodingSpec) (Some baseChild.childEncodingSpec) with
                        | Some combinedEncoingSpec  ->
                            Some ({name = nm; childEncodingSpec = combinedEncoingSpec; asn1Type = thisChild.asn1Type; argumentList = thisChild.argumentList})
                        | None                      -> None)

        Some {AcnTypeEncodingSpec.acnProperties = mergedProperties; children = mergedChildren; loc = thisType.loc}        

(*
    match asn1Type with
    | AcnTypes.Integer   -> {Asn1Type.Kind = Ast.Integer; Constraints=[]; Location=emptyLocation; AcnProperties=acnChild.Properties}
    | AcnTypes.Boolean   -> {Asn1Type.Kind = Ast.Boolean; Constraints=[]; Location=emptyLocation; AcnProperties=acnChild.Properties}
    | AcnTypes.NullType  -> {Asn1Type.Kind = Ast.NullType; Constraints=[]; Location=emptyLocation; AcnProperties=acnChild.Properties}
    | AcnTypes.RefTypeCon(md,ts)  -> {Asn1Type.Kind = Ast.ReferenceType(md,ts, false); Constraints=[]; Location=emptyLocation; AcnProperties=acnChild.Properties}

*)

(*Removes Type inclusion and RangeContraint_MIN_MAX constraints*)    
let rec fixConstraint (asn1:Asn1Ast.AstRoot) (c:Asn1Ast.Asn1Constraint) =
    let intersect c1 cs =
        cs |> List.fold(fun fc cc -> (Asn1Ast.IntersectionConstraint (fc,cc))) c1
    let mergeConstraints (cc:Asn1Ast.Asn1Constraint list)  constConstr =
        match cc with
        | []        -> []
        | x1::[]    -> [constConstr x1]
        | x1::xs    -> 
            let sc = intersect x1 xs
            [constConstr sc]
    let mergeConstraints2 (c1:Asn1Ast.Asn1Constraint list) (c2:Asn1Ast.Asn1Constraint list) constConstr =
        match c1,c2 with
        | [], []    -> []
        | _::_, []  -> c1
        | [], _::_  -> c2
        | x1::xs1, x2::xs2      -> [constConstr (intersect x1 xs1)  (intersect x2 xs2) ]        

    Asn1Ast.foldConstraint
        (fun v          -> [Asn1Ast.SingleValueContraint(v)] )
        (fun a b b1 b2  -> [Asn1Ast.RangeContraint(a,b,b1,b2)])
        (fun a b1       -> [Asn1Ast.RangeContraint_val_MAX (a,b1)])
        (fun a b1       -> [Asn1Ast.RangeContraint_MIN_val (a,b1)])
        (fun ()         -> [])           
        (fun md ts      ->
            let actTypeAllCons = Asn1Ast.GetActualTypeByNameAllConsIncluded md ts asn1
            actTypeAllCons.Constraints |> List.collect (fixConstraint asn1))     
        (fun sc         -> mergeConstraints sc (fun c -> Asn1Ast.SizeContraint c) )        
        (fun sc         -> mergeConstraints sc (fun c -> Asn1Ast.AlphabetContraint c) )        
        (fun sc         -> mergeConstraints sc (fun c -> Asn1Ast.WithComponentConstraint c) )        
        (fun ni cons    -> 
            match cons with
            | None          -> [{ni with Contraint = None}]
            | Some cons     -> cons |> List.map (fun c -> {ni with Contraint = Some c}))
        (fun nameItems      -> [Asn1Ast.WithComponentsConstraint (nameItems |> List.collect id)])   
        (fun c1 c2 b        -> mergeConstraints2 c1 c2 (fun c1 c2 -> Asn1Ast.UnionConstraint   (c1,c2,b)))       
        (fun c1 c2          -> mergeConstraints2 c1 c2 (fun c1 c2 -> Asn1Ast.IntersectionConstraint   (c1,c2)))       
        (fun sc             -> mergeConstraints sc (fun c -> Asn1Ast.AllExceptConstraint c) )        
        (fun c1 c2          ->
            match c1,c2 with
            | [], []    -> []
            | _::_, []  -> c1
            | [], _::_  -> mergeConstraints c2 (fun c -> Asn1Ast.AllExceptConstraint c)
            | _, _      -> mergeConstraints2 c1 c2 (fun c1 c2 -> Asn1Ast.ExceptConstraint   (c1,c2)))
        (fun sc             -> mergeConstraints sc (fun c -> Asn1Ast.RootConstraint c) )        
        (fun c1 c2          -> mergeConstraints2 c1 c2 (fun c1 c2 -> Asn1Ast.RootConstraint2   (c1,c2)))       
        c


let rec private mapAcnParamTypeToAcnAcnInsertedType (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (p:AcnParamType) (props:GenericAcnProperty list) =
    let  acnAligment     = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    match p with
    | AcnPrmInteger acnErrLoc -> 
        let acnProperties = {IntegerAcnProperties.encodingProp = getIntEncodingProperty acnErrLoc props; sizeProp = getIntSizeProperty acnErrLoc props; endiannessProp = getEndianessProperty props}
        let encProp = 
            match acnProperties.encodingProp with
            | Some e -> e
            | None   -> raise(SemanticError(acnErrLoc, "Mandatory ACN property 'encoding' is missing"))
        let isUnsigned =
            match encProp with
            | PosInt            -> true
            | TwosComplement    -> false
            | IntAscii          -> false
            | BCD               -> true
        let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetIntEncodingClass acnAligment acnErrLoc acnProperties 0 0 isUnsigned

        AcnInteger ({AcnInteger.acnProperties=acnProperties; acnAligment=acnAligment; acnEncodingClass = acnEncodingClass;  Location = acnErrLoc; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; cons=[]; withcons=[];isUnsigned=isUnsigned; uperRange= Full})
    | AcnPrmBoolean  acnErrLoc ->
        let acnProperties = 
            match tryGetProp props (fun x -> match x with TRUE_VALUE e -> Some e | _ -> None) with
            | Some tv   ->  {BooleanAcnProperties.encodingPattern  = Some (TrueValue tv)}
            | None      ->
                match tryGetProp props (fun x -> match x with FALSE_VALUE e -> Some e | _ -> None) with
                | Some tv   ->  {BooleanAcnProperties.encodingPattern  = Some (FalseValue tv)}
                | None      ->  {BooleanAcnProperties.encodingPattern  = None}
        let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetBooleanEncodingClass acnAligment acnErrLoc acnProperties
        AcnBoolean ({AcnBoolean.acnProperties=acnProperties; acnAligment=acnAligment; Location = acnErrLoc; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits})
    | AcnPrmNullType acnErrLoc ->
        let acnProperties = { NullTypeAcnProperties.encodingPattern  = tryGetProp props (fun x -> match x with PATTERN e -> Some e | _ -> None)}
        let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetNullEncodingClass acnAligment acnErrLoc acnProperties
        AcnNullType ({AcnNullType.acnProperties=acnProperties; acnAligment=acnAligment; Location = acnErrLoc; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits})
    | AcnPrmRefType (md,ts)->
        let asn1Type0 = Asn1Ast.GetBaseTypeByName md ts asn1
        match asn1Type0.Kind with
        | Asn1Ast.Enumerated nmItems    ->
            let cons =  asn1Type0.Constraints |> List.collect (fixConstraint asn1) |> List.map (ConstraintsMapping.getEnumConstraint asn1 asn1Type0)
            let enumerated = mergeEnumerated asn1 nmItems ts.Location (Some ts.Location) (Some {AcnTypeEncodingSpec.acnProperties = props; children = []; loc=ts.Location}) props cons []
            AcnReferenceToEnumerated({AcnReferenceToEnumerated.modName = md; tasName = ts; enumerated = enumerated; acnAligment= acnAligment})
        | Asn1Ast.IA5String     ->
            let cons =  asn1Type0.Constraints |> List.collect (fixConstraint asn1) |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 asn1Type0)
            let defaultCharSet = [|for i in 0..127 -> System.Convert.ToChar(i) |]
            let str = mergeStringType asn1 ts.Location (Some ts.Location) props cons [] defaultCharSet
            AcnReferenceToIA5String({AcnReferenceToIA5String.modName = md; tasName = ts; str = str; acnAligment= acnAligment})
        | Asn1Ast.Integer       ->
            let cons =  asn1Type0.Constraints |> List.collect (fixConstraint asn1) |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 asn1Type0)
            let uperRange    = uPER.getIntTypeConstraintUperRange cons  ts.Location
            let alignmentSize = AcnEncodingClasses.getAlignmentSize acnAligment
            let uperMinSizeInBits, uperMaxSizeInBits = uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes uperRange
            let acnProperties = {IntegerAcnProperties.encodingProp = getIntEncodingProperty ts.Location props; sizeProp = getIntSizeProperty ts.Location props; endiannessProp = getEndianessProperty props}
            let isUnsigned =
                match acnProperties.encodingProp with
                | Some encProp -> 
                    match encProp with
                    | PosInt            -> true
                    | TwosComplement    -> false
                    | IntAscii          -> false
                    | BCD               -> true
                | None         ->
                    match acnProperties.sizeProp.IsNone && acnProperties.endiannessProp.IsNone with     
                    | true  -> 
                        match uperRange with
                        | Concrete  (a,b) when a >= 0I -> true      //[a, b]
                        | Concrete  _                  -> false
                        | NegInf    _                  -> false    //(-inf, b]
                        | PosInf   a when a >= 0I  -> true     //[a, +inf)
                        | PosInf  _                    -> false    //[a, +inf)
                        | Full    _                    -> false    // (-inf, +inf)
                    | false -> raise(SemanticError(ts.Location, "Mandatory ACN property 'encoding' is missing"))
                    
            let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits  =           
                AcnEncodingClasses.GetIntEncodingClass acnAligment ts.Location acnProperties uperMinSizeInBits uperMaxSizeInBits isUnsigned
            
            AcnInteger ({AcnInteger.acnProperties=acnProperties; acnAligment=acnAligment; acnEncodingClass = acnEncodingClass;  Location = ts.Location; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits;cons=cons;withcons=[];isUnsigned=isUnsigned; uperRange= uperRange})
        | _                               ->
            let newParma  = 
                match asn1Type0.Kind with
                //| Asn1Ast.Integer           -> AcnPrmInteger ts.Location
                | Asn1Ast.NullType          -> AcnPrmNullType ts.Location
                | Asn1Ast.Boolean           -> AcnPrmBoolean ts.Location
                | Asn1Ast.ReferenceType rf  -> AcnPrmRefType (rf.modName, rf.tasName)
                | _                         -> raise(SemanticError(ts.Location, "Invalid type for ACN inserted type"))
            let baseTypeAcnProps =
                match tryFindAcnTypeByName md ts acn with
                | None      -> []
                | Some x    -> x.typeEncodingSpec.acnProperties
            mapAcnParamTypeToAcnAcnInsertedType asn1 acn newParma (props@baseTypeAcnProps)





let rec private mergeType (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (m:Asn1Ast.Asn1Module) (t:Asn1Ast.Asn1Type) (curPath : ScopeNode list)
                           (acnType:AcnTypeEncodingSpec option) 
                           (refTypeCons:Asn1Ast.Asn1Constraint list)      // constraints applied to this type originating from reference types --> uPER visible
                           (withCons:Asn1Ast.Asn1Constraint list)         // constraints applied to this type originating from with component and  with components --> non uPER visible
                           (acnArgs : Asn1AcnAst.RelativePath list)
                           (acnParameters   : AcnParameter list)
                           (inferitInfo : InheritanceInfo option) 
                           (typeAssignmentInfo : AssignmentInfo option): Asn1Type=
    let acnProps =
        match acnType with
        | None      -> []
        | Some x    -> x.acnProperties
    let acnErrLoc = acnType |> Option.map(fun x -> x.loc)
    let combinedProperties = acnProps
    let allCons = t.Constraints@refTypeCons@withCons

    let fixConstraint  = (fixConstraint asn1)

    let asn1Kind =
        match t.Kind with
        | Asn1Ast.Integer                  -> 
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 t)
            Integer (mergeInteger asn1 t.Location acnErrLoc combinedProperties cons wcons)
        | Asn1Ast.Real                     -> 
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getRealTypeConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getRealTypeConstraint asn1 t)
            Real (mergeReal asn1 t.Location acnErrLoc combinedProperties cons wcons)
        | Asn1Ast.IA5String                ->  
            let defaultCharSet = [|for i in 0..127 -> System.Convert.ToChar(i) |]
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            IA5String (mergeStringType asn1 t.Location acnErrLoc combinedProperties cons wcons defaultCharSet)
        | Asn1Ast.NumericString            ->  
            let defaultCharSet = [| ' ';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|]

            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            NumericString (mergeStringType asn1 t.Location acnErrLoc combinedProperties cons wcons defaultCharSet)
        | Asn1Ast.OctetString              ->  
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getOctetStringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getOctetStringConstraint asn1 t)
            OctetString(mergeOctetStringType asn1 t.Location acnErrLoc combinedProperties cons wcons)
        | Asn1Ast.BitString                ->  
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBitStringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBitStringConstraint asn1 t)
            BitString(mergeBitStringType asn1 t.Location acnErrLoc combinedProperties cons wcons)
        | Asn1Ast.NullType                 ->  
            let constraints = []
            NullType(mergeNullType acnErrLoc combinedProperties)
        | Asn1Ast.Boolean                  ->  
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBoolConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBoolConstraint asn1 t)
            Boolean(mergeBooleanType acnErrLoc combinedProperties cons wcons)
        | Asn1Ast.Enumerated  items        ->  
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getEnumConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getEnumConstraint asn1 t)
            Enumerated (mergeEnumerated asn1 items t.Location acnErrLoc acnType combinedProperties cons wcons)
        | Asn1Ast.SequenceOf  chType       -> 
            let childWithCons = allCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint w -> Some w| _ -> None)
            let myVisibleConstraints = t.Constraints@refTypeCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> None | _ -> Some c)
            let myNonVisibleConstraints = withCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> None | _ -> Some c)

            let cons =  myVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getSequenceOfConstraint asn1 t)
            let wcons = myNonVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getSequenceOfConstraint asn1 t)

            let childEncSpec, acnArgs = 
                match acnType with
                | None          -> None, []
                | Some acnType  -> 
                    match acnType.children with
                    | []    -> None, []
                    | c1::[] -> Some c1.childEncodingSpec, c1.argumentList
                    | c1::c2::_      -> raise(SemanticError(c1.name.Location, (sprintf "%s Unexpected field name" c2.name.Value)))
            let newChType : Asn1Type = mergeType asn1 acn m chType (curPath@[SQF]) childEncSpec [] childWithCons  acnArgs [] None None

            let sizeUperRange = uPER.getSequenceOfUperRange cons t.Location
            let minSize, maxSize = uPER.getSizeMinAndMaxValue t.Location sizeUperRange
            let fixAsn1Size = match minSize = maxSize with true -> Some minSize | false -> None

            let acnProperties = 
                match acnErrLoc with
                | Some acnErrLoc    -> { SizeableAcnProperties.sizeProp  = getSizeableSizeProperty minSize maxSize acnErrLoc combinedProperties}
                | None              -> {SizeableAcnProperties.sizeProp = None }

            let uperMinSizeInBits, _ = uPER.getSizeableTypeSize minSize maxSize newChType.uperMinSizeInBits
            let _, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize maxSize newChType.uperMaxSizeInBits

            let aligment = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
            let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetSequenceOfEncodingClass aligment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize maxSize newChType.acnMinSizeInBits newChType.acnMaxSizeInBits

            let newKind = {SequenceOf.child=newChType; acnProperties   = acnProperties; cons = cons; withcons = wcons;minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits = acnMinSizeInBits; acnMaxSizeInBits=acnMaxSizeInBits}
            SequenceOf newKind
        | Asn1Ast.Sequence    children     -> 
            let childrenNameConstraints = allCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint w -> Some w| _ -> None) |> List.collect id
            let myVisibleConstraints = refTypeCons@t.Constraints |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)
            let myNonVisibleConstraints = withCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)

            let cons =  myVisibleConstraints|> List.collect fixConstraint |> List.map (ConstraintsMapping.getSequenceConstraint asn1 t)
            let wcons = myNonVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getSequenceConstraint asn1 t)

            let mergeChild (cc:ChildSpec option) (c:Asn1Ast.ChildInfo)  =
                let childNamedConstraints = childrenNameConstraints |> List.filter(fun x -> x.Name = c.Name)
                let childWithCons = childNamedConstraints |> List.choose(fun nc -> nc.Contraint)
                let asn1OptionalityFromWithComponents = 
                    childNamedConstraints |> 
                    List.choose(fun nc -> 
                        match nc.Mark with
                        | Asn1Ast.NoMark            -> None
                        | Asn1Ast.MarkPresent       -> Some AlwaysAbsent
                        | Asn1Ast.MarkAbsent        -> Some AlwaysAbsent
                        | Asn1Ast.MarkOptional      -> Some (Optional ({Optional.defaultValue = None; acnPresentWhen= None})) ) |>
                    Seq.distinct |> Seq.toList 

                let newOptionality =
                    match c.Optionality with
                    | None  ->
                        match asn1OptionalityFromWithComponents with
                        | []  -> ()
                        | _   -> raise(SemanticError(c.Name.Location, (sprintf "component %s is not optional to apply ALWAYS PRESENT or ALWAYS ABSENT constraints" c.Name.Value)))
                        None
                    | Some Asn1Ast.AlwaysAbsent  ->
                        match asn1OptionalityFromWithComponents with
                        | []          -> Some AlwaysAbsent
                        | newOpt::_   -> Some newOpt
                    | Some Asn1Ast.AlwaysPresent  ->
                        match asn1OptionalityFromWithComponents with
                        | []  -> Some AlwaysPresent
                        | newOpt::_   -> Some newOpt
                    | Some (Asn1Ast.Optional opt) ->
                        match asn1OptionalityFromWithComponents with
                        | []          
                        | (Optional _)::_   -> 
                            Some (Optional {Optional.defaultValue = opt.defaultValue |> Option.map (ValuesMapping.mapValue asn1 c.Type) ; acnPresentWhen = None})
                        | x::_              -> Some x

                let newOptionality =
                    let acnPresentWhenConditions = 
                        match cc with
                        | None      -> []
                        | Some cc   -> 
                            match tryGetProp cc.childEncodingSpec.acnProperties (fun x -> match x with PRESENT_WHEN e -> Some e | _ -> None) with
                            | None      -> []
                            | Some lst  -> lst |> List.choose(fun gp -> match gp with GP_PresenceBool l -> Some (PresenceWhenBool l) | _ -> None)
                    let checkForPresentWhenConditions () =
                        match acnPresentWhenConditions with
                        | []    -> ()
                        | _     -> raise(SemanticError(cc.Value.name.Location, (sprintf "present-when attribute cannot be applied here since component %s is not optional" cc.Value.name.Value)))

                    match newOptionality with
                    | None  
                    | Some AlwaysAbsent  
                    | Some AlwaysPresent  ->
                        checkForPresentWhenConditions ()
                        newOptionality
                    | Some (Optional opt)   ->
                        let acnPresentWhen = 
                            match acnPresentWhenConditions with
                            | []        -> None
                            | x::_      -> Some x    
                        Some (Optional {Optional.defaultValue = opt.defaultValue ; acnPresentWhen = acnPresentWhen})
                
                match cc with
                | None      ->  
                    let newChild = mergeType asn1 acn m c.Type (curPath@[SEQ_CHILD c.Name.Value]) None [] childWithCons [] [] None  None
                    Asn1Child ({Asn1Child.Name = c.Name; c_name = c.c_name; ada_name = c.ada_name; py_name = c.py_name; Type  = newChild; Optionality = newOptionality;Comments = c.Comments})
                | Some cc   ->
                    match cc.asn1Type with
                    | None  -> 
                        let newChild = mergeType asn1 acn m c.Type (curPath@[SEQ_CHILD c.Name.Value]) (Some cc.childEncodingSpec) [] childWithCons cc.argumentList [] None  None
                        Asn1Child ({Asn1Child.Name = c.Name; c_name = c.c_name; ada_name = c.ada_name; py_name = c.py_name; Type  = newChild; Optionality = newOptionality; Comments = c.Comments})
                    | Some xx  ->
                        AcnChild({AcnChild.Name = c.Name; id = ReferenceToType(curPath@[SEQ_CHILD c.Name.Value]); Type = mapAcnParamTypeToAcnAcnInsertedType asn1 acn xx cc.childEncodingSpec.acnProperties})

            let mergedChildren = 
                match acnType with
                | None            -> children |> List.map (mergeChild None)
                | Some acnEncSpec ->
                    match acnEncSpec.children with
                    | []            -> children |> List.map (mergeChild None)
                    | acnChildren   ->
                        // MAKE SURE ACN CHILDREN ARE SUPERSET OF ASN1 CHILDREN !!!!!
                        children |> List.filter(fun c -> not (acnChildren |> List.exists(fun c2 -> c2.name = c.Name))) |> List.iter(fun c -> raise(SemanticError(acnEncSpec.loc, (sprintf "No ACN encoding specification was provided for component %s" c.Name.Value)))  )

                        acnChildren |>
                        List.map(fun acnChild ->
                            match children |> Seq.tryFind (fun a -> a.Name = acnChild.name) with
                            | Some x -> mergeChild (Some acnChild) x
                            | None   -> 
                                match acnChild.asn1Type with
                                | Some xx ->
                                    AcnChild({AcnChild.Name = acnChild.name; id = ReferenceToType(curPath@[SEQ_CHILD acnChild.name.Value]); Type = mapAcnParamTypeToAcnAcnInsertedType asn1 acn xx acnChild.childEncodingSpec.acnProperties})
                                | None ->
                                    raise(SemanticError(acnChild.name.Location, (sprintf "invalid name %s" acnChild.name.Value))))
            
            let uperBitMaskSize      = children |> Seq.filter(fun c -> c.Optionality.IsSome) |> Seq.length 
            let asn1Children     = mergedChildren |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
            let uperMaxChildrenSize  = asn1Children |> List.map(fun x -> x.Type.uperMaxSizeInBits) |> Seq.sum
            let uperMinChildrenSize  = asn1Children |> List.filter(fun x -> x.Optionality.IsNone) |> List.map(fun x -> x.Type.uperMinSizeInBits) |> Seq.sum
            
            let aligment = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
            let alignmentSize = AcnEncodingClasses.getAlignmentSize aligment
            let acnBitMaskSize =
                mergedChildren |> 
                List.filter(fun c ->
                    match c.Optionality with
                    | Some (Optional p) when p.acnPresentWhen.IsNone    -> true
                    | _                                                 -> false) |> Seq.length
            let minChildrenSize = 
                mergedChildren |> 
                List.map(fun c ->
                    match c.Optionality with
                    | Some (Optional _) -> 0
                    | _                 -> c.acnMinSizeInBits) |> Seq.sum
            let maxChildrenSize = mergedChildren |> List.map(fun c -> c.acnMaxSizeInBits) |> Seq.sum
            let acnMaxSizeInBits = alignmentSize + acnBitMaskSize + maxChildrenSize
            let acnMinSizeInBits = alignmentSize + acnBitMaskSize + minChildrenSize

            Sequence ({Sequence.children = mergedChildren;    cons=cons; withcons = wcons;uperMaxSizeInBits=uperBitMaskSize+uperMaxChildrenSize; uperMinSizeInBits=uperBitMaskSize+uperMinChildrenSize;acnMaxSizeInBits=acnMaxSizeInBits;acnMinSizeInBits=acnMinSizeInBits})
        | Asn1Ast.Choice      children     -> 
            let childrenNameConstraints = t.Constraints@refTypeCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint w -> Some w| _ -> None) |> List.collect id
            let myVisibleConstraints = t.Constraints@refTypeCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)
            let myNonVisibleConstraints = withCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)

            let cons =  myVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getChoiceConstraint asn1 t)
            let wcons = myNonVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getChoiceConstraint asn1 t)
            
            let mergeChild (cc:ChildSpec option) (c:Asn1Ast.ChildInfo)  =
                let childNamedConstraints = childrenNameConstraints |> List.filter(fun x -> x.Name = c.Name)
                let childWithCons = childNamedConstraints |> List.choose(fun nc -> nc.Contraint)
                let acnPresentWhenConditions = 
                    match cc with
                    | None      -> []
                    | Some cc   -> 
                        match tryGetProp cc.childEncodingSpec.acnProperties (fun x -> match x with PRESENT_WHEN e -> Some e | _ -> None) with
                        | None      -> []
                        | Some lst  -> 
                            lst |> 
                            List.choose(fun gp -> 
                                match gp with 
                                | GP_PresenceInt (p,v) -> Some (PresenceInt (p,v)) 
                                | GP_PresenceStr (p,v) -> Some (PresenceStr (p,v)) 
                                | _ -> None)
                match cc with
                | None      ->  
                    let newChild = mergeType asn1 acn m c.Type (curPath@[CH_CHILD (c.Name.Value, c.present_when_name)]) None [] childWithCons [] [] None  None
                    {ChChildInfo.Name = c.Name; c_name = c.c_name; ada_name = c.ada_name; py_name = c.py_name; Type  = newChild; acnPresentWhenConditions = acnPresentWhenConditions; Comments = c.Comments;present_when_name = c.present_when_name}
                | Some cc   ->
                    let newChild = mergeType asn1 acn m c.Type (curPath@[CH_CHILD (c.Name.Value, c.present_when_name)]) (Some cc.childEncodingSpec) [] childWithCons cc.argumentList [] None  None
                    {ChChildInfo.Name = c.Name; c_name = c.c_name; ada_name = c.ada_name; py_name = c.py_name; Type  = newChild; acnPresentWhenConditions = acnPresentWhenConditions; Comments = c.Comments; present_when_name = c.present_when_name}
            let mergedChildren = 
                match acnType with
                | None            -> children |> List.map (mergeChild None)
                | Some acnEncSpec ->
                    match acnEncSpec.children with
                    | []            -> children |> List.map (mergeChild None)
                    | acnChildren   ->
                        // MAKE SURE ACN CHILDREN ARE THE SAME OF ASN1 CHILDREN !!!!!
                        acnChildren |>
                        List.map(fun acnChild ->
                            match children |> Seq.tryFind (fun a -> a.Name = acnChild.name) with
                            | Some x -> mergeChild (Some acnChild) x
                            | None   -> raise(SemanticError(acnChild.name.Location, (sprintf "invalid name %s" acnChild.name.Value))))
            let acnProperties = 
                {ChoiceAcnProperties.enumDeterminant = tryGetProp combinedProperties (fun x -> match x with CHOICE_DETERMINANT e -> Some e | _ -> None)}
            let acnLoc = acnType |> Option.map (fun z -> z.loc)
            let indexSize = int (GetNumberOfBitsForNonNegativeInteger(BigInteger(Seq.length children)))
            let minChildSize = mergedChildren  |> List.map(fun x -> x.Type.uperMinSizeInBits) |> Seq.min
            let maxChildSize = mergedChildren  |> List.map(fun x -> x.Type.uperMaxSizeInBits) |> Seq.max

            let aligment = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
            let acnMinSizeInBits, acnMaxSizeInBits = AcnEncodingClasses.GetChoiceEncodingClass  mergedChildren aligment t.Location acnProperties

            Choice ({Choice.children = mergedChildren; acnProperties = acnProperties;   cons=cons; withcons = wcons;uperMaxSizeInBits=indexSize+maxChildSize; uperMinSizeInBits=indexSize+minChildSize; acnMinSizeInBits =acnMinSizeInBits; acnMaxSizeInBits=acnMaxSizeInBits; acnLoc = acnLoc})
        | Asn1Ast.ReferenceType rf    -> 
            let acnArguments = acnArgs
            let oldBaseType  = Asn1Ast.GetBaseTypeByName rf.modName rf.tasName asn1

            let withCompCons = allCons  |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> Some c| Asn1Ast.WithComponentsConstraint _ -> Some c | _ -> None)
            let restCons = allCons  |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> None | Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)
            let acnTypeAssig = tryFindAcnTypeByName rf.modName rf.tasName acn
            let baseTypeAcnParams = 
                match acnTypeAssig with
                | None      -> []
                | Some x    -> x.acnParameters
                
            let baseTypeAcnEncSpec =
                match acnTypeAssig with
                | None      -> None
                | Some x    -> Some x.typeEncodingSpec
            let mergedAcnEncSpec = mergeAcnEncodingSpecs acnType baseTypeAcnEncSpec
            let hasAdditionalConstraints = restCons.Length > 0
            let inheritanceInfo = (Some {InheritanceInfo.modName = rf.modName.Value; tasName = rf.tasName.Value; hasAdditionalConstraints=hasAdditionalConstraints})
            let resolvedType     = mergeType asn1 acn m oldBaseType curPath mergedAcnEncSpec restCons withCompCons acnArgs baseTypeAcnParams inheritanceInfo typeAssignmentInfo
            let newRef       = {ReferenceType.modName = rf.modName; tasName = rf.tasName; tabularized = rf.tabularized; acnArguments = acnArguments; resolvedType=resolvedType}
            ReferenceType newRef
    //let dummy = sprintf "%A" typeAssignmentInfo
    //let dummy2 = dummy
    {
        Asn1Type.Kind   = asn1Kind
        id              = ReferenceToType curPath
        acnAligment     = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
        Location        = t.Location
        acnParameters   = acnParameters |> List.map(fun prm -> {prm with id = (ReferenceToType (curPath@[(PRM prm.name)]))})
        inheritInfo   = inferitInfo
        typeAssignmentInfo = typeAssignmentInfo
        
    }

let private mergeTAS (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (m:Asn1Ast.Asn1Module) (am:AcnModule option)  (tas:Asn1Ast.TypeAssignment) : TypeAssignment =
    let acnTas = 
        match am with
        | None  -> None
        | Some x -> x.typeAssignments |> Seq.tryFind(fun z -> z.name = tas.Name)
    let acnParameters  = 
            match acnTas with 
            | None -> [] 
            | Some acnTas -> acnTas.acnParameters

    {
        TypeAssignment.Name = tas.Name
        c_name = tas.c_name
        ada_name = tas.ada_name
        py_name = tas.py_name; 
        //Type = mergeType asn1 acn m tas.Type [MD m.Name.Value; TA tas.Name.Value] (acnTas |> Option.map(fun x -> x.typeEncodingSpec)) [] [] [] acnParameters (Some {TypeAssignmentInfo.modName = m.Name.Value; tasName = tas.Name.Value})
        Type = mergeType asn1 acn m tas.Type [MD m.Name.Value; TA tas.Name.Value] (acnTas |> Option.map(fun x -> x.typeEncodingSpec)) [] [] [] acnParameters None (Some (TypeAssignmentInfo {TypeAssignmentInfo.modName = m.Name.Value; tasName = tas.Name.Value}))
        Comments = tas.Comments
    }

let private mergeValueAssigment (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (m:Asn1Ast.Asn1Module) (am:AcnModule option)  (vas:Asn1Ast.ValueAssignment) :ValueAssignment =
    {
        ValueAssignment.Name = vas.Name
        c_name = vas.c_name
        ada_name = vas.ada_name
        py_name = vas.py_name
        Type = mergeType asn1 acn m vas.Type [MD m.Name.Value; VA vas.Name.Value] None [] [] [] [] None (Some (ValueAssignmentInfo {ValueAssignmentInfo.modName = m.Name.Value; vasName = vas.Name.Value}))
        Value = ValuesMapping.mapValue asn1 vas.Type vas.Value
    }

let private mergeModule (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (m:Asn1Ast.Asn1Module) : Asn1Module =
    let acnModule = acn.files |> Seq.collect(fun f -> f.modules)  |> Seq.tryFind (fun x -> x.name = m.Name)
    {
        Asn1Module.Name = m.Name
        TypeAssignments = m.TypeAssignments |> List.map (mergeTAS asn1 acn m acnModule)
        ValueAssignments = m.ValueAssignments |> List.map (mergeValueAssigment asn1 acn m acnModule)
        Imports  =  m.Imports
        Exports = m.Exports
        Comments = m.Comments
    }

let private mergeFile (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (f:Asn1Ast.Asn1File) : Asn1File =
    {
        Asn1File.FileName = f.FileName
        Tokens = f.Tokens
        Modules =  f.Modules |> List.map (mergeModule asn1 acn)
    }

let mergeAsn1WithAcnAst (asn1:Asn1Ast.AstRoot) (acnParseResults:ParameterizedAsn1Ast.AntlrParserResult list) : AstRoot=
    let acn = CreateAcnAst acnParseResults
    let files = asn1.Files |> List.map (mergeFile asn1 acn)
    {AstRoot.Files = files; args = asn1.args; acnConstants = acn.acnConstants}
