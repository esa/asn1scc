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

module AcnCreateFromAntlr

open System
open System.Linq
open System.Numerics
open Antlr.Acn
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open FsUtils
open FE_TypeDefinition

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN PRIVATE TYPES (NOT EXPOSED TO OTHER MODULES) /////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


type  GenericAcnPresentWhenCondition =
    | GP_PresenceBool  of Asn1AcnAst.RelativePath                         
    | GP_PresenceInt   of Asn1AcnAst.RelativePath*IntLoc
    | GP_PresenceStr   of Asn1AcnAst.RelativePath*StringLoc          
    
type  GenAcnEncodingProp =
    | GP_PosInt
    | GP_TwosComplement
    | GP_Ascii
    | GP_BCD
    | GP_IEEE754_32
    | GP_IEEE754_64

type  GenSizeProperty = 
    | GP_Fixed                 of IntLoc
    | GP_NullTerminated        
    | GP_SizeDeterminant       of Asn1AcnAst.RelativePath



type  GenericAcnProperty = 
    | ENCODING          of GenAcnEncodingProp
    | SIZE              of GenSizeProperty
    | ALIGNTONEXT       of Asn1AcnAst.AcnAligment
    | ENCODE_VALUES   
    | PRESENT_WHEN      of GenericAcnPresentWhenCondition list
    | TRUE_VALUE        of StringLoc
    | FALSE_VALUE       of StringLoc
    | PATTERN           of Asn1AcnAst.PATERN_PROP_VALUE
    | CHOICE_DETERMINANT of Asn1AcnAst.RelativePath
    | ENDIANNES         of Asn1AcnAst.AcnEndianness
    | ENUM_SET_VALUE    of IntLoc
    | TERMINATION_PATTERN of byte
    | MAPPING_FUNCTION  of StringLoc




type  AcnTypeEncodingSpec = {
    acnProperties   : GenericAcnProperty list
    children        : ChildSpec list
    loc             : SrcLoc
    comments        : string list
}

and  ChildSpec = {
    name            : StringLoc
    childEncodingSpec : AcnTypeEncodingSpec
    asn1Type        : Asn1AcnAst.AcnParamType option    // if present then it indicates an ACN inserted type
    argumentList    : Asn1AcnAst.RelativePath list
    comments        : string list
}

type  AcnTypeAssignment = {
    name            : StringLoc
    acnParameters   : Asn1AcnAst.AcnParameter list
    typeEncodingSpec: AcnTypeEncodingSpec
    comments        : string list
}

type  AcnModule = {
    name            : StringLoc
    typeAssignments : AcnTypeAssignment list
}


type  AcnFile = {
    antlrResult : ParameterizedAsn1Ast.AntlrParserResult
    modules     : AcnModule list
}

type  AcnAst = {
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
        //let tp = t
        match t.GetChild(0).Type with
        | acnParser.BitStringLiteral    ->
            let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
            PATTERN (Asn1AcnAst.PATERN_PROP_BITSTR_VALUE v)
        | acnParser.OctectStringLiteral ->
            let strVal = GetActualString(t.GetChild(0).Text)
            let chars = strVal.ToCharArray() 
            let bytes = CreateAsn1AstFromAntlrTree.getAsTupples chars '0' |> List.map (fun (x1,x2)-> t.GetValueL (System.Byte.Parse(x1.ToString()+x2.ToString(), System.Globalization.NumberStyles.AllowHexSpecifier))) 
            PATTERN (Asn1AcnAst.PATERN_PROP_OCTSTR_VALUE bytes)
        | _     ->  raise(BugErrorException("creareAcnProperty_PATTERN"))
                    
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

let rec  private createTypeEncodingSpec (allAcnFiles: ParameterizedAsn1Ast.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: ParameterizedAsn1Ast.AntlrParserResult)  (alreadyTakenComments:System.Collections.Generic.List<IToken>) (encSpecITree:ITree) : AcnTypeEncodingSpec =
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
                let comments = Antlr.Comment.GetComments(thisAcnFile.tokens, alreadyTakenComments, thisAcnFile.tokens.[t.TokenStopIndex].Line, t.TokenStartIndex - 1, t.TokenStopIndex + 2, true)
                let childEncodingSpec = createTypeEncodingSpec allAcnFiles acnConstants thisAcnFile alreadyTakenComments (t.GetChildByType acnParser.ENCODING_SPEC) 
                let asn1Type  =
                    match t.Type with
                    | acnParser.CHILD       -> None
                    | acnParser.CHILD_NEW   -> Some (CreateAcnParamType (t.GetChild 1) ) 
                    | _     ->  raise(BugErrorException("createTypeEncodingSpec_CHILD"))

                {ChildSpec.name = name; childEncodingSpec= childEncodingSpec; asn1Type=asn1Type; argumentList=argumentList; comments = comments |> Seq.toList}
            childrenList.Children |> List.map createChild
    {AcnTypeEncodingSpec.acnProperties = acnProperties; children = children; loc = encSpecITree.Location; comments=[]}

let private CreateTypeAssignment (allAcnFiles: ParameterizedAsn1Ast.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: ParameterizedAsn1Ast.AntlrParserResult)  (alreadyTakenComments:System.Collections.Generic.List<IToken>) (tasTree:ITree) : AcnTypeAssignment =
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

    let comments = Antlr.Comment.GetComments(thisAcnFile.tokens, alreadyTakenComments, thisAcnFile.tokens.[tasTree.TokenStopIndex].Line, tasTree.TokenStartIndex - 1, tasTree.TokenStopIndex + 1, true)
    let typeEncodingSpec = createTypeEncodingSpec allAcnFiles acnConstants thisAcnFile alreadyTakenComments encSpecITree

    {AcnTypeAssignment.name = tasNameL; acnParameters = prms; typeEncodingSpec = typeEncodingSpec; comments = comments |> Seq.toList}


let private CreateModule (allAcnFiles: ParameterizedAsn1Ast.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: ParameterizedAsn1Ast.AntlrParserResult)   (alreadyTakenComments:System.Collections.Generic.List<IToken>)  (modTree : ITree) : AcnModule =
    let modNameL = modTree.GetChildByType(acnParser.UID).TextL

    let tasITreeList = modTree.GetChildrenByType(acnParser.TYPE_ENCODING)
    
    //check for duplicate type assignments in the ACN module
    tasITreeList |> List.map(fun x -> x.GetChildByType(acnParser.UID).TextL) |> CheckForDuplicates

    let newTasses = tasITreeList |> List.map(fun tasTree -> CreateTypeAssignment allAcnFiles acnConstants thisAcnFile alreadyTakenComments tasTree) 
    
    {AcnModule.name = modNameL; typeAssignments = newTasses}


let private LoadAcnFile (allAcnFiles: ParameterizedAsn1Ast.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: ParameterizedAsn1Ast.AntlrParserResult)   : AcnFile = 
    let alreadyTakenComments = new System.Collections.Generic.List<IToken>();

    let modules = thisAcnFile.rootItem.Children |> List.map (CreateModule allAcnFiles acnConstants thisAcnFile alreadyTakenComments)
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
    | Some (GP_Fixed           v)   -> Some(Fixed (v.Value))
    | Some (GP_NullTerminated   )   -> 
        match tryGetProp props (fun x -> match x with TERMINATION_PATTERN e -> Some e | _ -> None) with
        | Some b    -> Some(IntNullTerminated b)
        | None      -> Some(IntNullTerminated (byte 0))
    | Some (GP_SizeDeterminant _)   -> raise(SemanticError(errLoc ,"Expecting an Integer value or an ACN constant as value for the size property"))

let private getStringSizeProperty (minSize:BigInteger) (maxSize:BigInteger) errLoc (props:GenericAcnProperty list) = 
    match tryGetProp props (fun x -> match x with SIZE e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_Fixed           v)   -> 
        match minSize = maxSize with
        | false ->
            let errMessage = sprintf "The size constraints of the ASN.1  allows variable items (%A .. %A). Therefore, you should either remove the size property (in which case the size determinant will be encoded automatically exactly like uPER), or use a an Integer field as size determinant"  minSize maxSize
            raise(SemanticError(errLoc ,errMessage))
        | true  ->
            match minSize = v.Value with
            | true  -> None
            | false -> raise(SemanticError(errLoc , (sprintf "The size property must either be ommited or have the fixed value %A" minSize)))
    | Some (GP_NullTerminated   )   -> 
        match tryGetProp props (fun x -> match x with TERMINATION_PATTERN e -> Some e | _ -> None) with
        | Some b    -> Some(StrNullTerminated b)
        | None      -> Some(StrNullTerminated (byte 0))
    | Some (GP_SizeDeterminant fld)   -> (Some (StrExternalField fld))

let private getSizeableSizeProperty (minSize:BigInteger) (maxSize:BigInteger) errLoc (props:GenericAcnProperty list) = 
    match tryGetProp props (fun x -> match x with SIZE e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_NullTerminated   )        -> raise(SemanticError(errLoc ,"Acn proporty 'size null-terminated' is supported only in IA5String and NumericString string types and in Integer types and when encoding is ASCII"))
    | Some (GP_Fixed           v)   ->
        match minSize = maxSize with
        | false ->
            let errMessage = sprintf "The size constraints of the ASN.1  allows variable items (%A .. %A). Therefore, you should either remove the size property (in which case the size determinant will be encoded automatically exactly like uPER), or use a an Integer field as size determinant"  minSize maxSize
            raise(SemanticError(errLoc ,errMessage))
        | true  ->
            match minSize = v.Value with
            | true  -> None
            | false -> raise(SemanticError(errLoc , (sprintf "The size property must either be ommited or have the fixed value %A" minSize)))
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

let private getMappingFunctionProperty acnErrLoc (props:GenericAcnProperty list) = 
    match tryGetProp props (fun x -> match x with MAPPING_FUNCTION e -> Some e | _ -> None) with
    | None  -> None
    | Some mapFuncName  -> Some (MappingFunction mapFuncName)

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







let checkIntHasEnoughSpace acnEncodingClass (hasMappingFunction:bool) acnErrLoc0 asn1Min asn1Max =
    let check_ (minEnc : BigInteger) (maxEnc:BigInteger) =
        match minEnc <= asn1Min && asn1Max <= maxEnc with
        | true                              -> ()
        | false  when not (asn1Max <= maxEnc)     -> 
            let errMessage = sprintf "The applied ACN encoding does not allow values larger than %A to be encoded, while the corresponding ASN.1 type allows values up to %A" maxEnc asn1Max
            raise(SemanticError(acnErrLoc0, errMessage))
        | false                             -> 
            let errMessage = sprintf "The applied ACN encoding does not allow values smaller than %A to be encoded, while the corresponding ASN.1 type allows values up to %A" minEnc asn1Min
            raise(SemanticError(acnErrLoc0, errMessage))
    let checkBinary isUnsigned (lengthInBiths:BigInteger) =
        match isUnsigned  with
        | true              -> check_ 0I (BigInteger.Pow(2I, int lengthInBiths) - 1I) 
        | false             -> check_ (-BigInteger.Pow(2I, int (lengthInBiths - 1I))) ((BigInteger.Pow(2I, int (lengthInBiths - 1I))) - 1I)
    let checkAscii isUnsigned (lengthInBiths:BigInteger) =
        let digits = int (lengthInBiths / 8I)
        match isUnsigned  with
        | true              -> check_ 0I (BigInteger.Pow(10I, digits) - 1I) 
        | false             -> check_ (-BigInteger.Pow(10I, digits - 1)) ((BigInteger.Pow(10I, digits - 1)) - 1I)
    let checkBCD (lengthInBiths:BigInteger) =
        let digits = int (lengthInBiths / 4I)
        check_ 0I (BigInteger.Pow(10I, digits) - 1I) 

    match hasMappingFunction with
    | true  -> ()       //when there is a mapping function we are performing no size check
    | false -> 
        match acnEncodingClass with
        |Integer_uPER                                   -> ()
        |PositiveInteger_ConstSize_8                    -> checkBinary true 8I
        |PositiveInteger_ConstSize_big_endian_16        -> checkBinary true 16I
        |PositiveInteger_ConstSize_little_endian_16     -> checkBinary true 16I
        |PositiveInteger_ConstSize_big_endian_32        -> checkBinary true 32I
        |PositiveInteger_ConstSize_little_endian_32     -> checkBinary true 32I
        |PositiveInteger_ConstSize_big_endian_64        -> checkBinary true 64I
        |PositiveInteger_ConstSize_little_endian_64     -> checkBinary true 64I
        |PositiveInteger_ConstSize nBits                -> checkBinary true nBits
        |TwosComplement_ConstSize_8                     -> checkBinary false 8I
        |TwosComplement_ConstSize_big_endian_16         -> checkBinary false 16I
        |TwosComplement_ConstSize_little_endian_16      -> checkBinary false 16I
        |TwosComplement_ConstSize_big_endian_32         -> checkBinary false 32I
        |TwosComplement_ConstSize_little_endian_32      -> checkBinary false 32I
        |TwosComplement_ConstSize_big_endian_64         -> checkBinary false 64I
        |TwosComplement_ConstSize_little_endian_64      -> checkBinary false 64I
        |TwosComplement_ConstSize nBits                 -> checkBinary false nBits
        |ASCII_ConstSize nBits                          -> checkAscii false nBits
        |ASCII_VarSize_NullTerminated _                 -> ()
        |ASCII_UINT_ConstSize nBits                     -> checkAscii false nBits
        |ASCII_UINT_VarSize_NullTerminated _            -> ()
        |BCD_ConstSize nBits                            -> checkBCD nBits
        |BCD_VarSize_NullTerminated _                   -> ()














let private mergeInteger (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (typeAssignmentInfo : AssignmentInfo option) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons (tdarg:GetTypeDifition_arg) (us:Asn1AcnMergeState) =
    let declare_IntegerNoRTL       l     = 
        match l with 
        | C     -> "", header_c.Declare_Integer (), "INTEGER"
        | Ada   -> "adaasn1rtl", header_a.Declare_IntegerNoRTL(), "INTEGER"
    let declare_PosIntegerNoRTL    l     = 
        match l with 
        | C     -> "", header_c.Declare_PosInteger () , "INTEGER"               
        | Ada   -> "adaasn1rtl", header_a.Declare_PosIntegerNoRTL  () , "INTEGER"               

    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let rootCons = cons |> List.filter(fun c -> match c with RangeRootConstraint _  | RangeRootConstraint2 _ -> true | _ -> false)
    let uperRange    = uPER.getIntTypeConstraintUperRange cons  loc
    uPER.getIntTypeConstraintUperRange (cons@withcons)  loc |> ignore
    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                IntegerAcnProperties.encodingProp    = getIntEncodingProperty acnErrLoc props
                sizeProp                             = getIntSizeProperty acnErrLoc props
                endiannessProp                       = getEndianessProperty props
                mappingFunction                      = getMappingFunctionProperty acnErrLoc props
            }
        | None  -> {IntegerAcnProperties.encodingProp = None; sizeProp = None; endiannessProp = None; mappingFunction = None }
    let uperMinSizeInBits, uperMaxSizeInBits = 
        match rootCons with
        | []  -> uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes uperRange
        | _   -> 
            let mn,mx = uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes uperRange
            1I + mn, 1I + mx

    let isUnsigned =
        match uperRange with
        | Concrete  (a,b) when a >= 0I && rootCons.IsEmpty-> true      //[a, b]
        | Concrete  _                  -> false
        | NegInf    _                  -> false    //(-inf, b]
        | PosInf   a when a >= 0I  && rootCons.IsEmpty    -> true     //[a, +inf)
        | PosInf  _                    -> false    //[a, +inf)
        | Full    _                    -> false    // (-inf, +inf)


    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetIntEncodingClass asn1.args.integerSizeInBytes aligment acnErrLoc0 acnProperties uperMinSizeInBits uperMaxSizeInBits isUnsigned

    let asn1Min, asn1Max =
        match uperRange with
        | Concrete  (a,b)                   -> (a,b)
        | NegInf    b                       -> (asn1.args.SIntMin, b)    //(-inf, b]
        | PosInf   a    when a >= 0I        -> (a, asn1.args.UIntMax)     //[a, +inf)
        | PosInf   a                        -> (a, asn1.args.SIntMax)
        | Full    _                         -> (asn1.args.SIntMin, asn1.args.SIntMax)


    let rtlFnc = 
        match uperRange with
        | Full                        -> Some declare_IntegerNoRTL
        | PosInf (a)  when a=0I       -> Some declare_PosIntegerNoRTL
        | _                           -> 
            match typeAssignmentInfo with
            | Some (ValueAssignmentInfo _)  -> 
                //this is a case where a new type should have been defined. However, the type is under a value assignment e.g. max-Instr INTEGER (12 .. 504) ::= 12
                //and currently, we do not declare a new type
                match asn1Min >= 0I with
                | true                       -> Some declare_PosIntegerNoRTL
                | false                      -> Some declare_IntegerNoRTL
            | _                             -> None

    checkIntHasEnoughSpace acnEncodingClass acnProperties.mappingFunction.IsSome acnErrLoc0 asn1Min asn1Max
    let typeDef, us1 = getPrimitiveTypeDifition {tdarg with rtlFnc = rtlFnc} us
    {Integer.acnProperties = acnProperties; cons = cons; withcons = withcons; uperRange = uperRange;uperMinSizeInBits=uperMinSizeInBits; uperMaxSizeInBits=uperMaxSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; isUnsigned= isUnsigned; typeDef=typeDef}, us1

let private mergeReal (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons (tdarg:GetTypeDifition_arg) (us:Asn1AcnMergeState) =
    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let getRtlTypeName  l = match l with C -> "", header_c.Declare_Real  (), "REAL" | Ada  -> "adaasn1rtl", header_a.Declare_REALNoRTL (), "REAL" 
    
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
    let uperMaxSizeInBits=(5I+asn1.args.integerSizeInBytes)*8I
    let uperMinSizeInBits=8I
    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetRealEncodingClass aligment loc acnProperties uperMinSizeInBits uperMaxSizeInBits 
    match acnEncodingClass with
    | Real_IEEE754_64_big_endian        when asn1.args.floatingPointSizeInBytes = 4I -> raise(SemanticError(acnErrLoc0, "Acn property 'IEEE754-1985-64' cannot be applied when -fpWordSize  4"))
    | Real_IEEE754_64_little_endian     when asn1.args.floatingPointSizeInBytes = 4I -> raise(SemanticError(acnErrLoc0, "Acn property 'IEEE754-1985-64' cannot be applied when -fpWordSize  4"))
    | _                                                                              -> ()
    let typeDef, us1 = getPrimitiveTypeDifition {tdarg with rtlFnc = Some getRtlTypeName} us
    {Real.acnProperties = acnProperties; cons = cons; withcons = withcons; uperRange=uperRange; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef}, us1












let private mergeObjectIdentifier (asn1:Asn1Ast.AstRoot) (relativeId:bool) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons (tdarg:GetTypeDifition_arg) (us:Asn1AcnMergeState) =
    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let getRtlTypeName  l = 
        let asn1Name = if relativeId then "RELATIVE-OID" else "OBJECT IDENTIFIER"
        match l with 
        | C     -> "",           header_c.Declare_ObjectIdentifier  (), asn1Name
        | Ada   -> "adaasn1rtl", header_a.Declare_ObjectIdentifierNoRTL (), asn1Name
    
    //check for invalid properties
    props |> Seq.iter(fun pr -> raise(SemanticError(acnErrLoc0, "Acn property cannot be applied to OBJECT IDENTIFIER types")))

    let acnProperties =  {ObjectIdTypeAcnProperties.sizeProperties = None; itemProperties = None }

    let lengthSize = 1I //+++ SOS, must be valiader
    let compUperMinSizeInBits, compUperMaxSizeInBits = uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes Full
    let uperMaxSizeInBits= compUperMaxSizeInBits*asn1.args.objectIdentifierMaxLength + lengthSize
    let uperMinSizeInBits= lengthSize
    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnMinSizeInBits, acnMaxSizeInBits= uperMinSizeInBits, uperMaxSizeInBits
    let typeDef, us1 = getPrimitiveTypeDifition {tdarg with rtlFnc = Some getRtlTypeName} us
    {ObjectIdentifier.acnProperties = acnProperties; cons = cons; withcons = withcons; relativeObjectId=relativeId; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef}, us1


















type EnmStrGetTypeDifition_arg =
    | EnmStrGetTypeDifition_arg of GetTypeDifition_arg
    | AcnPrmGetTypeDefinition of ((ScopeNode list)* string*string)

let private mergeStringType (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons defaultCharSet isNumeric (tdarg:EnmStrGetTypeDifition_arg) (us:Asn1AcnMergeState) =
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
    
    let charSize =  GetNumberOfBitsForNonNegativeInteger (BigInteger (uperCharSet.Length-1))
    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize maxSize charSize
    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)

    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetStringEncodingClass aligment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize maxSize uperCharSet

    match acnEncodingClass with                                          
    | Acn_Enc_String_uPER                                _                -> ()
    | Acn_Enc_String_uPER_Ascii                          _                -> ()
    | Acn_Enc_String_Ascii_Null_Teminated                (_, nullChar)     -> 
        match uperCharSet |> Seq.exists ((=) (System.Convert.ToChar nullChar)) with
        | true  when nullChar <> (byte 0) -> raise(SemanticError(acnErrLoc0, "The termination-pattern defines a character which belongs to the allowed values of the ASN.1 type. Use another value in the termination-pattern or apply different constraints in the ASN.1 type."))
        | _ -> ()
    | Acn_Enc_String_Ascii_External_Field_Determinant       (_,relativePath) -> ()
    | Acn_Enc_String_CharIndex_External_Field_Determinant   (_,relativePath) -> ()




    let typeDef, us1 = 
        match tdarg with
        | EnmStrGetTypeDifition_arg tdarg   -> getStringTypeDifition tdarg us
        | AcnPrmGetTypeDefinition (curPath, md, ts)   -> 
            let lanDefs, us1 =
                [C;Ada] |> foldMap (fun us l -> 
                    let itm, ns = registerStringTypeDefinition us l (ReferenceToType curPath) (FEI_Reference2OtherType (ReferenceToType [MD md; TA ts])) 
                    (l,itm), ns) us
            lanDefs |> Map.ofList, us1

    {StringType.acnProperties = acnProperties; cons = cons; withcons = withcons; minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; uperCharSet=uperCharSet; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits;isNumeric=isNumeric; typeDef=typeDef}, us1

let private mergeOctetStringType (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons (tdarg:GetTypeDifition_arg) (us:Asn1AcnMergeState) =
    let sizeUperRange = uPER.getOctetStringUperRange cons loc
    let minSize, maxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange
    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize maxSize 8I
    let fixAsn1Size = match minSize = maxSize with true -> Some minSize | false -> None

    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    -> {SizeableAcnProperties.sizeProp = getSizeableSizeProperty minSize maxSize acnErrLoc props}
        | None              -> {SizeableAcnProperties.sizeProp = None }


    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetOctetStringEncodingClass aligment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize maxSize 

    let typeDef, us1 = getSizeableTypeDifition tdarg us
    {OctetString.acnProperties = acnProperties; cons = cons; withcons = withcons; minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef}, us1

let private mergeBitStringType (asn1:Asn1Ast.AstRoot) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons (tdarg:GetTypeDifition_arg) (us:Asn1AcnMergeState) =
    let sizeUperRange = uPER.getBitStringUperRange cons loc
    let minSize, maxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange
    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize maxSize 1I
    let fixAsn1Size = match minSize = maxSize with true -> Some minSize | false -> None

    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    -> { SizeableAcnProperties.sizeProp  = getSizeableSizeProperty minSize maxSize acnErrLoc props}
        | None              -> {SizeableAcnProperties.sizeProp = None }

    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetBitStringEncodingClass aligment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize maxSize 

    let typeDef, us1 = getSizeableTypeDifition tdarg us
    {BitString.acnProperties = acnProperties; cons = cons; withcons = withcons; minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef}, us1

let private mergeNullType (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) (tdarg:GetTypeDifition_arg) (us:Asn1AcnMergeState) =
    let getRtlTypeName  l = match l with C -> "", header_c.Declare_NullType  (), "NULL" | Ada -> "adaasn1rtl", header_a.Declare_NULLNoRTL  (), "NULL" 
    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    -> { NullTypeAcnProperties.encodingPattern  = tryGetProp props (fun x -> match x with PATTERN e -> Some e | _ -> None)}
        | None              -> {NullTypeAcnProperties.encodingPattern = None }

    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetNullEncodingClass aligment loc acnProperties
    let typeDef, us1 = getPrimitiveTypeDifition {tdarg with rtlFnc = Some getRtlTypeName} us
    {NullType.acnProperties = acnProperties; uperMaxSizeInBits = 0I; uperMinSizeInBits=0I;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef}, us1

let private mergeBooleanType (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons  (tdarg:GetTypeDifition_arg) (us:Asn1AcnMergeState)=
    let getRtlTypeName  l = match l with C -> "",header_c.Declare_Boolean  (),"BOOLEAN" | Ada  -> "adaasn1rtl", header_a.Declare_BOOLEANNoRTL  (), "BOOLEAN" 
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
    let typeDef, us1 = getPrimitiveTypeDifition {tdarg with rtlFnc = Some getRtlTypeName} us
    {Boolean.acnProperties = acnProperties; cons = cons; withcons = withcons;uperMaxSizeInBits = 1I; uperMinSizeInBits=1I; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef}, us1




let private mergeEnumerated (asn1:Asn1Ast.AstRoot) (items: Asn1Ast.NamedItem list) (loc:SrcLoc) (acnErrLoc: SrcLoc option) (acnType:AcnTypeEncodingSpec option) (props:GenericAcnProperty list) cons withcons  (tdarg:EnmStrGetTypeDifition_arg) (us:Asn1AcnMergeState) =
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
            {NamedItem.Name = itm.Name; Comments = itm.Comments; c_name = asn1.args.TypePrefix + itm.c_name;  ada_name = asn1.args.TypePrefix + itm.ada_name; definitionValue = definitionValue; acnEncodeValue = acnEncodeValue}        
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
            {NamedItem.Name = itm.Name; Comments = itm.Comments; c_name = asn1.args.TypePrefix + itm.c_name; ada_name = asn1.args.TypePrefix + itm.ada_name; definitionValue = definitionValue; acnEncodeValue = acnEncodeValue}        

    let items0, userDefinedValues = 
        match items |> Seq.exists (fun nm -> nm._value.IsSome) with
        | false -> allocatedValuesToAllEnumItems items, false 
        | true -> allocatedValuesToAllEnumItems items, true
    let uperSizeInBits = GetNumberOfBitsForNonNegativeInteger(BigInteger((Seq.length items) - 1))
    let items = items0|> List.mapi mapItem
    
    let acnProperties = 
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                IntegerAcnProperties.encodingProp    = getIntEncodingProperty acnErrLoc props
                sizeProp                             = getIntSizeProperty  acnErrLoc props
                endiannessProp                       = getEndianessProperty props
                mappingFunction                      = getMappingFunctionProperty acnErrLoc props
            }
        | None  -> {IntegerAcnProperties.encodingProp = None; sizeProp = None; endiannessProp = None; mappingFunction = None }
    
    let aligment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetEnumeratedEncodingClass asn1.args.integerSizeInBytes items aligment loc acnProperties uperSizeInBits uperSizeInBits endodeValues
    let typeDef, us1 = 
        match tdarg with
        | EnmStrGetTypeDifition_arg tdarg   -> getEnumeratedTypeDifition tdarg us
        | AcnPrmGetTypeDefinition (curPath, md, ts)   -> 
            let lanDefs, us1 =
                [C;Ada] |> foldMap (fun us l -> 
                    let itm, ns = registerEnumeratedTypeDefinition us l (ReferenceToType curPath) (FEI_Reference2OtherType (ReferenceToType [MD md; TA ts])) 
                    (l,itm), ns) us
            lanDefs |> Map.ofList, us1
        
    {Enumerated.acnProperties = acnProperties; items=items; cons = cons; withcons = withcons;uperMaxSizeInBits = uperSizeInBits; uperMinSizeInBits=uperSizeInBits;encodeValues=endodeValues; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits;userDefinedValues=userDefinedValues; typeDef=typeDef}, us1

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
                            Some ({name = nm; childEncodingSpec = combinedEncoingSpec; asn1Type = thisChild.asn1Type; argumentList = thisChild.argumentList; comments=thisChild.comments})
                        | None                      -> None)

        Some {AcnTypeEncodingSpec.acnProperties = mergedProperties; children = mergedChildren; loc = thisType.loc; comments = thisType.comments}        

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


let rec private mapAcnParamTypeToAcnAcnInsertedType (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (p:AcnParamType) (props:GenericAcnProperty list) (curPath : ScopeNode list) (us:Asn1AcnMergeState) =
    let  acnAligment     = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    match p with
    | AcnPrmInteger acnErrLoc -> 
        let acnProperties = {IntegerAcnProperties.encodingProp = getIntEncodingProperty acnErrLoc props; sizeProp = getIntSizeProperty acnErrLoc props; endiannessProp = getEndianessProperty props; mappingFunction = getMappingFunctionProperty acnErrLoc props}
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
        let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetIntEncodingClass asn1.args.integerSizeInBytes acnAligment acnErrLoc acnProperties 0I 0I isUnsigned

        let checkIntHasEnoughSpace asn1Min asn1Max =
            checkIntHasEnoughSpace acnEncodingClass acnProperties.mappingFunction.IsSome acnErrLoc asn1Min asn1Max

        AcnInteger ({AcnInteger.acnProperties=acnProperties; acnAligment=acnAligment; acnEncodingClass = acnEncodingClass;  Location = acnErrLoc; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; cons=[]; withcons=[];isUnsigned=isUnsigned; uperRange= Full; checkIntHasEnoughSpace=checkIntHasEnoughSpace; inheritInfo=None}), us
    | AcnPrmBoolean  acnErrLoc ->
        let acnProperties = 
            match tryGetProp props (fun x -> match x with TRUE_VALUE e -> Some e | _ -> None) with
            | Some tv   ->  {BooleanAcnProperties.encodingPattern  = Some (TrueValue tv)}
            | None      ->
                match tryGetProp props (fun x -> match x with FALSE_VALUE e -> Some e | _ -> None) with
                | Some tv   ->  {BooleanAcnProperties.encodingPattern  = Some (FalseValue tv)}
                | None      ->  {BooleanAcnProperties.encodingPattern  = None}
        let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetBooleanEncodingClass acnAligment acnErrLoc acnProperties
        AcnBoolean ({AcnBoolean.acnProperties=acnProperties; acnAligment=acnAligment; Location = acnErrLoc; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits}), us
    | AcnPrmNullType acnErrLoc ->
        let acnProperties = { NullTypeAcnProperties.encodingPattern  = tryGetProp props (fun x -> match x with PATTERN e -> Some e | _ -> None)}
        let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetNullEncodingClass acnAligment acnErrLoc acnProperties
        AcnNullType ({AcnNullType.acnProperties=acnProperties; acnAligment=acnAligment; Location = acnErrLoc; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits}), us
    | AcnPrmRefType (md,ts)->
        let asn1Type0 = Asn1Ast.GetBaseTypeByName md ts asn1
        match asn1Type0.Kind with
        | Asn1Ast.Enumerated nmItems    ->
            let cons =  asn1Type0.Constraints |> List.collect (fixConstraint asn1) |> List.map (ConstraintsMapping.getEnumConstraint asn1 asn1Type0)
            let enumerated, ns = mergeEnumerated asn1 nmItems ts.Location (Some ts.Location) (Some {AcnTypeEncodingSpec.acnProperties = props; children = []; loc=ts.Location; comments = []}) props cons [] (AcnPrmGetTypeDefinition (curPath,md.Value,ts.Value)) us
            AcnReferenceToEnumerated({AcnReferenceToEnumerated.modName = md; tasName = ts; enumerated = enumerated; acnAligment= acnAligment}), ns
        | Asn1Ast.IA5String    
        | Asn1Ast.NumericString  ->
            let isNumeric = (asn1Type0.Kind = Asn1Ast.NumericString)
            let cons =  asn1Type0.Constraints |> List.collect (fixConstraint asn1) |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 asn1Type0)
            let defaultCharSet = [|for i in 0..127 -> System.Convert.ToChar(i) |]
            let str, ns = mergeStringType asn1 ts.Location (Some ts.Location) props cons [] defaultCharSet isNumeric (AcnPrmGetTypeDefinition (curPath,md.Value,ts.Value)) us
            AcnReferenceToIA5String({AcnReferenceToIA5String.modName = md; tasName = ts; str = str; acnAligment= acnAligment}), ns
        | Asn1Ast.Integer       ->
            let cons =  asn1Type0.Constraints |> List.collect (fixConstraint asn1) |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 asn1Type0)
            let uperRange    = uPER.getIntTypeConstraintUperRange cons  ts.Location
            let alignmentSize = AcnEncodingClasses.getAlignmentSize acnAligment
            let uperMinSizeInBits, uperMaxSizeInBits = uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes uperRange
            let acnProperties = {IntegerAcnProperties.encodingProp = getIntEncodingProperty ts.Location props; sizeProp = getIntSizeProperty ts.Location props; endiannessProp = getEndianessProperty props; mappingFunction = getMappingFunctionProperty ts.Location props}
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
                AcnEncodingClasses.GetIntEncodingClass asn1.args.integerSizeInBytes acnAligment ts.Location acnProperties uperMinSizeInBits uperMaxSizeInBits isUnsigned
            
            let checkIntHasEnoughSpace asn1Min asn1Max =
                checkIntHasEnoughSpace acnEncodingClass acnProperties.mappingFunction.IsSome ts.Location asn1Min asn1Max

            AcnInteger ({AcnInteger.acnProperties=acnProperties; acnAligment=acnAligment; acnEncodingClass = acnEncodingClass;  Location = ts.Location; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits;cons=cons;withcons=[];isUnsigned=isUnsigned; uperRange= uperRange;checkIntHasEnoughSpace=checkIntHasEnoughSpace; inheritInfo=Some {InheritanceInfo.modName=md.Value; tasName=ts.Value;hasAdditionalConstraints=false}}), us
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
            mapAcnParamTypeToAcnAcnInsertedType asn1 acn newParma (props@baseTypeAcnProps) curPath us





let rec private mergeType  (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (m:Asn1Ast.Asn1Module) (t:Asn1Ast.Asn1Type) (curPath : ScopeNode list)
                           (typeDefPath : ScopeNode list)
                           (acnType:AcnTypeEncodingSpec option) 
                           (originalLocation : SrcLoc option)             //parameter not used.
                           (refTypeCons:Asn1Ast.Asn1Constraint list)      // constraints applied to this type originating from reference types --> uPER visible
                           (withCons:Asn1Ast.Asn1Constraint list)         // constraints applied to this type originating from with component and  with components --> non uPER visible
                           (acnArgs : Asn1AcnAst.RelativePath list)
                           (acnParameters   : AcnParameter list)
                           (inferitInfo : InheritanceInfo option) 
                           (typeAssignmentInfo : AssignmentInfo option)
                           (us:Asn1AcnMergeState) : (Asn1Type*Asn1AcnMergeState)=
    let acnProps =
        match acnType with
        | None      -> []
        | Some x    -> x.acnProperties
    let acnErrLoc = acnType |> Option.map(fun x -> x.loc)
    let combinedProperties = acnProps
    let allCons = t.Constraints@refTypeCons@withCons

    let tfdArg = {GetTypeDifition_arg.asn1TypeKind = t.Kind; loc = t.Location; curPath = curPath; typeDefPath = typeDefPath; inferitInfo =inferitInfo ; typeAssignmentInfo = typeAssignmentInfo; rtlFnc = None}

    let fixConstraint  = (fixConstraint asn1)
    //let actualLocation = match originalLocation with Some l -> l | None -> t.Location
    let asn1Kind, kindState =
        match t.Kind with
        | Asn1Ast.Integer                  -> 
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 t)
            let o, us1  = mergeInteger asn1 t.Location typeAssignmentInfo  acnErrLoc combinedProperties cons wcons tfdArg us
            Integer o, us1
        | Asn1Ast.Real                     -> 
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getRealTypeConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getRealTypeConstraint asn1 t)
            let o, us1 = mergeReal asn1 t.Location acnErrLoc combinedProperties cons wcons tfdArg us
            Real o, us1
        | Asn1Ast.ObjectIdentifier                 
        | Asn1Ast.RelativeObjectIdentifier         -> 
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getObjectIdConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getObjectIdConstraint asn1 t)
            let o, us1 = mergeObjectIdentifier asn1 (t.Kind=Asn1Ast.RelativeObjectIdentifier) t.Location acnErrLoc combinedProperties cons wcons tfdArg us
            ObjectIdentifier o, us1
        | Asn1Ast.IA5String                ->  
            let defaultCharSet = [|for i in 0..127 -> System.Convert.ToChar(i) |]
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let o, us1 = mergeStringType asn1 t.Location acnErrLoc combinedProperties cons wcons defaultCharSet false (EnmStrGetTypeDifition_arg tfdArg) us
            IA5String o , us1
        | Asn1Ast.NumericString            ->  
            let defaultCharSet = [| ' ';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|]

            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let o, us1 = mergeStringType asn1 t.Location acnErrLoc combinedProperties cons wcons defaultCharSet true (EnmStrGetTypeDifition_arg tfdArg) us
            NumericString o, us1
        | Asn1Ast.OctetString              ->  
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getOctetStringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getOctetStringConstraint asn1 t)
            let o, us1 = mergeOctetStringType asn1 t.Location acnErrLoc combinedProperties cons wcons tfdArg us
            OctetString o, us1
        | Asn1Ast.BitString                ->  
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBitStringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBitStringConstraint asn1 t)
            let o, us1 = mergeBitStringType asn1 t.Location acnErrLoc combinedProperties cons wcons tfdArg us
            BitString o, us1
        | Asn1Ast.NullType                 ->  
            let constraints = []
            let o, us1 = mergeNullType acnErrLoc combinedProperties tfdArg us
            NullType o, us1
        | Asn1Ast.Boolean                  ->  
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBoolConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBoolConstraint asn1 t)
            let o, us1 = mergeBooleanType acnErrLoc combinedProperties cons wcons tfdArg us
            Boolean o, us1
        | Asn1Ast.Enumerated  items        ->  
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getEnumConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getEnumConstraint asn1 t)
            let o, us1 = mergeEnumerated asn1 items t.Location acnErrLoc acnType combinedProperties cons wcons (EnmStrGetTypeDifition_arg tfdArg) us
            Enumerated o, us1
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
            


            let typeDef, us1 = getSizeableTypeDifition tfdArg us
                        

            let newChType, us2  = mergeType asn1 acn m chType (curPath@[SQF]) (typeDefPath@[SQF]) childEncSpec None [] childWithCons  acnArgs [] None None  us1

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

            let newKind = {SequenceOf.child=newChType; acnProperties   = acnProperties; cons = cons; withcons = wcons;minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits = acnMinSizeInBits; acnMaxSizeInBits=acnMaxSizeInBits; typeDef=typeDef}
            SequenceOf newKind, us2
        | Asn1Ast.Sequence    children     -> 
            let childrenNameConstraints = allCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint w -> Some w| _ -> None) |> List.collect id
            let myVisibleConstraints = refTypeCons@t.Constraints |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)
            let myNonVisibleConstraints = withCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)

            let cons =  myVisibleConstraints|> List.collect fixConstraint |> List.map (ConstraintsMapping.getSequenceConstraint asn1 t)
            let wcons = myNonVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getSequenceConstraint asn1 t)

            let typeDef, us1 = getSequenceTypeDifition tfdArg us

            let mergeChild (cc:ChildSpec option) (c:Asn1Ast.ChildInfo) (us:Asn1AcnMergeState)  =
                let childNamedConstraints = childrenNameConstraints |> List.filter(fun x -> x.Name = c.Name)
                let childWithCons = childNamedConstraints |> List.choose(fun nc -> nc.Contraint)
                let asn1OptionalityFromWithComponents = 
                    childNamedConstraints |> 
                    List.choose(fun nc -> 
                        match nc.Mark with
                        | Asn1Ast.NoMark            -> None
                        | Asn1Ast.MarkPresent       -> Some AlwaysPresent
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
                    let newChild, us1 = mergeType asn1 acn m c.Type (curPath@[SEQ_CHILD c.Name.Value]) (typeDefPath@[SEQ_CHILD c.Name.Value]) None None [] childWithCons [] [] None  None  us
                    Asn1Child ({Asn1Child.Name = c.Name; _c_name = c.c_name; _ada_name = c.ada_name; Type  = newChild; Optionality = newOptionality;Comments = c.Comments}), us1
                | Some cc   ->
                    match cc.asn1Type with
                    | None  -> 
                        let newChild, us1 = mergeType asn1 acn m c.Type (curPath@[SEQ_CHILD c.Name.Value]) (typeDefPath@[SEQ_CHILD c.Name.Value]) (Some cc.childEncodingSpec) None [] childWithCons cc.argumentList [] None  None us
                        Asn1Child ({Asn1Child.Name = c.Name; _c_name = c.c_name; _ada_name = c.ada_name; Type  = newChild; Optionality = newOptionality; Comments = ((c.Comments |> Seq.toList) @  cc.comments) |> Seq.toArray}), us1
                    | Some xx  ->
                        //let tdprm = {GetTypeDifition_arg.asn1TypeKind = t.Kind; loc = t.Location; curPath = (curPath@[SEQ_CHILD c.Name.Value]); typeDefPath = (typeDefPath@[SEQ_CHILD c.Name.Value]); inferitInfo =None ; typeAssignmentInfo = None; rtlFnc = None}
                        let newType, us1 = mapAcnParamTypeToAcnAcnInsertedType asn1 acn xx cc.childEncodingSpec.acnProperties  (curPath@[SEQ_CHILD c.Name.Value]) us
                        AcnChild({AcnChild.Name = c.Name; id = ReferenceToType(curPath@[SEQ_CHILD c.Name.Value]); Type = newType; Comments = cc.comments |> Seq.toArray}), us1

            let mergedChildren, chus = 
                match acnType with
                | None            -> children |> foldMap (fun st ch -> mergeChild None ch st ) us1
                | Some acnEncSpec ->
                    match acnEncSpec.children with
                    | []            -> children |> foldMap (fun st ch -> mergeChild None ch st) us1
                    | acnChildren   ->
                        // MAKE SURE ACN CHILDREN ARE SUPERSET OF ASN1 CHILDREN !!!!!
                        children |> List.filter(fun c -> not (acnChildren |> List.exists(fun c2 -> c2.name = c.Name))) |> List.iter(fun c -> raise(SemanticError(acnEncSpec.loc, (sprintf "No ACN encoding specification was provided for component %s" c.Name.Value)))  )

                        acnChildren |>
                        foldMap(fun st acnChild ->
                            match children |> Seq.tryFind (fun a -> a.Name = acnChild.name) with
                            | Some x -> mergeChild (Some acnChild) x st
                            | None   -> 
                                match acnChild.asn1Type with
                                | Some xx ->
                                    let newType, nest = mapAcnParamTypeToAcnAcnInsertedType asn1 acn xx acnChild.childEncodingSpec.acnProperties (curPath@[SEQ_CHILD acnChild.name.Value]) st
                                    AcnChild({AcnChild.Name = acnChild.name; id = ReferenceToType(curPath@[SEQ_CHILD acnChild.name.Value]); Type = newType; Comments = acnChild.comments |> Seq.toArray}), nest
                                | None ->
                                    raise(SemanticError(acnChild.name.Location, (sprintf "invalid name %s" acnChild.name.Value)))) us1
            
            let uperBitMaskSize      = children |> Seq.filter(fun c -> c.Optionality.IsSome) |> Seq.length |> BigInteger
            let asn1Children     = mergedChildren |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
            let uperMaxChildrenSize  = asn1Children |> List.map(fun x -> x.Type.uperMaxSizeInBits) |> Seq.sum
            let uperMinChildrenSize  = asn1Children |> List.filter(fun x -> x.Optionality.IsNone) |> List.map(fun x -> x.Type.uperMinSizeInBits) |> Seq.sum
            
            let aligment = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
            let alignmentSize = AcnEncodingClasses.getAlignmentSize aligment
            let acnBitMaskSize =
                mergedChildren |> 
                List.filter(fun c ->
                    match c.Optionality with
                    | Some AlwaysAbsent                                 -> false
                    | Some (Optional p) when p.acnPresentWhen.IsNone    -> true
                    | _                                                 -> false) |> Seq.length |> BigInteger
            let minChildrenSize = 
                mergedChildren |> 
                List.map(fun c ->
                    match c.Optionality with
                    | Some (Optional _) -> 0I
                    | _                 -> c.acnMinSizeInBits) |> Seq.sum
            let maxChildrenSize = mergedChildren |> List.map(fun c -> c.acnMaxSizeInBits) |> Seq.sum
            let acnMaxSizeInBits = alignmentSize + acnBitMaskSize + maxChildrenSize
            let acnMinSizeInBits = alignmentSize + acnBitMaskSize + minChildrenSize

            Sequence ({Sequence.children = mergedChildren;    cons=cons; withcons = wcons;uperMaxSizeInBits=uperBitMaskSize+uperMaxChildrenSize; uperMinSizeInBits=uperBitMaskSize+uperMinChildrenSize;acnMaxSizeInBits=acnMaxSizeInBits;acnMinSizeInBits=acnMinSizeInBits; typeDef=typeDef}), chus
        | Asn1Ast.Choice      children     -> 
            let childrenNameConstraints = t.Constraints@refTypeCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint w -> Some w| _ -> None) |> List.collect id
            let myVisibleConstraints = t.Constraints@refTypeCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)
            let myNonVisibleConstraints = withCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)

            let cons =  myVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getChoiceConstraint asn1 t)
            let wcons = myNonVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getChoiceConstraint asn1 t)
            let typeDef, us1 = getChoiceTypeDifition tfdArg us
            

            let mergeChild (cc:ChildSpec option) (c:Asn1Ast.ChildInfo)  (us:Asn1AcnMergeState)=
                let childNamedConstraints = childrenNameConstraints |> List.filter(fun x -> x.Name = c.Name)
                let childWithCons = childNamedConstraints |> List.choose(fun nc -> nc.Contraint)
                let asn1OptionalityFromWithComponents = 
                    childNamedConstraints |> 
                    List.choose(fun nc -> 
                        match nc.Mark with
                        | Asn1Ast.NoMark            -> None
                        | Asn1Ast.MarkPresent       -> Some ChoiceAlwaysPresent
                        | Asn1Ast.MarkAbsent        -> Some ChoiceAlwaysAbsent
                        | Asn1Ast.MarkOptional      -> None ) |>
                    Seq.distinct |> Seq.toList 
                let newOptionality =
                    match c.Optionality with
                    | None  
                    | Some (Asn1Ast.Optional _)                  ->
                        match asn1OptionalityFromWithComponents with
                        | []          -> None
                        | newOpt::_   -> Some newOpt
                    | Some Asn1Ast.AlwaysAbsent  ->
                        match asn1OptionalityFromWithComponents with
                        | []          -> Some ChoiceAlwaysAbsent
                        | newOpt::_   -> Some newOpt
                    | Some Asn1Ast.AlwaysPresent  ->
                        match asn1OptionalityFromWithComponents with
                        | []          -> Some ChoiceAlwaysPresent
                        | newOpt::_   -> Some newOpt



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
                    let newChild, us1 = mergeType asn1 acn m c.Type (curPath@[CH_CHILD (c.Name.Value, c.present_when_name)]) (typeDefPath@[CH_CHILD (c.Name.Value, c.present_when_name)]) None None [] childWithCons [] [] None  None  us
                    {ChChildInfo.Name = c.Name; _c_name = c.c_name; _ada_name = c.ada_name; Type  = newChild; acnPresentWhenConditions = acnPresentWhenConditions; Comments = c.Comments;present_when_name = c.present_when_name; Optionality = newOptionality}, us1
                | Some cc   ->
                    let newChild, us1 = mergeType asn1 acn m c.Type (curPath@[CH_CHILD (c.Name.Value, c.present_when_name)]) (typeDefPath@[CH_CHILD (c.Name.Value, c.present_when_name)]) (Some cc.childEncodingSpec) None [] childWithCons cc.argumentList [] None  None us
                    {ChChildInfo.Name = c.Name; _c_name = c.c_name; _ada_name = c.ada_name; Type  = newChild; acnPresentWhenConditions = acnPresentWhenConditions; Comments = c.Comments; present_when_name = c.present_when_name; Optionality = newOptionality}, us1
            let mergedChildren, chus = 
                match acnType with
                | None            -> children |> foldMap (fun st ch -> mergeChild None ch st) us1
                | Some acnEncSpec ->
                    match acnEncSpec.children with
                    | []            -> children |> foldMap (fun st ch -> mergeChild None ch st) us1
                    | acnChildren   ->
                        // MAKE SURE ACN CHILDREN ARE THE SAME OF ASN1 CHILDREN !!!!!
                        let invalidAcnChildren =
                            acnChildren |> List.filter(fun acnChild -> not (children |> List.exists (fun asn1Child -> acnChild.name.Value = asn1Child.Name.Value)) )
                        match invalidAcnChildren with
                        | []    -> ()
                        | acnChild::_     -> raise(SemanticError(acnChild.name.Location, (sprintf "unexpected child name '%s'" acnChild.name.Value)))

                        children |>
                        foldMap(fun st asn1Child ->
                            match acnChildren |> Seq.tryFind(fun a -> a.name.Value = asn1Child.Name.Value) with
                            | Some acnChild -> mergeChild (Some acnChild) asn1Child st
                            | None          -> mergeChild None asn1Child st) us1
//                        acnChildren |>
//                        List.map(fun acnChild ->
//                            match children |> Seq.tryFind (fun a -> a.Name = acnChild.name) with
//                            | Some x -> mergeChild (Some acnChild) x
//                            | None   -> raise(SemanticError(acnChild.name.Location, (sprintf "invalid name %s" acnChild.name.Value))))
            let alwaysPresentChildren = mergedChildren |> List.filter(fun x -> x.Optionality = Some (ChoiceAlwaysPresent))
            match alwaysPresentChildren with
            | []        -> ()
            | x1::[]    -> ()
            | _         -> raise(SemanticError(t.Location,"Only one alternative can be marked as ALWAYS PRESENT"))
                
            let acnProperties = 
                {ChoiceAcnProperties.enumDeterminant = tryGetProp combinedProperties (fun x -> match x with CHOICE_DETERMINANT e -> Some e | _ -> None)}
            let acnLoc = acnType |> Option.map (fun z -> z.loc)
            let indexSize = GetChoiceUperDeterminantLengthInBits(BigInteger(Seq.length children))
            let minChildSize = mergedChildren  |> List.map(fun x -> x.Type.uperMinSizeInBits) |> Seq.min
            let maxChildSize = mergedChildren  |> List.map(fun x -> x.Type.uperMaxSizeInBits) |> Seq.max

            let aligment = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
            let acnMinSizeInBits, acnMaxSizeInBits = AcnEncodingClasses.GetChoiceEncodingClass  mergedChildren aligment t.Location acnProperties

            Choice ({Choice.children = mergedChildren; acnProperties = acnProperties;   cons=cons; withcons = wcons;uperMaxSizeInBits=indexSize+maxChildSize; uperMinSizeInBits=indexSize+minChildSize; acnMinSizeInBits =acnMinSizeInBits; acnMaxSizeInBits=acnMaxSizeInBits; acnLoc = acnLoc; typeDef=typeDef}), chus
        | Asn1Ast.ReferenceType rf    -> 
            let acnArguments = acnArgs
            let oldBaseType  = Asn1Ast.GetBaseTypeByName rf.modName rf.tasName asn1
            //t.Constraints@refTypeCons@withCons
            let withCompCons = withCons//allCons  |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> Some c| Asn1Ast.WithComponentsConstraint _ -> Some c | _ -> None)
            let restCons = t.Constraints@refTypeCons//allCons  |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> None | Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c)
            let acnTypeAssig = tryFindAcnTypeByName rf.modName rf.tasName acn
            let baseTypeAcnParams = 
                match acnTypeAssig with
                | None      -> []
                | Some x    -> x.acnParameters
                
            let baseTypeAcnEncSpec =
                match acnTypeAssig with
                | None      -> None
                | Some x    -> Some x.typeEncodingSpec
            let mergedAcnEncSpec = 
                //if a reference type has a component constraint (i.e. it is actually a SEQUENCE, CHOICE or SEQUENCE OF) then we should not merge the ACN spec
                //We must take the the ACN specification only from this type and not the base type. The reason is that with the WITH COMONENTS constraints you can
                //change the definition of the type (i.e. make child as always absent). 
                match t.Constraints@refTypeCons |> Seq.exists(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> true | Asn1Ast.WithComponentsConstraint _ -> true | _ -> false) with
                | true  -> acnType
                | false -> mergeAcnEncodingSpecs acnType baseTypeAcnEncSpec
            let hasAdditionalConstraints = restCons.Length > 0
            let inheritanceInfo = (Some {InheritanceInfo.modName = rf.modName.Value; tasName = rf.tasName.Value; hasAdditionalConstraints=hasAdditionalConstraints})

            //The current type definition path changes to this referenced type path, if this referenced type has no constraints (with component constraints are ignored)
            let newTypeDefPath =
                match t.Constraints |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> None | Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c) with
                | []     -> [MD rf.modName.Value; TA rf.tasName.Value]  
                | _      -> typeDefPath 

            let typeDef, us1 = getRefereceTypeDefinition asn1 t {tfdArg with typeDefPath = newTypeDefPath} us

            let resolvedType, us2     = mergeType asn1 acn m oldBaseType curPath newTypeDefPath mergedAcnEncSpec (Some t.Location) restCons withCompCons acnArgs baseTypeAcnParams inheritanceInfo typeAssignmentInfo  us1
            let newRef       = {ReferenceType.modName = rf.modName; tasName = rf.tasName; tabularized = rf.tabularized; acnArguments = acnArguments; resolvedType=resolvedType; hasConstraints = hasAdditionalConstraints; typeDef=typeDef}
            ReferenceType newRef, us2
    //let dummy = sprintf "%A" typeAssignmentInfo
    //let dummy2 = dummy
    {
        Asn1Type.Kind   = asn1Kind
        id              = ReferenceToType curPath
        acnAligment     = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
        Location        = t.Location
        acnLocation     = acnErrLoc
        acnParameters   = acnParameters |> List.map(fun prm -> {prm with id = (ReferenceToType (curPath@[(PRM prm.name)]))})
        inheritInfo   = inferitInfo
        typeAssignmentInfo = typeAssignmentInfo
        parameterizedTypeInstance = t.parameterizedTypeInstance

        
    }, kindState

let private mergeTAS (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (m:Asn1Ast.Asn1Module) (am:AcnModule option)  (tas:Asn1Ast.TypeAssignment) (us:Asn1AcnMergeState) : (TypeAssignment*Asn1AcnMergeState) =
    let acnTas = 
        match am with
        | None  -> None
        | Some x -> x.typeAssignments |> Seq.tryFind(fun z -> z.name = tas.Name)
    let acnParameters  = 
            match acnTas with 
            | None -> [] 
            | Some acnTas -> acnTas.acnParameters
    let newType, us1 = mergeType asn1 acn m tas.Type [MD m.Name.Value; TA tas.Name.Value] [MD m.Name.Value; TA tas.Name.Value] (acnTas |> Option.map(fun x -> x.typeEncodingSpec)) None [] [] [] acnParameters None (Some (TypeAssignmentInfo {TypeAssignmentInfo.modName = m.Name.Value; tasName = tas.Name.Value}))  us
    let newTas = 
        {
            TypeAssignment.Name = tas.Name
            c_name = tas.c_name
            ada_name = tas.ada_name
            Type = newType
            Comments = tas.Comments
        }
    newTas, us1

let private mergeValueAssignment (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (m:Asn1Ast.Asn1Module) (am:AcnModule option)  (vas:Asn1Ast.ValueAssignment) (us:Asn1AcnMergeState) : (ValueAssignment * Asn1AcnMergeState)=
    let inheritInfo =
        match vas.Type.Kind with
        | Asn1Ast.ReferenceType rf    -> (Some ({InheritanceInfo.modName = rf.modName.Value; tasName = rf.tasName.Value; hasAdditionalConstraints=false}))//(Some {InheritanceInfo.id = ReferenceToType [MD rf.modName.Value; TA rf.tasName.Value]; hasAdditionalConstraints=false})
        | _                           -> None
    let newType, us1 = mergeType asn1 acn m vas.Type [MD m.Name.Value; VA vas.Name.Value] [MD m.Name.Value; VA vas.Name.Value] None None [] [] [] [] inheritInfo (Some (ValueAssignmentInfo {ValueAssignmentInfo.modName = m.Name.Value; vasName = vas.Name.Value})) us
    let newVas = 
        {
            ValueAssignment.Name = vas.Name
            c_name = vas.c_name
            ada_name = vas.ada_name
            Type = newType
            Value = ValuesMapping.mapValue asn1 vas.Type vas.Value
        }
    newVas, us1

let private mergeModule (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (m:Asn1Ast.Asn1Module) (us:Asn1AcnMergeState) : (Asn1Module*Asn1AcnMergeState) =
    let acnModule = acn.files |> Seq.collect(fun f -> f.modules)  |> Seq.tryFind (fun x -> x.name = m.Name)
    let newTases, us1 = m.TypeAssignments |> foldMap (fun st tas -> mergeTAS asn1 acn m acnModule tas st) us
    let newVaes, us2 = m.ValueAssignments |> foldMap (fun st vas -> mergeValueAssignment asn1 acn m acnModule vas st) us1
    let newModule = 
        {
            Asn1Module.Name = m.Name
            TypeAssignments = newTases
            ValueAssignments = newVaes
            Imports  =  m.Imports
            Exports = m.Exports
            Comments = m.Comments
        }
    newModule, us2

let private mergeFile (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (f:Asn1Ast.Asn1File) (us:Asn1AcnMergeState): (Asn1File*Asn1AcnMergeState) =
    let newModules, us1 = f.Modules |> foldMap (fun st m -> mergeModule asn1 acn m st) us
    let newFile = 
        {
            Asn1File.FileName = f.FileName
            Tokens = f.Tokens
            Modules = newModules 
        }
    newFile, us1



//let rec registerPrimitiveTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKind) getRtlDefinitionFunc : (FE_PrimitiveTypeDefinition*Asn1AcnMergeState)=
let mergeAsn1WithAcnAst (asn1:Asn1Ast.AstRoot) (acnParseResults:ParameterizedAsn1Ast.AntlrParserResult list) defaultStgLang =
    let initialState = {Asn1AcnMergeState.allocatedTypeNames = []; allocatedFE_TypeDefinition= Map.empty; args = asn1.args; temporaryTypesAllocation = Map.empty} 
    let state =
        seq {
            for l in [C;Ada] do           
                for f in asn1.Files do
                    for m in f.Modules do
                        for tas in m.TypeAssignments do
                            let id = ReferenceToType [MD m.Name.Value; TA tas.Name.Value]
                            let programUnit = ToC m.Name.Value
                            let proposedTypedefName = ToC (asn1.args.TypePrefix + tas.Name.Value)
                            yield (l, id, tas.Type, programUnit, proposedTypedefName)
        } |> Seq.toList 
        |> foldMap (fun st (l, id, t, programUnit, proposedTypedefName) -> 
            temporaryRegisterTypeDefinition st l id programUnit proposedTypedefName
            //match t.Kind with
            //| Asn1Ast.ReferenceType rf  -> registerAnyTypeDefinition asn1 t st l id (FEI_NewSubTypeDefinition (ReferenceToType [MD rf.modName.Value; TA rf.tasName.Value])) 
            //| _                         -> registerAnyTypeDefinition asn1 t st l id FEI_NewTypeDefinition 
            ) initialState |> snd
    let acn = CreateAcnAst acnParseResults
    let files, finalState = asn1.Files |> foldMap (fun st f -> mergeFile asn1 acn f st) state
    {AstRoot.Files = files; args = asn1.args; acnConstants = acn.acnConstants; acnParseResults=acnParseResults; stg=defaultStgLang}, acn
