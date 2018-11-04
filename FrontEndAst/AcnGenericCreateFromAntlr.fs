module AcnGenericCreateFromAntlr
open System
open System.Linq
open System.Numerics
open Antlr.Acn
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open FsUtils
open AcnGenericTypes
//open FE_TypeDefinition


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// ACN PRIVATE TYPES (NOT EXPOSED TO OTHER MODULES) /////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////





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



let private CreateLongField(t:ITree) = t.Children |> List.map(fun x -> x.TextL) |> AcnGenericTypes.RelativePath

let private CreateAcnParamType (t:ITree) =
    match t.Type with
    | acnParser.INTEGER         -> AcnGenericTypes.AcnParamType.AcnPrmInteger t.Location
    | acnParser.BOOLEAN         -> AcnGenericTypes.AcnParamType.AcnPrmBoolean t.Location
    | acnParser.NULL            -> AcnGenericTypes.AcnParamType.AcnPrmNullType t.Location
    | acnParser.REFERENCED_TYPE -> 
        let mdName, tsName = 
            match t.Children with
            | first::[]             -> first.GetAncestor(acnParser.MODULE_DEF).GetChild(0).TextL,first.TextL
            | first::sec::[]        -> first.TextL,sec.TextL
            | _                     -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnAsn1Type 1"))
        AcnGenericTypes.AcnParamType.AcnPrmRefType(mdName, tsName)
    | _                         -> raise(BugErrorException("AcnCreateFromAntlr::CreateAcnAsn1Type 2"))



//Search in the antlr acn AST for a specific module, typeassigment and
//return the ACN parameters as a string list
let private GetParams (files:CommonTypes.AntlrParserResult list) modName tasName  =
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
        | acnParser.BYTE                -> ALIGNTONEXT AcnGenericTypes.NextByte
        | acnParser.WORD                -> ALIGNTONEXT AcnGenericTypes.NextWord
        | acnParser.DWORD               -> ALIGNTONEXT AcnGenericTypes.NextDWord
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
            PATTERN (AcnGenericTypes.PATERN_PROP_BITSTR_VALUE v)
        | acnParser.OctectStringLiteral ->
            let strVal = GetActualString(t.GetChild(0).Text)
            let chars = strVal.ToCharArray() 
            let bytes = FsUtils.getAsTupples chars '0' |> List.map (fun (x1,x2)-> t.GetValueL (System.Byte.Parse(x1.ToString()+x2.ToString(), System.Globalization.NumberStyles.AllowHexSpecifier))) 
            PATTERN (AcnGenericTypes.PATERN_PROP_OCTSTR_VALUE bytes)
        | _     ->  raise(BugErrorException("creareAcnProperty_PATTERN"))
                    
    | acnParser.DETERMINANT             -> CHOICE_DETERMINANT (CreateLongField(t.GetChild 0))
    | acnParser.ENDIANNES               -> 
        match t.GetChild(0).Type with 
        | acnParser.BIG                 -> ENDIANNES AcnGenericTypes.BigEndianness
        | acnParser.LITTLE              -> ENDIANNES AcnGenericTypes.LittleEndianness
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
    | _                             -> raise(SemanticError(t.Location, (sprintf "Unexpected token '%s'" t.Text)))

let rec  private createTypeEncodingSpec (allAcnFiles: CommonTypes.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: CommonTypes.AntlrParserResult)  (alreadyTakenComments:System.Collections.Generic.List<IToken>) (encSpecITree:ITree) : AcnTypeEncodingSpec =
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

let private CreateTypeAssignment (allAcnFiles: CommonTypes.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: CommonTypes.AntlrParserResult)  (alreadyTakenComments:System.Collections.Generic.List<IToken>) (tasTree:ITree) : AcnTypeAssignment =
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
                    {AcnGenericTypes.AcnParameter.name = prmName; AcnGenericTypes.AcnParameter.asn1Type=CreateAcnParamType (x.GetChild(0)) ; AcnGenericTypes.AcnParameter.loc = loc; AcnGenericTypes.id = ReferenceToType([]) }
                | None      -> raise(SemanticError(loc, sprintf "unreferenced parameter '%s'" prmName))
            paramList.Children |> List.map CreateParam

    let comments = Antlr.Comment.GetComments(thisAcnFile.tokens, alreadyTakenComments, thisAcnFile.tokens.[tasTree.TokenStopIndex].Line, tasTree.TokenStartIndex - 1, tasTree.TokenStopIndex + 1, true)
    let typeEncodingSpec = createTypeEncodingSpec allAcnFiles acnConstants thisAcnFile alreadyTakenComments encSpecITree

    {AcnTypeAssignment.name = tasNameL; acnParameters = prms; typeEncodingSpec = typeEncodingSpec; comments = comments |> Seq.toList}


let private CreateModule (allAcnFiles: CommonTypes.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: CommonTypes.AntlrParserResult)   (alreadyTakenComments:System.Collections.Generic.List<IToken>)  (modTree : ITree) : AcnModule =
    let modNameL = modTree.GetChildByType(acnParser.UID).TextL

    let tasITreeList = modTree.GetChildrenByType(acnParser.TYPE_ENCODING)
    
    //check for duplicate type assignments in the ACN module
    tasITreeList |> List.map(fun x -> x.GetChildByType(acnParser.UID).TextL) |> CheckForDuplicates

    let newTasses = tasITreeList |> List.map(fun tasTree -> CreateTypeAssignment allAcnFiles acnConstants thisAcnFile alreadyTakenComments tasTree) 
    
    {AcnModule.name = modNameL; typeAssignments = newTasses}


let private LoadAcnFile (allAcnFiles: CommonTypes.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: CommonTypes.AntlrParserResult)   : AcnFile = 
    let alreadyTakenComments = new System.Collections.Generic.List<IToken>();

    let modules = thisAcnFile.rootItem.Children |> List.map (CreateModule allAcnFiles acnConstants thisAcnFile alreadyTakenComments)
    {AcnFile.antlrResult = thisAcnFile; modules = modules}

let CreateAcnAst (allAcnFiles: CommonTypes.AntlrParserResult list) : AcnAst =  
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


let tryFindAcnTypeByName modName tasName (r:AcnAst) =
    match r.files |> List.collect (fun f -> f.modules) |> Seq.tryFind(fun m -> m.name = modName) with
    | None  -> None
    | Some m-> m.typeAssignments |> Seq.tryFind (fun t -> t.name = tasName)
