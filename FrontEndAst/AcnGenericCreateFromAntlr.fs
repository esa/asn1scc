module AcnGenericCreateFromAntlr
//module LspAcnGenericCreateFromAntlr
#nowarn "3536"

open System
open System.Linq
open System.Numerics
open Antlr.Acn
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open FsUtils
open AcnGenericTypes
open FsToolkit.ErrorHandling

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

type MyErr = Err1 | Err2

let rec private EvaluateConstant (constants:AcnConstant list) intConstant : Result<BigInteger, Asn1ParseError>=
    match intConstant with
    | IntConst(a)   -> Ok a.Value
    | RefConst(consLookUp)  ->
        match constants |> Seq.tryFind(fun c-> c.Name.Value = consLookUp.Value) with
        |None       -> Error (Semantic_Error(consLookUp.Location, (sprintf "Unknown symbol '%s'" consLookUp.Value)))
        |Some(cn)   -> EvaluateAcnIntExpression constants cn.Value

and private  EvaluateAcnIntExpression (constants:AcnConstant list) acnExpr : Result<BigInteger, Asn1ParseError> =
    match  acnExpr with
    | IntegerExpr(consta)   -> EvaluateConstant constants consta
    | UnMinExp(exp1)        -> //-(EvaluateAcnIntExpression constants exp1)
        result {
            let! a1 = EvaluateAcnIntExpression constants exp1
            return -a1;
        }
    | SumExpr(exp1,exp2)
    | MinExpr(exp1,exp2)
    | MulExpr(exp1,exp2)
    | DivExpr(exp1,exp2)
    | ModExpr(exp1,exp2)
    | PowExpr(exp1,exp2)    ->
        result {
            let! a1 = EvaluateAcnIntExpression constants exp1
            let! a2 = EvaluateAcnIntExpression constants exp2
            let! c =
                match  acnExpr with
                | SumExpr(_,_)  -> Ok (a1 + a2)
                | MinExpr(_,_)  -> Ok (a1 - a2)
                | MulExpr(_,_)  -> Ok (a1 * a2)
                | DivExpr(_,_)  -> Ok (a1 / a2)
                | ModExpr(_,_)  -> Ok (a1 % a2)
                | PowExpr(_,_)  -> Ok ((System.Numerics.BigInteger.Pow(a1, int a2)))
                | _             -> Error ( Bug_Error "EvaluateAcnIntExpression")
            return c
        }



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////// CONVERT ACN ANTLR TO ACN PRIVATE TYPEA ///////////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let private CreateLongField(t:ITree) = t.Children |> List.map(fun x -> x.TextL) |> AcnGenericTypes.RelativePath

let private CreateAcnParamType (t:ITree) : Result<AcnParamType, Asn1ParseError> =

    match t.Type with
    | acnParser.INTEGER         -> Ok (AcnGenericTypes.AcnParamType.AcnPrmInteger t.Location  )
    | acnParser.BOOLEAN         -> Ok (AcnGenericTypes.AcnParamType.AcnPrmBoolean t.Location  )
    | acnParser.NULL            -> Ok (AcnGenericTypes.AcnParamType.AcnPrmNullType t.Location )
    | acnParser.REFERENCED_TYPE ->
        result {
            let! (mdName, tsName) =
                match t.Children with
                | first::[]             -> Ok (first.GetAncestor(acnParser.MODULE_DEF).GetChild(0).TextL,first.TextL)
                | first::sec::[]        -> Ok (first.TextL,sec.TextL)
                | _                     -> Error (Bug_Error ("AcnCreateFromAntlr::CreateAcnAsn1Type 1"))
            return AcnGenericTypes.AcnParamType.AcnPrmRefType(mdName, tsName)
        }
    | _                         -> Error (Bug_Error ("AcnCreateFromAntlr::CreateAcnAsn1Type 2"))

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


let rec createPresentWhenBoolExpresssion (t:ITree) integerSizeInBytes : Result<AcnExpression , Asn1ParseError> =
    result {
        match t.Type with
        | acnParser.INT                 -> return (IntegerConstantExp(t.BigIntL integerSizeInBytes))
        | acnParser.UID                 -> return (AcnIntegerConstExp(t.TextL))
        | acnParser.REAL                -> return (RealConstantExp(t.DoubleL))
        | acnParser.LONG_FIELD          -> return (Asn1LongField (CreateLongField t))
        | acnParser.OR
        | acnParser.AND
        | acnParser.EQUAL
        | acnParser.NOTEQUAL
        | acnParser.LTE
        | acnParser.LT
        | acnParser.GTE
        | acnParser.GT
        | acnParser.EQUAL
        | acnParser.NOTEQUAL
        | acnParser.LTE
        | acnParser.LT
        | acnParser.GTE
        | acnParser.GT
        | acnParser.MULTIPLICATION
        | acnParser.DIVISION
        | acnParser.MODULO           ->

            let! a1 = createPresentWhenBoolExpresssion  (t.GetChild 0) integerSizeInBytes
            let! a2 = createPresentWhenBoolExpresssion (t.GetChild 1) integerSizeInBytes
            let! ret =
                match t.Type with
                | acnParser.OR                  -> Ok( OrExpression(t.Location, a1, a2)                )
                | acnParser.AND                 -> Ok( AndExpression(t.Location, a1, a2)               )
                | acnParser.EQUAL               -> Ok( EqualExpression(t.Location, a1, a2)             )
                | acnParser.NOTEQUAL            -> Ok( NotEqualExpression(t.Location, a1, a2)          )
                | acnParser.LTE                 -> Ok( LessThanEqualExpression(t.Location, a1, a2)     )
                | acnParser.LT                  -> Ok( LessThanExpression(t.Location, a1, a2)          )
                | acnParser.GTE                 -> Ok( GreaterThanEqualExpression(t.Location, a1, a2)  )
                | acnParser.GT                  -> Ok( GreaterThanExpression(t.Location, a1, a2)       )
                | acnParser.MULTIPLICATION      -> Ok( MultiplicationExpression(t.Location, a1, a2) )
                | acnParser.DIVISION            -> Ok( DivisionExpression(t.Location, a1, a2))
                | acnParser.MODULO              -> Ok( ModuloExpression(t.Location, a1, a2))
                | _                             -> Error (Bug_Error "createPresentWhenBoolExpresssion")
            return ret

        | acnParser.PLUS   when t.Children.Length > 1             ->
            let! a1 = createPresentWhenBoolExpresssion  (t.GetChild 0) integerSizeInBytes
            let! a2 = createPresentWhenBoolExpresssion (t.GetChild 1) integerSizeInBytes
            return AdditionExpression(t.Location, a1, a2)
        | acnParser.MINUS  when t.Children.Length > 1             ->
            let! a1 = createPresentWhenBoolExpresssion  (t.GetChild 0) integerSizeInBytes
            let! a2 = createPresentWhenBoolExpresssion (t.GetChild 1) integerSizeInBytes
            return SubtractionExpression(t.Location, a1, a2)
        | acnParser.PLUS   (*unary*)    ->
            let! a1 = createPresentWhenBoolExpresssion  (t.GetChild 0) integerSizeInBytes
            return a1
        | acnParser.MINUS  (*unary*)    ->
            let! a1 = createPresentWhenBoolExpresssion  (t.GetChild 0) integerSizeInBytes
            return MinusUnaryExpression(t.Location, a1)
        | acnParser.BANG   (*unary*)    ->
            let! a1 = createPresentWhenBoolExpresssion  (t.GetChild 0) integerSizeInBytes
            return NotUnaryExpression(t.Location, a1)
        | _                             ->
            let! e = Error (Bug_Error("createPresentWhenBoolExpresssion Unsupported operation"))
            return e
    }




let private CreateNamedExpression  integerSizeInBytes (t:ITree) : Result<AcnConstant , Asn1ParseError>=
    let CreateAcnIntegerConstant  (t:ITree) =
        match t.Type with
        | acnParser.INT                 -> Ok (IntConst(t.BigIntL  integerSizeInBytes))
        | acnParser.UID                 -> Ok (RefConst(t.TextL))
        | _                             -> Error (Bug_Error("AcnCreateFromAntlr::CreateAcnIntegerConstant"))
    let rec CreateExpression  (t:ITree) =
        result {
            match t.Type with
            | acnParser.INT | acnParser.UID ->
                let! a1 = CreateAcnIntegerConstant  t
                return IntegerExpr(a1)
            | acnParser.PLUS
            | acnParser.MINUS
            | acnParser.MULTIPLICATION
            | acnParser.DIVISION
            | acnParser.MODULO
            | acnParser.POWER_SYMBOL        ->
                let! a1 = CreateExpression  (t.GetChild(0))
                let! a2 = CreateExpression  (t.GetChild(1))
                match t.Type with
                | acnParser.PLUS                -> return SumExpr(a1, a2)
                | acnParser.MINUS               -> return MinExpr(a1, a2)
                | acnParser.MULTIPLICATION      -> return MulExpr(a1, a2)
                | acnParser.DIVISION            -> return DivExpr(a1, a2)
                | acnParser.MODULO              -> return ModExpr(a1, a2)
                | acnParser.POWER_SYMBOL        -> return PowExpr(a1, a2)
                | _                             ->
                    let! e = Error (Bug_Error "CreateExpression")
                    return e
            | acnParser.UNARY_MINUS         ->
                let! a1 = CreateExpression (t.GetChild(0))
                return UnMinExp(a1)
            | _                             ->
                let! e = Error (Bug_Error "AcnCreateFromAntlr::CreateExpression Unsupported operator")
                return e
        }
    result {
        let! v1 = CreateExpression  (t.GetChild(1))
        return {AcnConstant.Name = t.GetChild(0).TextL;  Value = v1 }
    }






let private createAcnProperty integerSizeInBytes (acnConstants : Map<string, BigInteger>) (t:ITree) : Result<GenericAcnProperty, Asn1ParseError> =
    let CreateAcnIntegerConstant  (t:ITree) : Result<IntLoc, Asn1ParseError> =
        match t.Type with
        | acnParser.INT                 -> Ok (t.BigIntL integerSizeInBytes)
        | acnParser.UID                 ->
            match acnConstants.TryFind t.Text with
            | Some ret -> Ok ({IntLoc.Location = t.Location; Value=ret})
            | None     -> Error (Semantic_Error(t.Location, (sprintf "No ACN constant is defined with name '%s'" t.Text)))
        | _                             -> Error (Bug_Error("AcnCreateFromAntlr::CreateAcnIntegerConstant"))
    let GetActualString (str:string) =
        let strVal = str.Substring(1)
        strVal.Remove(strVal.Length-2).Replace("\r", "").Replace("\n", "").Replace("\t", "").Replace(" ", "")

    match t.Type with
    | acnParser.ENCODING    ->
        match t.GetChild(0).Type with
        | acnParser.POS_INT             -> Ok (ENCODING GP_PosInt            )
        | acnParser.TWOSCOMPLEMENT      -> Ok (ENCODING GP_TwosComplement    )
        | acnParser.BCD                 -> Ok (ENCODING GP_BCD               )
        | acnParser.ASCII               -> Ok (ENCODING GP_Ascii             )
        | acnParser.IEEE754_1985_32     -> Ok (ENCODING GP_IEEE754_32        )
        | acnParser.IEEE754_1985_64     -> Ok (ENCODING GP_IEEE754_64        )
        | _                             -> Error (Bug_Error("createAcnProperty_ENCODING"))
    | acnParser.SIZE    ->
        result {
            match t.GetChild(0).Type with
            | acnParser.NULL_TERMINATED     -> return SIZE GP_NullTerminated
            | acnParser.INT
            | acnParser.UID                 ->
                let! exp = CreateAcnIntegerConstant (t.GetChild 0)
                return (SIZE (GP_Fixed exp)    )
            | acnParser.LONG_FIELD          -> return (SIZE (GP_SizeDeterminant (CreateLongField (t.GetChild 0)) ))
            | _                             ->
                let! e = Error (Bug_Error("createAcnProperty_SIZE"))
                return e
        }
    | acnParser.ALIGNTONEXT ->
        match t.GetChild(0).Type with
        | acnParser.BYTE                -> Ok( ALIGNTONEXT AcnGenericTypes.NextByte  )
        | acnParser.WORD                -> Ok( ALIGNTONEXT AcnGenericTypes.NextWord  )
        | acnParser.DWORD               -> Ok( ALIGNTONEXT AcnGenericTypes.NextDWord )
        | _                             -> Error (Bug_Error("createAcnProperty_ALIGNTONEXT"))
    | acnParser.ENCODE_VALUES           -> Ok ENCODE_VALUES
    | acnParser.SAVE_POSITION           -> Ok SAVE_POSITION

    | acnParser.PRESENT_WHEN            ->
        let CreateAcnPresenseCondition(t:ITree) =
            result {
                match t.Type with
                | acnParser.LONG_FIELD  -> return (GP_PresenceBool(CreateLongField t))
                | acnParser.EQUAL       ->
                    let! exp = CreateAcnIntegerConstant (t.GetChild 1)
                    return (GP_PresenceInt ((CreateLongField(t.GetChild 0)), exp))
                | acnParser.PRESENT_WHEN_STR_EQUAL ->
                    let txt = (t.GetChild 1).Text.Replace("\"","")
                    let txtL = { StringLoc.Value = txt; Location = (t.GetChild 1).Location}
                    return (GP_PresenceStr ((CreateLongField(t.GetChild 0)), txtL ))
                | _                     ->
                    let! e = Error (Bug_Error("createAcnProperty_PRESENT_WHEN"))
                    return e
            }
        result {
            //let! aaa =  t.Children |> List.map CreateAcnPresenseCondition |> List.sequenceResultM
            let! aaa =  t.Children |> List.traverseResultM CreateAcnPresenseCondition
            return PRESENT_WHEN (aaa )
        }
    | acnParser.PRESENT_WHEN_EXP            ->
        result {
            let! retExp = createPresentWhenBoolExpresssion (t.GetChild 0) integerSizeInBytes
            return PRESENT_WHEN_EXP retExp
        }
    | acnParser.TRUE_VALUE              ->
        let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
        Ok (TRUE_VALUE v)
    | acnParser.FALSE_VALUE             ->
        let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
        Ok (FALSE_VALUE v)
    | acnParser.PATTERN                 ->
        //let tp = t
        match t.GetChild(0).Type with
        | acnParser.BitStringLiteral    ->
            let v = { StringLoc.Value = GetActualString(t.GetChild(0).Text); Location = t.GetChild(0).Location}
            Ok (PATTERN (AcnGenericTypes.PATTERN_PROP_BITSTR_VALUE v))
        | acnParser.OctectStringLiteral ->
            let strVal = GetActualString(t.GetChild(0).Text)
            let chars = strVal.ToCharArray()
            let bytes = FsUtils.getAsTupples chars '0' |> List.map (fun (x1,x2)-> t.GetValueL (System.Byte.Parse(x1.ToString()+x2.ToString(), System.Globalization.NumberStyles.AllowHexSpecifier)))
            Ok (PATTERN (AcnGenericTypes.PATTERN_PROP_OCTSTR_VALUE bytes))
        | _     ->  raise(BugErrorException("createAcnProperty_PATTERN"))

    | acnParser.DETERMINANT             -> Ok (CHOICE_DETERMINANT (CreateLongField(t.GetChild 0)))
    | acnParser.ENDIANNESS               ->
        match t.GetChild(0).Type with
        | acnParser.BIG                 -> Ok (ENDIANNESS AcnGenericTypes.BigEndianness)
        | acnParser.LITTLE              -> Ok (ENDIANNESS AcnGenericTypes.LittleEndianness)
        | _                             -> Error (Bug_Error("createAcnProperty_ENDIANNES"))
    | acnParser.MAPPING_FUNCTION        ->
        match t.ChildCount > 1 with
        | false  -> Ok (MAPPING_FUNCTION (None, t.GetChild(0).TextL))
        | true   -> Ok (MAPPING_FUNCTION (Some (t.GetChild(0).TextL), t.GetChild(2).TextL))
    | acnParser.POST_ENCODING_FUNCTION  ->
        match t.ChildCount > 1 with
        | false  -> Ok (POST_ENCODING_FUNCTION (None, t.GetChild(0).TextL))
        | true   -> Ok (POST_ENCODING_FUNCTION (Some (t.GetChild(0).TextL), t.GetChild(2).TextL))

    | acnParser.POST_DECODING_VALIDATOR ->
        match t.ChildCount > 1 with
        | false  -> Ok (PRE_DECODING_FUNCTION (None, t.GetChild(0).TextL))
        | true   -> Ok (PRE_DECODING_FUNCTION (Some (t.GetChild(0).TextL), t.GetChild(2).TextL))
    | acnParser.INT                     -> Ok (ENUM_SET_VALUE (t.BigIntL integerSizeInBytes))
    | acnParser.TERMINATION_PATTERN     ->
        let tp = t
        result {
            let! bitPattern =
                let literal = GetActualString (tp.GetChild(0).Text)
                match tp.GetChild(0).Type with
                | acnParser.BitStringLiteral    ->
                    Ok ({ StringLoc.Value = literal; Location = tp.GetChild(0).Location})
                | acnParser.OctectStringLiteral ->
                    let byteArr = octetStringLiteralToByteArray literal
                    Ok ({ StringLoc.Value = byteArrayToBitStringValue byteArr; Location = tp.GetChild(0).Location})
                | _     ->  Error (Bug_Error("createAcnProperty_TERMINATION_PATTERN"))

            return (TERMINATION_PATTERN bitPattern)
        }
    | _                             -> Error (Semantic_Error(t.Location, (sprintf "Unexpected token '%s'" t.Text)))




let rec  private createTypeEncodingSpec integerSizeInBytes (allAcnFiles: CommonTypes.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: CommonTypes.AntlrParserResult)  (alreadyTakenComments:System.Collections.Generic.List<IToken>) (encSpecITree:ITree) : Result<AcnTypeEncodingSpec, Asn1ParseError> =
    result {
        let! acnProperties =
            match encSpecITree.GetOptChild(acnParser.ENCODING_PROPERTIES) with
            | None              -> Ok []
            | Some(propList)    -> propList.Children |> List.traverseResultM (createAcnProperty integerSizeInBytes acnConstants)

        let! children =
            match encSpecITree.GetOptChild(acnParser.CHILDREN_ENC_SPEC) with
            | Some childrenList ->
                let createChild (t:ITree) =
                    result {
                        let name  =
                            match t.GetOptChild(acnParser.LID) with
                            | None          -> StringLoc.ByValue "#"
                            | Some(lid)     -> lid.TextL
                        let argumentList    =
                                match t.GetOptChild(acnParser.ARGUMENTS) with
                                | None            -> []
                                | Some(argList)   -> argList.Children |> List.map CreateLongField
                        let comments = Antlr.Comment.GetComments(thisAcnFile.tokens, alreadyTakenComments, thisAcnFile.tokens.[t.TokenStopIndex].Line, t.TokenStartIndex - 1, t.TokenStopIndex + 2, true)
                        let! childEncodingSpec = createTypeEncodingSpec integerSizeInBytes allAcnFiles acnConstants thisAcnFile alreadyTakenComments (t.GetChildByType acnParser.ENCODING_SPEC)
                        let! asn1Type  =
                            result {
                                match t.Type with
                                | acnParser.CHILD       -> return None
                                | acnParser.CHILD_NEW   ->
                                    let! p = CreateAcnParamType (t.GetChild 1)
                                    return (Some p )
                                | _     ->
                                    let! e = Error (Bug_Error("createTypeEncodingSpec_CHILD"))
                                    return e
                            }

                        return {ChildSpec.name = name; childEncodingSpec= childEncodingSpec; asn1Type=asn1Type; argumentList=argumentList; inserted = false; comments = comments |> Seq.toList}
                    }
                childrenList.Children |> List.traverseResultM createChild
            | None              -> Ok []
        let pos =
            encSpecITree.Location, (encSpecITree.GetChildByType(acnParser.R_SBRACKET)).Location
        return {AcnTypeEncodingSpec.acnProperties = acnProperties; children = children; loc = encSpecITree.Location; comments=[]; position = pos; antlrSubTree = Some encSpecITree}
    }

let private CreateTypeAssignment integerSizeInBytes (allAcnFiles: CommonTypes.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: CommonTypes.AntlrParserResult)  (alreadyTakenComments:System.Collections.Generic.List<IToken>) (tasTree:ITree) : Result<AcnTypeAssignment,Asn1ParseError> =
    result {
        let tasNameL = tasTree.GetChildByType(acnParser.UID).TextL

        let encSpecITree = tasTree.GetChildByType(acnParser.ENCODING_SPEC)
        let! prms =
            match tasTree.GetOptChild(acnParser.PARAM_LIST) with
            | None -> Ok []
            | Some(paramList) ->
                let CreateParam (x:ITree) =
                    result {
                        let prmName = x.GetChild(1).Text
                        let loc = x.GetChild(1).Location
                        //check that all parameters are used
                        let refs = encSpecITree.AllChildren |> List.filter(fun x -> x.Type = acnParser.LONG_FIELD && x.ChildCount=1) |> List.map(fun x -> x.GetChild(0).Text)
                        match refs |> Seq.tryFind(fun x -> x = prmName) with
                        | Some(_)   ->
                            //parameter id is initially set to an invalid value.
                            //It takes the correct value when the ASN.1 is constructed.
                            let! t = CreateAcnParamType (x.GetChild(0))
                            return {AcnGenericTypes.AcnParameter.name = prmName; AcnGenericTypes.AcnParameter.asn1Type=t ; AcnGenericTypes.AcnParameter.loc = loc; AcnGenericTypes.id = ReferenceToType([]) }
                        | None      ->
                            let! e = Error(Semantic_Error(loc, sprintf "unreferenced parameter '%s'" prmName))
                            return e
                    }
                paramList.Children |> List.traverseResultM CreateParam

        let comments = Antlr.Comment.GetComments(thisAcnFile.tokens, alreadyTakenComments, thisAcnFile.tokens.[tasTree.TokenStopIndex].Line, tasTree.TokenStartIndex - 1, tasTree.TokenStopIndex + 1, true)
        let! typeEncodingSpec = createTypeEncodingSpec integerSizeInBytes allAcnFiles acnConstants thisAcnFile alreadyTakenComments encSpecITree
        //tasTree.TokenStartIndex
        let acnFile = allAcnFiles |> Seq.find(fun z -> z.rootItem = tasTree.Root)
        let pos = {
            RangeWithinFile.filename=  tasTree.Root.FileName;
            startPos=
                let startToken = acnFile.tokens.[tasTree.TokenStartIndex]
                {|line=startToken.Line;charPos=startToken.CharPositionInLine |};
            endPos=
                let endToken = acnFile.tokens.[tasTree.TokenStopIndex]
                {|line=endToken.Line;charPos=endToken.CharPositionInLine + tasTree.Text.Length|}
            }
        return {AcnTypeAssignment.name = tasNameL; acnParameters = prms; typeEncodingSpec = typeEncodingSpec; comments = comments |> Seq.toList; position=pos}
    }

let private CreateModule integerSizeInBytes (allAcnFiles: CommonTypes.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: CommonTypes.AntlrParserResult)   (alreadyTakenComments:System.Collections.Generic.List<IToken>)  (modTree : ITree) : Result<AcnModule, Asn1ParseError> =
    result {
        let modNameL = modTree.GetChildByType(acnParser.UID).TextL

        let tasITreeList = modTree.GetChildrenByType(acnParser.TYPE_ENCODING)

        //check for duplicate type assignments in the ACN module
        do! tasITreeList |> List.map(fun x -> x.GetChildByType(acnParser.UID).TextL) |> CheckForDuplicates2

        let! newTasses = tasITreeList |> List.traverseResultM (fun tasTree -> CreateTypeAssignment integerSizeInBytes allAcnFiles acnConstants thisAcnFile alreadyTakenComments tasTree)

        return {AcnModule.name = modNameL; typeAssignments = newTasses}
    }




let private CheckCircularDependenciesInAcnConstants (constants : List<ITree>) : Result<unit, Asn1ParseError>=
    let HandleConstant (t:ITree) =
        let rec GetNamesFromExpr (t:ITree) : Result<StringLoc list, Asn1ParseError>=
            result {
                match t.Type with
                | acnParser.UID                 -> return [t.TextL]
                | acnParser.INT                 -> return []
                | acnParser.PLUS |acnParser.MINUS | acnParser.MULTIPLICATION | acnParser.DIVISION | acnParser.MODULO | acnParser.POWER_SYMBOL        ->
                    let! child1Names = GetNamesFromExpr (t.GetChild(0))
                    let! child2Names = GetNamesFromExpr (t.GetChild(1))
                    return child1Names@child2Names
                | acnParser.UNARY_MINUS         ->
                    let! childNames = GetNamesFromExpr (t.GetChild(0))
                    return childNames
                | _                             ->
                    let! e = Error (Bug_Error("CheckCircularDependenciesInAcnConstants.HandleConstant.GetNamesFromExpr Unsupported operator"))
                    return e
            }
        result {
            let! e = GetNamesFromExpr (t.GetChild(1))
            return (t.GetChild(0).TextL, e)
        }
    result {
        let! constantsExpanded = constants |> List.traverseResultM HandleConstant
        let independentConstants = constantsExpanded |> List.filter(fun (nm, lst) -> lst.IsEmpty ) |> List.map fst
        let dependentConstants = constantsExpanded |> List.filter(fun (nm, lst) -> not (lst.IsEmpty) )
        let comparer (s1:StringLoc) (s2:StringLoc) = s1.Value = s2.Value
        let aa = DoTopologicalSort2_noexc independentConstants dependentConstants comparer
        match aa with
        | Ok _  -> Ok() |> ignore
        | Error cyclicDepds ->
            match cyclicDepds with
            | []        ->
                let! e = Error (Bug_Error(""))
                return e
            | (x,_)::xs ->
                let printConstant (md:StringLoc, deps: StringLoc list) =
                    sprintf "Anc constant '%s' depends on : %s" md.Value (deps |> List.map(fun z -> "'" + z.Value + "'") |> Seq.StrJoin ", ")
                let names = cyclicDepds |> List.map printConstant |> Seq.StrJoin "\n\tand\n"
                //let names = cyclicDepds |> Seq.map (fun (n,_) -> n.Value) |> Seq.StrJoin ", "
                let! e = Error (Semantic_Error(x.Location, sprintf "Cyclic dependencies in ACN constants: %s" names))
                return e

    }



let private LoadAcnFile integerSizeInBytes (allAcnFiles: CommonTypes.AntlrParserResult list) (acnConstants : Map<string, BigInteger>) (thisAcnFile: CommonTypes.AntlrParserResult)   : Result<AcnFile, Asn1ParseError> =
    result {
        let alreadyTakenComments = new System.Collections.Generic.List<IToken>();

        let! modules = thisAcnFile.rootItem.Children |> List.traverseResultM (CreateModule integerSizeInBytes allAcnFiles acnConstants thisAcnFile alreadyTakenComments)
        return {AcnFile.antlrResult = thisAcnFile; modules = modules}
    }

let CreateAcnAst_no_exc  integerSizeInBytes (allAcnFiles: CommonTypes.AntlrParserResult list) : Result<AcnAst,Asn1ParseError> =
    result {
        ITree.RegisterFiles(allAcnFiles|> Seq.map(fun pr -> (pr.rootItem, pr.fileName)))
        let constants = seq {
            for acnFile in allAcnFiles do
                for m in acnFile.rootItem.Children do
                    for c in m.GetChildrenByType(acnParser.CONSTANT) do
                        yield c } |> Seq.toList

        let constantNames = constants |> List.map(fun c -> c.GetChild(0).TextL)

        // check that all constant names are unique
        do! constantNames |> CheckForDuplicates2

        do! CheckCircularDependenciesInAcnConstants constants

        let! constantValues = constants |> List.traverseResultM (CreateNamedExpression  integerSizeInBytes)
        let! acnConstantsList =
            constantValues |>
            List.traverseResultM(fun c ->
                result {
                    let! e = EvaluateAcnIntExpression constantValues c.Value
                    return (c.Name.Value, e)
                }
            )
        let acnConstantsMap = acnConstantsList |> Map.ofList
        let! acnFiles = allAcnFiles |> List.traverseResultM (LoadAcnFile integerSizeInBytes allAcnFiles acnConstantsMap)
        return {AcnAst.files = acnFiles; acnConstants = acnConstantsMap}
    }


let CreateAcnAst  integerSizeInBytes (allAcnFiles: CommonTypes.AntlrParserResult list) : AcnAst =
    match CreateAcnAst_no_exc  integerSizeInBytes allAcnFiles with
    | Ok ret -> ret
    | Error(Semantic_Error(l,m))    -> raise (SemanticError(l,m))
    | Error(Bug_Error m)            -> raise (BugErrorException m)
    | Error(User_Error m)           -> raise (UserException m)


let someTests () =

    let c1 = IntegerExpr(IntConst (IntLoc.ByValue 100I))

    let constants = [{AcnConstant.Name = StringLoc.ByValue "t"; Value = IntegerExpr(IntConst (IntLoc.ByValue 100I))}]


    let e1 = IntegerExpr(IntConst (IntLoc.ByValue 10I))
    let e2 = IntegerExpr(RefConst (StringLoc.ByValue "z"))

    let add12 = SumExpr(e2,e1)

    let res = EvaluateAcnIntExpression constants add12

    match res with
    | Ok (a1)                            -> printfn "%A" a1
    | Error (Semantic_Error(_,e))
    | Error (User_Error e)
    | Error (Bug_Error e)               -> printfn "%A" e

    0


let tryFindAcnTypeByName modName tasName (r:AcnAst) =
    match r.files |> List.collect (fun f -> f.modules) |> Seq.tryFind(fun m -> m.name = modName) with
    | None  -> None
    | Some m-> m.typeAssignments |> Seq.tryFind (fun t -> t.name = tasName)


let rec printDebug (exp:AcnExpression) : (int*string) =
    let printUnary op e1 mp =
        let cp, ct = printDebug e1
        mp, if cp >= mp then sprintf "%s(%s)" op ct else sprintf "%s%s" op ct
    let printBinary op e1 e2 mp =
        let cp1, ct1 = printDebug e1
        let cp2, ct2 = printDebug e2
        mp, (if cp1 >= mp then "(" + ct1 + ")" else ct1 ) + " " + op + " " + (if cp2 >= mp then "(" + ct2 + ")" else ct2 )
    match exp with
    | IntegerConstantExp            x      -> 0, x.Value.ToString()
    | AcnIntegerConstExp            x      -> 0, x.Value.ToString()
    | RealConstantExp               x      -> 0, x.Value.ToString()
    | BooleanConstantExp            x      -> 0, x.Value.ToString()
    | Asn1LongField                 x      -> 0, x.AsString
    | NotUnaryExpression            (_,e1)     -> printUnary "!" e1 1
    | MinusUnaryExpression          (_,e1)     -> printUnary "-" e1 1
    | AdditionExpression            (_,e1, e2) -> printBinary "+" e1 e2 3 // 1, sprintf "(%s) + (%s)" (printDebug e1) (printDebug e2)
    | SubtractionExpression         (_,e1, e2) -> printBinary "-" e1 e2 3 //3, sprintf "(%s) - (%s)" (printDebug e1) (printDebug e2)
    | MultiplicationExpression       (_,e1, e2) -> printBinary "*" e1 e2 2 //3, sprintf "(%s) * (%s)" (printDebug e1) (printDebug e2)
    | DivisionExpression            (_,e1, e2) -> printBinary "/" e1 e2 2 //2, sprintf "(%s) / (%s)" (printDebug e1) (printDebug e2)
    | ModuloExpression              (_,e1, e2) -> printBinary "%" e1 e2 2 //2, sprintf "(%s) %% (%s)" (printDebug e1) (printDebug e2)
    | LessThanEqualExpression       (_,e1, e2) -> printBinary "<=" e1 e2 4 //4, sprintf "(%s) <= (%s)" (printDebug e1) (printDebug e2)
    | LessThanExpression            (_,e1, e2) -> printBinary "<" e1 e2 4 //4, sprintf "(%s) < (%s)" (printDebug e1) (printDebug e2)
    | GreaterThanEqualExpression    (_,e1, e2) -> printBinary ">=" e1 e2 4 //4, sprintf "(%s) >= (%s)" (printDebug e1) (printDebug e2)
    | GreaterThanExpression         (_,e1, e2) -> printBinary ">" e1 e2 4 //4, sprintf "(%s) > (%s)" (printDebug e1) (printDebug e2)
    | EqualExpression               (_,e1, e2) -> printBinary "==" e1 e2 5 //5, sprintf "(%s) == (%s)" (printDebug e1) (printDebug e2)
    | NotEqualExpression            (_,e1, e2) -> printBinary "!=" e1 e2 5 //5, sprintf "(%s) != (%s)" (printDebug e1) (printDebug e2)
    | AndExpression                 (_,e1, e2) -> printBinary "and" e1 e2 6 //6, sprintf "(%s) and (%s)" (printDebug e1) (printDebug e2)
    | OrExpression                  (_,e1, e2) -> printBinary "or" e1 e2 6 //6, sprintf "(%s) or (%s)" (printDebug e1) (printDebug e2)
