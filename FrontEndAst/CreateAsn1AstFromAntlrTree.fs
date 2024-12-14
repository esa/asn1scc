module CreateAsn1AstFromAntlrTree

#nowarn "3536"
open System
open System.Numerics
open FsUtils
open ParameterizedAsn1Ast
open Antlr.Asn1
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils
open CommonTypes
open AbstractMacros
open AcnGenericTypes
open FsToolkit.ErrorHandling


let ConstraintNodes = [ asn1Parser.EXT_MARK; asn1Parser.UnionMark; asn1Parser.ALL_EXCEPT; asn1Parser.IntersectionMark; asn1Parser.EXCEPT;
                        asn1Parser.VALUE_RANGE_EXPR; asn1Parser.SUBTYPE_EXPR; asn1Parser.SIZE_EXPR; asn1Parser.PERMITTED_ALPHABET_EXPR;
                        asn1Parser.WITH_COMPONENT_CONSTR; asn1Parser.WITH_COMPONENTS_CONSTR ]


let TimeClassMap  =
    [
        ("Basic=Time Time=HMS Local-or-UTC=L"                            , Asn1LocalTime 0  )
        ("Basic=Time Time=HMS Local-or-UTC=Z"                            , Asn1UtcTime  0 )
        ("Basic=Time Time=HMS Local-or-UTC=LD"                           , Asn1LocalTimeWithTimeZone  0 )
        ("Basic=Date Date=YMD Year=Basic"                                , Asn1Date   )
        ("Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=L"   , Asn1Date_LocalTime  0 )
        ("Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=Z"   , Asn1Date_UtcTime  0 )
        ("Basic=Date-Time Date=YMD Year=Basic Time=HMS Local-or-UTC=LD"  , Asn1Date_LocalTimeWithTimeZone 0  ) ] |>
    List.collect (fun (str, cl) ->
        let arr = str.Split ' '
        let combs = combinations arr |> List.map (fun l -> Seq.StrJoin "" l ) |> List.map (fun s -> (s,cl))
        combs) |> Map.ofList




let CreateRefTypeContent (tree:ITree) =
    match getTreeChildren(tree) |> List.filter(fun x -> x.Type<> asn1Parser.ACTUAL_PARAM_LIST) with
    | refTypeName::[]           ->
        let mdTree = tree.GetAncestor(asn1Parser.MODULE_DEF)
        let mdName = mdTree.GetChild(0).TextL
        let imports = mdTree.GetChildrenByType(asn1Parser.IMPORTS_FROM_MODULE)
        let importedFromModule = imports |> List.tryFind(fun imp-> imp.GetChildrenByType(asn1Parser.UID) |> Seq.exists(fun impTypeName -> impTypeName.Text = refTypeName.Text ))
        match importedFromModule with
        |Some(imp)  -> Ok ( imp.GetChild(0).TextL,  refTypeName.TextL)
        |None       -> Ok ( tree.GetAncestor(asn1Parser.MODULE_DEF).GetChild(0).TextL,  refTypeName.TextL)

    | modName::refTypeName::[]  ->  Ok ( modName.TextL, refTypeName.TextL)
    | _                         ->  Error (Bug_Error("Bug in CrateType(refType)"))


type RegisterObjIDKeyword =
    | LEAF  of (string list*int*RegisterObjIDKeyword list)
let registeredObjectIdentifierKeywords =
    let letters = ['a' .. 'z'] |> List.mapi (fun i l -> LEAF([l.ToString()],i+1, []))
    [
        LEAF (["itu-t";"ccitt"],0, [
                    LEAF(["recommendation"],0, letters)
                    LEAF(["question"],1, [])
                    LEAF(["administration"],2, [])
                    LEAF(["network-operator"],3, [])
                    LEAF(["identified-organization"],4, [])
                ])
        LEAF (["iso"],1, [
                    LEAF(["standard"],0, [])
                    LEAF(["member-body"],2, [])
                    LEAF(["identified-organization"],3, [])
                ])
        LEAF (["joint-iso-itu-t";"joint-iso-ccitt"],2, [])
    ]


let rec isRegisterObjIDKeyword (curLevelKeywords: RegisterObjIDKeyword list) (path:string list) =
    match path with
    | []        -> Error(Bug_Error "expecting non empty list")
    | x1::xs    ->
        let curLevelKeyword =
            curLevelKeywords |> List.tryFind(fun (LEAF(names,nVal,  _)) -> names |> Seq.exists ((=) x1) )
        match xs with
        | []    -> Ok (curLevelKeyword|> Option.map(fun (LEAF(_,nVal,  _)) -> nVal))
        | _     ->
            match curLevelKeyword with
            | None  -> Ok None
            | Some (LEAF(_,_,nextLevelKeywords))    -> isRegisterObjIDKeyword nextLevelKeywords xs

let rec isRegisterObjIDKeyword2 (parentKeyword: RegisterObjIDKeyword option) (curName:string) =
    match parentKeyword with
    | None  ->
            registeredObjectIdentifierKeywords |> List.tryFind(fun (LEAF(names,nVal,  _)) -> names |> Seq.exists ((=) curName) )
    | Some (LEAF(_,_,curLevelKeywords)) ->
        curLevelKeywords |> List.tryFind(fun (LEAF(names,nVal,  _)) -> names |> Seq.exists ((=) curName) )

let createDefaultConstraintsForEnumeratedTypes (tree:ITree) (namedItems:NamedItem list) =
    let mdTree = tree.GetAncestor(asn1Parser.MODULE_DEF)
    let mdName = mdTree.GetChild(0).TextL
    let createSingleValueConstraintFromNamedItem (ni:NamedItem) =
        let v = {Asn1Value.Kind = (RefValue ({StringLoc.Value = mdName.Value; Location=ni.Name.Location}, ni.Name)); Location = ni.Name.Location; moduleName=mdName.Value  }
        SingleValueConstraint (ni.Name.Value, v)
    match namedItems with
    | x::xs ->
        let initialCon = createSingleValueConstraintFromNamedItem x
        let asn1Cons =
            namedItems |> List.map(fun z -> z.Name.Value) |> Seq.StrJoin " | "
        let ret =
            xs |> List.fold(fun accConst ni ->
                                let curCon = createSingleValueConstraintFromNamedItem ni
                                UnionConstraint (asn1Cons, accConst, curCon, true) ) initialCon
        Ok ret
    | []    ->
            Error (Semantic_Error (tree.Location, "Enumerated type has no enumerants"))


let singleReference2DoubleReference (tree:ITree) =
    let strVal = tree.GetChild(0).TextL
    let modl = tree.GetAncestor(asn1Parser.MODULE_DEF)
    let modName = modl.GetChild(0).TextL
    let imports = modl.GetChildrenByType(asn1Parser.IMPORTS_FROM_MODULE)
    let importedFromModule = imports |> List.tryFind(fun imp-> imp.GetChildrenByType(asn1Parser.LID) |> Seq.exists(fun impTypeName -> impTypeName.Text = strVal.Value ))
    let valToReturn =
        match importedFromModule with
        |Some(imp)  -> ( imp.GetChild(0).TextL,  strVal)
        |None       -> ( modName,  strVal)
    valToReturn



let rec foldResult f total ns =
    match ns with
    | []        -> Ok (total)
    | n :: tail ->
        result {
            let! ns = f total n
            //let! retVal = foldResult f ns tail
            //return retVal
            return! foldResult f ns tail
        }





let rec CreateValue integerSizeInBytes (astRoot:list<ITree>) (tree:ITree ) : Result<Asn1Value, Asn1ParseError>=
    result {

        let GetActualString (str:string) =
            let strVal = str.Substring(1)
            strVal.Remove(strVal.Length-2).Replace("\r", "").Replace("\n", "").Replace("\t", "").Replace(" ", "")

        let mdTree = tree.GetAncestor(asn1Parser.MODULE_DEF)
        let mdName = mdTree.GetChild(0).Text

        let! asn1ValueKind =
                match tree.Type with
                | asn1Parser.INT                    -> Ok (IntegerValue(tree.BigIntL integerSizeInBytes))
                | asn1Parser.FloatingPointLiteral   -> Ok (RealValue(tree.DoubleL))
                | asn1Parser.NUMERIC_VALUE2         ->
                    result {
                        let mantissa = double (tree.GetChild(0).BigInt integerSizeInBytes)
                        let! bas =
                                result {
                                    if tree.GetChild(1).BigInt integerSizeInBytes = 2I then
                                        return 2.0
                                    elif tree.GetChild(1).BigInt integerSizeInBytes = 10I then
                                        return 10.0
                                    else
                                        let! e = Error (Semantic_Error(tree.GetChild(1).Location, "Only 2 or 10 values are allowed"))
                                        return e
                                }
                        let exponent = double (tree.GetChild(2).BigInt integerSizeInBytes)
                        let d = mantissa*Math.Pow(bas, exponent)
                        return (RealValue({DoubleLoc.Value=d;Location=tree.Location}))
                    }
                | asn1Parser.PLUS_INFINITY          -> Ok (RealValue(tree.GetValueL Double.PositiveInfinity))
                | asn1Parser.MINUS_INFINITY         -> Ok (RealValue(tree.GetValueL Double.NegativeInfinity))
                | asn1Parser.TRUE                   -> Ok (BooleanValue(tree.GetValueL true))
                | asn1Parser.FALSE                  -> Ok (BooleanValue(tree.GetValueL false))
                | asn1Parser.StringLiteral          ->
                    let text = tree.Text.Substring(1, tree.Text.Length-2)
                    Ok (StringValue({ StringLoc.Value = text; Location = tree.Location}))
                | asn1Parser.NULL                   -> Ok NullValue
                | asn1Parser.BitStringLiteral       -> Ok (BitStringValue(tree.GetValueL(GetActualString(tree.Text))))
                | asn1Parser.VALUE_REFERENCE        -> Ok (RefValue (singleReference2DoubleReference tree))
                | asn1Parser.OBJECT_ID_VALUE        ->
                    let handleObjectIdComponent (rootComponent:bool) (parentKeyword: RegisterObjIDKeyword option) (tree:ITree ) =
                        match tree.Type with
                        | asn1Parser.OBJ_LST_ITEM2  ->
                            let nVal = (tree.GetChild(0).BigIntL integerSizeInBytes).Value
                            match nVal >= 0I with
                            | true  -> Ok (ObjInteger (tree.GetChild(0).BigIntL integerSizeInBytes ) , None)
                            | false -> Error (Semantic_Error(tree.GetChild(0).Location, "Negative values are not permitted in OJECT-IDENTIFIER"))

                        | asn1Parser.OBJ_LST_ITEM1  ->
                            let name = tree.GetChild(0).TextL
                            match tree.ChildCount with
                            | 2  ->
                                let secChild = tree.GetChild(1)
                                match secChild.Type with
                                | asn1Parser.INT            ->
                                    match (secChild.BigIntL integerSizeInBytes ).Value >= 0I with
                                    | true  -> Ok (ObjNamedIntValue (name, secChild.BigIntL integerSizeInBytes ), None)
                                    | false -> Error (Semantic_Error(secChild.Location, "Negative values are not permitted in OJECT-IDENTIFIER"))
                                | asn1Parser.DEFINED_VALUE  ->
                                    match secChild.ChildCount with
                                    | 2     -> Ok (ObjNamedDefValue(name, (secChild.GetChild(0).TextL, secChild.GetChild(1).TextL)), None)
                                    | 1     -> Ok (ObjNamedDefValue(name, ((singleReference2DoubleReference secChild))), None)
                                    | _     -> Error (Bug_Error("Bug in CreateValue asn1Parser.OBJECT_ID_VALUE 1"))
                                | _         -> Error (Bug_Error("Bug in CreateValue asn1Parser.OBJECT_ID_VALUE 2"))
                            | 1 ->
                                let regKeyWord =
                                    match rootComponent with
                                    | true  -> isRegisterObjIDKeyword2 parentKeyword name.Value
                                    | false ->
                                        match parentKeyword with
                                        | Some _    -> isRegisterObjIDKeyword2 parentKeyword name.Value
                                        | None      -> None

                                match regKeyWord with
                                | Some (LEAF(_,nVal,_)) -> Ok (ObjRegisteredKeyword(name,BigInteger nVal), regKeyWord)
                                | None                  -> Ok (ObjDefinedValue (singleReference2DoubleReference tree) , None)

                            | _ -> Error (Bug_Error("Bug in CreateValue asn1Parser.OBJECT_ID_VALUE 3"))
                        | _ -> Error (Bug_Error("Bug in CreateValue asn1Parser.OBJECT_ID_VALUE 4"))

                    result {
                        let! _,components,_ =
                            tree.Children |>
                            foldResult (fun (rootComponent, curComponents, parentKeyword) curTree ->
                                result {
                                    let! compent, regKeyword = handleObjectIdComponent rootComponent parentKeyword curTree
                                    return (false, curComponents@[compent], regKeyword)
                                }) (true, [],None)
                        return (ObjOrRelObjIdValue components)
                    }
                | asn1Parser.VALUE_LIST             ->
                    result {
                        let! childVals = getTreeChildren(tree)|> List.traverseResultM (fun x -> CreateValue integerSizeInBytes  astRoot (x) )
                        return (SeqOfValue(childVals))
                    }
                | asn1Parser.NAMED_VALUE_LIST       ->
                    let HandleChild (childTree:ITree) =
                        result {
                            let chName = childTree.GetChild(0).TextL
                            let! value = CreateValue integerSizeInBytes  astRoot (childTree.GetChild(1))
                            return (chName, value)
                        }
                    result {
                        let! childVals = getTreeChildren(tree)|> List.traverseResultM HandleChild
                        return SeqValue(childVals)
                    }
                | asn1Parser.CHOICE_VALUE           ->
                    result {
                        let chName = tree.GetChild(0).TextL
                        let! value = CreateValue integerSizeInBytes  astRoot (tree.GetChild(1))
                        return (ChValue(chName, value))
                    }
                | asn1Parser.OctectStringLiteral    ->
                    let strVal = GetActualString(tree.Text)
                    let chars = strVal.ToCharArray()
                    let bytes = getAsTupples chars '0' |> List.map (fun (x1,x2)-> tree.GetValueL (System.Byte.Parse(x1.ToString()+x2.ToString(), System.Globalization.NumberStyles.AllowHexSpecifier)))
                    Ok (OctetStringValue(bytes))
                | asn1Parser.EMPTY_LIST             ->  Ok EmptyList
                | _ -> Error (Bug_Error("Bug in CreateValue " + (sprintf "%d" tree.Type)))


        return { Asn1Value.Kind = asn1ValueKind; Location = tree.Location; moduleName = mdName}
    }


let CheckValueRange (v1: Asn1Value) (v2: Asn1Value) (integerSizeInBytes: BigInteger): Unit =
    match (v1.Kind, v2.Kind) with
    | IntegerValue i1, IntegerValue i2 -> 
        let min = i1.Value
        let max = i2.Value
        
        let maxSigned = BigInteger Int64.MaxValue
        let minSigned = BigInteger Int64.MinValue
        let zero = BigInteger 0
        
        match (min < zero && max > maxSigned) with
        | true -> raise(SemanticError(v2.Location, (sprintf "Both values must fit in an %A-byte signed int. 
            Supported values are within range %A to %A" integerSizeInBytes minSigned maxSigned)))
        | _ -> ()
    | _ -> ()

let rec CreateConstraint (integerSizeInBytes: BigInteger)  (astRoot:list<ITree>) (fileTokens:array<IToken>) (tree:ITree) : Result<Asn1Constraint option, Asn1ParseError>=
    result {
        let asn1Str =
            fileTokens.[tree.TokenStartIndex .. tree.TokenStopIndex] |> Seq.map(fun s -> s.Text) |> Seq.StrJoin ""
        //printfn "%s" asn1Str
        match tree.Type with
        |asn1Parser.UnionMark           ->
            let! c1 = CreateConstraint  integerSizeInBytes  astRoot fileTokens (tree.GetChild(0))
            let! c2 = CreateConstraint  integerSizeInBytes  astRoot fileTokens (tree.GetChild(1))
            match c1, c2 with
            |Some(k1),Some(k2)  -> return (Some(UnionConstraint(asn1Str, k1 , k2, false )))
            |Some(k1),None      -> return None
            |None, Some(_)      -> return None
            |None, None         -> return None
        |asn1Parser.IntersectionMark    ->
            let! c1 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(0))
            let! c2 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(1))
            match c1, c2 with
            |Some(k1),Some(k2)  -> return (Some(IntersectionConstraint(asn1Str, k1 , k2 )))
            |Some(k1),None      -> return (Some k1)
            |None, Some(k2)     -> return (Some k2)
            |None, None         -> return None
        |asn1Parser.SIZE_EXPR           ->
            let! c1 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(0))
            match c1 with
            |Some(k1)   -> return (Some(SizeConstraint (asn1Str, k1)))
            |None       -> return None
        |asn1Parser.SUBTYPE_EXPR        ->
            let! (a,b) = CreateRefTypeContent(tree.GetChild(0))
            return (Some(TypeInclusionConstraint(asn1Str,a,b) ))
        |asn1Parser.PERMITTED_ALPHABET_EXPR ->
            let! c1 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(0))
            match c1 with
            |Some(k1)   -> return (Some(AlphabetConstraint (asn1Str, k1)))
            |None       -> return None
        |asn1Parser.ALL_EXCEPT          ->
            let! c1 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(0))
            match c1 with
            |Some(k1)   -> return (Some(AllExceptConstraint (asn1Str, k1)))
            |None       ->
                let! e = Error (Semantic_Error(tree.Location, "Invalid constraints definition"))
                return e
        |asn1Parser.EXCEPT              ->
            let! c1 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(0))
            let! c2 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(1))
            match c1, c2 with
            |Some(k1),Some(k2)  -> return (Some(ExceptConstraint(asn1Str, k1 , k2 )))
            |Some(k1),None      ->
                let! e = Error(Semantic_Error(tree.Location, "Invalid constraints definition"))
                return e
            |None, Some(k2)     -> return (Some(AllExceptConstraint (asn1Str, k2)))
            |None, None         ->
                let! e = Error(Semantic_Error(tree.Location, "Invalid constraints definition"))
                return e
        |asn1Parser.EXT_MARK            ->
            let! c1 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(0))
            if tree.ChildCount = 1 then
                match c1 with
                | Some k1 -> return (Some( RootConstraint(asn1Str, k1)))
                | None    -> return None
            else
                let! c2 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(1))
                match c1, c2 with
                |Some(k1),Some(k2)  -> return (Some(RootConstraint2(asn1Str, k1 , k2 )))
                |Some(k1),None      -> return (Some( RootConstraint(asn1Str, k1)))
                |None, Some(k2)     ->
                    let! e = Error(Semantic_Error(tree.Location, "Invalid constraints definition"))
                    return e
                |None, None         ->
                    let! e = Error(Semantic_Error(tree.Location, "Invalid constraints definition"))
                    return e
        |asn1Parser.WITH_COMPONENT_CONSTR   ->
            let! c1 = CreateConstraint integerSizeInBytes  astRoot fileTokens (tree.GetChild(0))
            match c1 with
            | Some k1   -> return (Some(WithComponentConstraint(asn1Str, k1, tree.Location)))
            | None      -> return None

        |asn1Parser.WITH_COMPONENTS_CONSTR ->
                let CreateNamedConstraint(tree:ITree) =
                    result {
                        let mark = getOptionalChildByType(tree, asn1Parser.EXT_MARK)
                        let nm = tree.GetChild(0).TextL
                        let! cstrt =
                            match getOptionalChildByType(tree, asn1Parser.INNER_CONSTRAINT)  with
                            | None -> Ok None
                            | Some(con) -> CreateConstraint   integerSizeInBytes  astRoot fileTokens (con.GetChild(0))

                        let! mark =
                            match getOptionalChildByType(tree, asn1Parser.EXT_MARK) with
                            | None -> Ok NoMark
                            | Some(markNode) ->
                                match markNode.GetChild(0).Type with
                                | asn1Parser.PRESENT    -> Ok MarkPresent
                                | asn1Parser.ABSENT     -> Ok MarkAbsent
                                | asn1Parser.OPTIONAL   -> Ok MarkOptional
                                | _ -> Error (Bug_Error("Bug in CreateConstraint.CreateNamedConstraint"))
                        return
                            {
                                NamedConstraint.Name=nm
                                Mark = mark
                                Constraint = cstrt
                            }
                    }
                let! cs = tree.GetChildrenByType(asn1Parser.NAME_CONSTRAINT_EXPR) |> List.traverseResultM CreateNamedConstraint
                return (Some(WithComponentsConstraint (asn1Str, cs)))
        |asn1Parser.VALUE_RANGE_EXPR    ->

            let maxValIncl = getOptionalChildByType(tree, asn1Parser.MAX_VAL_PRESENT)
            let minValIsIncluded= match getOptionalChildByType(tree, asn1Parser.MIN_VAL_INCLUDED) with
                                  | Some _   -> false
                                  | None     -> true
            let maxValIsIncluded = match getOptionalChildByType(tree, asn1Parser.MAX_VAL_INCLUDED) with
                                   | Some _   -> false
                                   | None     -> true

            match maxValIncl with
            | None         ->
                let! v = CreateValue integerSizeInBytes astRoot (tree.GetChild(0))
                return (Some(SingleValueConstraint(asn1Str, v)))
            | Some(v)      ->
                let a = tree.GetChild(0)
                let b = v.GetChild(0)
                match a.Type, b.Type with
                | asn1Parser.MIN, asn1Parser.MAX    -> return None //RangeConstraint_MIN_MAX
                | asn1Parser.MIN, _                 ->
                    let! v = CreateValue  integerSizeInBytes astRoot b
                    return (Some(RangeConstraint_MIN_val(asn1Str, v,  maxValIsIncluded)))
                | _, asn1Parser.MAX                 ->
                    let! v = CreateValue  integerSizeInBytes  astRoot a
                    return (Some(RangeConstraint_val_MAX(asn1Str, v, minValIsIncluded )))
                | _, _  ->
                    let! v1 = CreateValue integerSizeInBytes astRoot a
                    let! v2 = CreateValue integerSizeInBytes astRoot b
                    CheckValueRange v1 v2 integerSizeInBytes
                    return (Some(RangeConstraint(asn1Str, v1 , v2, minValIsIncluded, maxValIsIncluded)))
                    
        | _ ->
            return! Error (Bug_Error("Bug in CreateConstraint"))

    }


let rec CreateType integerSizeInBytes (tasParameters : TemplateParameter list) (acnTypeEncodingSpec  : AcnTypeEncodingSpec option) (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Result<Asn1Type, Asn1ParseError>=

    let createReferenceType (typeNode:ITree) refEnc : Result<Asn1TypeKind, Asn1ParseError> =
        result {
            let! (md,ts) = CreateRefTypeContent(typeNode)
            let! templateArgs =
                match typeNode.GetOptChild asn1Parser.ACTUAL_PARAM_LIST with
                | None              -> Ok []
                | Some(argList)     ->
                    argList.Children |>
                    List.traverseResultM(fun x ->
                        result {
                            match x.Type with
                            | asn1Parser.ACTUAL_TYPE_PARAM      ->
                                let argTypeTree = x.GetChild(0)
                                let children = getTreeChildren(argTypeTree)
                                let typeNodes = children |> List.filter(fun x -> (not (ConstraintNodes |> List.exists(fun y -> y=x.Type) ) ) && (x.Type <> asn1Parser.TYPE_TAG) )
                                let typeNode = List.head(typeNodes)
                                match typeNode.Type with
                                | asn1Parser.REFERENCED_TYPE    ->
                                    let! (md,ts) = CreateRefTypeContent(typeNode)
                                    match tasParameters |> Seq.tryFind (fun tp -> match tp with TypeParameter prmName | ValueParameter (_,prmName)  -> prmName.Value = ts.Value) with
                                    | Some _   -> return (TemplateParameter ts)
                                    | None                  ->
                                        let! t = CreateType integerSizeInBytes tasParameters None astRoot (x.GetChild(0)) fileTokens alreadyTakenComments
                                        return (ArgType t)
                                | _                             ->
                                    let! t = CreateType integerSizeInBytes tasParameters None astRoot (x.GetChild(0)) fileTokens alreadyTakenComments
                                    return (ArgType t)
                            | asn1Parser.ACTUAL_VALUE_PARAM     ->
                                let! v = CreateValue integerSizeInBytes astRoot (x.GetChild(0))
                                return (ArgValue v )
                            | _                         ->
                                let! e = Error (Bug_Error("Bug in CrateType(refType)"))
                                return e
                        })

            return ReferenceType(md, ts, refEnc, templateArgs)
        }

    result {
        let children = getTreeChildren(tree)
        let mdTree = tree.GetAncestor(asn1Parser.MODULE_DEF)
        let mdName = mdTree.GetChild(0).Text

        let typeNodes = children |> List.filter(fun x -> (not (ConstraintNodes |> List.exists(fun y -> y=x.Type) ) ) && (x.Type <> asn1Parser.TYPE_TAG) )
        let typeNode = List.head(typeNodes)
        let children_cons = if typeNode.Type=asn1Parser.SEQUENCE_OF_TYPE || typeNode.Type=asn1Parser.SET_OF_TYPE then getTreeChildren(typeNode)
                            else children
        let units =
            match children_cons |> List.filter(fun x -> x.Type = asn1Parser.UNITS) |> List.map(fun z -> z.Text) with
            | []    -> None
            | x1::_ ->
                let ret = x1.Replace("--{","").Replace("}--","")
                Some ret
        let constraintNodes = children_cons |> List.filter(fun x -> ConstraintNodes |> List.exists(fun y -> y=x.Type) )

        let! asn1Kind =
            match typeNode.Type with
            | asn1Parser.INTEGER_TYPE       -> Ok Integer
            | asn1Parser.REAL               -> Ok Real
            | asn1Parser.BOOLEAN            -> Ok Boolean
            | asn1Parser.CHOICE_TYPE        ->
                result {
                    let! ch = CreateChoiceChild  integerSizeInBytes tasParameters acnTypeEncodingSpec astRoot typeNode fileTokens alreadyTakenComments
                    return (Choice ch )
                }
            | asn1Parser.SET_TYPE
            | asn1Parser.SEQUENCE_TYPE      ->
                result {
                    let! ch = CreateSequenceChild  integerSizeInBytes tasParameters acnTypeEncodingSpec astRoot typeNode fileTokens alreadyTakenComments
                    return (Sequence ch)
                }
            | asn1Parser.ENUMERATED_TYPE    ->
                result {
                    let! items = CreateNamedItems integerSizeInBytes  astRoot  typeNode fileTokens alreadyTakenComments
                    return (Enumerated items)
                }
            | asn1Parser.BIT_STRING_TYPE    ->
                result {
                    let! items = CreateNamedBitList integerSizeInBytes  astRoot  typeNode fileTokens alreadyTakenComments
                    return (BitString items)
                }
            | asn1Parser.OCTET_STRING       -> Ok OctetString
            | asn1Parser.IA5String          -> Ok IA5String
            | asn1Parser.NumericString      -> Ok NumericString
            | asn1Parser.OBJECT_TYPE        -> Ok ObjectIdentifier
            | asn1Parser.RELATIVE_OID       -> Ok RelativeObjectIdentifier
            | asn1Parser.TIME_TYPE          ->
                result {
                    let! timeClass = CreateTimeClass astRoot  typeNode fileTokens alreadyTakenComments
                    return TimeType timeClass
                }
            | asn1Parser.VisibleString      ->
                Error (Semantic_Error (tree.Location, "VisibleString is not supported - please use IA5String"))
            | asn1Parser.PrintableString      ->
                Error (Semantic_Error (tree.Location, "PrintableString is not supported - please use IA5String"))
            | asn1Parser.NULL               -> Ok NullType
            | asn1Parser.REFERENCED_TYPE
            | asn1Parser.PREFERENCED_TYPE   -> createReferenceType typeNode None
            | asn1Parser.OCT_STR_CONTAINING -> createReferenceType (typeNode.GetChild(0)) (Some ContainedInOctString)
            | asn1Parser.BIT_STR_CONTAINING -> createReferenceType (typeNode.GetChild(0)) (Some ContainedInBitString)
            | asn1Parser.SET_OF_TYPE
            | asn1Parser.SEQUENCE_OF_TYPE   ->
                result {
                    let childAcnEncodingSpec =
                        match acnTypeEncodingSpec with
                        | None      -> None
                        | Some sqe  ->
                            match sqe.children with
                            | []    -> None
                            | z::_  -> Some z.childEncodingSpec
                    let! child = CreateType integerSizeInBytes tasParameters childAcnEncodingSpec astRoot (getChildByType (typeNode, asn1Parser.TYPE_DEF)) fileTokens alreadyTakenComments
                    return (SequenceOf child )
                }
            | asn1Parser.UTF8String         -> Error (Semantic_Error (tree.Location, "UTF8String is not supported, use IA5String"))
            | asn1Parser.TeletexString      -> Error (Semantic_Error (tree.Location, "TeletexString is not supported, use IA5String"))
            | asn1Parser.VideotexString     -> Error (Semantic_Error (tree.Location, "VideotexString is not supported"))
            | asn1Parser.GraphicString      -> Error (Semantic_Error (tree.Location, "GraphicString is not supported"))
            | asn1Parser.GeneralString      -> Error (Semantic_Error (tree.Location, "GeneralString is not supported"))
            | asn1Parser.BMPString          -> Error (Semantic_Error (tree.Location, "BMPString is not supported"))
            | asn1Parser.UniversalString    -> Error (Semantic_Error (tree.Location, "UniversalString is not supported"))
            | asn1Parser.UTCTime            -> Error (Semantic_Error (tree.Location, "UTCTime type is not supported (contact us for DATE-TIME support)"))
            | asn1Parser.GeneralizedTime    -> Error (Semantic_Error (tree.Location, "GeneralizedTime type is not supported"))
            | _                             -> Error (Bug_Error("Bug in CreateType"))

        let! constraints =
            result {
                let! userConstraints0 =
                    constraintNodes |> List.traverseResultM(fun x-> CreateConstraint integerSizeInBytes astRoot fileTokens x )
                let userConstraints = userConstraints0 |> List.choose(fun x -> x)
                match asn1Kind with
                | Enumerated(itms)  ->
                    match userConstraints with
                    | _::_     -> return userConstraints
                    | []    ->
                        let! dc = createDefaultConstraintsForEnumeratedTypes tree itms
                        return [dc]
                | _                 -> return userConstraints
            }


        let ret =
            {
                Asn1Type.Kind = asn1Kind
                Constraints= constraints
                Location = tree.Location
                parameterizedTypeInstance = false
                acnInfo = acnTypeEncodingSpec
                unitsOfMeasure = units
                moduleName = mdName
            }
        return ret
    }


and CreateChoiceChild integerSizeInBytes (tasParameters : TemplateParameter list) (chAcnTypeEncodingSpec  : AcnTypeEncodingSpec option) (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Result<ChildInfo list, Asn1ParseError>=
    getChildrenByType(tree, asn1Parser.CHOICE_ITEM) |>
    List.traverseResultM(fun x ->
        result {
            match getTreeChildren(x) with
            | first::sec::tail  ->
                let childAcnEncodingSpec =
                    match chAcnTypeEncodingSpec with
                    | None      -> None
                    | Some sqe  -> sqe.children |> Seq.tryFind(fun z -> z.name.Value = first.Text) |> Option.map(fun z -> z.childEncodingSpec)
                let! tt = CreateType integerSizeInBytes tasParameters childAcnEncodingSpec astRoot sec fileTokens alreadyTakenComments ;
                return
                    {
                        ChildInfo.Name = first.TextL;
                        Type = tt
                        Optionality=None;
                        AcnInsertedField=false
                        Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[x.TokenStopIndex].Line, x.TokenStartIndex - 1, x.TokenStopIndex + 2)
                    }
            | _ -> return! Error (Bug_Error("Bug in CreateChoiceChild"))
        }
    )

and CreateSequenceChild integerSizeInBytes  (tasParameters : TemplateParameter list) (seqAcnTypeEncodingSpec  : AcnTypeEncodingSpec option) (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Result<list<SequenceChild>, Asn1ParseError> =
    let CreateChild(x:ITree) =
        result {
            match x.Type with
            | asn1Parser.SEQUENCE_ITEM ->
                let lid = getChildByType(x, asn1Parser.LID)
                let typeDef = getChildByType(x, asn1Parser.TYPE_DEF)
                let optionalVal = getOptionalChildByType(x, asn1Parser.OPTIONAL)
                let defVal = getOptionalChildByType(x, asn1Parser.DEFAULT_VALUE)
                let! chType =
                    let childAcnEncodingSpec =
                        match seqAcnTypeEncodingSpec with
                        | None      -> None
                        | Some sqe  -> sqe.children |> Seq.tryFind(fun z -> z.name.Value = lid.Text) |> Option.map(fun z -> z.childEncodingSpec)
                    CreateType integerSizeInBytes  tasParameters childAcnEncodingSpec astRoot typeDef fileTokens alreadyTakenComments
                let! optVal =
                        match (optionalVal,defVal) with
                        | (None, Some(v))   ->
                            result {
                                let! dv = CreateValue integerSizeInBytes  astRoot (v.GetChild(0))
                                return Some(Default(dv ))
                            }
                        | (Some(_), None)   -> Ok (Some Optional)
                        | (None, None)      -> Ok None
                        | _                 -> Error (Bug_Error("Bug in CreateSequenceChild"))


                let comments =
                    match fileTokens.Length > 0 with
                    | true -> Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[x.TokenStopIndex].Line, x.TokenStartIndex - 1, x.TokenStopIndex + 2)
                    | false -> [||]
                let chInfo =
                    {
                        ChildInfo.Name = lid.TextL;
                        Type = chType;
                        Optionality = optVal
                        AcnInsertedField=false
                        Comments = comments
                    }
                return ChildInfo chInfo
            | asn1Parser.COMPONENTS_OF ->
                let! (md,ts) = CreateRefTypeContent(x.GetChild(0))
                return ComponentsOf(md,ts)
            | asn1Parser.SEQUENCE_EXT_BODY
            | asn1Parser.SEQUENCE_EXT_GROUP
            | asn1Parser.CHOICE_EXT_BODY ->
                return! Error(Semantic_Error(x.Location, "Unsupported ASN.1 feature (extensions)\n\nASN1SCC targets the S/W of space vehicles (it has been built and it is being\n\
                    maintained under European Space Agency's supervision). This means that\n\
                    we target ASN.1 grammars where the maximum message representation can be\n\
                    statically computed (and reserved) at compile-time.\n\n\
                    Think about it: what would you do when the embedded platform in your satellite\n\
                    runs out of memory? Blue screen? :-)\n\n\
                    Most telecom protocols (i.e. telecom-related ASN.1 grammars) are unfortunately\n\
                    not in that category (of grammars we support) - for example, the '...'\n\
                    construct allows the data of that message to potentially expand (in e.g.\n\
                    future versions of the protocols) with additional information. That however\n\
                    means that we can't statically compute the maximum size of these messages,\n\
                    which is, in effect, infinite.\n"))
            | _ -> return! Error (Bug_Error("Unexpected input in CreateSequenceChild"))
        }
    let asn1Children =
        match getOptionChildByType(tree, asn1Parser.SEQUENCE_BODY) with
        | Some(sequenceBody)    -> getTreeChildren(sequenceBody) |> List.traverseResultM(fun x -> CreateChild(x))
        | None                  -> Ok []
    match seqAcnTypeEncodingSpec with
    | None  -> asn1Children
    | Some es   ->
        match es.children with
        | []  -> asn1Children
        | acnChildren   ->
            //let invalidAcnChildren = acnChildren |> List.filter(fun acnChild -> not (asn1Children |> List.choose(fun z -> match z with ChildInfo z -> Some z | ComponentsOf _ -> None  ) |> List.exists (fun asn1Child -> acnChild.name.Value = asn1Child.Name.Value)) )

            asn1Children


and CreateNamedItems integerSizeInBytes  (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Result<NamedItem list, Asn1ParseError>=
    let CreateItem(itemItree:ITree) =
        result {
            let itemChildren = getTreeChildren(itemItree)
            match itemChildren with
            | name::value::_    ->
                let! value = CreateValue integerSizeInBytes  astRoot (value)
                return {NamedItem.Name=name.TextL; _value=Some value; Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[itemItree.TokenStopIndex].Line, itemItree.TokenStartIndex - 1, itemItree.TokenStopIndex + 2)}
            | name::[]          ->
                return {NamedItem.Name=name.TextL; _value= None; Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[itemItree.TokenStopIndex].Line, itemItree.TokenStartIndex - 1, itemItree.TokenStopIndex + 2)}
            | _                 -> return! Error (Bug_Error("Bug in CreateNamedItems.CreateItem"))
        }
    let enumItes = getChildrenByType(tree, asn1Parser.NUMBER_LST_ITEM)
    enumItes |> List.traverseResultM CreateItem

and CreateNamedBitList integerSizeInBytes  (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Result<NamedBit0 list, Asn1ParseError>=
    let CreateNamedBit(itemItree:ITree) : Result<NamedBit0, Asn1ParseError> =
        result {
            let itemChildren = getTreeChildren(itemItree)
            match itemChildren with
            | name::vlue::_    ->
                let! value =
                    result {
                        match vlue.Type with
                        | asn1Parser.INT            ->
                            match (vlue.BigIntL integerSizeInBytes ).Value >= 0I with
                            | true  -> return (IDV_IntegerValue(vlue.BigIntL integerSizeInBytes ))
                            | false -> return! Error (Semantic_Error(vlue.Location, "Negative values are not permitted"))
                        | asn1Parser.DEFINED_VALUE  ->
                            match vlue.ChildCount with
                            | 2     -> return (IDV_DefinedValue(vlue.GetChild(0).TextL, vlue.GetChild(1).TextL))
                            | 1     -> return (IDV_DefinedValue(singleReference2DoubleReference vlue))
                            | _     -> return! Error (Bug_Error("Bug in CreateValue CreateNamedBit 1"))
                        | _         -> return! Error (Bug_Error("Bug in CreateValue CreateNamedBit 2"))
                    }
                return {NamedBit0.Name=name.TextL; _value=value; Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[itemItree.TokenStopIndex].Line, itemItree.TokenStartIndex - 1, itemItree.TokenStopIndex + 2)}
            | _                 ->
                return! Error (Bug_Error("Bug in CreateNamedBitList.CreateItem"))
        }
    let namedBits = getChildrenByType(tree, asn1Parser.NUMBER_LST_ITEM)
    namedBits |> List.traverseResultM CreateNamedBit

and CreateTimeClass (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Result<TimeTypeClass, Asn1ParseError>=
    let rec removeSpaceArountEqual (str:string) =
        let rs = [" =";"\t=";"\r\n=";"\r=";"\n=";"= ";"=\t";"=\r\n";"=\r";"=\n"]
        match rs |> Seq.tryFind(fun s -> str.Contains s) with
        | None   -> str
        | Some s -> removeSpaceArountEqual (str.Replace(s,"="))

    let strItem = getChildByType(tree, asn1Parser.StringLiteral)
    let text = "Basic=Time Time= HMSF4 Local-or-UTC=L"

    let text = strItem.Text.Substring(1, strItem.Text.Length-2)

    let text_no_space_around_eq =  removeSpaceArountEqual text
    let text_no_space_around_eq, Fraction =
        let keyWord = "Time=HMSF"
        let i = text_no_space_around_eq.IndexOf(keyWord)
        if i > 0 then
            let ss = text_no_space_around_eq.Substring(i+keyWord.Length)
            let fr = ss.ToCharArray() |> Seq.takeWhile(fun c -> Char.IsDigit c) |> Seq.toArray |> string |> Int32.Parse
            let retStr = text_no_space_around_eq.Replace(keyWord + fr.ToString(),"Time=HMS")
            retStr , fr
        else
            text_no_space_around_eq, 0
    let text_no_space = text_no_space_around_eq.Replace(" ", "")
    match TimeClassMap.TryFind text_no_space with
    | Some cl   ->
        match cl with
            |Asn1LocalTime                  _ -> Ok (Asn1LocalTime                  Fraction)
            |Asn1UtcTime                    _ -> Ok (Asn1UtcTime                    Fraction)
            |Asn1LocalTimeWithTimeZone      _ -> Ok (Asn1LocalTimeWithTimeZone      Fraction)
            |Asn1Date                         -> Ok (Asn1Date)
            |Asn1Date_LocalTime             _ -> Ok (Asn1Date_LocalTime             Fraction)
            |Asn1Date_UtcTime               _ -> Ok (Asn1Date_UtcTime               Fraction)
            |Asn1Date_LocalTimeWithTimeZone _ -> Ok (Asn1Date_LocalTimeWithTimeZone Fraction)

    | None      -> Error (Semantic_Error(tree.Location, (sprintf "Invalid SETTINGS definition '%s'" text)))


let CreateTemplateParameter integerSizeInBytes (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>)=
    match tree.Type with
    |asn1Parser.TYPE_PARAM  -> Ok (TypeParameter(tree.GetChild(0).TextL))
    |asn1Parser.VALUE_PARAM ->
        result {
            let! newType = CreateType integerSizeInBytes [] None astRoot (tree.GetChild(0)) fileTokens alreadyTakenComments;
            return (ValueParameter (newType, tree.GetChild(1).TextL))
        }
    | _ -> Error (Bug_Error("Bug in CreateConstraint"))

let CreateTypeAssignment integerSizeInBytes (astRoot:list<ITree>) (acnAst:AcnAst) (acnModule : AcnModule option) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Result<TypeAssignment, Asn1ParseError> =
    result {
        let! parameters =
                match tree.GetOptChild asn1Parser.PARAM_LIST with
                | Some(prmList) ->
                    prmList.Children |> List.traverseResultM(fun x -> CreateTemplateParameter integerSizeInBytes astRoot x fileTokens alreadyTakenComments)
                | None          -> Ok []
        let tasName = tree.GetChild(0).TextL
        let acnTypeAssignment =
            match acnModule with
            | None          -> None
            | Some acnMod   -> acnMod.typeAssignments |> Seq.tryFind(fun acnTas -> acnTas.name.Value = tasName.Value)
        let acnTypeEncodingSpec =
                match acnTypeAssignment with
                | None   -> None
                | Some a -> Some a.typeEncodingSpec

        let! tp = CreateType integerSizeInBytes parameters acnTypeEncodingSpec astRoot (tree.GetChild(1)) fileTokens alreadyTakenComments;
        return
            {
                TypeAssignment.Name = tasName
                Type = tp
                Parameters = parameters
                Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[tree.TokenStopIndex].Line, tree.TokenStartIndex - 1, tree.TokenStopIndex + 1)
                acnInfo =
                    match acnTypeAssignment with
                    | None -> None
                    | Some a -> Some ({AcnTypeAssignmentExtraInfo.loc = a.name.Location; acnParameters = a.acnParameters; comments = a.comments})
        }
    }

let CreateValueAssignment integerSizeInBytes (astRoot:list<ITree>) (tree:ITree) : Result<ValueAssignment,Asn1ParseError>=
    result {
        let alreadyTakenComments = System.Collections.Generic.List<IToken>()
        let name = tree.GetChild(0).TextL;
        let! typ = CreateType integerSizeInBytes [] None astRoot (tree.GetChild(1)) [||] alreadyTakenComments
        let! vl = CreateValue integerSizeInBytes astRoot (tree.GetChild(2))
        return
            {
                ValueAssignment.Name = name
                Type = typ
                Value = vl
                Scope = ParameterizedAsn1Ast.GlobalScope
                c_name = ToC2 name.Value
                scala_name = ToC2 name.Value
                ada_name = ToC2 name.Value
            }
    }


let CreateAsn1Module integerSizeInBytes (astRoot:list<ITree>) (acnAst:AcnAst) (implicitlyImportedTypes: (string*ImportedModule list) list) (tree:ITree)   (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Result<Asn1Module, Asn1ParseError> =
    let createImport  (tree:ITree) =
        {   ImportedModule.Name = tree.GetChild(0).TextL;
            Types = getChildrenByType(tree, asn1Parser.UID) |> List.tail |> List.map (fun x -> x.TextL)
            Values = getChildrenByType(tree, asn1Parser.LID) |> List.map (fun x -> x.TextL)
         }
    let HandleExports () =
        match getOptionChildByType(tree, asn1Parser.EXPORTS_ALL) with
        | Some(_)   ->        Exports.All
        | None      ->
            match getOptionChildByType(tree, asn1Parser.EXPORTS) with
            | Some(exps)    -> Exports.OnlySome (exps.Children |> List.map (fun x -> x.Text))
            | None          -> Exports.All
    let handleIntegerValues (tree:ITree) =
        let mdName = tree.GetChild(0).TextL
        tree.AllChildren |>
        List.filter(fun x -> x.Type = asn1Parser.INTEGER_TYPE && not (x.Children.IsEmpty) && x.Parent.Parent.Type = asn1Parser.TYPE_ASSIG) |>
        List.collect(fun x ->
            let tas = x.Parent.Parent.GetChild(0).TextL
            let Type = { Asn1Type.Kind =  ReferenceType(mdName, tas, None, []); Constraints= []; Location = tas.Location; parameterizedTypeInstance = false; acnInfo = None;unitsOfMeasure = None; moduleName=mdName.Value }

            let scope = TypeScope(mdName, tas)

            let namedItems =
                x.Children |>
                List.filter(fun ni -> ni.Type = asn1Parser.NUMBER_LST_ITEM) |>
                List.map (fun ni ->
                    result {
                        let! vl = CreateValue integerSizeInBytes astRoot (ni.GetChild(1))
                        let vasName = ni.GetChild(0).TextL
                        let c_name = ToC2 (tas.Value + "_" + ni.GetChild(0).Text)
                        return
                            {
                                ValueAssignment.Name = vasName
                                Type = Type
                                Value = vl
                                Scope = scope
                                c_name = c_name
                                scala_name = c_name
                                ada_name = c_name
                            }})
            namedItems) |> List.traverseResultM id
    result {
        match tree.Type with
        | asn1Parser.MODULE_DEF ->
            match getOptionChildByType(tree, asn1Parser.EXTENSIBILITY) with
            | Some(_) -> return! Error (Semantic_Error(tree.Location, "Unsupported ASN.1 feature: EXTENSIBILIY IMPLED. Extensibility is incompatible with embedded systems"))
            | None ->
                let modName = getChildByType(tree, asn1Parser.UID).TextL
                let acnModule =
                    acnAst.files |>
                    List.collect(fun f -> f.modules) |>
                    Seq.tryFind(fun am -> am.name.Value = modName.Value)
                let! typeAssignments=
                    getChildrenByType(tree, asn1Parser.TYPE_ASSIG) |> List.traverseResultM(fun x -> CreateTypeAssignment integerSizeInBytes astRoot acnAst acnModule x fileTokens alreadyTakenComments)
                let! globalValueAssignments =
                    getChildrenByType(tree, asn1Parser.VAL_ASSIG) |> List.traverseResultM(fun x -> CreateValueAssignment integerSizeInBytes astRoot x)
                let! typeScopedValueAssignments = handleIntegerValues tree

                let ret =
                      {
                            Name=  modName
                            TypeAssignments= typeAssignments
                            ValueAssignments = globalValueAssignments@typeScopedValueAssignments
                            Imports =
                                let explicitImports = getChildrenByType(tree, asn1Parser.IMPORTS_FROM_MODULE) |> List.map createImport
                                let implicitlyImports = implicitlyImportedTypes |> List.filter(fun (m,_) -> m = modName.Value) |> List.collect snd
                                let mergedImports =
                                    implicitlyImports |>
                                    List.fold (fun (curImps:ImportedModule list)  iimp ->
                                        match curImps |> List.tryFind (fun z -> z.Name.Value = iimp.Name.Value) with
                                        | Some eimp        ->
                                            let rest = curImps |> List.filter (fun z -> z.Name.Value <> eimp.Name.Value)
                                            let mergedImport = {eimp with Types = (eimp.Types @ iimp.Types) |> List.distinctBy(fun z -> z.Value) }
                                            mergedImport::rest
                                        | None          -> iimp::curImps ) explicitImports
                                mergedImports
                            Exports = HandleExports()
                            Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[tree.TokenStopIndex].Line, tree.TokenStartIndex - 1, tree.TokenStopIndex + 2)
                            position =
                                let modEnd = getChildByType(tree, asn1Parser.END)
                                modName.Location, modEnd.Location
                      }
                match acnModule with
                | None -> return ret
                | Some acnMod ->
                    let orphanAcnTasses = acnMod.typeAssignments |> List.filter(fun acnTas -> not (typeAssignments |> Seq.exists(fun asn1Tas -> asn1Tas.Name.Value = acnTas.name.Value) ) )
                    match orphanAcnTasses with
                    | [] -> return ret
                    | _  ->
                        let orphanAcnTassesStr = orphanAcnTasses |> Seq.map(fun z -> z.name.Value) |> Seq.StrJoin (", ")
                        let errMsg = sprintf "The ACN module '%s' contains type assignments that do not exist in the corresponding ASN.1 module. For example, assignments '%s' are not defined in the ASN.1 module." acnMod.name.Value orphanAcnTassesStr
                        Console.Error.WriteLine(AntlrParse.formatSemanticWarning acnMod.name.Location errMsg)
                        //return! Error (Semantic_Error (acnMod.name.Location, errMess))
                        return ret
        | _ -> return! Error (Bug_Error("Bug in CreateAsn1Module"))
    }


let CreateAsn1File integerSizeInBytes (astRoot:list<ITree>) (acnAst:AcnAst) (implicitlyImportedTypes: (string*ImportedModule list) list) (tree:ITree, file, tokens)   =
    match tree.Type with
    | asn1Parser.ASN1_FILE ->
        result {
            let alreadyTakenComments = new System.Collections.Generic.List<IToken>();
            let! modules= getTreeChildren(tree) |> List.traverseResultM(fun t-> CreateAsn1Module integerSizeInBytes astRoot acnAst implicitlyImportedTypes t tokens alreadyTakenComments)
            return
                {
                        Asn1File.FileName=file;
                        Modules= modules
                        Tokens = tokens
                }
        }
    | _ -> Error (Bug_Error("Bug in CreateAsn1File"))


let getReftypeName (tree:ITree) =
    match getTreeChildren(tree) |> List.filter(fun x -> x.Type<> asn1Parser.ACTUAL_PARAM_LIST) with
    | refTypeName::[]           ->  Ok refTypeName.TextL
    | _::refTypeName::[]        ->  Ok refTypeName.TextL
    | _                         ->  Error (Bug_Error("Bug in CrateType(refType)"))


let rootCheckCyclicDeps (astRoot:list<ITree>) : Result<(string*ImportedModule list) list, Asn1ParseError>=
    result {
        let! allTasses =
            astRoot |>
            List.collect(fun f -> f.Children) |>
            List.collect(fun m -> m.GetChildrenByType(asn1Parser.TYPE_ASSIG) |> List.map(fun t -> m,t )) |>
            List.traverseResultM(fun (m,a) ->
                result {
                let (md,ts) = (m.GetChild(0).TextL, a.GetChild(0).TextL)
                let paramsSet =
                    a.AllChildren |> List.filter(fun x -> x.Type = asn1Parser.TYPE_PARAM) |> List.map(fun z -> (z.GetChild 0).Text) |> Set.ofList
                let! children =
                    a.AllChildren |>
                    List.filter(fun x -> x.Type = asn1Parser.REFERENCED_TYPE || x.Type = asn1Parser.PREFERENCED_TYPE ) |>
                    List.traverseResultM(fun x ->        //Exclude reference to type parameters
                        result {
                            let! refTypeName = getReftypeName x
                            match paramsSet.Contains(refTypeName.Value) with
                            | true -> return None
                            | false  ->
                                let! a = CreateRefTypeContent x
                                return (Some a)
                        })
                let children = children |> List.choose id
                return ((md,ts), children) })

        let tasses = allTasses |> List.map fst
        let refTypes = allTasses |> List.collect snd
        let orphanRefTypes =
            refTypes |>
            List.choose(fun (rfm, rft)  ->
                match tasses |> List.tryFind( fun (m,t) -> m.Value = rfm.Value && t.Value = rft.Value)  with
                | Some _    -> None
                | None      -> Some (rft.Location, sprintf "No type assignment with name %s exists (or exported) in module %s" rft.Value  rfm.Value) )
        let! ee =
            match orphanRefTypes with
            | []    -> Ok ()
            | (l,msg)::_-> Error(Semantic_Error(l,msg))

        let implicitlyImportedTypes =
            allTasses |>
            List.map (fun ((md,ts), rfList) ->  ((md,ts), rfList |> List.filter(fun (rm, rt) -> rm.Value <> md.Value) ) ) |>
            List.filter (fun (_, rfList) ->  not rfList.IsEmpty) |>
            List.map(fun ((md,ts), rfList) -> (md, rfList) ) |>
            List.groupBy (fun (md,list) -> md.Value)    |>
            List.map(fun (md, grp) ->
                let impModules = grp |> List.collect snd |> List.groupBy fst |> List.map(fun (rfm, rfTypes) -> {ImportedModule.Name = rfm; Types=rfTypes |> List.map snd |> List.distinctBy (fun a -> a.Value); Values=[]} )
                md, impModules)


        let independentNodes = allTasses |> List.filter(fun (_,lst) -> lst.IsEmpty)  |> List.map fst
        let dependentNodes = allTasses |> List.filter(fun (_,lst) -> not lst.IsEmpty)

        let comparer (m1:StringLoc, t1:StringLoc) (m2:StringLoc, t2:StringLoc) = m1.Value = m2.Value && t1.Value=t2.Value
        let ret = DoTopologicalSort2_noexc independentNodes dependentNodes comparer
        match ret with
        | Error lst ->
            match lst with
            | []        -> return! Error(Bug_Error(""))
            | ((m,t),_)::xs ->
                let printTas ((md:StringLoc,ts:StringLoc), deps: (StringLoc*StringLoc) list) =
                    sprintf "Type assignment '%s.%s' depends on : %s" md.Value ts.Value (deps |> List.map(fun (m,t) -> "'" + m.Value + "." + t.Value + "'") |> Seq.StrJoin ", ")
                let names = lst |> List.map printTas |> Seq.StrJoin " and "
                return! Error (Semantic_Error (t.Location, sprintf "Cyclic dependencies detected : %s" names))
        | Ok _     ->
            return implicitlyImportedTypes
    }


let CreateAstRoot_no_exc (list:CommonTypes.AntlrParserResult list) (acnAst:AcnAst) (args:CommandLineSettings) =
    result {
        let astRoot = list |> List.map (fun r -> r.rootItem)
        ITree.RegisterFiles(list |> Seq.map (fun x -> (x.rootItem, x.fileName)))
        let! implicitlyImportedTypes = rootCheckCyclicDeps astRoot
        let! files =
            list |>
            List.traverseResultM(fun x ->
                CreateAsn1File args.integerSizeInBytes astRoot acnAst implicitlyImportedTypes (x.rootItem,x.fileName, x.tokens))
        return {
            AstRoot.Files = files
            args = args
        }
    }

let CreateAstRoot (list:CommonTypes.AntlrParserResult list) (acnAst:AcnAst) (args:CommandLineSettings) =
    match   CreateAstRoot_no_exc list acnAst args  with
    | Ok ret -> ret
    | Error(Semantic_Error(l,m))    -> raise (SemanticError(l,m))
    | Error(Bug_Error m)            -> raise (BugErrorException m)
    | Error(User_Error m)           -> raise (UserException m)
