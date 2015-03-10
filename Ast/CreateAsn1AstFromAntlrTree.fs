﻿module CreateAsn1AstFromAntlrTree


open System
open System.Numerics
open FsUtils
open ParameterizedAsn1Ast
open Antlr.Asn1
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils

//#nowarn "40"


let rec getAsTupples<'T> (list:list<'T>) (empty:'T) =
    match list with
    | [] ->  []
    | head::[]  -> [(head,empty)]
    | a1::a2::tail -> (a1,a2)::getAsTupples tail empty


let ConstraintNodes = [ asn1Parser.EXT_MARK; asn1Parser.UnionMark; asn1Parser.ALL_EXCEPT; asn1Parser.IntersectionMark; asn1Parser.EXCEPT;
                        asn1Parser.VALUE_RANGE_EXPR; asn1Parser.SUBTYPE_EXPR; asn1Parser.SIZE_EXPR; asn1Parser.PERMITTED_ALPHABET_EXPR;
                        asn1Parser.WITH_COMPONENT_CONSTR; asn1Parser.WITH_COMPONENTS_CONSTR ]


let CreateRefTypeContent (tree:ITree) = 
    match getTreeChildren(tree) |> List.filter(fun x -> x.Type<> asn1Parser.ACTUAL_PARAM_LIST) with
    | refTypeName::[]           ->  
        let mdTree = tree.GetAncestor(asn1Parser.MODULE_DEF)
        let mdName = mdTree.GetChild(0).TextL
        let imports = mdTree.GetChildrenByType(asn1Parser.IMPORTS_FROM_MODULE)
        let importedFromModule = imports |> List.tryFind(fun imp-> imp.GetChildrenByType(asn1Parser.UID) |> Seq.exists(fun impTypeName -> impTypeName.Text = refTypeName.Text ))
        match importedFromModule with
        |Some(imp)  -> ( imp.GetChild(0).TextL,  refTypeName.TextL)
        |None       -> ( tree.GetAncestor(asn1Parser.MODULE_DEF).GetChild(0).TextL,  refTypeName.TextL)
        
    | modName::refTypeName::[]  ->  ( modName.TextL, refTypeName.TextL)
    | _                         ->  raise (BugErrorException("Bug in CrateType(refType)"))  
    
(*
let rec GetActualTypeKind (astRoot:list<ITree>) (kind:Asn1TypeKind)  =
    match kind with
    | ReferenceType(md, ts, _) -> 
        let mdl = astRoot |> List.collect(fun f -> f.Children) |> List.tryFind(fun m -> m.GetChild(0).Text = md.Value)
        let alreadyTakenComments = System.Collections.Generic.List<IToken>()
        match mdl with
        | None      ->  raise(SemanticError(md.Location, sprintf "Unknown module '%s'" md.Value))
        | Some(m)   ->
            let modName = m.GetChild(0).TextL
            let tas = m.GetChildrenByType(asn1Parser.TYPE_ASSIG) |> List.tryFind(fun x -> x.GetChild(0).Text = ts.Value)
            match tas with
            | None  -> raise(SemanticError(md.Location, sprintf "Unknown type assignment '%s'" ts.Value))
            | Some(ts) ->
                let t = CreateType astRoot (ts.GetChild 1) [||] alreadyTakenComments 
                match t.Kind with
                | ReferenceType(_)  -> GetActualTypeKind astRoot t.Kind 
                | _                 -> t.Kind, Some(modName)
    | _                     -> kind, None
    
*)
let rec CreateType (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Asn1Type= 
    let children = getTreeChildren(tree)
    let typeNodes = children |> List.filter(fun x -> (not (ConstraintNodes |> List.exists(fun y -> y=x.Type) ) ) && (x.Type <> asn1Parser.TYPE_TAG) )
    let typeNode = List.head(typeNodes)
    let children_cons = if typeNode.Type=asn1Parser.SEQUENCE_OF_TYPE || typeNode.Type=asn1Parser.SET_OF_TYPE then getTreeChildren(typeNode) 
                        else children
    let contraintNodes = children_cons |> List.filter(fun x -> ConstraintNodes |> List.exists(fun y -> y=x.Type) )
    let asn1Kind =
        match typeNode.Type with
        | asn1Parser.INTEGER_TYPE       -> Integer
        | asn1Parser.REAL               -> Real
        | asn1Parser.BOOLEAN            -> Boolean
        | asn1Parser.CHOICE_TYPE        -> Choice(CreateChoiceChild  astRoot typeNode fileTokens alreadyTakenComments )
        | asn1Parser.SEQUENCE_TYPE      -> Sequence(CreateSequenceChild  astRoot typeNode fileTokens alreadyTakenComments )
        | asn1Parser.SET_TYPE           -> Sequence(CreateSequenceChild  astRoot typeNode fileTokens alreadyTakenComments )
        | asn1Parser.ENUMERATED_TYPE    -> Enumerated(CreateNamedItems astRoot  typeNode fileTokens alreadyTakenComments)
        | asn1Parser.BIT_STRING_TYPE    -> BitString
        | asn1Parser.OCTECT_STING       -> OctetString
        | asn1Parser.IA5String          -> IA5String
        | asn1Parser.NumericString      -> NumericString
        | asn1Parser.OBJECT_TYPE        -> raise (SemanticError (tree.Location, "OBJECT IDs not supported"))
        | asn1Parser.VisibleString      -> 
            raise (SemanticError (tree.Location, "VisibleString is not supported - please use IA5String"))
        | asn1Parser.NULL               -> NullType
        | asn1Parser.REFERENCED_TYPE    
        | asn1Parser.PREFERENCED_TYPE   -> 
            let (md,ts) = CreateRefTypeContent(typeNode)
            let templateArgs =
                match typeNode.GetOptChild asn1Parser.ACTUAL_PARAM_LIST with
                | None              -> []
                | Some(argList)     ->
                    argList.Children |> List.map(fun x -> match x.Type with
                                                          | asn1Parser.ACTUAL_TYPE_PARAM      ->  ArgType (CreateType astRoot (x.GetChild(0)) fileTokens alreadyTakenComments)
                                                          | asn1Parser.ACTUAL_VALUE_PARAM     ->  ArgValue (CreateValue astRoot (x.GetChild(0)) )    
                                                          | _                         ->  raise (BugErrorException("Bug in CrateType(refType)"))  )

            ReferenceType(md, ts, templateArgs)
        | asn1Parser.SEQUENCE_OF_TYPE   -> SequenceOf(CreateType  astRoot (getChildByType (typeNode, asn1Parser.TYPE_DEF)) fileTokens alreadyTakenComments )
        | asn1Parser.SET_OF_TYPE        -> SequenceOf(CreateType  astRoot (getChildByType (typeNode, asn1Parser.TYPE_DEF)) fileTokens alreadyTakenComments )
        | asn1Parser.UTF8String         -> raise (SemanticError (tree.Location, "UTF8String is not supported"))
        | _                             -> raise (BugErrorException("Bug in CreateType"))
    {
        Asn1Type.Kind = asn1Kind
        Constraints= contraintNodes |> List.map(fun x-> CreateConstraint  astRoot x ) |> List.choose(fun x -> x)
        Location = tree.Location
    }

and CreateChoiceChild (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) = 
    getChildrenByType(tree, asn1Parser.CHOICE_ITEM) |> 
    List.map(fun x ->
        match getTreeChildren(x) with
        | first::sec::tail  -> 
            { 
                ChildInfo.Name = first.TextL; 
                Type = CreateType astRoot sec fileTokens alreadyTakenComments ; 
                Optionality=None; 
                AcnInsertedField=false
                Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[tree.TokenStopIndex].Line, tree.TokenStartIndex - 1, tree.TokenStopIndex + 2)
            }
        | _ -> raise (BugErrorException("Bug in CreateChoiceChild"))
    )

and CreateValue (astRoot:list<ITree>) (tree:ITree ) : Asn1Value=
    let GetActualString (str:string) = 
        let strVal = str.Substring(1)
        strVal.Remove(strVal.Length-2).Replace("\r", "").Replace("\n", "").Replace("\t", "").Replace(" ", "")
    {
        Asn1Value.Kind = 
            match tree.Type with
            | asn1Parser.INT                    -> IntegerValue(tree.BigIntL)
            | asn1Parser.FloatingPointLiteral   -> RealValue(tree.DoubleL)
            | asn1Parser.PLUS_INFINITY          -> RealValue(tree.GetValueL Double.PositiveInfinity)
            | asn1Parser.MINUS_INFINITY         -> RealValue(tree.GetValueL Double.NegativeInfinity)
            | asn1Parser.TRUE                   -> BooleanValue(tree.GetValueL true)
            | asn1Parser.FALSE                  -> BooleanValue(tree.GetValueL false)
            | asn1Parser.StringLiteral          -> 
                let text = tree.Text.Substring(1, tree.Text.Length-2)
                StringValue({ StringLoc.Value = text; Location = tree.Location})
            | asn1Parser.NULL                   -> NullValue
            | asn1Parser.BitStringLiteral       -> BitStringValue(tree.GetValueL(GetActualString(tree.Text)))
            | asn1Parser.VALUE_REFERENCE        -> 
                let strVal = tree.GetChild(0).TextL
                let modl = tree.GetAncestor(asn1Parser.MODULE_DEF)
                let modName = modl.GetChild(0).TextL
                let imports = modl.GetChildrenByType(asn1Parser.IMPORTS_FROM_MODULE)
                let importedFromModule = imports |> List.tryFind(fun imp-> imp.GetChildrenByType(asn1Parser.LID) |> Seq.exists(fun impTypeName -> impTypeName.Text = strVal.Value ))
                let valToReturn = 
                    match importedFromModule with
                    |Some(imp)  -> RefValue( imp.GetChild(0).TextL,  strVal)
                    |None       -> RefValue( modName,  strVal)
                valToReturn
//                let actType, newModName = GetActualTypeKind astRoot  typeKind
//                match actType, newModName with
//                | Enumerated(enmItems), Some(modName) -> 
//                    match enmItems |> Seq.tryFind(fun it -> it.Name.Value = strVal.Value) with
//                    | Some(it)      ->   RefValue(modName, strVal)
//                    | None          ->   valToReturn
//                | _                    -> valToReturn
            | asn1Parser.VALUE_LIST             -> 
                SeqOfValue(getTreeChildren(tree)|> List.map (fun x -> CreateValue astRoot (x) ))
//                match (GetActualTypeKind  astRoot typeKind |> fst) with
//                | SequenceOf(child)     ->
//                    SeqOfValue(getTreeChildren(tree)|> List.map (fun x -> CreateValue astRoot (x) child.Kind ))
//                | _                     -> raise(SemanticError(tree.Location, "Expecting SequenceOf value"))
            | asn1Parser.NAMED_VALUE_LIST       -> 
                let HandleChild (childTree:ITree) =
                    let chName = childTree.GetChild(0).TextL
                    let value = CreateValue astRoot (childTree.GetChild(1)) 
                    chName, value
                SeqValue(getTreeChildren(tree)|> List.map HandleChild)
            | asn1Parser.CHOICE_VALUE           -> 
                let chName = tree.GetChild(0).TextL
                let value = CreateValue astRoot (tree.GetChild(1)) 
                ChValue(chName, value)
            | asn1Parser.OctectStringLiteral    -> 
                let strVal = GetActualString(tree.Text)
                let chars = strVal.ToCharArray() |> Array.toList 
                let bytes = getAsTupples chars '0' |> List.map (fun (x1,x2)-> tree.GetValueL (System.Byte.Parse(x1.ToString()+x2.ToString(), System.Globalization.NumberStyles.AllowHexSpecifier))) 
                OctetStringValue(bytes)
            | asn1Parser.EMPTY_LIST             ->  EmptyList
            | _ -> raise (BugErrorException("Bug in CreateValue " + (sprintf "%d" tree.Type)))
        Location = tree.Location
    }

and CreateSequenceChild (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : list<SequenceChild>= 
    let CreateChild(x:ITree) = 
        match x.Type with
        | asn1Parser.SEQUENCE_ITEM ->
            let lid = getChildByType(x, asn1Parser.LID)
            let typeDef = getChildByType(x, asn1Parser.TYPE_DEF)
            let optionalVal = getOptionalChildByType(x, asn1Parser.OPTIONAL)
            let defVal = getOptionalChildByType(x, asn1Parser.DEFAULT_VALUE)
            let chType = CreateType astRoot typeDef fileTokens alreadyTakenComments
            let chInfo =
                { 
                    ChildInfo.Name = lid.TextL; 
                    Type = chType;
                    Optionality = match (optionalVal,defVal) with
                                    | (None, Some(v))   -> 
                                        Some(Default(CreateValue astRoot (v.GetChild(0)) ))
                                    | (Some(_), None)   -> Some Optional
                                    | (None, None)      -> None
                                    | _                 -> raise (BugErrorException("Bug in CreateSequenceChild")) 
                    AcnInsertedField=false
                    Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[tree.TokenStopIndex].Line, tree.TokenStartIndex - 1, tree.TokenStopIndex + 2)
                }
            ChildInfo chInfo
        | asn1Parser.COMPONENTS_OF -> 
            let (md,ts) = CreateRefTypeContent(x.GetChild(0))
            ComponentsOf(md,ts)
        | asn1Parser.SEQUENCE_EXT_BODY
        | asn1Parser.SEQUENCE_EXT_GROUP
        | asn1Parser.CHOICE_EXT_BODY 
          -> raise (SemanticError(x.Location, "Unsupported ASN.1 feature (extensions)\n\nASN1SCC targets the S/W of space vehicles (it has been built and it is being\n\
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
        | _ -> raise (BugErrorException("Unexpected input in CreateSequenceChild")) 
    match getOptionChildByType(tree, asn1Parser.SEQUENCE_BODY) with
    | Some(sequenceBody)    -> getTreeChildren(sequenceBody) |> List.map(fun x -> CreateChild(x))
    | None                  -> []
    

and CreateNamedItems (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>)=
    let CreateItem(itemItree:ITree) =
        let itemChildren = getTreeChildren(itemItree)
        match itemChildren with
        | name::value::_    -> 
            let value = Some(CreateValue astRoot (value) )
            {NamedItem.Name=name.TextL; _value=value; Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[tree.TokenStopIndex].Line, tree.TokenStartIndex - 1, tree.TokenStopIndex + 2)}
        | name::[]          -> {NamedItem.Name=name.TextL; _value= None; Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[tree.TokenStopIndex].Line, tree.TokenStartIndex - 1, tree.TokenStopIndex + 2)}
        | _                 -> raise (BugErrorException("Bug in CreateNamedItems.CreateItem")) 
    let enumItes = getChildrenByType(tree, asn1Parser.NUMBER_LST_ITEM)
    enumItes |> List.map CreateItem

and CreateConstraint (astRoot:list<ITree>) (tree:ITree) : Asn1Constraint option=
        match tree.Type with
        |asn1Parser.UnionMark           -> 
            let c1 = CreateConstraint  astRoot (tree.GetChild(0)) 
            let c2 = CreateConstraint  astRoot (tree.GetChild(1)) 
            match c1, c2 with
            |Some(k1),Some(k2)  ->Some(UnionConstraint(k1 , k2 ))
            |Some(k1),None      -> None
            |None, Some(_)      -> None
            |None, None         -> None
        |asn1Parser.IntersectionMark    -> 
            let c1 = CreateConstraint astRoot (tree.GetChild(0)) 
            let c2 = CreateConstraint astRoot (tree.GetChild(1)) 
            match c1, c2 with
            |Some(k1),Some(k2)  ->Some(IntersectionConstraint(k1 , k2 ))
            |Some(k1),None      -> Some k1
            |None, Some(k2)     -> Some k2
            |None, None         -> None
        |asn1Parser.SIZE_EXPR           -> 
            let c1 = CreateConstraint astRoot (tree.GetChild(0)) 
            match c1 with
            |Some(k1)   -> Some(SizeContraint k1)
            |None       -> None
        |asn1Parser.SUBTYPE_EXPR        -> 
            Some(TypeInclusionConstraint(CreateRefTypeContent(tree.GetChild(0))))
        |asn1Parser.PERMITTED_ALPHABET_EXPR -> 
            let c1 = CreateConstraint astRoot (tree.GetChild(0)) 
            match c1 with
            |Some(k1)   -> Some(AlphabetContraint k1)
            |None       -> None
        |asn1Parser.ALL_EXCEPT          -> 
            let c1 = CreateConstraint astRoot (tree.GetChild(0)) 
            match c1 with
            |Some(k1)   -> Some(AllExceptConstraint k1)
            |None       -> raise(SemanticError(tree.Location, "Invalid constraints definition"))
        |asn1Parser.EXCEPT              -> 
            let c1 = CreateConstraint astRoot (tree.GetChild(0)) 
            let c2 = CreateConstraint astRoot (tree.GetChild(1)) 
            match c1, c2 with
            |Some(k1),Some(k2)  ->Some(ExceptConstraint(k1 , k2 ))
            |Some(k1),None      -> raise(SemanticError(tree.Location, "Invalid constraints definition"))
            |None, Some(k2)     -> Some(AllExceptConstraint k2)
            |None, None         -> raise(SemanticError(tree.Location, "Invalid constraints definition"))
        |asn1Parser.EXT_MARK            ->
            let c1 = CreateConstraint astRoot (tree.GetChild(0)) 
            if tree.ChildCount = 1 then 
                match c1 with
                | Some k1 -> Some( RootConstraint(k1)) 
                | None    -> None
            else 
                let c2 = CreateConstraint astRoot (tree.GetChild(1)) 
                match c1, c2 with
                |Some(k1),Some(k2)  -> Some(RootConstraint2(k1 , k2 ))
                |Some(k1),None      -> Some( RootConstraint(k1)) 
                |None, Some(k2)     -> raise(SemanticError(tree.Location, "Invalid constraints definition"))
                |None, None         -> raise(SemanticError(tree.Location, "Invalid constraints definition"))
        |asn1Parser.WITH_COMPONENT_CONSTR   -> 
            let c1 = CreateConstraint astRoot (tree.GetChild(0)) 
            match c1 with
            | Some k1   -> Some(WithComponentConstraint(k1))
            | None      -> None

        |asn1Parser.WITH_COMPONENTS_CONSTR -> 
//            match (GetActualTypeKind  astRoot typeKind |> fst) with
//            | Sequence(children) | Choice(children) ->
                let CreateNamedConstraint(tree:ITree) =
                    let mark = getOptionalChildByType(tree, asn1Parser.EXT_MARK) 
                    let nm = tree.GetChild(0).TextL
                    {   
                        NamedConstraint.Name=nm
                        Contraint = match getOptionalChildByType(tree, asn1Parser.INNER_CONSTRAINT)  with
                                        | None -> None
                                        | Some(con) -> CreateConstraint  astRoot (con.GetChild(0)) 
                        Mark = match getOptionalChildByType(tree, asn1Parser.EXT_MARK) with
                                | None -> NoMark
                                | Some(markNode) -> match markNode.GetChild(0).Type with
                                                    | asn1Parser.PRESENT    -> MarkPresent
                                                    | asn1Parser.ABSENT     -> MarkAbsent
                                                    | asn1Parser.OPTIONAL   -> MarkOptional
                                                    | _ -> raise (BugErrorException("Bug in CreateConstraint.CreateNamedConstraint"))
                    }
                Some(WithComponentsConstraint(tree.GetChildrenByType(asn1Parser.NAME_CONSTRAINT_EXPR) |> List.map CreateNamedConstraint))
//            | _         -> raise(SemanticError(tree.Location, "Invalid constraint"))
        |asn1Parser.VALUE_RANGE_EXPR    ->
            let maxValIncl = getOptionalChildByType(tree, asn1Parser.MAX_VAL_PRESENT)
            let minValIsIncluded= match getOptionalChildByType(tree, asn1Parser.MIN_VAL_INCLUDED) with
                                  | Some _   -> false
                                  | None     -> true
            let maxValIsIncluded = match getOptionalChildByType(tree, asn1Parser.MAX_VAL_INCLUDED) with
                                   | Some _   -> false
                                   | None     -> true
            
            match maxValIncl with
            | None         -> Some(SingleValueContraint( CreateValue astRoot (tree.GetChild(0))  ))
            | Some(v)      -> 
                let a = tree.GetChild(0)
                let b = v.GetChild(0)
                match a.Type, b.Type with
                | asn1Parser.MIN, asn1Parser.MAX    -> None //RangeContraint_MIN_MAX
                | asn1Parser.MIN, _                 -> Some(RangeContraint_MIN_val(CreateValue  astRoot b,  maxValIsIncluded))
                | _, asn1Parser.MAX                 -> Some(RangeContraint_val_MAX(CreateValue  astRoot a, minValIsIncluded ))
                | _, _  ->  Some(RangeContraint(CreateValue  astRoot a , CreateValue  astRoot  b, minValIsIncluded, maxValIsIncluded  ))
        | _ -> raise (BugErrorException("Bug in CreateConstraint"))




let CreateTemplateParameter (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>)=
    match tree.Type with
    |asn1Parser.TYPE_PARAM  -> TypeParameter(tree.GetChild(0).TextL)
    |asn1Parser.VALUE_PARAM -> 
        let Type = CreateType astRoot (tree.GetChild(0)) fileTokens alreadyTakenComments; 
        ValueParameter (Type, tree.GetChild(1).TextL)
    | _ -> raise (BugErrorException("Bug in CreateConstraint"))
    

let CreateTypeAssigment (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) = 
    { 
        TypeAssignment.Name = tree.GetChild(0).TextL;
        Type = CreateType astRoot (tree.GetChild(1)) fileTokens alreadyTakenComments; 
        Parameters = 
            match tree.GetOptChild asn1Parser.PARAM_LIST with
            | None          -> []
            | Some(prmList) -> prmList.Children |> List.map(fun x -> CreateTemplateParameter astRoot x fileTokens alreadyTakenComments)
        Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[tree.TokenStopIndex].Line, tree.TokenStartIndex - 1, tree.TokenStopIndex + 1)
    }

let CreateValueAssigment (astRoot:list<ITree>) (tree:ITree) = 
    let alreadyTakenComments = System.Collections.Generic.List<IToken>()
    let typ = CreateType astRoot (tree.GetChild(1)) [||] alreadyTakenComments
    {
        ValueAssignment.Name = tree.GetChild(0).TextL;
        Type = typ
        Value = CreateValue astRoot (tree.GetChild(2)) 
    }

let CreateAsn1Module (astRoot:list<ITree>) (tree:ITree)   (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>)= 
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
    match tree.Type with
    | asn1Parser.MODULE_DEF ->  
          { 
                Name=  getChildByType(tree, asn1Parser.UID).TextL
                TypeAssignments= getChildrenByType(tree, asn1Parser.TYPE_ASSIG) |> List.map(fun x -> CreateTypeAssigment astRoot x fileTokens alreadyTakenComments)
                ValueAssignments = getChildrenByType(tree, asn1Parser.VAL_ASSIG) |> List.map(fun x -> CreateValueAssigment astRoot x)
                Imports = getChildrenByType(tree, asn1Parser.IMPORTS_FROM_MODULE) |> List.map createImport
                Exports = HandleExports()
          }
    | _ -> raise (BugErrorException("Bug in CreateAsn1Module"))



let CreateAsn1File (astRoot:list<ITree>) (tree:ITree, file, tokens)   = 
    match tree.Type with
    | asn1Parser.ASN1_FILE ->  
        let alreadyTakenComments = new System.Collections.Generic.List<IToken>();
        { 
                Asn1File.FileName=file;  
                Modules= getTreeChildren(tree) |> List.map(fun t-> CreateAsn1Module astRoot t tokens alreadyTakenComments) 
                Tokens = tokens
        }
    | _ -> raise (BugErrorException("Bug in CreateAsn1File"))


let rootCheckCyclicDeps (astRoot:list<ITree>) =
    let allTasses = astRoot |> 
                    List.collect(fun f -> f.Children) |> 
                    List.collect(fun m -> m.GetChildrenByType(asn1Parser.TYPE_ASSIG) |> List.map(fun t -> m,t )) |> 
                    List.map(fun (m,a) -> (m.GetChild(0).TextL, a.GetChild(0).TextL), a.AllChildren |> 
                                                                                    List.filter(fun x -> x.Type = asn1Parser.REFERENCED_TYPE || x.Type = asn1Parser.PREFERENCED_TYPE) |>
                                                                                    List.map(fun r -> CreateRefTypeContent r ))

    let independentNodes = allTasses |> List.filter(fun (_,lst) -> lst.IsEmpty)  |> List.map fst
    let dependentNodes = allTasses |> List.filter(fun (_,lst) -> not lst.IsEmpty)    
    let excToThrow (lst:list<(StringLoc*StringLoc)*list<StringLoc*StringLoc>>) =
        let depNodes = lst |> List.map(fun ((m,v),_) -> v)
        let loc = depNodes.Head.Location
        let str = depNodes |> List.map(fun x -> x.Value) |> Seq.StrJoin ", "
        SemanticError (loc, sprintf "Cyclic dependencies detected : %s " str)
    let comparer (m1:StringLoc, t1:StringLoc) (m2:StringLoc, t2:StringLoc) = m1.Value = m2.Value && t1.Value=t2.Value
    DoTopologicalSort2 independentNodes dependentNodes comparer excToThrow |> ignore

let CreateAstRoot (list:(ITree*string*array<IToken>) seq) (encodings:array<Asn1Encoding>) generateEqualFunctions typePrefix checkWithOss astXmlFileName icdUperHtmlFileName icdAcnHtmlFileName=  
    let astRoot = list |> Seq.toList |> List.map (fun (a,_,_) -> a)
    ITree.RegisterFiles(list |> Seq.map (fun (a,b,_) -> (a,b)))
    //rootCheckCyclicDeps astRoot 
    {
        AstRoot.Files = list |> Seq.toList  |> List.map(fun (t,f, ar) -> CreateAsn1File astRoot (t,f, ar))
        Encodings = encodings |> Array.toList
        GenerateEqualFunctions = generateEqualFunctions
        TypePrefix = typePrefix
        CheckWithOss = checkWithOss
        AstXmlAbsFileName = astXmlFileName
        IcdUperHtmlFileName = icdUperHtmlFileName
        IcdAcnHtmlFileName = icdAcnHtmlFileName
    }




