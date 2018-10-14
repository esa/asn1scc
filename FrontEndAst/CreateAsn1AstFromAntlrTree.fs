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

module CreateAsn1AstFromAntlrTree


open System
open System.Numerics
open FsUtils
open ParameterizedAsn1Ast
open Antlr.Asn1
open Antlr.Runtime.Tree
open Antlr.Runtime
open FsUtils
open CommonTypes

//#nowarn "40"

(*
let rec getAsTupples<'T> (list:list<'T>) (empty:'T) =
    match list with
    | [] ->  []
    | head::[]  -> [(head,empty)]
    | a1::a2::tail -> (a1,a2)::getAsTupples tail empty
*)

let rec getAsTupples<'T> (list:array<'T>) (empty:'T) =
    let mutable x = 0
    seq {
        while x < list.Length do
            let a = list.[x]
            let b = if x+1 < list.Length then list.[x+1] else empty
            yield (a,b)
            x <- x + 2
    } |> Seq.toList

let ConstraintNodes = [ asn1Parser.EXT_MARK; asn1Parser.UnionMark; asn1Parser.ALL_EXCEPT; asn1Parser.IntersectionMark; asn1Parser.EXCEPT;
                        asn1Parser.VALUE_RANGE_EXPR; asn1Parser.SUBTYPE_EXPR; asn1Parser.SIZE_EXPR; asn1Parser.PERMITTED_ALPHABET_EXPR;
                        asn1Parser.WITH_COMPONENT_CONSTR; asn1Parser.WITH_COMPONENTS_CONSTR ]

let getReftypeName (tree:ITree) = 
    match getTreeChildren(tree) |> List.filter(fun x -> x.Type<> asn1Parser.ACTUAL_PARAM_LIST) with
    | refTypeName::[]           ->  refTypeName.TextL
    | _::refTypeName::[]        ->  refTypeName.TextL
    | _                         ->  raise (BugErrorException("Bug in CrateType(refType)"))  

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
    | []        -> raise(BugErrorException "expecting non empty list")
    | x1::xs    ->
        let curLevelKeyword =
            curLevelKeywords |> List.tryFind(fun (LEAF(names,nVal,  _)) -> names |> Seq.exists ((=) x1) ) 
        match xs with
        | []    -> curLevelKeyword|> Option.map(fun (LEAF(_,nVal,  _)) -> nVal)
        | _     -> 
            match curLevelKeyword with
            | None  -> None
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
        let v = {Asn1Value.Kind = (RefValue ({StringLoc.Value = mdName.Value; Location=ni.Name.Location}, ni.Name)); Location = ni.Name.Location  }
        SingleValueContraint v
    match namedItems with
    | []    -> 
            raise (SemanticError (tree.Location, "Enumerated type has no enumerants"))
    | x::xs ->
        let initialCon = createSingleValueConstraintFromNamedItem x
        xs |> List.fold(fun accConst ni -> 
                            let curCon = createSingleValueConstraintFromNamedItem ni
                            UnionConstraint (accConst, curCon, true) ) initialCon
    

let rec CreateType (tasParameters : TemplateParameter list) (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : Asn1Type= 
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
        | asn1Parser.CHOICE_TYPE        -> Choice(CreateChoiceChild  tasParameters astRoot typeNode fileTokens alreadyTakenComments )
        | asn1Parser.SEQUENCE_TYPE      -> Sequence(CreateSequenceChild  tasParameters astRoot typeNode fileTokens alreadyTakenComments )
        | asn1Parser.SET_TYPE           -> Sequence(CreateSequenceChild  tasParameters astRoot typeNode fileTokens alreadyTakenComments )
        | asn1Parser.ENUMERATED_TYPE    -> Enumerated(CreateNamedItems astRoot  typeNode fileTokens alreadyTakenComments)
        | asn1Parser.BIT_STRING_TYPE    -> BitString
        | asn1Parser.OCTECT_STING       -> OctetString
        | asn1Parser.IA5String          -> IA5String
        | asn1Parser.NumericString      -> NumericString
        | asn1Parser.OBJECT_TYPE        -> ObjectIdentifier
        | asn1Parser.RELATIVE_OID       -> RelativeObjectIdentifier
        | asn1Parser.VisibleString      -> 
            raise (SemanticError (tree.Location, "VisibleString is not supported - please use IA5String"))
        | asn1Parser.PrintableString      -> 
            raise (SemanticError (tree.Location, "PrintableString is not supported - please use IA5String"))
        | asn1Parser.NULL               -> NullType
        | asn1Parser.REFERENCED_TYPE    
        | asn1Parser.PREFERENCED_TYPE   -> 
            let (md,ts) = CreateRefTypeContent(typeNode)
            let templateArgs =
                match typeNode.GetOptChild asn1Parser.ACTUAL_PARAM_LIST with
                | None              -> []
                | Some(argList)     ->
                    argList.Children |> 
                    List.map(fun x -> 
                        match x.Type with
                        | asn1Parser.ACTUAL_TYPE_PARAM      ->  
                            let argTypeTree = x.GetChild(0)
                            let children = getTreeChildren(argTypeTree)
                            let typeNodes = children |> List.filter(fun x -> (not (ConstraintNodes |> List.exists(fun y -> y=x.Type) ) ) && (x.Type <> asn1Parser.TYPE_TAG) )
                            let typeNode = List.head(typeNodes)
                            match typeNode.Type with
                            | asn1Parser.REFERENCED_TYPE    ->
                                let (md,ts) = CreateRefTypeContent(typeNode)
                                match tasParameters |> Seq.tryFind (fun tp -> match tp with TypeParameter prmName | ValueParameter (_,prmName)  -> prmName.Value = ts.Value) with
                                | Some _   -> TemplateParameter ts
                                | None                  -> ArgType (CreateType tasParameters astRoot (x.GetChild(0)) fileTokens alreadyTakenComments)
                            | _                             -> ArgType (CreateType tasParameters astRoot (x.GetChild(0)) fileTokens alreadyTakenComments)
                        | asn1Parser.ACTUAL_VALUE_PARAM     ->  ArgValue (CreateValue astRoot (x.GetChild(0)) )    
                        | _                         ->  raise (BugErrorException("Bug in CrateType(refType)"))  )

            ReferenceType(md, ts, templateArgs)
        | asn1Parser.SEQUENCE_OF_TYPE   -> SequenceOf(CreateType tasParameters astRoot (getChildByType (typeNode, asn1Parser.TYPE_DEF)) fileTokens alreadyTakenComments )
        | asn1Parser.SET_OF_TYPE        -> SequenceOf(CreateType tasParameters astRoot (getChildByType (typeNode, asn1Parser.TYPE_DEF)) fileTokens alreadyTakenComments )
        | asn1Parser.UTF8String         -> raise (SemanticError (tree.Location, "UTF8String is not supported, use IA5String"))
        | asn1Parser.TeletexString      -> raise (SemanticError (tree.Location, "TeletexString is not supported, use IA5String"))
        | asn1Parser.VideotexString     -> raise (SemanticError (tree.Location, "VideotexString is not supported"))
        | asn1Parser.GraphicString      -> raise (SemanticError (tree.Location, "GraphicString is not supported"))
        | asn1Parser.GeneralString      -> raise (SemanticError (tree.Location, "GeneralString is not supported"))
        | asn1Parser.BMPString          -> raise (SemanticError (tree.Location, "BMPString is not supported"))
        | asn1Parser.UniversalString    -> raise (SemanticError (tree.Location, "UniversalString is not supported"))
        | asn1Parser.UTCTime            -> raise (SemanticError (tree.Location, "UTCTime type is not supported (contact us for DATE-TIME support)"))
        | asn1Parser.GeneralizedTime    -> raise (SemanticError (tree.Location, "GeneralizedTime type is not supported"))
        | _                             -> raise (BugErrorException("Bug in CreateType"))
    {
        Asn1Type.Kind = asn1Kind
        Constraints= 
            let userConstraints = contraintNodes |> List.map(fun x-> CreateConstraint  astRoot x ) |> List.choose(fun x -> x)
            match asn1Kind with
            | Enumerated(itms)  -> 
                match userConstraints with
                | []    -> [createDefaultConstraintsForEnumeratedTypes tree itms]
                | _     -> userConstraints
            | _                 -> userConstraints
        Location = tree.Location
        parameterizedTypeInstance = false
    }

and CreateChoiceChild (tasParameters : TemplateParameter list) (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) = 
    getChildrenByType(tree, asn1Parser.CHOICE_ITEM) |> 
    List.map(fun x ->
        match getTreeChildren(x) with
        | first::sec::tail  -> 
            { 
                ChildInfo.Name = first.TextL; 
                Type = CreateType tasParameters astRoot sec fileTokens alreadyTakenComments ; 
                Optionality=None; 
                AcnInsertedField=false
                Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[x.TokenStopIndex].Line, x.TokenStartIndex - 1, x.TokenStopIndex + 2)
            }
        | _ -> raise (BugErrorException("Bug in CreateChoiceChild"))
    )

and CreateValue (astRoot:list<ITree>) (tree:ITree ) : Asn1Value=
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
            | asn1Parser.VALUE_REFERENCE        -> RefValue (singleReference2DoubleReference tree)
//                let actType, newModName = GetActualTypeKind astRoot  typeKind
//                match actType, newModName with
//                | Enumerated(enmItems), Some(modName) -> 
//                    match enmItems |> Seq.tryFind(fun it -> it.Name.Value = strVal.Value) with
//                    | Some(it)      ->   RefValue(modName, strVal)
//                    | None          ->   valToReturn
//                | _                    -> valToReturn
            | asn1Parser.OBJECT_ID_VALUE        ->
                let handleObjectIdComponent (rootComponent:bool) (parentKeyword: RegisterObjIDKeyword option) (tree:ITree ) =
                    match tree.Type with
                    | asn1Parser.OBJ_LST_ITEM2  -> 
                        let nVal = tree.GetChild(0).BigIntL.Value
                        match nVal >= 0I with
                        | true  -> ObjInteger (tree.GetChild(0).BigIntL) , None
                        | false -> raise (SemanticError(tree.GetChild(0).Location, "Negative values are not permitted in OJECT-IDENTIFIER"))

                    | asn1Parser.OBJ_LST_ITEM1  -> 
                        let name = tree.GetChild(0).TextL
                        match tree.ChildCount with
                        | 2  ->
                            let secChild = tree.GetChild(1)
                            match secChild.Type with
                            | asn1Parser.INT            -> 
                                match secChild.BigIntL.Value >= 0I with
                                | true  -> ObjNamedIntValue (name, secChild.BigIntL), None
                                | false -> raise (SemanticError(secChild.Location, "Negative values are not permitted in OJECT-IDENTIFIER"))
                            | asn1Parser.DEFINED_VALUE  ->
                                match secChild.ChildCount with
                                | 2     -> ObjNamedDefValue(name, (secChild.GetChild(0).TextL, secChild.GetChild(1).TextL)), None
                                | 1     -> ObjNamedDefValue(name, ((singleReference2DoubleReference secChild))), None
                                | _     -> raise (BugErrorException("Bug in CreateValue asn1Parser.OBJECT_ID_VALUE 1"))
                            | _         -> raise (BugErrorException("Bug in CreateValue asn1Parser.OBJECT_ID_VALUE 2"))
                        | 1 ->
                            let regKeyWord = 
                                match rootComponent with
                                | true  -> isRegisterObjIDKeyword2 parentKeyword name.Value
                                | false ->
                                    match parentKeyword with
                                    | Some _    -> isRegisterObjIDKeyword2 parentKeyword name.Value
                                    | None      -> None

                            match regKeyWord with
                            | Some (LEAF(_,nVal,_)) -> ObjRegisteredKeyword(name,BigInteger nVal), regKeyWord
                            | None                  -> ObjDefinedValue (singleReference2DoubleReference tree) , None

                        | _ -> raise (BugErrorException("Bug in CreateValue asn1Parser.OBJECT_ID_VALUE 3"))
                    | _ -> raise (BugErrorException("Bug in CreateValue asn1Parser.OBJECT_ID_VALUE 4"))


                let _,components,_ =                    
                    tree.Children |> 
                    List.fold (fun (rootComponent, curComponents, parentKeyword) curTree -> 
                        let compent, regKeyword = handleObjectIdComponent rootComponent parentKeyword curTree
                        (false, curComponents@[compent], regKeyword) ) (true, [],None) 
                ObjOrRelObjIdValue components
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
                let chars = strVal.ToCharArray() 
                let bytes = getAsTupples chars '0' |> List.map (fun (x1,x2)-> tree.GetValueL (System.Byte.Parse(x1.ToString()+x2.ToString(), System.Globalization.NumberStyles.AllowHexSpecifier))) 
                OctetStringValue(bytes)
            | asn1Parser.EMPTY_LIST             ->  EmptyList
            | _ -> raise (BugErrorException("Bug in CreateValue " + (sprintf "%d" tree.Type)))
        Location = tree.Location
    }

and CreateSequenceChild (tasParameters : TemplateParameter list) (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) : list<SequenceChild>= 
    let CreateChild(x:ITree) = 
        match x.Type with
        | asn1Parser.SEQUENCE_ITEM ->
            let lid = getChildByType(x, asn1Parser.LID)
            let typeDef = getChildByType(x, asn1Parser.TYPE_DEF)
            let optionalVal = getOptionalChildByType(x, asn1Parser.OPTIONAL)
            let defVal = getOptionalChildByType(x, asn1Parser.DEFAULT_VALUE)
            let chType = CreateType tasParameters astRoot typeDef fileTokens alreadyTakenComments
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
                    Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[x.TokenStopIndex].Line, x.TokenStartIndex - 1, x.TokenStopIndex + 2)
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
            {NamedItem.Name=name.TextL; _value=value; Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[itemItree.TokenStopIndex].Line, itemItree.TokenStartIndex - 1, itemItree.TokenStopIndex + 2)}
        | name::[]          -> {NamedItem.Name=name.TextL; _value= None; Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[itemItree.TokenStopIndex].Line, itemItree.TokenStartIndex - 1, itemItree.TokenStopIndex + 2)}
        | _                 -> raise (BugErrorException("Bug in CreateNamedItems.CreateItem")) 
    let enumItes = getChildrenByType(tree, asn1Parser.NUMBER_LST_ITEM)
    enumItes |> List.map CreateItem

and CreateConstraint (astRoot:list<ITree>) (tree:ITree) : Asn1Constraint option=
        match tree.Type with
        |asn1Parser.UnionMark           -> 
            let c1 = CreateConstraint  astRoot (tree.GetChild(0)) 
            let c2 = CreateConstraint  astRoot (tree.GetChild(1)) 
            match c1, c2 with
            |Some(k1),Some(k2)  ->Some(UnionConstraint(k1 , k2, false ))
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
        let Type = CreateType [] astRoot (tree.GetChild(0)) fileTokens alreadyTakenComments; 
        ValueParameter (Type, tree.GetChild(1).TextL)
    | _ -> raise (BugErrorException("Bug in CreateConstraint"))
    

let CreateTypeAssignment (astRoot:list<ITree>) (tree:ITree) (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>) = 
    let parameters = 
            match tree.GetOptChild asn1Parser.PARAM_LIST with
            | None          -> []
            | Some(prmList) -> prmList.Children |> List.map(fun x -> CreateTemplateParameter astRoot x fileTokens alreadyTakenComments)
    { 
        TypeAssignment.Name = tree.GetChild(0).TextL;
        Type = CreateType parameters astRoot (tree.GetChild(1)) fileTokens alreadyTakenComments; 
        Parameters = parameters
        Comments = Antlr.Comment.GetComments(fileTokens, alreadyTakenComments, fileTokens.[tree.TokenStopIndex].Line, tree.TokenStartIndex - 1, tree.TokenStopIndex + 1)
    }

let CreateValueAssignment (astRoot:list<ITree>) (tree:ITree) = 
    let alreadyTakenComments = System.Collections.Generic.List<IToken>()
    let name = tree.GetChild(0).TextL;
    let typ = CreateType [] astRoot (tree.GetChild(1)) [||] alreadyTakenComments
    {
        ValueAssignment.Name = name
        Type = typ
        Value = CreateValue astRoot (tree.GetChild(2)) 
        Scope = ParameterizedAsn1Ast.GlobalScope
        c_name = ToC2 name.Value
        ada_name = ToC2 name.Value
    }

let CreateAsn1Module (astRoot:list<ITree>) (implicitlyImportedTypes: (string*ImportedModule list) list) (tree:ITree)   (fileTokens:array<IToken>) (alreadyTakenComments:System.Collections.Generic.List<IToken>)= 
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
            let Type = { Asn1Type.Kind =  ReferenceType(mdName, tas, []); Constraints= []; Location = tas.Location; parameterizedTypeInstance = false}
            
            //CreateType astRoot x.Parent fileTokens alreadyTakenComments 

            let scope = TypeScope(mdName, tas)

            let namedItems = 
                seq {
                    for ni in x.Children do
                        if ni.Type = asn1Parser.NUMBER_LST_ITEM then
                            let Value = CreateValue astRoot (ni.GetChild(1))
                            let vasName = ni.GetChild(0).TextL
                            let c_name = ToC2 (tas.Value + "_" + ni.GetChild(0).Text)
                            yield   {
                                        ValueAssignment.Name = vasName
                                        Type = Type
                                        Value = Value
                                        Scope = scope
                                        c_name = c_name
                                        ada_name = c_name
                                    }
                    } |> Seq.toList
            namedItems)
    match tree.Type with
    | asn1Parser.MODULE_DEF ->  
          match getOptionChildByType(tree, asn1Parser.EXTENSIBILITY) with
          | Some(_) -> raise (SemanticError(tree.Location, "Unsupported ASN.1 feature: EXTENSIBILIY IMPLED. Extensibility is incompatible with embedded systems"))
          | None ->
          let modName = getChildByType(tree, asn1Parser.UID).TextL
          { 
                Name=  modName
                TypeAssignments= getChildrenByType(tree, asn1Parser.TYPE_ASSIG) |> List.map(fun x -> CreateTypeAssignment astRoot x fileTokens alreadyTakenComments)
                ValueAssignments = 
                    let globalValueAssignments = getChildrenByType(tree, asn1Parser.VAL_ASSIG) |> List.map(fun x -> CreateValueAssignment astRoot x)
                    let typeScopedValueAssignments = handleIntegerValues tree
                    globalValueAssignments@typeScopedValueAssignments
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
          }
    | _ -> raise (BugErrorException("Bug in CreateAsn1Module"))



let CreateAsn1File (astRoot:list<ITree>) (implicitlyImportedTypes: (string*ImportedModule list) list) (tree:ITree, file, tokens)   = 
    match tree.Type with
    | asn1Parser.ASN1_FILE ->  
        let alreadyTakenComments = new System.Collections.Generic.List<IToken>();
        { 
                Asn1File.FileName=file;  
                Modules= getTreeChildren(tree) |> List.map(fun t-> CreateAsn1Module astRoot implicitlyImportedTypes t tokens alreadyTakenComments) 
                Tokens = tokens
        }
    | _ -> raise (BugErrorException("Bug in CreateAsn1File"))




let rootCheckCyclicDeps (astRoot:list<ITree>) =
    let allTasses = astRoot |> 
                    List.collect(fun f -> f.Children) |> 
                    List.collect(fun m -> m.GetChildrenByType(asn1Parser.TYPE_ASSIG) |> List.map(fun t -> m,t )) |> 
                    List.map(fun (m,a) -> 
                        let (md,ts) = (m.GetChild(0).TextL, a.GetChild(0).TextL)
                        let paramsSet = 
                            a.AllChildren |> List.filter(fun x -> x.Type = asn1Parser.TYPE_PARAM) |> List.map(fun z -> (z.GetChild 0).Text) |> Set.ofList
                        let children = 
                            a.AllChildren |> 
                            List.filter(fun x -> x.Type = asn1Parser.REFERENCED_TYPE || x.Type = asn1Parser.PREFERENCED_TYPE ) |> 
                            List.filter(fun x ->        //Exclude reference to type parameters
                                let refTypeName = getReftypeName x
                                not (paramsSet.Contains( refTypeName.Value)  )) |>
                            List.map(fun r -> CreateRefTypeContent r )
                        ((md,ts), children) )

    let tasses = allTasses |> List.map fst
    let refTypes = allTasses |> List.collect snd
    let orphanRefTypes = 
        refTypes |> 
        List.choose(fun (rfm, rft)  ->
            match tasses |> List.tryFind( fun (m,t) -> m.Value = rfm.Value && t.Value = rft.Value)  with
            | Some _    -> None
            | None      -> Some (rft.Location, sprintf "No type assignemt with name %s exists (or exported) in module %s" rft.Value  rfm.Value) )
    match orphanRefTypes with
    | []    -> ()
    | (l,msg)::_-> raise(SemanticError(l,msg))

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
    let excToThrow (lst:list<(StringLoc*StringLoc)*list<StringLoc*StringLoc>>) =
        match lst with
        | []        -> raise(BugErrorException(""))
        | ((m,t),_)::xs -> 
            let printTas ((md:StringLoc,ts:StringLoc), deps: (StringLoc*StringLoc) list) = 
                sprintf "Type assignment '%s.%s' depends on : %s" md.Value ts.Value (deps |> List.map(fun (m,t) -> "'" + m.Value + "." + t.Value + "'") |> Seq.StrJoin ", ")
            let names = lst |> List.map printTas |> Seq.StrJoin " and "
            //let depNodes = lst |> List.map(fun ((m,v),_) -> v)
            //let loc = depNodes.Head.Location
            //let str = depNodes |> List.map(fun x -> x.Value) |> Seq.StrJoin ", "
            SemanticError (t.Location, sprintf "Cyclic dependencies detected : %s" names)
    let comparer (m1:StringLoc, t1:StringLoc) (m2:StringLoc, t2:StringLoc) = m1.Value = m2.Value && t1.Value=t2.Value
    DoTopologicalSort2 independentNodes dependentNodes comparer excToThrow |> ignore

    implicitlyImportedTypes


let CreateAstRoot (list:ParameterizedAsn1Ast.AntlrParserResult list) (args:CommandLineSettings) =  
    let astRoot = list |> List.map (fun r -> r.rootItem)
    ITree.RegisterFiles(list |> Seq.map (fun x -> (x.rootItem, x.fileName)))
    let implicitlyImportedTypes = rootCheckCyclicDeps astRoot
    {
        AstRoot.Files = list |> Seq.toList  |> List.map(fun x -> CreateAsn1File astRoot implicitlyImportedTypes (x.rootItem,x.fileName, x.tokens))
        args = args
    }




