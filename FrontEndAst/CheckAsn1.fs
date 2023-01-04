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

module CheckAsn1

open System.Numerics
open Asn1Ast
open FsUtils
open CommonTypes

let c_keywords =  CommonTypes.c_keyworkds

let ada_keywords =  CommonTypes.ada_keyworkds

let all_keywords =
    let ada_set = ada_keywords |> Set.ofList
    let cmn_keys = c_keywords |> List.filter(fun ck -> ada_set.Contains ck)
    let cmn_set = cmn_keys |> Set.ofList
    let cmn_keys = cmn_keys  |> List.map(fun k -> (k, "is a C and Ada"))
    let c_keys = c_keywords |> List.filter(fun z -> not (cmn_set.Contains z))  |> List.map(fun k -> (k, "is a C"))
    let a_keys = ada_keywords |> List.filter(fun z -> not (cmn_set.Contains z)) |> List.map(fun k -> (k, "is an Ada"))
    cmn_keys@c_keys@a_keys |> Map.ofList

let checkAgainstKeywords (strLc : StringLoc) =
    match all_keywords.TryFind(strLc.Value.ToLower()) with
    | None      -> ()
    | Some langMsg ->
        let errMsg = sprintf "'%s' %s keyword." strLc.Value langMsg
        match checkForAdaKeywords () with
        | false   -> ()
        | true      ->raise (SemanticError (strLc.Location, errMsg))
        

    
    

///chekcs whether the input values is int or not
let rec IsValueInt (v:Asn1Value) ast =
    match v.Kind with
    | IntegerValue(_)                          -> true
    | RefValue(modName,vasName)                         -> IsValueInt (GetBaseValue modName vasName ast) ast
    | _                                        -> false

///it checks if the input type t matches with input value v. It returns true/false

let rec TypeValueMatch (t:Asn1Type) (typeNames:(StringLoc*StringLoc)list) (v:Asn1Value) (valueTypeNames:(StringLoc*StringLoc)list) (ast:AstRoot) =
    let getRefValueBaseTypes modName vasName =
        let mdl = ast.GetModuleByName(modName)
        let vas = mdl.GetValueAsigByName vasName ast
        match vas.Type.Kind with
        | ReferenceType ref    -> [(ref.modName,ref.tasName)]
        | _                         -> []
    let isNumeric s = true
    let typesMatch = 
        match typeNames, valueTypeNames with
        | [], []    -> true
        | _::_, [] -> true
        | [], _::_ -> true
        | (t_mdName,tasName)::_, (v_mdName,v_tasName)::_   -> 
            match t_mdName = v_mdName && tasName = v_tasName with
            | true -> true
            | false -> raise(SemanticError(v.Location, sprintf "Expecting value of type '%s.%s' but a value of type '%s.%s' was provided" t_mdName.Value tasName.Value v_mdName.Value v_tasName.Value))
    match typesMatch with
    | false -> false
    | true  ->
        match t.Kind, v.Kind with
        | ReferenceType rf, _               -> TypeValueMatch (GetActualType t ast) ((rf.modName,rf.tasName)::typeNames) v valueTypeNames ast
        | Integer, IntegerValue(_)                          -> true
        | Real, RealValue(_)                                -> true
        | Real, IntegerValue(_)                             -> true
        | IA5String, StringValue(_)                         -> true
        | NumericString, StringValue(s) when isNumeric s    -> true
        | NullType, NullValue                               -> true
        | Boolean, BooleanValue(_)                          -> true
        | BitString(_), BitStringValue(_)                   -> true
        | BitString(_), OctetStringValue(_)                 -> true
        | OctetString, OctetStringValue(_)                  -> true
        | OctetString, BitStringValue(bitVal)               -> true
        | ObjectIdentifier, ObjOrRelObjIdValue _            -> true
        | RelativeObjectIdentifier, ObjOrRelObjIdValue _    -> true
        | Enumerated(enItems), RefValue(modName,vasName)      -> 
            if  enItems |> Seq.exists(fun en -> en.Name = vasName) then 
                true
            else
                let basType = getRefValueBaseTypes modName vasName
                TypeValueMatch t typeNames (GetBaseValue modName vasName ast) (basType@valueTypeNames) ast
        | SequenceOf(child), SeqOfValue(chValues)           -> 
            chValues |> Seq.forall(fun chv -> TypeValueMatch child [] chv [] ast)
        | Sequence(children), SeqValue(chValues)            -> 
            let checkChild (ch:ChildInfo) = 
                let chValue = chValues |> Seq.tryFind(fun v -> ch.Name = (fst v))
                match chValue with
                | Some(_,actVal)    ->  TypeValueMatch ch.Type [] actVal [] ast
                | None              ->  match ch.Optionality with
                                        | Some(Optional(_))    -> true
                                        | Some(AlwaysAbsent)   -> true
                                        | Some(AlwaysPresent)  -> false
                                        | None                 -> false
            let childrenStatus = children |> Seq.forall checkChild
            let invalidValues = chValues |> Seq.exists(fun v -> not (children |> Seq.exists(fun c -> c.Name = (fst v))) )
            childrenStatus &&  not(invalidValues)
        | Choice(children), ChValue(altName, chVal)         ->
            let ch = children |> Seq.tryFind(fun x-> x.Name = altName)
            match ch with
            | None  -> false
            | Some(child)                                   -> TypeValueMatch child.Type [] chVal [] ast
        | _, RefValue(modName,vasName)                               -> 
            let vas = GetBaseValue modName vasName ast
            let basType = getRefValueBaseTypes modName vasName
            TypeValueMatch t typeNames vas (basType@valueTypeNames) ast
        | _,_                                               -> false



let rec AreAsn1ValuesEqual (v1:Asn1Value) (v2:Asn1Value) (isOfEnumType:bool) ast =
    match v1.Kind, v2.Kind with
    | IntegerValue(a1), IntegerValue(a2)            -> a1 = a2
    | IntegerValue(a1), RealValue(a2)               -> a1.Value = BigInteger(a2.Value)
    | RealValue(a1), IntegerValue(a2)               -> BigInteger(a1.Value) = a2.Value
    | RealValue(a1), RealValue(a2)                  -> a1 = a2
    | StringValue(a1,_), StringValue(a2,_)          -> a1 = a2
    | BooleanValue(a1), BooleanValue(a2)            -> a1 = a2
    | BitStringValue(a1), BitStringValue(a2)        -> a1 = a2
    | OctetStringValue(a1), OctetStringValue(a2)    -> a1 = a2
    | BitStringValue(a1), OctetStringValue(a2)      -> (bitStringValueToByteArray a1) = (a2 |> List.map(fun x -> x.Value) |> List.toArray)
    | OctetStringValue(a1), BitStringValue(a2)      -> (a1 |> Seq.map(fun x -> x.Value)|>Seq.toArray) = (bitStringValueToByteArray a2)
    | SeqOfValue(a1), SeqOfValue(a2)                -> (Seq.length a1 = Seq.length a2) && (Seq.forall2 (fun v1 v2 -> AreAsn1ValuesEqual v1 v2 isOfEnumType ast) a1 a2)
    | SeqValue(a1),  SeqValue(a2)                   -> (Seq.length a1 = Seq.length a2) && (Seq.forall2 (fun (n1,v1) (n2,v2) -> n1=n2 && (AreAsn1ValuesEqual v1 v2 isOfEnumType ast)) a1 a2)
    | ChValue(n1,v1), ChValue(n2,v2)                -> n1=n2 && (AreAsn1ValuesEqual v1 v2 isOfEnumType ast)
    | NullValue, NullValue                          -> true
    | RefValue(m1,vas1), RefValue(m2,vas2)          -> m1=m1 && vas1=vas2
    | RefValue(modName,vasName), _  when not isOfEnumType -> AreAsn1ValuesEqual (GetBaseValue modName vasName ast) v2 isOfEnumType ast
    | _, RefValue(modName,vasName)  when not isOfEnumType -> AreAsn1ValuesEqual v1 (GetBaseValue modName vasName ast) isOfEnumType ast
    |_,_                                            -> false
        

let rec CompareAsn1Value (v1:Asn1Value) (v2:Asn1Value) ast =
    let comparePrimitiveValues a1 a2 =
        if a1 = a2 then      0
        elif a1 >a2 then      1
        else                -1
    match v1.Kind, v2.Kind with
    | IntegerValue(a1), IntegerValue(a2)            -> comparePrimitiveValues a1.Value a2.Value
    | IntegerValue(a1), RealValue(a2)               -> comparePrimitiveValues a1.Value  (BigInteger a2.Value)
    | RealValue(a1), IntegerValue(a2)               -> comparePrimitiveValues (BigInteger a1.Value)  a2.Value
    | RealValue(a1), RealValue(a2)                  -> comparePrimitiveValues a1.Value  a2.Value
    | StringValue(a1,_), StringValue(a2,_)              -> comparePrimitiveValues (StringValue2String a1)  (StringValue2String a2)
    | RefValue(modName,vasName), _                  -> CompareAsn1Value (GetBaseValue modName vasName ast) v2 ast
    | _, RefValue(modName,vasName)                  -> CompareAsn1Value v1 (GetBaseValue modName vasName ast)  ast
    | _                                             -> raise (BugErrorException(""))

type TYPE_BIT_OR_OCT_STR =
    | TP_OCT_STR
    | TP_BIT_STR

let rec IsValueAllowed (c:Asn1Constraint) (v:Asn1Value) (isOfEnumType:bool) (bitOrOctSrt:TYPE_BIT_OR_OCT_STR option) (ast:AstRoot) =
    let CreateDummyValueByKind valKind  = 
        {
            Asn1Value.Kind = valKind
            Location = {SrcLoc.srcFilename="";srcLine=0; charPos=0}
            id = ReferenceToValue([],[])
        }
    match c with
    | SingleValueContraint(_, v1)          -> AreAsn1ValuesEqual v1 v isOfEnumType ast
    | RangeContraint(_, v1, v2, minInclusi, maxInclusive)            -> 
        match minInclusi, maxInclusive with
        | true, true  ->CompareAsn1Value v1 v ast <=0 && CompareAsn1Value v v2 ast <= 0
        | true, false ->CompareAsn1Value v1 v ast <=0 && CompareAsn1Value v v2 ast < 0
        | false,true  ->CompareAsn1Value v1 v ast <0 && CompareAsn1Value v v2 ast <= 0
        | false,false ->CompareAsn1Value v1 v ast <0 && CompareAsn1Value v v2 ast < 0
    | RangeContraint_val_MAX(_, v1, minInclusi)        -> 
        match minInclusi with
        | true      -> CompareAsn1Value v1 v ast <=0
        | false     -> CompareAsn1Value v1 v ast <0
    | RangeContraint_MIN_val(_, v2, maxInclusive)        -> 
        match maxInclusive with
        | true  -> CompareAsn1Value v v2 ast <= 0
        | false -> CompareAsn1Value v v2 ast < 0
    | RangeContraint_MIN_MAX            -> true
    | SizeContraint(_, sc)                 -> 
        let rec IsSizeContraintOK (v:Asn1Value) (sc:Asn1Constraint) =
            match v.Kind with
            | StringValue(s, l)                -> 
                let mergedStr = StringValue2String s
                IsValueAllowed sc (CreateDummyValueByKind (IntegerValue(IntLoc.ByValue (BigInteger mergedStr.Length))) ) isOfEnumType bitOrOctSrt ast
            | BitStringValue(s)             -> 
                match bitOrOctSrt with
                | Some TP_BIT_STR   -> IsValueAllowed sc (CreateDummyValueByKind (IntegerValue(IntLoc.ByValue (BigInteger s.Value.Length))) ) isOfEnumType bitOrOctSrt ast
                | Some TP_OCT_STR   when s.Value.Length%8 = 0 -> IsValueAllowed sc (CreateDummyValueByKind (IntegerValue(IntLoc.ByValue (BigInteger (s.Value.Length/8)))) ) isOfEnumType bitOrOctSrt ast
                | _              -> false
            | OctetStringValue(a)           -> 
                match bitOrOctSrt with
                | Some TP_BIT_STR   -> IsValueAllowed sc (CreateDummyValueByKind (IntegerValue(IntLoc.ByValue (BigInteger (a.Length*8)))) ) isOfEnumType bitOrOctSrt ast
                | Some TP_OCT_STR   -> IsValueAllowed sc (CreateDummyValueByKind (IntegerValue(IntLoc.ByValue (BigInteger (a.Length*1)))) ) isOfEnumType bitOrOctSrt ast
                | None              -> false
            | SeqOfValue(a)                 -> IsValueAllowed sc (CreateDummyValueByKind (IntegerValue(IntLoc.ByValue (BigInteger (Seq.length a)))) ) isOfEnumType bitOrOctSrt ast
            | RefValue(modName,vasName)                  -> IsSizeContraintOK (GetBaseValue modName vasName ast) sc
            | _                             -> raise (BugErrorException(""))
        IsSizeContraintOK v sc
    | AlphabetContraint(_, ac)             ->
        let rec IsAlphabetConstraintOK (v:Asn1Value) (ac:Asn1Constraint) =
            match v.Kind with
            | StringValue(s, l)    -> 
                let sValue = StringValue2String s
                match ac with
                | SingleValueContraint (_, setVal)   ->
                    match setVal.Kind with
                    | StringValue (setvaluesstr,l2)     ->
                        let setvaluesstrValue = StringValue2String setvaluesstr
                        let setvals = setvaluesstrValue.ToCharArray() |> Set.ofArray
                        sValue.ToCharArray()  |> Seq.forall(fun c -> setvals.Contains c)
                    | _                         -> false
                | _     ->
                    let sDummyVal (c:SingleStringValue) = CreateDummyValueByKind (StringValue([c], emptyLocation  ) )
                    sValue.ToCharArray() |> 
                    Seq.map char2SingleStringValue |>
                    Seq.forall(fun c -> 
                        let ret = IsValueAllowed ac (sDummyVal c) isOfEnumType bitOrOctSrt ast
                        if not ret then 
                            printfn "Character '%A' is not allowed." c
                        ret
                    )
            | RefValue(modName,vasName)      -> IsAlphabetConstraintOK (GetBaseValue modName vasName ast) ac
            | _                             -> raise (BugErrorException(""))
        IsAlphabetConstraintOK v ac 
    | UnionConstraint(_, c1,c2,_)            -> 
        let ret1 = IsValueAllowed c1 v isOfEnumType bitOrOctSrt ast  
        let ret2 = IsValueAllowed c2 v isOfEnumType bitOrOctSrt ast
        ret1 || ret2
    | IntersectionConstraint(_, c1,c2)     -> IsValueAllowed c1 v isOfEnumType bitOrOctSrt ast && IsValueAllowed c2 v isOfEnumType bitOrOctSrt ast
    | AllExceptConstraint(_, c1)           -> not (IsValueAllowed c1 v isOfEnumType bitOrOctSrt ast)
    | ExceptConstraint(_, c1,c2)           -> IsValueAllowed c1 v isOfEnumType bitOrOctSrt ast && not(IsValueAllowed c2 v isOfEnumType bitOrOctSrt ast)
    | RootConstraint(_, c1)                -> IsValueAllowed c1 v isOfEnumType bitOrOctSrt ast
    | RootConstraint2(_, c1,c2)            -> IsValueAllowed c1 v isOfEnumType bitOrOctSrt ast || IsValueAllowed c2 v isOfEnumType bitOrOctSrt ast
    | TypeInclusionConstraint(_, modName,tasName)   ->
        let otherType = GetBaseTypeByName modName tasName ast
        otherType.Constraints |> Seq.forall(fun c -> IsValueAllowed c v isOfEnumType bitOrOctSrt ast)
    | WithComponentConstraint(_, innerCon, loc)       ->
        let rec IsWithComponentConstraintOK (v:Asn1Value) (innerCon:Asn1Constraint) =
            match v.Kind with
            | SeqOfValue(innerValues) ->
                innerValues |> Seq.forall(fun iv -> IsValueAllowed innerCon iv isOfEnumType bitOrOctSrt ast)
            | RefValue(modName,vasName)      -> IsWithComponentConstraintOK (GetBaseValue modName vasName ast) innerCon
            | _                             -> raise (SemanticError(loc,"Invalid constraint"))
        IsWithComponentConstraintOK v innerCon
    | WithComponentsConstraint(_, namedConstraints)    ->
        let rec IsWithComponentsConstraintOK (v:Asn1Value) =
            match v.Kind with
            | SeqValue(children)    ->
                let IsNamedConstraintOK (nc:NamedConstraint) =
                    let ch = children |> Seq.tryFind(fun (nm, chV) -> nm=nc.Name )
                    match nc.Mark, ch, nc.Contraint with
                    | NoMark, None, _                       -> true
                    | NoMark, Some(_,chv), Some(ic)         -> IsValueAllowed ic chv isOfEnumType bitOrOctSrt ast
                    | NoMark, Some(_,chv), None             -> true
                    | MarkPresent, None, _                  -> false
                    | MarkPresent, Some(_,chv), None        -> true
                    | MarkPresent, Some(_,chv), Some(ic)    -> IsValueAllowed ic chv isOfEnumType bitOrOctSrt ast
                    | MarkAbsent, None, _                   -> true
                    | MarkAbsent, Some(_), _                -> false
                    | MarkOptional, Some(_,chv), Some(ic)    -> IsValueAllowed ic chv isOfEnumType bitOrOctSrt ast
                    | MarkOptional, Some(_,chv), None        -> true
                    | MarkOptional, None, None               -> true
                    | MarkOptional, None, Some(_)            -> false
                namedConstraints |> Seq.forall IsNamedConstraintOK
            | ChValue(aName, inVal)    ->
                let nc = namedConstraints |> Seq.tryFind(fun x -> x.Name = aName)
                match nc with
                | None      -> true
                | Some(rnc) ->
                    match rnc.Mark, rnc.Contraint with
                    | MarkAbsent, _     -> false
                    | _,  Some(ic)      -> IsValueAllowed ic inVal isOfEnumType bitOrOctSrt ast  
                    | _,  None          -> true
            | RefValue(modName,vasName)      -> IsWithComponentsConstraintOK (GetBaseValue modName vasName ast) 
            | _                             -> raise (BugErrorException(""))
        IsWithComponentsConstraintOK v
        
    
let rec CheckIfVariableViolatesTypeConstraints (t:Asn1Type) (v:Asn1Value) ast =
    match v.Kind, t.Kind with
    |_,ReferenceType rf           ->
        let baseType = Asn1Ast.GetBaseTypeConsIncluded t ast
        CheckIfVariableViolatesTypeConstraints baseType v ast
    |RefValue(modName,vasName), Enumerated(items)   -> 
        let bIsEnumItem = items |> Seq.exists(fun x -> x.Name.Value = vasName.Value)
        t.Constraints |> Seq.forall(fun c -> IsValueAllowed c v bIsEnumItem None ast )
    | _                         -> 
        let bitOrOctSrt =
            match (Asn1Ast.GetActualType t  ast).Kind with
            | OctetString       -> Some TP_OCT_STR
            | BitString  _      -> Some TP_BIT_STR
            | _                 -> None
        let ret = t.Constraints |> Seq.forall(fun c -> IsValueAllowed c v false bitOrOctSrt ast )
        match v.Kind, t.Kind with
        |   SeqOfValue chvs, SequenceOf ch     -> ret && (chvs |> Seq.forall(fun cv -> CheckIfVariableViolatesTypeConstraints ch cv ast))
        |   SeqValue   chvs, Sequence chldn    -> 
            let chRet =
                chvs |> 
                Seq.forall(fun (cn,cv) -> 
                    match chldn |> Seq.tryFind(fun c -> c.Name.Value = cn.Value) with
                    | Some ch -> CheckIfVariableViolatesTypeConstraints ch.Type cv ast
                    | None    -> false)
            ret && chRet
        |   ChValue    (nm,chv), Choice chldn  -> 
            let chRet =
                match chldn |> Seq.tryFind(fun c -> c.Name.Value = nm.Value) with
                | Some ch -> CheckIfVariableViolatesTypeConstraints ch.Type chv ast
                | None    -> false
            ret && chRet
        | _                     -> ret 

let rec getEnumeratedAllowedEnumerations ast (m:Asn1Module) (t:Asn1Type) =
    match t.Kind with
    |ReferenceType rf          ->
        let baseType = Asn1Ast.GetBaseTypeConsIncluded t ast
        getEnumeratedAllowedEnumerations ast m baseType  
    |Enumerated(items)   -> 
            items |>
            List.choose(fun itm -> 
                let v = {Asn1Value.Location = itm.Name.Location; Kind = RefValue(m.Name, itm.Name); id = ReferenceToValue([],[])} 
                match t.Constraints |> Seq.forall(fun c -> IsValueAllowed c v true None ast ) with
                | true -> Some itm
                | false-> None)
    | _                         -> raise (BugErrorException("getEnumItemTypeAllowedEnums can be called only for Enumerated types"))
    


///checks if the input type t matches with input value v (by calling TypeValueMatch) and raises a user exception if not
let CheckValueType (ot:Asn1Type) (v:Asn1Value) ast=
    let rec CheckValueType (t:Asn1Type) (v:Asn1Value) ast=
        if not (TypeValueMatch t [] v [] ast) then
            match t.Kind, v.Kind with
            | SequenceOf(child), SeqOfValue(chValues)  -> chValues |> Seq.iter(fun chv -> CheckValueType child chv ast)
            | Choice(children), ChValue(altName, chVal)         ->
                let ch = children |> Seq.tryFind(fun x-> x.Name = altName)
                match ch with
                | None  -> raise(SemanticError(v.Location, (sprintf "Invalid id:%s" altName.Value) ))
                | Some(child)           -> CheckValueType child.Type chVal ast

            | Sequence(children), SeqValue(chValues)            -> 
                let checkChild (ch:ChildInfo) = 
                    let chValue = chValues |> Seq.tryFind(fun v -> ch.Name = (fst v))
                    match chValue with
                    | Some(_,actVal)    ->  CheckValueType ch.Type actVal ast
                    | None              ->  match ch.Optionality with
                                            | Some(Optional(_))    -> ()
                                            | Some(AlwaysAbsent)   -> ()
                                            | _                    -> raise(SemanticError(v.Location, sprintf "missing value for component: %s" ch.Name.Value ))
                                            
                let childrenStatus = children |>  Seq.iter checkChild
                chValues |> Seq.iter(fun v -> if not (children |> Seq.exists(fun c -> c.Name = (fst v))) then
                                                let unknownName = (fst v)
                                                let (nm, loc) = unknownName.AsTupple
                                                raise (SemanticError(loc, (sprintf "Invalid id: %s" nm)) )
                                        )
            | ReferenceType(_),_                        -> CheckValueType (GetActualType t ast) v ast
            | _                                        -> 
                let typeName =
                    match ot.Kind with
                    | ReferenceType rf  -> sprintf "%s.%s" rf.modName.Value rf.tasName.Value
                    | _                     -> sprintf "%A" t.Kind
                raise(SemanticError(v.Location, sprintf "Expecting a '%s' value" typeName))

                //raise(SemanticError(v.Location, sprintf "Expecting a '%s' value, got a '%s' " typeName (v.ToString())))
    CheckValueType ot v ast
///checks if two types are ASN.1 compatible
let rec AreTypesCompatible (t1:Asn1Type) (t2:Asn1Type) ast =
    match t1.Kind, t2.Kind with
    | ReferenceType(_), _       -> AreTypesCompatible (GetActualType t1 ast) t2 ast
    | _, ReferenceType(_)       -> AreTypesCompatible t1 (GetActualType t2 ast) ast
    | Sequence(c1), Sequence(c2)  | Choice(c1),  Choice(c2)  -> 
        let sameSize = Seq.length c1 = Seq.length c2
        let names1 = c1 |> Seq.map(fun x -> x.Name.Value, x.Optionality) |> Seq.toList
        let names2 = c2 |> Seq.map(fun x -> x.Name.Value, x.Optionality) |> Seq.toList
        let sameNames = c1 = c2
        let zipped = Seq.zip (c1 |> Seq.map(fun x-> x.Type)) (c2 |> Seq.map(fun x-> x.Type))
        sameSize && sameNames && zipped |> Seq.forall(fun (a1,a2)-> AreTypesCompatible a1 a2 ast) 
    | SequenceOf(child1), SequenceOf(child2)      -> AreTypesCompatible child1 child2 ast
    | Enumerated(items1), Enumerated(items2)      -> 
        let sameSize = Seq.length items1 = Seq.length items2
        let names1 = items1 |> Seq.map(fun x -> x.Name.Value) |> Seq.toList
        let names2 = items1 |> Seq.map(fun x -> x.Name.Value) |> Seq.toList
        sameSize && names1=names2
    | _ ,_                                        -> t1.Kind.ToString() = t2.Kind.ToString()
    
/// it checks if the input type t can have the constraint c 
/// returns true/false
let rec isConstraintValid (t:Asn1Type) (c:Asn1Constraint) ast =
    let rec CanHaveRangeContraint (t:Asn1Type) =
        match t.Kind with
        | Integer | Real | IA5String | NumericString                    -> true
        | ObjectIdentifier          | RelativeObjectIdentifier          -> false
        | NullType | Boolean | Enumerated(_) | Sequence(_) | Choice(_)  -> false
        | OctetString | BitString(_) | SequenceOf(_)  | TimeType _      -> false
        | ReferenceType(_)                                              -> CanHaveRangeContraint (GetActualType t ast)

    let rec CanHaveSizeContraint (t:Asn1Type) =
        match t.Kind with
        | Integer | Real | NullType | Boolean | Enumerated(_) | Sequence(_) | Choice(_)     -> false
        | ObjectIdentifier          | RelativeObjectIdentifier  | TimeType _                -> false
        | IA5String | NumericString | OctetString | BitString(_) | SequenceOf(_)            -> true
        | ReferenceType(_)                                                                  -> CanHaveSizeContraint (GetActualType t ast)
    let rec CanHaveFromContraint (t:Asn1Type) =
        match t.Kind with
        | Integer | Real | NullType | Boolean | Enumerated(_) | Sequence(_) | Choice(_) | OctetString | BitString(_) | SequenceOf(_)   -> false
        | ObjectIdentifier          | RelativeObjectIdentifier   | TimeType _                           -> false
        | IA5String | NumericString                                                                   -> true
        | ReferenceType(_)                                                                            -> CanHaveFromContraint (GetActualType t ast)
    let rec CanHaveWithComponentConstraint (t:Asn1Type) =
        match t.Kind with
        | Integer | Real | NullType | Boolean | Enumerated(_) | Choice(_) | Sequence(_)   -> None
        | ObjectIdentifier          | RelativeObjectIdentifier    | TimeType _            -> None
        | OctetString | BitString(_) | IA5String | NumericString                          -> None
        | SequenceOf(ch)                                                                  -> Some(ch)
        | ReferenceType(_)                                                                -> CanHaveWithComponentConstraint (GetActualType t ast)
    let rec CanHaveWithComponentsConstraint (t:Asn1Type) =
        match t.Kind with
        | Integer | Real | NullType | Boolean | Enumerated(_) | SequenceOf(_)  -> None
        | ObjectIdentifier          | RelativeObjectIdentifier    | TimeType _ -> None
        | OctetString | BitString(_) | IA5String | NumericString               -> None
        | Sequence(children) | Choice(children)                                -> Some(children)
        | ReferenceType(_)                                                     -> CanHaveWithComponentsConstraint (GetActualType t ast)
    match c with
    | SingleValueContraint(_, v1)          ->  CheckValueType t v1 ast
    | RangeContraint(_, v1,v2,_,_)             -> 
        if not(CanHaveRangeContraint t) then
            raise(SemanticError(t.Location, "Type does not support range constraints"))
        CheckValueType t v1 ast
        CheckValueType t v2 ast
    | RangeContraint_val_MAX(_, v1,_)       
    | RangeContraint_MIN_val(_, v1,_)       ->
        if not(CanHaveRangeContraint t) then
            raise(SemanticError(t.Location, "Type does not support range constraints"))
        CheckValueType t v1 ast
    | RangeContraint_MIN_MAX           -> 
        if not(CanHaveRangeContraint t) then
            raise(SemanticError(t.Location, "Type does not support range constraints"))
    | SizeContraint(_, c1)                 -> 
        if not(CanHaveSizeContraint t) then
            raise(SemanticError(t.Location, "Type does not support size constraints"))
        isConstraintValid { t with Kind=Integer; Constraints=[] } c1 ast
    | AlphabetContraint(_, c1)             -> 
        if not(CanHaveFromContraint t) then
            raise(SemanticError(t.Location, "Type does not support alphabet constraints"))
        isConstraintValid t c1 ast
    | UnionConstraint(_, c1,c2,_)  | IntersectionConstraint(_, c1,c2) | ExceptConstraint(_, c1,c2) | RootConstraint2(_, c1,c2) ->
        isConstraintValid t c1 ast
        isConstraintValid t c2 ast
    | AllExceptConstraint(_, c1) | RootConstraint(_, c1)       -> isConstraintValid t c1 ast
    | TypeInclusionConstraint(_, mdName, refName)  -> 
        let typeInclusion = GetActualTypeByName mdName refName ast
        let actType = GetActualType t ast
        if not(AreTypesCompatible typeInclusion actType ast) then
            raise (SemanticError(t.Location, "Incompatible types used in type inclusion constraint"))
    | WithComponentConstraint(_, c1, loc)       -> 
        match CanHaveWithComponentConstraint t with
        | None -> raise (SemanticError(loc, "Type does not support WITH COMPONENT constraints"))
        | Some(ch)  -> isConstraintValid ch c1 ast
    | WithComponentsConstraint(_, namedCons)       -> 
        match CanHaveWithComponentsConstraint t with
        | None  -> raise (SemanticError(t.Location, "Type does not support WITH COMPONENTS constraints"))
        | Some(children)    ->  
            let checkNamedConstraint (nc:NamedConstraint) = 
                let (conName, loc) =  nc.Name.AsTupple
                match children |> Seq.tryFind(fun c -> c.Name.Value = conName) with
                | None          -> raise (SemanticError(loc, sprintf "Invalid id: %s" conName))
                | Some(child)   -> 
                    let isChoice = match (GetActualType t ast).Kind with Choice(_) -> true | _ -> false
                    match nc.Contraint with 
                    | Some(newC)    -> isConstraintValid child.Type newC ast 
                    | _   -> ()
                    match child.Optionality, nc.Mark, isChoice with
                    | Some(Optional opt), MarkAbsent,false  when opt.defaultValue.IsSome-> raise(SemanticError (loc, sprintf "Component %s has default value and therefore it cannot be constraint to ABSENT" conName))
                    | None, MarkAbsent,false  
                    | None, MarkPresent,false             -> raise(SemanticError (loc, sprintf "Component %s is not optional. So, it cannot be constraint to ABSENT or PRESENT" conName))
                    | _, MarkPresent, true
                    | _, MarkOptional, true               -> raise(SemanticError (loc, sprintf "Choice alternative %s cannot be constraint to PRESENT or OPTIONAL" conName))
                    | _                                   -> ()
            namedCons |> Seq.iter checkNamedConstraint


/// it checks the input type for semantic error
/// raises a user exception if an error is found.
let rec CheckType(t:Asn1Type) (m:Asn1Module) ast =
    let CheckSeqChoiceChildren bChoice (children:seq<ChildInfo>) =
        let childrenNames = children |> Seq.map(fun c -> c.Name) |> Seq.toList
        match bChoice with
        | true ->   childrenNames  |> CheckForDuplicatesCaseCaseInsensitive 
        | false ->  childrenNames  |> CheckForDuplicates
        
        match ast.args.fieldPrefix with
        | None      ->  childrenNames |> Seq.iter checkAgainstKeywords
        | Some _    ->  ()
        
        //check that component name does not conflict with a type assignment
        children |> 
            Seq.map(fun c -> c.Name) |>
            Seq.iter(fun c ->
                match ast.Modules |> Seq.tryFind(fun m -> m.Name.Value.ToLower() = ToC (c.Value.ToLower())) with
                | Some m    -> 
                    let errMsg = sprintf "component name '%s' conflicts with module '%s'. May cause compilation errors in case insensitive languages.\nTo overcome this problem use --field-prefix argument." c.Value m.Name.Value
                    match ast.args.fieldPrefix with
                    | None           -> raise(SemanticError(c.Location, errMsg))
                    | Some _         -> ()

                | None      ->
                    match m.TypeAssignments |> Seq.tryFind(fun tas -> ToC ((ast.args.TypePrefix + tas.Name.Value).ToLower()) = ToC (c.Value.ToLower()) ) with
                    | Some tas -> 
                        let errMsg = sprintf "component name '%s' conflicts with type assignment '%s'. May cause compilation errors in case insensitive languages.\nTo overcome this problem use the -typePrefix argument to add a prefix to all generated types or\nuse --field-prefix argument." c.Value tas.Name.Value
                        match checkForAdaKeywords () with
                        | false   -> ()
                        | true      ->
                            match ast.args.fieldPrefix with
                            | None           -> raise(SemanticError(c.Location, errMsg))
                            | Some _         -> ()
                    
                    | None     -> ())
        
        children |> Seq.map(fun c -> c.Type) |> Seq.iter (fun x -> CheckType x m ast)
        children |> 
        Seq.choose(fun c -> 
            match c.Optionality with 
            | Some(Optional opt) -> 
                match opt.defaultValue with
                | Some v -> Some (c.Type, v) 
                | None   -> None
            |_->None) 
        |> Seq.iter (fun (t,v) -> 
            CheckValueType t v ast
            let ret = CheckIfVariableViolatesTypeConstraints t v ast
            match ret with
            | false  -> 
                let msg = sprintf "Value does not conform to its type constraints"
                raise(SemanticError(v.Location,msg))
            | true -> ()
            )
    let CheckNamedItem (children:seq<NamedItem>) =
        children |> Seq.map(fun c -> c.Name) |> CheckForDuplicates 
        children |> 
        Seq.iter(fun c -> match c._value with
                            |Some (intVal)  -> 
                                if IsValueInt intVal ast then ()
                                else raise(SemanticError(intVal.Location,"Expecting integer value"))
                            |None           -> ())
    match t.Kind with
    | Sequence(children)    ->  CheckSeqChoiceChildren false children
    | Choice(children)      ->  CheckSeqChoiceChildren true children
    | SequenceOf(child)     ->  CheckType child m ast
    | Enumerated(children)  ->  
        let getVal vKind = { Asn1Value.Kind = vKind; Location = emptyLocation; id = ReferenceToValue([],[])}
        CheckNamedItem  children 
        children |> Seq.filter(fun x -> x._value.IsSome) |> Seq.map(fun c -> {IntLoc.Value=GetValueAsInt c._value.Value ast; Location=c._value.Value.Location}) |> CheckForDuplicates 
        match children |> Seq.tryFind (fun itm -> 
            let checkCon c = IsValueAllowed c (getVal (RefValue (m.Name, itm.Name) )) true None ast
            t.Constraints |> List.fold(fun state cn -> state && (checkCon cn)) true) with
        | Some itm -> ()
        | None     -> raise(SemanticError(t.Location, "The constraints defined for this type do not allow any value"))

    | BitString _           ->   ()
    | Integer               ->  ()
    | TimeType _           -> ()
    (*  ++++
        match (uPER.GetTypeUperRange t.Kind t.Constraints ast) with
        | Empty                   -> raise(SemanticError(t.Location, "The constraints defined for this type do not allow any value"))
        | _                       -> () *)


    | ObjectIdentifier          -> ()
    | RelativeObjectIdentifier  -> ()
    | Real                  ->  ()
    | Boolean               ->  ()
    | IA5String             ->  ()
    | NumericString         ->  ()
    | NullType              ->  ()
    | OctetString           ->  ()
    | ReferenceType impRef       -> 
        let impMod = ast.GetModuleByName impRef.modName
        match impMod.ExportedTypes |> Seq.tryFind ( (=) impRef.tasName.Value) with
        | Some _    -> ()
        | None      -> raise(SemanticError(impRef.tasName.Location, sprintf "No type assignment with name %s exists (or exported) in module %s" impRef.tasName.Value  impMod.Name.Value))
    t.Constraints |> Seq.iter(fun c -> isConstraintValid t c ast)


/// semantically check module
let CheckModule (m:Asn1Module) ast (pass :int)=
    //check for duplicate type assignments
    let typeAssNames = m.TypeAssignments |> List.map(fun t -> t.Name) 
    typeAssNames |> CheckForDuplicates 
    typeAssNames |> CheckForDuplicatesCI "Type Assignment"
    
    //typeAssNames |> Seq.iter(fun z -> checkAgainstKeywords ({z with Value = ast.args.TypePrefix + z.Value}))
    
    //check for duplicate imported types
    let importedTypeAssNames = m.Imports |> Seq.collect(fun imp -> imp.Types) |> Seq.toList
    importedTypeAssNames |> CheckForDuplicates 

    //check for duplicate in type assignments AND imported types
    Seq.concat [typeAssNames; importedTypeAssNames] |> CheckForDuplicates 


    //check for duplicate value assignments
    let valAssNames = m.ValueAssignments  |> Seq.map(fun t -> t.Name) 
    valAssNames |> CheckForDuplicates 
    valAssNames |> CheckForDuplicatesCI "Value Assignment"
    valAssNames |> Seq.iter checkAgainstKeywords


    //check for duplicate imported values
    let importedValAssNames = m.Imports |> Seq.collect(fun imp -> imp.Values) 
    importedValAssNames |> CheckForDuplicates 

    //check for duplicate value assignments AND imported values
    Seq.concat [valAssNames; importedValAssNames] |> CheckForDuplicates 

    // Check Types
    m.TypeAssignments |> Seq.map(fun x -> x.Type) |> Seq.iter (fun x -> CheckType x m ast)
    m.ValueAssignments |> Seq.map(fun x -> x.Type) |> Seq.iter (fun x -> CheckType x m ast)

    // Check Values
    m.ValueAssignments |> Seq.iter(fun vas -> CheckValueType vas.Type vas.Value ast)

    if (pass = 0) then
        m.ValueAssignments |> Seq.iter(fun vas -> 
                                    let fname = System.IO.Path.GetFileName vas.Name.Location.srcFilename
                                    if not(CheckIfVariableViolatesTypeConstraints vas.Type vas.Value ast) 
                                        then System.Console.Error.WriteLine("Warning: Value {0} defined in File:{1}, Line:{2} does not conform to its type constraints.", vas.Name.Value, fname, vas.Name.Location.srcLine))

    let checkImport (imp: Asn1Ast.ImportedModule)  =
        //check that imported module does exists
        match ast.Modules |> Seq.tryFind(fun x -> x.Name.Value = imp.Name.Value) with
        | None      -> raise (SemanticError(imp.Name.Location, sprintf "No module with name %s exists" imp.Name.Value))
        | Some(im)  -> 
            let checkTasName tasName =
                match im.TypeAssignments |> Seq.tryFind(fun x-> x.Name.Value = tasName.Value ) with
                | Some(_) -> 
                    match im.ExportedTypes |> Seq.tryFind((=) tasName.Value ) with
                    | Some (_)  -> ()
                    | None      -> raise(SemanticError(tasName.Location, sprintf "Type assignment '%s' is privately defined in module '%s'. Use EXPORT keyword to make it visible to other modules." tasName.Value  imp.Name.Value))
                | None    -> raise(SemanticError(tasName.Location, sprintf "No type assignment with name %s exists in module %s" tasName.Value  imp.Name.Value))
            let checkVasName vasName =
                match im.ValueAssignments |> Seq.tryFind(fun x-> x.Name.Value = vasName.Value ) with
                | Some(_) -> 
                    match im.ExportedVars |> Seq.tryFind( (=) vasName.Value ) with
                    | Some (_)  -> ()
                    | None      -> raise(SemanticError(vasName.Location, sprintf "Value assignment %s is privately defined in module '%s'. Use EXPORT keyword to make it visible to other modules" vasName.Value  imp.Name.Value))
                | None    -> raise(SemanticError(vasName.Location, sprintf "No value assignment with name %s exists in module %s" vasName.Value  imp.Name.Value))
            imp.Types |> Seq.iter checkTasName
            imp.Values |> Seq.iter checkVasName
    m.Imports |> Seq.iter checkImport

    
let checkForCyclicModules ( ast:AstRoot) =
    let allModules = ast.Modules |> List.map(fun z -> z.Name)
    let independentModules = ast.Modules |> List.filter(fun m -> List.isEmpty m.Imports) |> List.map(fun m -> m.Name)
    let dependentModules = ast.Modules |> List.filter(fun m -> not (List.isEmpty m.Imports)) |> List.map(fun m -> (m.Name, m.Imports |> List.map(fun imp -> imp.Name) ))
    let sortedModules = 
        DoTopologicalSort independentModules dependentModules 
            (fun cyclicModules -> 
                match cyclicModules with
                | []    -> BugErrorException "Impossible"
                | (m1,deps) ::_ ->
                    let printModule (md:StringLoc, deps: StringLoc list) = 
                        sprintf "Module '%s' depends on modules: %s" md.Value (deps |> List.map(fun z -> "'" + z.Value + "'") |> Seq.StrJoin ", ")
                    let cycMods = cyclicModules |> List.map printModule |> Seq.StrJoin "\n\tand\n"
                    SemanticError
                        (m1.Location, 
                         sprintf 
                             "Cyclic modules detected:\n%s\n"  cycMods))
    sortedModules

let CheckFiles( ast:AstRoot) (pass :int) =
    checkForCyclicModules ast |> ignore
    let modules = ast.Files |> Seq.collect(fun f -> f.Modules ) |> Seq.toList
    // check for multiple module definitions
    modules |> Seq.map(fun m-> m.Name) |> CheckForDuplicates 
    // check each file
    modules |> Seq.iter (fun x -> CheckModule x ast pass)
    


    
