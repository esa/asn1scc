module CheckLongReferences


open System
open System.Numerics
open System.IO
open System.Xml.Linq
open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1Fold
open Asn1AcnAstUtilFunctions


let private addParameter (cur:AcnInsertedFieldDependencies) (asn1Type:Asn1Type) (p:AcnParameter) (d:AcnDependencyKind) =
    {acnDependencies = {AcnDependency.asn1Type = asn1Type.id; determinant = (AcnParameterDeterminant p); dependencyKind = d}::cur.acnDependencies }

let private addAcnChild (tasPositions:Map<ReferenceToType,int>) (cur:AcnInsertedFieldDependencies) (asn1Type:Asn1Type) (acnChild:AcnChild) (d:AcnDependencyKind) =
    let determinantPos =
        match tasPositions.ContainsKey acnChild.id with
        | true  -> tasPositions.[acnChild.id]
        | false ->
            let errMsg = sprintf "ACN determinant field '%s' not found in the dictionary" acnChild.id.AsString
            raise(SemanticError(acnChild.Name.Location, errMsg))

    let asn1TypePos    =
        match tasPositions.ContainsKey asn1Type.id with
        | true  -> tasPositions.[asn1Type.id]
        | false ->
            let errMsg = sprintf "ASN.1 TYPE '%s' not found in the dictionary" asn1Type.id.AsString
            raise(SemanticError(acnChild.Name.Location, errMsg))
    if asn1TypePos < determinantPos then
        let errMsg = sprintf "ACN determinant field '%s' must appear before ASN.1 type '%s'" acnChild.id.AsString asn1Type.id.AsString
        raise(SemanticError(acnChild.Name.Location, errMsg))
    {acnDependencies = {AcnDependency.asn1Type = asn1Type.id; determinant = (AcnChildDeterminant acnChild); dependencyKind = d}::cur.acnDependencies }

let private checkRelativePath (tasPositions:Map<ReferenceToType,int>) (curState:AcnInsertedFieldDependencies) (parents: Asn1Type list) (asn1TypeWithDependency:Asn1Type ) (visibleParameters:(ReferenceToType*AcnParameter) list) (RelativePath path : RelativePath) checkParameter checkAcnType =
    match path with
    | []        -> raise (BugErrorException("Invalid Argument"))
    | x1::_    ->
        match visibleParameters |> Seq.tryFind(fun (ref, p) -> p.name = x1.Value) with
        | Some  (rf,p)   ->
            //x1 is a parameter
            let d = checkParameter p
            addParameter curState asn1TypeWithDependency p d
        | None      ->
        (*
            //x1 is silbing child
            match parents |> List.rev with
            | []    -> raise(SemanticError(x1.Location, (sprintf "Invalid reference '%s'" x1.Value)))
            | parent::_ ->
                match parent.Kind with
                | Sequence seq ->
                    match seq.children |> Seq.tryFind(fun (c:SeqChildInfo) -> c.Name = x1) with
                    | Some ch   ->
                        match ch with
                        | Asn1Child ch  -> raise(SemanticError(x1.Location, (sprintf "ASN.1 fields cannot act as encoding determinants. Remove field '%s' from the ASN.1 grammar and introduce it in the ACN grammar" x1.Value)))
                        | AcnChild ch  ->
                            let d = checkAcnType ch
                            addAcnChild tasPositions curState asn1TypeWithDependency ch d
                    | None      -> raise(SemanticError(x1.Location, (sprintf "Invalid reference '%s'" x1.Value)))
                | _                 -> raise(SemanticError(x1.Location, (sprintf "Invalid reference '%s'" x1.Value)))
                *)
//    | x1::_  ->
            //x1 may be a silbing child or a child to one of my parents
            // so, find the first parent that has a child x1

            let parSeq =
                let rec findSeq p =
                    match p.Kind with
                    | Sequence seq      -> seq.children |> Seq.exists(fun c -> c.Name = x1)
                    | ReferenceType ref ->findSeq ref.resolvedType
                    | _            -> raise(SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (path |> Seq.StrJoin "."))))
                parents |>
                List.rev |>
                List.filter(fun z ->
                    match z.Kind with
                    | Sequence _ -> true
                    | ReferenceType r ->
                        match r.resolvedType.Kind with
                        | Sequence _  -> true
                        | _           -> false
                    | _ -> false ) |>  Seq.tryFind findSeq
            let rec checkSeqWithPath (seq:Asn1Type) (path : StringLoc list)=
                match seq.Kind with
                | Sequence seq ->
                    match path with
                    | []       -> raise(BugErrorException "111")
                    | x1::[]   ->
                        match seq.children |> Seq.tryFind(fun c -> c.Name = x1) with
                        | None -> raise(SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (path |> Seq.StrJoin "."))))
                        | Some ch ->
                            match ch with
                            | Asn1Child ch  -> raise(SemanticError(x1.Location, (sprintf "ASN.1 fields cannot act as encoding determinants. Remove field '%s' from the ASN.1 grammar and introduce it in the ACN grammar" x1.Value)))
                            | AcnChild ch  ->
                                let d = checkAcnType ch
                                addAcnChild tasPositions curState asn1TypeWithDependency ch d
                    | x1::xs     ->
                        match seq.children |> Seq.tryFind(fun c -> c.Name = x1) with
                        | None -> raise(SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (path |> Seq.StrJoin "."))))
                        | Some ch ->
                            match ch with
                            | Asn1Child ch -> checkSeqWithPath ch.Type xs
                            | _            -> raise(SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (path |> Seq.StrJoin "."))))
                | ReferenceType ref -> checkSeqWithPath ref.resolvedType path
                | _            -> raise(SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (path |> Seq.StrJoin "."))))

            match parSeq with
            | None  -> raise(SemanticError(x1.Location, (sprintf "Invalid reference '%s'" (path |> Seq.StrJoin "."))))
            | Some p ->
                checkSeqWithPath p path


let checkParamIsActuallyInteger (r:AstRoot) (prmMod0:StringLoc) (tasName0:StringLoc) =
    let rec checkParamIsActuallyInteger_aux (r:AstRoot) (prmMod:StringLoc) (tasName:StringLoc) =
        match r.Modules |> Seq.tryFind(fun z -> z.Name.Value = prmMod.Value) with
        | None      -> raise(SemanticError (prmMod.Location, (sprintf "No module defined with name '%s'"  prmMod.Value)))
        | Some md   ->
            match md.TypeAssignments |> Seq.tryFind(fun z -> z.Name.Value = tasName.Value) with
            | None      -> raise(SemanticError (tasName.Location, (sprintf "No type assignment defined with name '%s' in module '%s'"  tasName.Value prmMod.Value)))
            | Some ts   ->
                match ts.Type.Kind with
                | Integer   intInfo     -> intInfo
                | ReferenceType refInfo -> checkParamIsActuallyInteger_aux r refInfo.modName refInfo.tasName
                | _                     ->
                    raise(SemanticError (tasName.Location, (sprintf "Type assignment '%s' defined in module '%s' does not resolve to an Integer"  tasName.Value prmMod.Value)))
    checkParamIsActuallyInteger_aux r prmMod0 tasName0

let sizeReference (r:AstRoot) (tasPositions:Map<ReferenceToType,int>) (curState:AcnInsertedFieldDependencies) (parents: Asn1Type list) (t:Asn1Type) (sizeMin:BigInteger) (sizeMax:BigInteger) (visibleParameters:(ReferenceToType*AcnParameter) list)  (rp :RelativePath  option) (d:AcnDependencyKind) =
    match rp with
    | None      -> curState
    | Some (RelativePath path) ->
        let loc = path.Head.Location

        let checkParameter (p:AcnParameter) =
            match p.asn1Type with
            | AcnPrmInteger acnInt ->
                d
            | AcnPrmRefType  (mdName,tsName)    ->
                let intInfo = checkParamIsActuallyInteger r mdName tsName
                d

            | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting INTEGER got %s "  (p.asn1Type.ToString()))))
        let checkAcnType (c:AcnChild) =
            match c.Type with
            | AcnInteger    ai ->
                ai.checkIntHasEnoughSpace sizeMin sizeMax
                d
            | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting INTEGER got %s "  (c.Type.AsString))))

        // first check the sizeable type is a fixed size type (i.e. sizeMin = sizeMax). In this case emit a user error
//        match sizeMin = sizeMax with
//        | true  -> raise(SemanticError(loc, (sprintf "size acn property cannot be assigned to fixed size types. To fix this error remove 'size %s'." (path |> Seq.StrJoin "."))))
//        | false ->
        checkRelativePath tasPositions curState parents t visibleParameters   (RelativePath path) checkParameter checkAcnType


let checkChoicePresentWhen (r:AstRoot) (tasPositions:Map<ReferenceToType,int>) (curState:AcnInsertedFieldDependencies) (parents: Asn1Type list) (t:Asn1Type)  (ch:Choice) (visibleParameters:(ReferenceToType*AcnParameter) list)    =
    match ch.acnProperties.enumDeterminant with
    | Some _        ->
        //1 check that there is no child with present-when property
        match ch.children |> List.collect(fun z -> z.acnPresentWhenConditions) with
        | c1::_         -> raise(SemanticError(c1.location, (sprintf "‘present-when’ attribute cannot appear when ‘determinant’ is present in the parent choice type"  )))
        | []            -> curState
    | None          ->
        let childrenConditions = ch.children |>  List.filter(fun z -> z.acnPresentWhenConditions.Length > 0) |> List.map(fun z -> z.acnPresentWhenConditions |> List.map(fun pc -> (pc.kind, pc.relativePath.AsString, pc.valueAsString)) |> List.sortBy (fun (_,p,_) -> p) |> Seq.toList)
        let duplicateCondtions =  childrenConditions |> Seq.groupBy id |> Seq.map(fun (key, vals) -> Seq.length vals) |> Seq.filter(fun z -> z > 1) |> Seq.length
        match duplicateCondtions > 0 with
        | false     -> ()
        | true      -> raise(SemanticError(ch.acnLoc.Value, (sprintf "Duplicate condition found in choice alternatives" )))



        //2 check that all children have the same present when attributes
        match ch.children with
        | []        -> curState
        | c1::cs    ->
            let firstChildPresentWhenAttr = c1.acnPresentWhenConditions |> List.sortBy(fun z -> z.relativePath.AsString) |> List.map(fun z -> (z.kind, z.relativePath))
            cs |>
            Seq.iter(fun c ->
                let curChildPresentWhenAttr = c.acnPresentWhenConditions |> List.sortBy(fun z -> z.relativePath.AsString) |> List.map(fun z -> (z.kind, z.relativePath))
                match curChildPresentWhenAttr = firstChildPresentWhenAttr with
                | true  -> ()
                | false ->
                    match curChildPresentWhenAttr, firstChildPresentWhenAttr with
                    | [], []    -> ()
                    | [], (_,z)::_   -> raise(SemanticError(z.location, (sprintf "‘present-when’ attributes must appear in all alternatives" )))
                    | (_,z)::_,_     -> raise(SemanticError(z.location, (sprintf "‘present-when’ attributes must appear in all alternatives" ))) )


            let checkAcnPresentWhenConditionChoiceChild (curState:AcnInsertedFieldDependencies) (a:AcnPresentWhenConditionChoiceChild) =
                match a with
                | PresenceInt   ((RelativePath path),intVal) ->
                    let loc = path.Head.Location
                    let checkParameter (p:AcnParameter) =
                        match p.asn1Type with
                        | AcnPrmInteger _                   -> AcnDepPresence ((RelativePath path), ch)
                        | AcnPrmRefType    (prmMod, prmTas) ->
                            let intInfo = checkParamIsActuallyInteger r prmMod prmTas
                            let isWithinRange =
                                match intInfo.uperRange with
                                | Concrete   (a,b)   -> a <= intVal.Value && intVal.Value <= b
                                | NegInf     b       -> intVal.Value <= b
                                | PosInf     a       -> a <= intVal.Value
                                | Full               -> true
                            match isWithinRange with
                            | true  -> ()
                            | false -> raise(SemanticError(intVal.Location, (sprintf "Value '%A' is not within the allowed limits of type %s "  intVal.Value (p.asn1Type.ToString()))))
                            AcnDepPresence ((RelativePath path), ch)
                        | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting INTEGER got %s "  (p.asn1Type.ToString()))))
                    let rec checkAcnType (c:AcnChild) =
                        match c.Type with
                        | AcnInteger    intInfo ->
                            let isWithinRange =
                                match intInfo.uperRange with
                                | Concrete   (a,b)   -> a <= intVal.Value && intVal.Value <= b
                                | NegInf     b       -> intVal.Value <= b
                                | PosInf     a       -> a <= intVal.Value
                                | Full               -> true
                            match isWithinRange with
                            | true  -> ()
                            | false -> raise(SemanticError(intVal.Location, (sprintf "Value '%A' is not within the allowed limits"  intVal.Value )))
                            AcnDepPresence ((RelativePath path), ch)
                        | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting INTEGER got %s "  (c.Type.AsString))))
                    checkRelativePath tasPositions curState parents t visibleParameters   (RelativePath path)  checkParameter checkAcnType
                | PresenceStr   ((RelativePath path), strVal)->
                    let loc = path.Head.Location
                    let checkParameter (p:AcnParameter) =
                        match p.asn1Type with
                        | AcnPrmRefType (md,ts) ->
                            let actType = GetActualTypeByName r md ts
                            match actType.Kind with
                            | IA5String str ->
                                match  str.minSize.acn <= strVal.Value.Length.AsBigInt && strVal.Value.Length.AsBigInt <= str.maxSize.acn with
                                | true  -> AcnDepPresenceStr ((RelativePath path), ch, str)
                                | false -> raise(SemanticError(strVal.Location, (sprintf "Length of value '%s' is not within the expected range: i.e. %d not in (%A .. %A)"  strVal.Value  strVal.Value.Length str.minSize str.maxSize)))

                            | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting STRING got %s "  (p.asn1Type.ToString()))))
                        | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting STRING got %s "  (p.asn1Type.ToString()))))
                    let rec checkAcnType (c:AcnChild) =
                        match c.Type with
                        | AcnReferenceToIA5String    str ->
                            match  str.str.minSize.acn <= strVal.Value.Length.AsBigInt && strVal.Value.Length.AsBigInt <= str.str.maxSize.acn with
                            | true  -> AcnDepPresenceStr ((RelativePath path), ch, str.str)
                            | false -> raise(SemanticError(strVal.Location, (sprintf "Length of value '%s' is not within the expected range: i.e. %d not in (%A .. %A)"  strVal.Value  strVal.Value.Length str.str.minSize str.str.maxSize)))
                        | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting STRING got %s "  (c.Type.AsString))))
                    checkRelativePath tasPositions curState parents t visibleParameters   (RelativePath path)  checkParameter checkAcnType



            c1.acnPresentWhenConditions |> List.fold(fun ns pc -> checkAcnPresentWhenConditionChoiceChild ns pc ) curState



let choiceEnumReference (r:AstRoot) (tasPositions:Map<ReferenceToType,int>) (curState:AcnInsertedFieldDependencies) (parents: Asn1Type list) (t:Asn1Type) (ch:Choice) (visibleParameters:(ReferenceToType*AcnParameter) list)  (rp :RelativePath  option)  =
    let children = ch.children
    let checkEnumNamesConsistency loc (nmItems:NamedItem list) =
        match children.Length <> nmItems.Length with
        | true  -> raise(SemanticError(loc, (sprintf "expecting an enumerated field with names matching the choice names" )))
        | false ->
            let allNamesMatch = List.zip (children |> List.map(fun z -> z.Name.Value)) (nmItems |> List.map(fun z -> z.Name.Value)) |> Seq.forall(fun (x1,x2) -> x1 = x2)
            match allNamesMatch with
            | true  -> ()
            | false -> raise(SemanticError(loc, (sprintf "expecting an enumerated field with names matching the choice names" )))

    match rp with
    | None      -> curState
    | Some (RelativePath path) ->
        let loc = path.Head.Location
        let checkParameter (p:AcnParameter) =
            match p.asn1Type with
            | AcnPrmRefType (md,ts) ->
                let actType = GetActualTypeByName r md ts
                match actType.Kind with
                | Enumerated enm  ->
                    checkEnumNamesConsistency ts.Location enm.items
                    AcnDepChoiceDeterminant ({ReferenceToEnumerated.modName = md.Value; tasName = ts.Value; enm = enm} ,ch, t.id.lastItemIsOptional)
                | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting ENUMERATED got %s "  (p.asn1Type.ToString()))))
            | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting ENUMERATED got %s "  (p.asn1Type.ToString()))))
        let checkAcnType (c:AcnChild) =
            match c.Type with
            | AcnReferenceToEnumerated    enm ->
                checkEnumNamesConsistency c.Name.Location enm.enumerated.items
                AcnDepChoiceDeterminant ({ReferenceToEnumerated.modName = enm.modName.Value; tasName = enm.tasName.Value; enm = enm.enumerated}, ch, t.id.lastItemIsOptional)
            | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting ENUMERATED got %s "  (c.Type.AsString))))
        checkRelativePath tasPositions curState parents t visibleParameters   (RelativePath path) checkParameter checkAcnType

let rec private checkType (r:AstRoot) (tasPositions:Map<ReferenceToType,int>) (parents: Asn1Type list) (curentPath : ScopeNode list) (t:Asn1Type) (curState:AcnInsertedFieldDependencies) =
    //the visible parameters of a type are this type parameters if type has parameters or the
    //next parent with parameters
    let visibleParameters =
//        match parents@[t] |> List.rev |> List.filter(fun x -> not x.acnParameters.IsEmpty) with
//        | []    -> []
//        | p1::_ -> p1.acnParameters |> List.map(fun p -> (p1.id, p))
        (parents@[t])
        |> List.rev
        |> List.collect (fun p -> p.acnParameters |> List.map (fun prm -> (p.id, prm)))
        |> List.distinctBy (fun (_,prm) -> prm.name)

    match t.Kind with
    | Integer        _
    | Real           _
    | NullType       _
    | Boolean        _
    | Enumerated     _      -> curState
    | ObjectIdentifier   _ -> curState
    | TimeType      _      -> curState
    | IA5String      a
    | NumericString  a      ->
        match a.acnProperties.sizeProp with
        | Some (StrExternalField   relPath)    ->
            sizeReference r tasPositions curState parents t a.minSize.acn a.maxSize.acn visibleParameters (Some relPath) (AcnDepIA5StringSizeDeterminant (a.minSize, a.maxSize, a.acnProperties))
        | _          -> curState
    | OctetString    a      ->
        let rp =
            match a.acnProperties.sizeProp with
            | None -> None
            | Some (SzExternalField ef) -> Some ef
            | Some (SzNullTerminated _) -> None
        sizeReference r tasPositions curState parents t a.minSize.acn a.maxSize.acn visibleParameters rp (AcnDepSizeDeterminant (a.minSize, a.maxSize, a.acnProperties))
    | BitString      a      ->
        let rp =
            match a.acnProperties.sizeProp with
            | None -> None
            | Some (SzExternalField ef) -> Some ef
            | Some (SzNullTerminated _) -> None
        sizeReference r tasPositions curState parents t a.minSize.acn a.maxSize.acn visibleParameters rp (AcnDepSizeDeterminant (a.minSize, a.maxSize, a.acnProperties))
    | SequenceOf   seqOf    ->
        let rp =
            match seqOf.acnProperties.sizeProp with
            | None -> None
            | Some (SzExternalField ef) -> Some ef
            | Some (SzNullTerminated _) -> None

        let ns = sizeReference r tasPositions curState (parents) t seqOf.minSize.acn seqOf.maxSize.acn visibleParameters rp (AcnDepSizeDeterminant (seqOf.minSize, seqOf.maxSize, seqOf.acnProperties))
        checkType r tasPositions (parents@[t]) (curentPath@[SQF]) seqOf.child ns
    | Sequence   seq        ->
        seq.children |>
        List.choose (fun c -> match c with Asn1Child ac -> Some ac | AcnChild _ -> None) |>
        List.fold (fun ns ac ->
            let ns1 =
                match ac.Optionality with
                | Some (Optional opt)   ->
                    match opt.acnPresentWhen with
                    | None  -> ns
                    | Some   (PresenceWhenBool (RelativePath path)) ->
                        let loc = path.Head.Location
                        let checkParameter (p:AcnParameter) =
                            match p.asn1Type with
                            | AcnPrmBoolean _ -> AcnDepPresenceBool
                            | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting BOOLEAN got %s "  (p.asn1Type.ToString()))))
                        let rec checkAcnType (c:AcnChild) =
                            match c.Type with
                            | AcnBoolean    _ -> AcnDepPresenceBool
                            | _              -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting BOOLEAN got %s "  (c.Type.AsString))))
                        checkRelativePath tasPositions ns (parents@[t]) ac.Type visibleParameters   (RelativePath path)  checkParameter checkAcnType
                    | Some (PresenceWhenBoolExpression exp)  ->
                        let rec getChildResult (seq:Sequence) (RelativePath lp) =
                            match lp with
                            | []    -> raise(BugErrorException "empty relative path")
                            | x1::xs ->
                                match seq.children |> Seq.tryFind(fun c -> c.Name = x1) with
                                | None ->
                                    ValResultError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin ".")))
                                | Some ch ->
                                    match ch with
                                    | AcnChild ch  -> ValResultError(x1.Location, (sprintf "Invalid reference '%s'. Expecting an ASN.1 child" (lp |> Seq.StrJoin ".")))
                                    | Asn1Child ch  ->
                                        match ch.Optionality with
                                        | None  ->
                                            match ch.Type.ActualType.Kind with
                                            | Integer        _  -> ValResultOK (NonBooleanExpression IntExpType)
                                            | Real           _  -> ValResultOK (NonBooleanExpression RealExpType)
                                            | Boolean        _  -> ValResultOK (BoolExpression)
                                            | Sequence s when xs.Length > 1 -> getChildResult s (RelativePath xs)
                                            | _                 -> ValResultError(x1.Location, (sprintf "Invalid reference '%s'" (lp |> Seq.StrJoin ".")))
                                        | Some _        -> ValResultError(x1.Location, (sprintf "Optional component '%s' cannot be used in ACN present-when expressions" (lp |> Seq.StrJoin ".")))

                        let valResult = AcnGenericTypes.validateAcnExpression (fun lf -> getChildResult seq lf) exp
                        match valResult with
                        | ValResultOK   expType -> ns
                        | ValResultError (l,errMsg) -> raise(SemanticError(l, errMsg))
                | _                     -> ns
            checkType r tasPositions (parents@[t]) (curentPath@[SEQ_CHILD (ac.Name.Value, ac.Optionality.IsSome)]) ac.Type ns1
        ) curState
    | Choice ch ->
        let ns0 = checkChoicePresentWhen r tasPositions curState (parents) t ch visibleParameters
        let ns1 = choiceEnumReference r tasPositions ns0 (parents) t ch visibleParameters ch.acnProperties.enumDeterminant
        ch.children |>
        List.fold (fun ns ac -> checkType r tasPositions (parents@[t]) (curentPath@[SEQ_CHILD (ac.Name.Value, ac.Optionality.IsSome)])  ac.Type ns ) ns1
    | ReferenceType ref ->
        let dummy = ref.tasName
        let aaa = t.id.AsString

        let curState =
            match ref.encodingOptions with
            | None -> curState
            | Some props  ->

                let rp =
                    match props.acnEncodingClass with
                    | SZ_EC_FIXED_SIZE
                    | SZ_EC_LENGTH_EMBEDDED     _  -> None
                    | SZ_EC_ExternalField ef        -> Some ef
                    | SZ_EC_TerminationPattern _    -> None
                sizeReference r tasPositions curState parents t props.minSize.acn props.maxSize.acn visibleParameters rp (AcnDepSizeDeterminant_bit_oct_str_contain ref)

        let checkArgument (curState:AcnInsertedFieldDependencies) ((RelativePath path : RelativePath), (prm:AcnParameter)) =
            let loc = path.Head.Location
            let checkParameter (p:AcnParameter) =
                match p.asn1Type = prm.asn1Type with
                | true  -> AcnDepRefTypeArgument prm
                | false -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting %s got %s " (prm.asn1Type.ToString()) (p.asn1Type.ToString()))))
            let rec checkAcnType (c:AcnChild) =
                match c.Type, prm.asn1Type with
                | AcnInteger    _, AcnPrmInteger _   -> AcnDepRefTypeArgument prm
                | AcnNullType   _, AcnPrmNullType _  -> AcnDepRefTypeArgument prm
                | AcnBoolean    _, AcnPrmBoolean _   -> AcnDepRefTypeArgument prm
                | _             , AcnPrmRefType (md,ts)    -> AcnDepRefTypeArgument prm    //WARNING NOT CHECKED
                | _                                  -> raise(SemanticError(loc, (sprintf "Invalid argument type. Expecting %s got %s " (prm.asn1Type.ToString()) (c.Type.AsString))))
            checkRelativePath tasPositions  curState parents t visibleParameters   (RelativePath path)  checkParameter checkAcnType

        let acnLoc =
            match ref.acnArguments with
            | []        -> t.Location
            | arg1::_   -> arg1.location

        match ref.acnArguments.Length = ref.resolvedType.acnParameters.Length with
        | true  -> 
            let ziped = List.zip ref.acnArguments ref.resolvedType.acnParameters
            let ns = ziped |> List.fold(fun s c -> checkArgument s c) curState
            checkType r tasPositions parents curentPath ref.resolvedType ns

        | false ->
            curState
            //let errMgs = sprintf "Expecting %d ACN arguments, provide %d" ref.resolvedType.acnParameters.Length ref.acnArguments.Length
            //raise(SemanticError(acnLoc, errMgs))


let checkAst (r:AstRoot) =
    let rec GetReferences (t:Asn1Type) : ReferenceToType list =
        seq {
            yield t.id
            match t.ActualType.Kind with
            | SequenceOf(conType) ->  yield! GetReferences conType.child
            | Sequence seq ->
                for ch in seq.children do
                    match ch with
                    | Asn1Child a   -> yield! GetReferences a.Type
                    | AcnChild  a   -> yield a.id

            | Choice(ch)->
                for ch in ch.children do
                    yield! GetReferences ch.Type
            |_ -> ()
        } |> Seq.toList

    let allTasses =
        r.Files |>
            List.collect (fun f -> f.Modules) |>
            List.map (fun m -> m.TypeAssignments |> List.map(fun tas -> (m,tas)) ) |>
            List.collect id

    let positions =  allTasses

    let emptyState = {AcnInsertedFieldDependencies.acnDependencies=[]}

    let result =
        allTasses |>
        List.fold (fun ns (m,tas) ->
            let tasPositions = GetReferences tas.Type |> List.mapi(fun i ref -> (ref,i)) |> Map.ofList
            checkType r tasPositions [] [MD m.Name.Value; TA tas.Name.Value] tas.Type ns) emptyState

    result
