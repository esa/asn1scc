module CAstConstruction

open CAst
open FsUtils
open Constraints
open CAstAcnEncodingClasses


let mapBTypeToCType (r:BAst.AstRoot) (t:BAst.Asn1Type) (acn:AcnTypes.AcnAst) (acnTypes:Map<AcnTypes.AbsPath,AcnTypes.AcnType>) (acnParameters: AcnParameter list) (initialSate:State) =

    let getAcnParams (tid:ReferenceToType) =
        tid.BelongingTypeAssignment |> Option.map(fun ts -> acnParameters |> List.filter(fun x -> x.ModName = ts.moduName && x.TasName = ts.tasName)) |> Option.toList |> List.collect id


    BAstFold.foldAsn1Type
        t
        initialSate

        (fun o newBase us ->
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits = GetIntEncodingClass acn acnTypes o.Location o
            let ret= {Integer.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; uperRange=o.uperRange; baseType = newBase; Location = o.Location; acnMaxSizeInBits = acnMaxSizeInBits; acnMinSizeInBits = acnMinSizeInBits; acnEncodingClass = encClass; alignment = alignment}
            ret, {us with currentTypes = (Integer ret)::us.currentTypes})
        Integer

        (fun o newBase us ->
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits = GetRealEncodingClass acn acnTypes o.Location o
            let ret = {Real.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; uperRange=o.uperRange; baseType = newBase; Location = o.Location; acnMaxSizeInBits = acnMaxSizeInBits; acnMinSizeInBits = acnMinSizeInBits; acnEncodingClass = encClass; alignment= alignment}
            ret, {us with currentTypes = (Real ret)::us.currentTypes})
        Real

        (fun o newBase us ->
            let acnParams = getAcnParams o.id
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits, nus = GetStringEncodingClass acn acnTypes o.Location o acnParams us
            let ret = {StringType.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; minSize = o.minSize; maxSize = o.maxSize; charSet = o.charSet; baseType = newBase; Location = o.Location; acnMaxSizeInBits = acnMaxSizeInBits; acnMinSizeInBits = acnMinSizeInBits; acnEncodingClass = encClass; alignment=alignment}
            ret, {nus with currentTypes = (IA5String ret)::nus.currentTypes} )
        IA5String

        (fun o newBase us ->
            let acnParams = getAcnParams o.id
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits, nus = GetOctetStringEncodingClass acn acnTypes o.Location o acnParams us
            let ret = {OctetString.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; minSize = o.minSize; maxSize = o.maxSize; baseType = newBase; Location = o.Location; acnMaxSizeInBits = 0; acnMinSizeInBits = 0; acnEncodingClass = encClass; alignment=alignment}
            ret, {nus with currentTypes = (OctetString ret)::nus.currentTypes})
        OctetString

        (fun o newBase us ->
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits = GetNullTypeEncodingClass acn acnTypes o.Location o
            let ret = {NullType.id = o.id;  tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; baseType = newBase; Location = o.Location; acnMaxSizeInBits = 0; acnMinSizeInBits = 0; acnEncodingClass = encClass; alignment = alignment}
            ret, {us with currentTypes = (NullType ret)::us.currentTypes})
        NullType

        (fun o newBase us ->
            let acnParams = getAcnParams o.id
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits, nus = GetBitStringEncodingClass acn acnTypes o.Location o acnParams us
            let ret = {BitString.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; minSize = o.minSize; maxSize = o.maxSize; baseType = newBase; Location = o.Location; acnMaxSizeInBits = 0; acnMinSizeInBits = 0; acnEncodingClass = encClass; alignment = alignment}
            ret, {nus with currentTypes = (BitString ret)::nus.currentTypes})
        BitString

        (fun o newBase us ->
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits = GetBooleanTypeEncodingClass acn acnTypes o.Location o
            let ret = {Boolean.id = o.id;  tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; baseType = newBase; Location = o.Location; acnMaxSizeInBits = 0; acnMinSizeInBits = 0; acnEncodingClass = encClass; alignment = alignment}
            ret, {us with currentTypes = (Boolean ret)::us.currentTypes})
        Boolean

        (fun o newBase us ->
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits = GetEnumeratedEncodingClass acn acnTypes o.Location o
            let ret = {Enumerated.id = o.id;  tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; userDefinedValues = o.userDefinedValues; items = o.items; cons=o.cons; withcons = o.withcons; baseType = newBase; Location = o.Location; acnMaxSizeInBits = 0; acnMinSizeInBits = 0; enumEncodingClass = encClass; alignment = alignment}
            ret, {us with currentTypes = (Enumerated ret)::us.currentTypes})
        Enumerated

        (fun childType o newBase us ->
            let acnParams = getAcnParams o.id
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits, nus = GetSequenceOfEncodingClass acn acnTypes o.Location  o childType.acnMinSizeInBits childType.acnMaxSizeInBits acnParams us
            let ret = {SequenceOf.id = o.id; tasInfo = o.tasInfo; childType = childType; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; minSize = o.minSize; maxSize = o.maxSize; baseType = newBase; Location = o.Location; acnMaxSizeInBits = 0; acnMinSizeInBits = 0; acnEncodingClass = encClass; alignment = alignment}
            ret, {nus with currentTypes = (SequenceOf ret)::nus.currentTypes})
        SequenceOf

        //sequence
        (fun o newChild us ->
            let acnParams = getAcnParams newChild.id
            let newOptionality, nus = GetSeqChildOptionality acn  acnTypes o.Location newChild o.Optionality (acnParams: AcnParameter list) (us:State)
            {SeqChildInfo.name = o.Name; chType = newChild; optionality = newOptionality; acnInsertedField = o.acnInsertedField; comments = o.Comments; c_name = ToC o.Name}, nus)
        (fun children o newBase us ->
            let acnProps = getAcnProps acnTypes o.id
            let alignment, alignmentSize = GetAlignment acnProps
            let bitMaskSize = seqAcnOptionalChildrenHandledLikeuPER children |> Seq.length
            let reqChildren = children |> List.filter(fun c -> match c.optionality with Some (Optional _) -> false | _ -> true)
            let acnMaxSizeInBits = alignmentSize + bitMaskSize + (children |> List.map(fun c -> c.chType.acnMaxSizeInBits) |> List.sum)
            let acnMinSizeInBits = alignmentSize + bitMaskSize + (reqChildren |> List.map(fun c -> c.chType.acnMinSizeInBits) |> List.sum)
            let ret = {Sequence.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; children = children; cons=o.cons; withcons = o.withcons; baseType = newBase; Location = o.Location; acnMaxSizeInBits = acnMaxSizeInBits; acnMinSizeInBits = 0; alignment=alignment}
            ret, {us with currentTypes = (Sequence ret)::us.currentTypes})
        Sequence

        //Choice
        (fun o newChild us ->
            let acnParams = getAcnParams newChild.id
            let presenceIsHandleByExtField, nus = getChildLinks acn  acnTypes o.Location newChild  acnParams us
            {ChChildInfo.name = o.Name; chType = newChild; comments = o.Comments; presenceIsHandleByExtField=presenceIsHandleByExtField; presentWhenName = (ToC o.Name) + "_PRESENT"; c_name = ToC o.Name}, nus)
        (fun children o newBase us ->
            let acnParams = getAcnParams o.id
            let encClass, alignment, acnMinSizeInBits, acnMaxSizeInBits, nus = GetChoiceEncodingClass acn  acnTypes o.Location o children acnParams us
            let choiceIDForNone =  ToC (o.id.AcnAbsPath.Tail.StrJoin("_").Replace("#","elem")) + "_NONE"

            let ret = {Choice.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; children = children; cons=o.cons; withcons = o.withcons; baseType = newBase; Location = o.Location; choiceIDForNone = choiceIDForNone; acnMaxSizeInBits = acnMaxSizeInBits; acnMinSizeInBits = acnMinSizeInBits; acnEncodingClass = encClass; alignment = alignment}
            ret, {us with currentTypes = (Choice ret)::us.currentTypes})
        Choice



let foldMap = GenericFold2.foldMap



let mapAcnPathToReferenceToType (newTypeAssignments:Asn1Type list) (absPath : AcnTypes.AbsPath) =
    let rec createFromType (t:Asn1Type) (acnRestPath:string list) =
        match acnRestPath with
        | []                -> t.id
        | childName::rest       ->
            match t with
            | Sequence sq   ->
                match sq.children |> Seq.tryFind (fun x -> x.name = childName) with
                | None          -> raise(BugErrorException(sprintf "invalid path %s. Broken part is '%s'" (absPath |> Seq.StrJoin ".") childName))
                | Some child    -> createFromType child.chType rest
            | Choice sq   ->
                match sq.children |> Seq.tryFind (fun x -> x.name = childName) with
                | None          -> raise(BugErrorException(sprintf "invalid path %s. Broken part is '%s'" (absPath |> Seq.StrJoin ".") childName))
                | Some child    -> createFromType child.chType rest
            | SequenceOf sqof  when childName = "#" -> createFromType sqof.childType rest
            | SequenceOf sqof  ->  raise(BugErrorException(sprintf "invalid path %s. Sequence of cannot have child with name '%s' " (absPath |> Seq.StrJoin ".") childName ))
            | _                 -> raise(BugErrorException(sprintf "Non composite types cannot have children" ))
    match absPath with
    | []
    | _::[] -> raise(BugErrorException(sprintf "invalid path %s." (absPath |> Seq.StrJoin ".") ))
    | mdname::tsname::rest  ->
        let tas = ReferenceToType((GenericFold2.MD mdname)::(GenericFold2.TA tsname)::[])
        match newTypeAssignments |> Seq.tryFind(fun x -> x.id = tas) with
        | None      -> raise(BugErrorException(sprintf "invalid path %s." (absPath |> Seq.StrJoin ".") ))
        | Some t    -> createFromType t rest


let mapBAstToCast (r:BAst.AstRoot) (acn:AcnTypes.AcnAst) : AstRoot=
    let initialState = {State.currentTypes = []}
    let acnConstants    = acn.Constants
    let acnParameters   = acn.Parameters |> List.map(fun p -> {ModName = p.ModName;TasName = p.TasName; Name = p.Name; Asn1Type = p.Asn1Type;Location=p.Location})
    let acnTypes = acn.Types |> List.map(fun t -> t.TypeID, t) |> Map.ofList
    let newTypeAssignments, finalState =
        r.TypeAssignments |>
        foldMap (fun cs t ->
            let newType, newState = mapBTypeToCType r t acn acnTypes acnParameters cs
            newType, {newState with currentTypes = newState.currentTypes@[newType]}
        ) initialState
    let newTypes = finalState.currentTypes
    let newTypesMap = newTypes |> List.map(fun t -> t.id, t) |> Map.ofList
    let acnLinks    =
        acn.References |>
        List.map(fun l ->
            let decTypeId = mapAcnPathToReferenceToType newTypeAssignments l.TypeID
            let acnParams = acnParameters |> List.filter(fun x -> x.ModName = l.TypeID.Head && x.TasName = l.TypeID.Tail.Head)
            let decType =
                match newTypesMap.TryFind decTypeId with
                | Some t -> t
                | None   -> raise(SemanticError (l.Location, sprintf "Invalid Reference '%s'" (l.TypeID.ToString()) ))

            let determinantId  =
                let parentId = decTypeId.parentTypeId
                let determinantId childRelPath =
                    let determinantId = parentId |> Option.map (fun x -> x.appendLongChildId childRelPath)
                    match determinantId with
                    | None                  -> raise(SemanticError (l.Location, sprintf "Invalid Reference '%s'" (l.LongRef.ToString()) ))
                    | Some determinantId    -> determinantId

                let rec determinantId2 (parentId: ReferenceToType option) (firstPartOfLongPath:string) (restPartOfLongPath:string list) =
                    //parentId |> Option.map (fun x -> x.appendLongChildId childRelPath)
                    match parentId with
                    | None                  -> raise(SemanticError (l.Location, sprintf "Invalid Reference '%s'" (l.LongRef.ToString()) ))
                    | Some parentId    ->
                        match newTypesMap.TryFind parentId with
                        | None  -> raise(SemanticError (l.Location, sprintf "Invalid Reference '%s'" (l.LongRef.ToString()) ))
                        | Some parSq ->
                            match parSq with
                            | Sequence seq ->
                                match seq.children |> Seq.tryFind(fun c -> c.name = firstPartOfLongPath) with
                                | Some child    -> parentId.appendLongChildId (firstPartOfLongPath::restPartOfLongPath)
                                | None  ->
                                    // no children found => Try my parents scope
                                    determinantId2 (seq.id.parentTypeId) firstPartOfLongPath restPartOfLongPath
                            | _     -> raise(SemanticError (l.Location, sprintf "Invalid Reference '%s'" (l.LongRef.ToString()) ))



                match l.LongRef with
                | []            -> raise(SemanticError (l.Location,"Invalid Reference (empty path)" ))
                | fldName::[]   ->
                    match acnParams |> Seq.tryFind(fun p -> p.Name = fldName) with
                    | Some p    ->
                        match decTypeId.BelongingTypeAssignment with
                        | Some tasId    -> tasId.id.getParamId p.Name
                        | None          -> raise(SemanticError (l.Location,"Invalid type id" ))
                    | None      -> determinantId l.LongRef
                | firstPartOfLongPath::restPartOfLongPath             -> determinantId2 parentId firstPartOfLongPath restPartOfLongPath

            match l.Kind with
            | AcnTypes.RefTypeArgument prmName   -> {AcnLink.decType = decType; determinant = determinantId ; linkType = RefTypeArgument  prmName}
            | AcnTypes.SizeDeterminant           -> {AcnLink.decType = decType; determinant = determinantId ; linkType = SizeDeterminant}
            | AcnTypes.PresenceBool              -> {AcnLink.decType = decType; determinant = determinantId ; linkType = PresenceBool}
            | AcnTypes.PresenceInt intCon        -> {AcnLink.decType = decType; determinant = determinantId ; linkType = PresenceInt  (AcnTypes.EvaluateConstant acnConstants intCon)}
            | AcnTypes.PresenceStr strCon        -> {AcnLink.decType = decType; determinant = determinantId ; linkType = PresenceStr  strCon}
            | AcnTypes.ChoiceDeterminant       -> {AcnLink.decType = decType; determinant = determinantId ; linkType = ChoiceDeterminant})

    {
        AstRoot.Files = r.Files
        args = r.args
        valsMap  = r.valsMap
        typesMap = newTypesMap
        TypeAssignments = newTypeAssignments
        ValueAssignments = r.ValueAssignments
        acnParameters = acnParameters
        acnConstants = acnConstants
        acnLinks = acnLinks
    }

