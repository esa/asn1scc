module BastAddAcnInsertFields

open System
open System.Numerics
open FsUtils
open BAst

let mapBTypeToBType (r:BAst.AstRoot) (t:BAst.Asn1Type) (acn:AcnTypes.AcnAst) (acnTypes:Map<AcnTypes.AbsPath,AcnTypes.AcnType>) initialSate=
    BAstFold.foldAsn1Type
        t
        initialSate

        (fun o newBase us -> {Integer.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; uperRange=o.uperRange; baseType = newBase; Location = o.Location}, us)
        Integer

        (fun o newBase us -> {Real.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; uperRange=o.uperRange; baseType = newBase; Location = o.Location}, us)
        Real

        (fun o newBase us -> {StringType.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; minSize = o.minSize; maxSize = o.maxSize; charSet = o.charSet; baseType = newBase; Location = o.Location}, us)
        IA5String

        (fun o newBase us -> {OctetString.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; minSize = o.minSize; maxSize = o.maxSize; baseType = newBase; Location = o.Location}, us)
        OctetString

        (fun o newBase us -> {NullType.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; baseType = newBase; Location = o.Location}, us)
        NullType

        (fun o newBase us -> {BitString.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; minSize = o.minSize; maxSize = o.maxSize; baseType = newBase; Location = o.Location}, us)
        BitString

        (fun o newBase us -> {Boolean.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; baseType = newBase; Location = o.Location}, us)
        Boolean

        (fun o newBase us -> {Enumerated.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; userDefinedValues = o.userDefinedValues; items = o.items; cons=o.cons; withcons = o.withcons; baseType = newBase; Location = o.Location}, us)
        Enumerated

        (fun childType o newBase us -> {SequenceOf.id = o.id; tasInfo = o.tasInfo; childType = childType; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; cons=o.cons; withcons = o.withcons; minSize = o.minSize; maxSize = o.maxSize; baseType = newBase; Location = o.Location}, us)
        SequenceOf

        //sequence
        (fun o newChild us -> {ChildInfo.Name = o.Name; chType = newChild; Optionality = o.Optionality; acnInsertetField    = o.acnInsertetField; Comments = o.Comments; Location = o.Location}, us)
        (fun children o newBase us -> 
            let acnAbsPath = o.id.AcnAbsPath
            let acnChildren = acn.Types |> Seq.filter(fun x -> x.TypeID.Length-1=acnAbsPath.Length && (x.TypeID|>Seq.take acnAbsPath.Length|>Seq.toList)=acnAbsPath) |> Seq.toList
            let newChildren =
                match acnChildren with
                | []    -> children
                | _     ->
                    seq {
                        for acnChild in acnChildren do
                            let acnChildName = acnChild.TypeID |> List.rev |> List.head
                            match children |> Seq.tryFind(fun x-> x.Name = acnChildName) with
                            |Some(asn1Child)    -> yield asn1Child
                            |None               ->
                                let newTypeId = o.id.getSeqChildId acnChildName
                                yield {
                                        ChildInfo.Name = acnChildName
                                        chType =  
                                                match acnChild.ImpMode with
                                                | AcnTypes.RecordField       -> raise(BugErrorException "Child exists in ASN.1")
                                                | AcnTypes.AcnTypeImplMode.LocalVariable(asn1Type) | AcnTypes.FunctionParameter(asn1Type) ->
                                                    match asn1Type with
                                                    | AcnTypes.Integer   -> 
                                                        Integer {Integer.id= newTypeId; tasInfo = None; Location = acnChild.Location; cons=[]; withcons=[]; baseType=None; uperMaxSizeInBits=0; uperMinSizeInBits=0;uperRange=uPER2.Full}
                                                    | AcnTypes.Boolean   -> 
                                                        Boolean {Boolean.id= newTypeId; tasInfo = None; Location = acnChild.Location; cons=[]; withcons=[]; baseType=None; uperMaxSizeInBits=0; uperMinSizeInBits=0}
                                                    | AcnTypes.NullType  -> 
                                                        NullType {NullType.id= newTypeId; tasInfo = None; Location = acnChild.Location; baseType=None; uperMaxSizeInBits=0; uperMinSizeInBits=0}
                                                    | AcnTypes.RefTypeCon(md,ts)  -> 
                                                        match r.TypeAssignments |> List.tryFind(fun x -> x.id.beginsWith md.Value ts.Value) with
                                                        | Some t -> t
                                                        | None   -> raise(SemanticError(ts.Location, sprintf "Unknown referenced type %s.%s " md.Value ts.Value))
                                        acnInsertetField = true
                                        Optionality = None
                                        Comments = acnChild.Comments |> Seq.toList
                                        Location = acnChild.Location
                                    }

                    } |>Seq.toList

            {Sequence.id = o.id; tasInfo = o.tasInfo; uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; children = newChildren; cons=o.cons; withcons = o.withcons; baseType = newBase; Location = o.Location}, us)
        Sequence

        //Choice
        (fun o newChild us -> {ChildInfo.Name = o.Name; chType = newChild; Optionality = o.Optionality; Comments = o.Comments; acnInsertetField=false; Location = o.Location}, us)
        (fun children o newBase us -> {Choice.id = o.id; tasInfo = o.tasInfo;  uperMaxSizeInBits= o.uperMaxSizeInBits; uperMinSizeInBits=o.uperMinSizeInBits; children = children; cons=o.cons; withcons = o.withcons; baseType = newBase; Location = o.Location}, us)
        Choice

type State = {
    currentTypes : Asn1Type list
}

let foldMap = GenericFold2.foldMap

let doWork (r:BAst.AstRoot) (acn:AcnTypes.AcnAst) : AstRoot=
    let initialState = {State.currentTypes = []}
    let acnTypes = acn.Types |> List.map(fun t -> t.TypeID, t) |> Map.ofList
    let newTypes,_ = 
        r.TypeAssignments |>
        foldMap (fun cs t ->
            let newType, newState = mapBTypeToBType r t acn acnTypes cs
            newType, {newState with currentTypes = newState.currentTypes@[newType]}
        ) initialState  
    {
        AstRoot.Files = r.Files
        args = r.args
        valsMap  = r.valsMap
        typesMap = newTypes |> List.map(fun t -> t.id, t) |> Map.ofList
        TypeAssignments = newTypes
        ValueAssignments = r.ValueAssignments
    }
