module createDefinitions

open System.Numerics
open FsUtils
open Ast
open System.IO
open VisitTree
open uPER
open CloneTree
open BackendAst

let rec createTypeDeclaration (t:Asn1Type)  (p:list<string>) (r:AstRoot) =
    let SizeableTypeUperRange() =
            match (GetTypeUperRange t.Kind t.Constraints r) with
            |Concrete(min, max)        -> min,max
            |Empty                    -> raise(SemanticError(t.Location, "SEQUENCE OF or OCTET STRING ended up with no (or invalid) constraints"))
            |_                         -> raise(BugErrorException "SEQUENCE OF or OCTET STRING etc has no constraints")
    let PrintChoiceSeqChild (child:ChildInfo) =
        {
            ChoiceOrSeqChild.name = (child.CName ProgrammingLanguage.C)
            typeDef = createTypeDeclaration child.Type (p@[child.Name.Value]) r
            arrayPostfix = (TypeArrayPostfix child.Type r)
        }

    match t.Kind with
    | Integer       -> 
        match (GetTypeUperRange t.Kind t.Constraints r) with
        | Concrete(a,b) when a >= 0I    ->  POS_INTEGER
        | Concrete(a,b)     ->  INTEGER
        | NegInf(b)         ->  INTEGER
        | PosInf(a)  when a >= 0I       ->  POS_INTEGER
        | PosInf(a)         ->  INTEGER
        | Empty | Full      ->  INTEGER
    | Boolean       -> BOOLEAN
    | Real          -> REAL
    | IA5String 
    | NumericString -> STRING
    | NullType      -> VOID_TYPE
    | BitString(_)  -> 
        let nMin, nMax = SizeableTypeUperRange()
        let bsInfo  = {BitStringTypeInfo.nBitLen = (TypeItemsCount t r); nBytesLen = (TypeCArrayItemsCount t r) }
        match (nMin=nMax) with
        | true  -> FIX_SIZE_BIT_STRING bsInfo
        | false -> VAR_SIZE_BIT_STRING bsInfo
    | OctetString   -> 
        let nMin, nMax = SizeableTypeUperRange()
        let nOctLen = TypeCArrayItemsCount t r
        match (nMin=nMax) with
        | true  -> FIX_SIZE_OCT_STRING nOctLen
        | false -> VAR_SIZE_OCT_STRING nOctLen
    | Enumerated(items)    ->
        let PrintNamedItem (it:NamedItem,value:BigInteger) =
            match it._value with
            | Some(vl)  -> (it.CEnumName r C), (Ast.GetValueAsInt vl r) 
            | None      -> (it.CEnumName r C), value 
        let result = 
                items |> Seq.mapi(fun i ch -> (ch, (BigInteger i)) ) |> Seq.toList
        ENUM (result |> List.map PrintNamedItem ) 
    | Choice(children)  ->
        let choiceTypeInfo = {
                ChoiceTypeInfo.choiceIDForNone = (TypeLongName p)+"_NONE"
                enmItems = children |> List.map(fun x -> (x.CName_Present C))
                children = children |> List.map PrintChoiceSeqChild
            }
        CHOICE choiceTypeInfo
    | Sequence(chldrn)  ->
        let children = chldrn |> List.filter(fun x -> not x.AcnInsertedField)
        let sequenceTypeInfo =  {
                SequenceTypeInfo.optChildren = children |> List.filter(fun x -> x.Optionality.IsSome) |> List.map(fun x -> x.CName ProgrammingLanguage.C) 
                children = children |> List.map PrintChoiceSeqChild 
            }
        SEQUENCE sequenceTypeInfo
    | SequenceOf(child) ->
        let nMin, nMax = SizeableTypeUperRange()
        let sequenceOfTypeInfo = {
            SequenceOfTypeInfo.typeDef = createTypeDeclaration child (p@["#"]) r
            length       = (TypeCArrayItemsCount t r) 
            arrayPostfix = (TypeArrayPostfix child r) 

        }
        match nMin=nMax with
        | true  -> FIX_SIZE_SEQUENCE_OF  sequenceOfTypeInfo
        | false -> VAR_SIZE_SEQUENCE_OF  sequenceOfTypeInfo
    | ReferenceType(m,tasName, _) -> 
        LOCAL_REFENCED_TYPE (GetTasCName tasName.Value r.TypePrefix)



let createTypeAss  (r:AstRoot) (acn:AcnTypes.AcnAstResolved) (f:Asn1File) (m:Asn1Module) (t:TypeAssignment) = 
    let errorCodes = []
    let sName = t.GetCName r.TypePrefix
    let nMaxBitsInACN, nMaxBytesInACN = Acn.RequiredBitsForAcnEncodingInt t.Type [m.Name.Value; t.Name.Value] r acn
    {
        TasDefition.sTypeDecl   = createTypeDeclaration t.Type [m.Name.Value; t.Name.Value] r
        sarrPostfix                 = TypeArrayPostfix t.Type r
        sName                       = sName
        nMaxBitsInPER               = uperGetMaxSizeInBitsAsInt t.Type.Kind t.Type.Constraints t.Type.Location r    
        nMaxBytesInPER              = uperGetMaxSizeInBytesAsInt t.Type.Kind t.Type.Constraints t.Type.Location r   
        nMaxBitsInACN               = nMaxBitsInACN
        nMaxBytesInACN              = nMaxBytesInACN
        nMaxBytesInXER              = XER_bl.GetMaxSizeInBytesForXER t.Type t.Name.Value r   
        sStar                       = (TypeStar t.Type r)            
        errorCodes                  = []
        isConstraintValidFnc        = {TasIsConstraintValid.funcName = sprintf "%s_IsConstraintValid" sName}
        isEqualFnc                  = {TasIsEqual.funcName = sprintf "%s_Equal" sName}
        inititalizeFnc              = {TasInititalize.funcName = sprintf "%s_Initialize" sName}
    }


let SortTypeAssignments (f:Asn1File) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) =
    let GetTypeDependencies (tas:TypeAssignment)  = 
        seq {
            for ch in (GetMySelfAndChildren tas.Type) do
                match ch.Kind with
                | ReferenceType(_, tasName, _)   -> yield tasName.Value; 
                | _                                 ->      ()
        } |> Seq.distinct |> Seq.toList

    let allNodes = f.TypeAssignments |> List.map( fun tas -> (tas.Name.Value, GetTypeDependencies tas))
    let importedTypes = 
        let thisFileModules = f.Modules |> List.map(fun x -> x.Name.Value)
        f.Modules |> 
        Seq.collect(fun m -> m.Imports) |>
        Seq.filter(fun m -> not (thisFileModules |> Seq.exists ((=) m.Name.Value) )) |>
        Seq.collect(fun imp -> imp.Types) |> 
        Seq.map(fun x -> x.Value) |> 
        Seq.distinct |> Seq.toList

    let independentNodes = allNodes |> List.filter(fun (_,list) -> List.isEmpty list) |> List.map(fun (n,l) -> n)
    let dependentNodes = allNodes |> List.filter(fun (_,list) -> not (List.isEmpty list) )
    let sortedTypeAss = 
        DoTopologicalSort (importedTypes @ independentNodes) dependentNodes 
            (fun c -> 
            SemanticError
                (emptyLocation, 
                 sprintf 
                     "Recursive types are not compatible with embedded systems.\nASN.1 grammar has cyclic dependencies: %A" 
                     c))
    seq {
        for tasName in sortedTypeAss do
            for m in f.Modules do
                for tas in m.TypeAssignments do
                    if tasName = tas.Name.Value then
                        yield (m,tas)

    } |> Seq.toList


let createDefinitionsFile  (r:AstRoot) (acn:AcnTypes.AcnAstResolved) (l:ProgrammingLanguage) (f:Asn1File) =
    let fileNameNoExtUpper = f.FileNameWithoutExtension.ToUpper()
    let allImportedModules = f.Modules |> Seq.collect(fun m -> m.Imports) |> Seq.map(fun imp -> imp.Name.Value) |> Seq.distinct
    let includedModules  = seq {   
        for file in r.Files do
            if file.FileName <> f.FileName then
                if file.Modules |> Seq.exists (fun m -> allImportedModules |> Seq.exists(fun x -> x = m.Name.Value)) then
                    yield file.FileNameWithoutExtension } |> Seq.toList 
    let sortedTas = SortTypeAssignments f r acn
    //let protos  = sortedTas |> Seq.map(fun (m,tas) -> PrintAcnProtos tas m f r acn )

    {
        DefinitionsFile.fileName = Path.Combine(f.FileNameWithoutExtension+l.DefinitionsFileExt)
        fileNameNoExtUpper = (ToC fileNameNoExtUpper)
        tases = sortedTas |> List.map (fun (m,tas) -> createTypeAss r acn f m tas ) 
    }

let DoWork (r:AstRoot) (acn:AcnTypes.AcnAstResolved) (l:ProgrammingLanguage) =
    r.Files |> Seq.map (createDefinitionsFile r acn l)  
