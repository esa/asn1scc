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

module spark_spec
open System.Numerics
open FsUtils
open CommonTypes
open Ast
open System.IO
open VisitTree
open uPER
open CloneTree
open spark_utils






type State = {
    nErrorCode:int
}


let rec PrintType (t:Asn1Type) (path:list<string>) (parent:option<Asn1Type>) (tas:Assignment, m:Asn1Module, r:AstRoot) (state:State)=
    let result =
        match t.Kind with
        |Integer    -> 
            match (GetTypeUperRange t.Kind t.Constraints r) with
            |Concrete(min, max)        -> ss.Declare_Integer_min_max min max
            |NegInf(max)               -> ss.Declare_Integer_negInf max
            |PosInf(min)               -> ss.Declare_Integer_posInf min
            |Full                      -> ss.Declare_Integer()
            |Empty                     -> ss.Declare_Integer_Empty()
        |ReferenceType(modName, tasName, _)    ->
            match modName = m.Name with
            |true  -> ss.Declare_Reference1 (GetTasCName tasName.Value r.TypePrefix)
            |false  -> ss.Declare_Reference2 (ToC modName.Value) (GetTasCName tasName.Value r.TypePrefix)
        |Boolean    -> ss.Declare_BOOLEAN()
        |Real       -> ss.Declare_REAL()
        |NullType   -> ss.Declare_NULL()
        |_          -> raise(BugErrorException (sprintf "%A tt cannot appear inline" t.Kind))
    result, state


type ErrInfoState = {
    nGlobalErrorCode:int
    errorCodes:list<string*int*string>
}


let PrintTasDeclaration (t:TypeAssignment) (m:Asn1Module) (r:AstRoot) (state:State) =
    let SizeableTypeUperRange() =
            match (GetTypeUperRange t.Type.Kind t.Type.Constraints r) with
            |Concrete(min, max)        -> min,max
            |_                         -> raise(BugErrorException "SEQUENCE OF or OCTET STRING etc has no constraints")
    let sName = t.GetCName r.TypePrefix
    match t.Type.Kind with
    | Integer | Real  | Boolean | NullType |ReferenceType(_)             -> 
        let sTypeDecl, s1 = PrintType t.Type [m.Name.Value; t.Name.Value] None (TypeAssignment t,m,r) state
        ss.PRIMITIVE_tas_decl sName sTypeDecl, s1
    |Sequence(children)   ->
        let optionalChildren = children |> Seq.filter(fun c -> c.Optionality.IsSome) |> Seq.map(fun c -> ss.SEQUENCE_tas_decl_child_bit (c.CName ProgrammingLanguage.Spark))
        let printChild (s:State) (c:ChildInfo) =
            let typeDecl,s1 = PrintType c.Type [m.Name.Value; t.Name.Value; c.Name.Value] (Some t.Type) (TypeAssignment t,m,r) s
            ss.SEQUENCE_tas_decl_child (c.CName ProgrammingLanguage.Spark) typeDecl, s1 
        let arrChildren, newState =   children |> List.filter(fun c -> not c.AcnInsertedField) |> foldMap printChild state
        ss.SEQUENCE_tas_decl sName arrChildren optionalChildren,  newState
    |Choice(children)  -> 
        let printChild (s:State) (c:ChildInfo) =
            let typeDecl,s1 = PrintType c.Type [m.Name.Value; t.Name.Value; c.Name.Value] (Some t.Type) (TypeAssignment t,m,r) s
            ss.CHOICE_tas_decl_child sName (c.CName ProgrammingLanguage.Spark) typeDecl (c.CName_Present Spark), s1 
        let arrChildren, newState =   children |> foldMap printChild state
        let arrChPresent = children |> List.map(fun c -> (c.CName_Present  Spark))
        let nIndexMax = BigInteger ((Seq.length children)-1)
        ss.CHOICE_tas_decl sName arrChildren arrChPresent nIndexMax, state
    |Enumerated(items)  ->
        let orderedItems =
            match items |> Seq.exists(fun itm -> itm._value.IsNone) with
            | true  ->  items 
            | false ->  items |> Seq.sortBy(fun x -> Ast.GetValueAsInt x._value.Value r ) |> Seq.toList

        let arrsEnumNames = 
            orderedItems |> Seq.map(fun it -> (it.CEnumName r Spark))

        let printNamedItem (idx:int) (itm:NamedItem) =
            match itm._value with
            | Some(vl)  -> ss.ENUMERATED_tas_decl_item (itm.CEnumName r Spark) (Ast.GetValueAsInt vl r) 
            | None      -> ss.ENUMERATED_tas_decl_item (itm.CEnumName r Spark) (BigInteger idx)
        let arrsEnumNamesAndValues =
            orderedItems |> Seq.mapi printNamedItem
        let nIndexMax = BigInteger ((Seq.length items)-1)
        ss.ENUMERATED_tas_decl sName arrsEnumNames arrsEnumNamesAndValues nIndexMax, state
    |SequenceOf(c)  ->
        let nMin, nMax = SizeableTypeUperRange()
        let typeDecl,s1 =  PrintType c [m.Name.Value; t.Name.Value; "#"] (Some t.Type) (TypeAssignment t,m,r) state
        ss.SEQUENCE_OF_tas_decl sName nMin nMax (nMin=nMax) typeDecl,s1
    |IA5String |NumericString  ->
        let nMin, nMax = SizeableTypeUperRange()
        let alphaSet = GetTypeUperRangeFrom (t.Type.Kind, t.Type.Constraints, r) |> Array.map(fun x -> BigInteger(System.Convert.ToInt32 x))
        ss.IA5STRING_OF_tas_decl sName nMin nMax (nMax + 1I) alphaSet, state        
    |BitString  ->
        let nMin, nMax = SizeableTypeUperRange()
        ss.BIT_STRING_tas_decl sName nMin nMax (nMin=nMax), state        
    |OctetString                   -> 
        let nMin, nMax = SizeableTypeUperRange()
        ss.OCTET_STRING_tas_decl sName nMin nMax (nMin=nMax), state        


//vistor function
let OnType_collerErrInfo (t:Asn1Type) (path:list<string>) (parent:option<Asn1Type>) (ass:Assignment) (m:Asn1Module) (r:AstRoot) (state:ErrInfoState) = 
    let s0 = 
        match (GetTypeConstraintsErrorCode t.Constraints path r) with
        | None  -> state
        | Some(errCodeName) ->
            let errCodeValue = state.nGlobalErrorCode + 1
            let comment = (t.Constraints |> Seq.map PrintAsn1.PrintConstraint).StrJoin("").Replace("\r","").Replace("\n","")
            {nGlobalErrorCode = errCodeValue; errorCodes = (errCodeName,errCodeValue,comment)::state.errorCodes}
    let s1 = 
        match t.Kind with
        | Enumerated(_)     -> 
            let errCodeName = GetEnumErrorCode path r
            let errCodeValue = s0.nGlobalErrorCode + 1
            {nGlobalErrorCode = errCodeValue; errorCodes = (errCodeName,errCodeValue,"")::s0.errorCodes}
        | Choice(_)         ->
            let errCodeName = GetChoiceErrorCode path r
            let errCodeValue = s0.nGlobalErrorCode + 1
            {nGlobalErrorCode = errCodeValue; errorCodes = (errCodeName,errCodeValue,"")::s0.errorCodes}
        | _                 -> s0
    let s2 =
        match t.Kind with
        |Sequence(children) | Choice(children)  ->
            let handleChild (state:ErrInfoState) (c:ChildInfo) = 
                match (GetChildInfoErrorCode c (path@[c.Name.Value]) r) with
                | Some(errCodeName) ->
                    let errCodeValue = state.nGlobalErrorCode + 1
                    {nGlobalErrorCode = errCodeValue; errorCodes = (errCodeName,errCodeValue,"")::state.errorCodes}
                | None  -> state
            children |> Seq.fold handleChild s1
        | _         -> s1
    s2

let DeclateAcnAsn1Type (t:AcnTypes.AcnAsn1Type) (m:Asn1Module) (r:AstRoot) =
    match t with
    | AcnTypes.Integer    -> ss.Declare_spark_Integer()
    | AcnTypes.Boolean    -> ss.Declare_BOOLEAN()
    | AcnTypes.NullType   -> raise(BugErrorException "")
    | AcnTypes.RefTypeCon(md,ts) -> 
        match md.Value = m.Name.Value with
        |true  -> ss.Declare_Reference1 (GetTasCName ts.Value r.TypePrefix)   
        |false  -> ss.Declare_Reference2 (ToC md.Value) (GetTasCName ts.Value r.TypePrefix)   

let PrintParamType (p:AcnTypes.AcnParameter) (m:Asn1Module) (r:AstRoot) =
    DeclateAcnAsn1Type p.Asn1Type m r

let PrintExtracAcnParams (p:AcnTypes.AcnParameter) (m:Asn1Module) (r:AstRoot) codec =
    let prmType = PrintParamType p m r
    match p.Mode, codec with
    | AcnTypes.DecodeMode, Encode        -> None
    | AcnTypes.DecodeMode, Decode        -> Some (sa.PrintParam (ToC p.Name) "IN" prmType)
    | AcnTypes.EncodeDecodeMode, Encode  -> Some (sa.PrintParam (ToC p.Name) "IN" prmType)
    | AcnTypes.EncodeDecodeMode, Decode  -> Some (sa.PrintParam (ToC p.Name) "OUT" prmType)


(*
Emits the update procedure prototypes for the DecIn parameterized ACN types
E.g.
PROCEDURE assert_PACKET_DATA_FIELD_ACN_Encode_update_fldHeaderPresent(val : IN assert_PACKET_DATA_FIELD; fldHeaderPresent: OUT spark_asn1_rtl.Asn1Boolean);
--# derives fldHeaderPresent from val;
*)
let Acn_EmitUpdate_param_function_prototype(tas:TypeAssignment) (m:Asn1Module) (r:AstRoot)  (acn:AcnTypes.AcnAstResolved)  =
    let parms = acn.Parameters |> List.filter(fun p -> p.ModName = m.Name.Value && p.TasName = tas.Name.Value && p.Mode = AcnTypes.DecodeMode)
    let Update_param_Function (p:AcnTypes.AcnParameter) =
        let bHasResult = acn_backend_logic.Update_param_function_requires_result p.Name tas m r acn
        let prmName = ToC p.Name
        let prmType = PrintParamType p m r
        let sTasName = GetTasCName tas.Name.Value r.TypePrefix
        ss.Acn_update_param_protorype sTasName bHasResult prmName prmType 
    parms |> Seq.map Update_param_Function |> Seq.toList


let PrintTypeAss (t:TypeAssignment) (m:Asn1Module) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) (state:State) = 
    let sName = t.GetCName r.TypePrefix
    let sTasDecl,s1 = PrintTasDeclaration t m r state
    let nMaxBitsInPER = uperGetMaxSizeInBitsAsInt t.Type.Kind t.Type.Constraints t.Type.Location r
    let nMaxBytesInPER = uperGetMaxSizeInBytesAsInt t.Type.Kind t.Type.Constraints t.Type.Location r
    let nMaxBitsInACN, nMaxBytesInACN = Acn.RequiredBitsForAcnEncodingInt t.Type [m.Name.Value; t.Name.Value] r acn
    let GetErrorCodes (state:State) = 
        let retState = VisitType (RemoveWithComponents t.Type r)[m.Name.Value; t.Name.Value] None (TypeAssignment t)  m r {DefaultVisitors with visitType=OnType_collerErrInfo} {nGlobalErrorCode = state.nErrorCode; errorCodes = []}
        let errorCodes = retState.errorCodes |>  Seq.map (fun (sErrName, nErrCode, sComment) -> ss.PrintErrorCode sErrName (BigInteger nErrCode) sComment)
        errorCodes, {State.nErrorCode = retState.nGlobalErrorCode}
    let errorCodes, s2 = GetErrorCodes s1
    let bGenIsValidFunc = HasValidateFunc t.Type r
    let bGenEqual = r.GenerateEqualFunctions
    let print_encoding_protory (enc:Asn1Encoding) =
        match enc with
        | UPER  -> 
            let kDependedsOnValue = SparkDeps.KDependsOnValue_uper t.Type r
            let kIndexDependsOnData = SparkDeps.KIndexDependsOnData_uper t.Type r
            let decodingResultDependsOnData = SparkDeps.DecodingResultDependsOnData_uper t.Type r
            ss.UPER_encPrototypes sName  (nMaxBitsInPER = 0I) kDependedsOnValue (UperEncodeFuncRequiresResult t.Type r ) decodingResultDependsOnData kIndexDependsOnData
        | ACN   -> 
            let path = [m.Name.Value; t.Name.Value]
            let kDependedsOnValue = SparkDeps.KDependsOnValue_acn t.Type path r acn
            let kIndexDependsOnData = SparkDeps.KIndexDependsOnData_acn t.Type path r acn
            let decodingResultDependsOnData = SparkDeps.DecodingResultDependsOnData_acn t.Type path r acn

            let myParams = acn.Parameters |> List.filter(fun p -> p.TasName=t.Name.Value && p.ModName=m.Name.Value)
            let EncPrms = myParams |> Seq.choose(fun p -> PrintExtracAcnParams p m r Encode)
            let DecPrms = myParams |> Seq.choose(fun p -> PrintExtracAcnParams p m r Decode)
            let DecInPrmsNames = myParams |> Seq.choose(fun p -> match p.Mode with AcnTypes.DecodeMode -> Some (ToC p.Name) | _ -> None)
            let EncDecInOutPrmsNames = myParams |> Seq.choose(fun p -> match p.Mode with AcnTypes.EncodeDecodeMode -> Some (ToC p.Name) | _ -> None)
            let EncDecInOutPrmsNames_noBools = myParams |> Seq.filter(fun p -> SparkDeps.KDependsOnValue_acn (AcnAsn1Type2Asn1Type p.Asn1Type) [p.ModName; p.TasName; p.Name] r  acn (*p.Asn1Type <> AcnTypes.AcnAsn1Type.Boolean*)) |> Seq.choose(fun p -> match p.Mode with AcnTypes.EncodeDecodeMode -> Some (ToC p.Name) | _ -> None)
            let arrsUpdatePrototypes = Acn_EmitUpdate_param_function_prototype t m r acn
            ss.ACN_encPrototypes sName (nMaxBitsInACN = 0I) kDependedsOnValue (AcnEncodeFuncRequiresResult t.Type r acn) decodingResultDependsOnData kIndexDependsOnData EncPrms DecPrms DecInPrmsNames EncDecInOutPrmsNames EncDecInOutPrmsNames_noBools arrsUpdatePrototypes
        | BER   -> ""
        | XER   -> ""
    let arrsEncPrototypes = r.Encodings |>Seq.map print_encoding_protory 
    let isComplex = match t.Type.Kind with
                    | Integer
                    | Real
                    | NullType
                    | IA5String
                    | NumericString
                    | Boolean
                    | Enumerated(_)
                    | ReferenceType(_)  -> false
                    | _ -> true
    let result =
        ss.PrintTypeAssignment sName sTasDecl nMaxBitsInPER nMaxBytesInPER nMaxBitsInACN nMaxBytesInACN errorCodes bGenIsValidFunc bGenEqual arrsEncPrototypes isComplex
    result, s2

let PrintValueAss (v:ValueAssignment) (m:Asn1Module) (r:AstRoot) (state:State)= 
    let sName = v.ada_name 
    let sTypeDecl, s1 = PrintType v.Type [m.Name.Value; v.Name.Value] None (ValueAssignment v,m,r) state
    let sValue = spark_variables.PrintAsn1Value v.Value true false v.Type (sTypeDecl,0) m r 
    match (IsOrContainsChoice v.Type r) with
    |false  -> ss.PrintValueAssignment sName sTypeDecl sValue, s1
    |true   -> ss.PrintValueAssignment_Choice sName sTypeDecl sValue, s1


let SortTypeAssignments (m:Asn1Module) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) =
    let GetTypeDependencies (tas:TypeAssignment)  = 
        seq {
            for (acnInsertedType, ch) in (GetMySelfAndChildren2 tas.Type) do
                match ch.Kind with
                | ReferenceType(moduName, tasName, _)   -> 
                    match m.Name.Value = moduName.Value with
                    | true  -> yield (false, tasName.Value) 
                    | false -> yield (acnInsertedType, tasName.Value) 
                | _                                 ->      ()
            for acnpar in acn.Parameters do
                if acnpar.ModName = m.Name.Value && acnpar.TasName = tas.Name.Value then
                    match acnpar.Asn1Type with
                    | AcnTypes.Integer                      -> ()
                    | AcnTypes.Boolean                      -> ()
                    | AcnTypes.NullType                     -> ()
                    | AcnTypes.RefTypeCon(mod_ref,tas_ref)  ->  
                        match m.Name.Value = mod_ref.Value with
                        | true  -> yield (false, tas_ref.Value) 
                        | false -> yield (true, tas_ref.Value) 

        } |> Seq.distinct |> Seq.toList

    
    let allNodes = m.TypeAssignments |> List.map( fun tas -> (tas.Name.Value, GetTypeDependencies tas))
    let implicitImportedTypes = allNodes |> List.collect snd |> List.filter(fun (a,_) -> a) |> List.map snd
    let allNodes = allNodes |> List.map(fun (a,b) -> (a, b|> List.map snd))
    let importedTypes = m.Imports |> List.collect(fun imp -> imp.Types) |> List.map(fun x -> x.Value)

    let independentNodes = allNodes |> List.filter(fun (_,list) -> List.isEmpty list) |> List.map(fun (n,l) -> n)
    let dependentNodes = allNodes |> List.filter(fun (_,list) -> not (List.isEmpty list) )
    let sortedTypeAss = 
        DoTopologicalSort 
            (importedTypes@ implicitImportedTypes@independentNodes) 
            dependentNodes 
            (fun c -> 
                SemanticError(emptyLocation, sprintf "Recursive types are not compatible with embedded systems.\nASN.1 grammar has cyclic dependencies: %A" c )
            )
    seq {
        for tasName in sortedTypeAss do
        match m.TypeAssignments |> Seq.tryFind(fun x -> x.Name.Value = tasName) with
        | Some(tas)     -> yield tas
        | None          -> ()
    } |> Seq.toList


let SortValueAssignments (m:Asn1Module) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) =
    let GetValueDependencies (vas:ValueAssignment)  = 
        let actType = Ast.GetActualType vas.Type r
        seq {
            match vas.Value.Kind, actType.Kind with
            | RefValue(m,n), Enumerated(itms) -> 
                match itms |> Seq.exists(fun i -> i.Name.Value = n.Value) with
                | true  -> ()
                | false -> yield n.Value
            | RefValue(m,n), _ -> yield n.Value
            | _             -> ()
        } |> Seq.distinct |> Seq.toList

    
    let allNodes = m.ValueAssignments |> List.map( fun tas -> (tas.Name.Value, GetValueDependencies tas))
    let independentNodes = allNodes |> List.filter(fun (_,list) -> List.isEmpty list) |> List.map(fun (n,l) -> n)
    let dependentNodes = allNodes |> List.filter(fun (_,list) -> not (List.isEmpty list) )
    let sortedValueAss = DoTopologicalSort independentNodes dependentNodes (fun c -> SemanticError(emptyLocation, sprintf "Recursive types are not compatible with embedded systems.\nASN.1 grammar has cyclic dependencies: %A" c ))
    let mp = m.ValueAssignments |> List.map(fun v -> v.Name.Value, v) |> Map.ofList
    sortedValueAss |> List.map(fun x -> mp.[x])


let PrintModule (m:Asn1Module) (f:Asn1File) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) outDir fileExt (state:State) =
    let includedPackages = ss.rtlModuleName()::(m.Imports |> List.map (fun im -> ToC im.Name.Value))
    let sortedTas = SortTypeAssignments m r acn
    let tases, s1 = sortedTas |> foldMap(fun s tas -> PrintTypeAss tas m r acn s) state
    let sortedValueAssignments = SortValueAssignments m r acn
    let vases, s2 = sortedValueAssignments |> foldMap(fun s vas -> PrintValueAss vas m r s) s1
    let arrPrivChoices = 
        let choices = sortedTas |> Seq.choose(fun x -> match x.Type.Kind with Choice(ch) -> Some(x, ch) | _ -> None )
        let ChoicePrivate (t:TypeAssignment, children:list<ChildInfo>) =
            let sTasName = t.GetCName r.TypePrefix
            let printChild (c:ChildInfo) =
                let typeDecl,_ = PrintType c.Type [m.Name.Value; t.Name.Value; c.Name.Value] (Some t.Type) (TypeAssignment t,m,r) {State.nErrorCode = 0}
                ss.CHOICE_tas_decl_priv_child (c.CName ProgrammingLanguage.Spark) typeDecl (c.CName_Present Spark)
            ss.CHOICE_tas_decl_priv sTasName (children.Head.CName_Present Spark) (children |> Seq.map printChild)
        choices |> Seq.map ChoicePrivate
    let content = ss.PrintPackageSpec (ToC m.Name.Value) includedPackages tases vases arrPrivChoices
    let fileName = Path.Combine(outDir, ((ToC m.Name.Value)+fileExt).ToLower())
    File.WriteAllText(fileName, content.Replace("\r",""))
    s2


let PrintFile (f:Asn1File) outDir newFileExt (r:AstRoot) (acn:AcnTypes.AcnAstResolved) (state:State) =
    f.Modules |> Seq.fold (fun s m -> PrintModule m f r acn outDir newFileExt s) state

let DoWork (r:AstRoot) (acn:AcnTypes.AcnAstResolved) outDir newFileExt =
    r.Files |> Seq.fold(fun s f -> PrintFile f outDir newFileExt r acn s)  {State.nErrorCode = 1000}
