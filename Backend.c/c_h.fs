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

module c_h


open System.Numerics
open CommonTypes
open FsUtils
open Ast
open System.IO
open VisitTree
open uPER
open CloneTree

open c_utils


let rec PrintTypeDeclaration (t:Asn1Type)  (p:list<string>) (r:AstRoot) =
    let SizeableTypeUperRange() =
            match (GetTypeUperRange t.Kind t.Constraints r) with
            |Concrete(min, max)        -> min,max
            |Empty                    -> raise(SemanticError(t.Location, "SEQUENCE OF or OCTET STRING ended up with no (or invalid) constraints"))
            |_                         -> raise(BugErrorException "SEQUENCE OF or OCTET STRING etc has no constraints")
    let PrintChoiceSeqChild (child:ChildInfo) =
        ch.PrintSeq_ChoiceChild  (PrintTypeDeclaration child.Type (p@[child.Name.Value]) r) (child.CName ProgrammingLanguage.C) (TypeArrayPostfix child.Type r)

    match t.Kind with
    | Integer       -> 
        match (GetTypeUperRange t.Kind t.Constraints r) with
        | Concrete(a,b) when a >= 0I    ->  ch.Declare_PosInteger_min_max a b 
        | Concrete(a,b)     ->  ch.Declare_Integer_min_max a b 
        | NegInf(b)         ->  ch.Declare_Integer_negInf b 
        | PosInf(a)  when a >= 0I       ->  ch.Declare_PosInteger_posInf a 
        | PosInf(a)         ->  ch.Declare_Integer_posInf a 
        | Empty | Full      ->  ch.Declare_Integer ()
    | Boolean       -> ch.Declare_Boolean ()
    | Real          -> ch.Declare_Real ()
    | IA5String 
    | NumericString -> ch.Declare_IA5String ()
    | NullType      -> ch.Declare_NullType ()
    | BitString(_)  -> 
        let nMin, nMax = SizeableTypeUperRange()
        ch.Declare_BitString (nMin=nMax) (TypeCArrayItemsCount t r) (TypeItemsCount t r) 
    | OctetString   -> 
        let nMin, nMax = SizeableTypeUperRange()
        ch.Declare_OctetString (nMin=nMax) (TypeCArrayItemsCount t r) 
    | Enumerated(items)    ->
        let PrintNamedItem (it:NamedItem,value:BigInteger) =
            match it._value with
            | Some(vl)  -> ch.PrintNamedItem (it.CEnumName r C) (Ast.GetValueAsInt vl r) 
            | None      -> ch.PrintNamedItem (it.CEnumName r C) value 
        let result = 
                items |> Seq.mapi(fun i ch -> (ch, (BigInteger i)) )
        ch.Declare_Enumerated (result |> Seq.map PrintNamedItem ) 
    | Choice(children)  ->
        ch.Declare_Choice ((TypeLongName p)+"_NONE") 
                            (children |> Seq.map(fun x -> (x.CName_Present C)) ) 
                            (children |> Seq.map PrintChoiceSeqChild ) 
    | Sequence(chldrn)  ->
        let children = chldrn |> Seq.filter(fun x -> not x.AcnInsertedField)
        let optChilden = children |> Seq.filter(fun x -> x.Optionality.IsSome) |> Seq.map(fun x -> x.CName ProgrammingLanguage.C) 
        ch.Declare_Sequence (children |> Seq.map PrintChoiceSeqChild |> Seq.toArray) optChilden 
    | SequenceOf(child) ->
        let nMin, nMax = SizeableTypeUperRange()
        ch.Declare_SequenceOf (nMin=nMax) (PrintTypeDeclaration child (p@["#"]) r) (TypeCArrayItemsCount t r)  (TypeArrayPostfix child r) 
    | ReferenceType(m,tasName, _) -> (GetTasCName tasName.Value r.TypePrefix)


let DeclateAcnAsn1Type (t:AcnTypes.AcnAsn1Type) (m:Asn1Module) (r:AstRoot) =
    match t with
    | AcnTypes.Integer    -> ch.Declare_Integer ()
    | AcnTypes.Boolean    -> ch.Declare_Boolean ()
    | AcnTypes.NullType   -> raise(BugErrorException "")
    | AcnTypes.RefTypeCon(md,tasName) -> (GetTasCName tasName.Value r.TypePrefix)


let PrintParamType (p:AcnTypes.AcnParameter) (m:Asn1Module) (r:AstRoot) =
    DeclateAcnAsn1Type p.Asn1Type m r

let PrintExtracAcnParams (p:AcnTypes.AcnParameter) (m:Asn1Module) (r:AstRoot) codec =
    let prmType = PrintParamType p m r
    match p.Mode, codec with
    | AcnTypes.DecodeMode, Encode        -> None
    | AcnTypes.DecodeMode, Decode        -> Some (ch.PrintAcnParameter prmType false (ToC p.Name))
    | AcnTypes.EncodeDecodeMode, Encode  -> Some (ch.PrintAcnParameter prmType false (ToC p.Name))
    | AcnTypes.EncodeDecodeMode, Decode  -> Some (ch.PrintAcnParameter prmType true (ToC p.Name))

type ErrInfoState = {
    nGlobalErrorCode:int
    errorCodes:list<string*int*string>
}

let PrintTypeAss (t:TypeAssignment) (m:Asn1Module) (f:Asn1File) (r:AstRoot) (acn:AcnTypes.AcnAstResolved)  (curErrCode:int) = 
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
    let GetErrorCodes (state:int) = 
        let retState = VisitType (RemoveWithComponents t.Type r)[m.Name.Value; t.Name.Value] None (TypeAssignment t)  m r {DefaultVisitors with visitType=OnType_collerErrInfo} {nGlobalErrorCode = state; errorCodes = []}
        let errorCodes = retState.errorCodes |>  Seq.map (fun (sErrName, nErrCode, sComment) -> ch.PrintErrorCode sErrName (BigInteger nErrCode) sComment)
        errorCodes, retState.nGlobalErrorCode

    let errorCodes, newErrCode = GetErrorCodes curErrCode
    
    let sTypeDecl = PrintTypeDeclaration t.Type [m.Name.Value; t.Name.Value] r
    let sarrPostfix = TypeArrayPostfix t.Type r
    let sName = t.GetCName r.TypePrefix
    let nMaxBitsInPER = uperGetMaxSizeInBitsAsInt t.Type.Kind t.Type.Constraints t.Type.Location r
    let nMaxBytesInPER = uperGetMaxSizeInBytesAsInt t.Type.Kind t.Type.Constraints t.Type.Location r
    let nMaxBitsInACN, nMaxBytesInACN = Acn.RequiredBitsForAcnEncodingInt t.Type [m.Name.Value; t.Name.Value] r acn
    let nMaxBytesInXER = XER_bl.GetMaxSizeInBytesForXER t.Type t.Name.Value r
    let sStar = (TypeStar t.Type r)
    let result =  ch.PrintTypeAssignment  sTypeDecl sarrPostfix  sName  nMaxBitsInPER nMaxBytesInPER  nMaxBitsInACN nMaxBytesInACN (BigInteger nMaxBytesInXER) sStar  r.GenerateEqualFunctions errorCodes
    result, newErrCode


let PrintValueAss (v:ValueAssignment) (m:Asn1Module) (f:Asn1File) (r:AstRoot) = 
    let sName = v.c_name
    let sTypeDecl= PrintTypeDeclaration v.Type [m.Name.Value; v.Name.Value] r
    ch.PrintValueAssignment sTypeDecl sName


let PrintAcnProtos (t:TypeAssignment) (m:Asn1Module) (f:Asn1File) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) = 
    let sName = t.GetCName r.TypePrefix
    let sStar = (TypeStar t.Type r)
    let print_encoding_protory (enc:Asn1Encoding) =
        match enc with
        | BER   -> ch.BER_encPrototypes sName sStar
        | XER   -> ch.XER_encPrototypes sName sStar        
        | UPER  -> ch.UPER_encPrototypes sName sStar
        | ACN   -> 
            let path = [m.Name.Value; t.Name.Value]
            let myParams = acn.Parameters |> List.filter(fun p -> p.TasName=t.Name.Value && p.ModName=m.Name.Value)
            let EncPrms = myParams |> Seq.choose(fun p -> PrintExtracAcnParams p m r Encode)
            let DecPrms = myParams |> Seq.choose(fun p -> PrintExtracAcnParams p m r Decode)
            ch.ACN_encPrototypes sName sStar EncPrms DecPrms
    let result = ch.PrintPrototypes (r.Encodings |> Seq.map print_encoding_protory )
    result


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



let PrintFile (f:Asn1File) outDir newFileExt (r:AstRoot) (acn:AcnTypes.AcnAstResolved)  =
    let fileNameNoExtUpper = f.FileNameWithoutExtension.ToUpper()
    let allImportedModules = f.Modules |> Seq.collect(fun m -> m.Imports) |> Seq.map(fun imp -> imp.Name.Value) |> Seq.distinct
    let includedModules  = seq {   
        for file in r.Files do
            if file.FileName <> f.FileName then
                if file.Modules |> Seq.exists (fun m -> allImportedModules |> Seq.exists(fun x -> x = m.Name.Value)) then
                    yield file.FileNameWithoutExtension } |> Seq.toList 
    let sortedTas = SortTypeAssignments f r acn
    let tases, s1 = sortedTas |> foldMap(fun s (m,tas) -> PrintTypeAss tas m f r acn s) 1000
    let protos  = sortedTas |> Seq.map(fun (m,tas) -> PrintAcnProtos tas m f r acn )
    let vases= seq {
                for m in f.Modules do
                    for vas in m.ValueAssignments do
                        yield (PrintValueAss vas m f r) 
                } |> Seq.toList
    let EnumUtils = 
            seq {
                for (m,tas) in sortedTas do
                    match tas.Type.Kind with
                    | Enumerated(items)     -> for item in items do yield ch.EnumUtilDefine (item.CEnumName r C)
                    | _                     ->
                        for chType in Ast.GetMySelfAndChildren tas.Type  do
                            match chType.Kind with
                            | Enumerated(items)     -> 
                                let sTasName = tas.GetCName r.TypePrefix
                                for item in items do yield ch.EnumInnerUtilDefine sTasName (item.CEnumName r C)
                            | _                     -> ()
              } |> Seq.toList

    let ChoiceUtils =
        seq {
            for (m,tas) in sortedTas do
                for t in Ast.GetMySelfAndChildren tas.Type do
                    match t.Kind with
                    | Choice(children)  -> 
                        let sTasName = tas.GetCName r.TypePrefix
                        for child in children do 
                            yield ch.ChoiceUtilDefine sTasName (child.CName_Present C)
                    | _                 -> ()

        } |> Seq.toList
                    
    let content = ch.PrintHeaderFile (ToC fileNameNoExtUpper) includedModules tases vases protos (EnumUtils@ChoiceUtils)
    let fileName = Path.Combine(outDir, (f.FileNameWithoutExtension+newFileExt))
    File.WriteAllText(fileName, content.Replace("\r",""))

let DoWork (r:AstRoot) (acn:AcnTypes.AcnAstResolved) outDir newFileExt =
    r.Files |> Seq.iter(fun f -> PrintFile f outDir newFileExt r acn )  


