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

module c_body

open System.Numerics
open FsUtils
open CommonTypes
open Ast
open System.IO
open VisitTree
open uPER
open CloneTree
open c_utils




let PrintTypeAss (t:TypeAssignment) (m:Asn1Module) (f:Asn1File) (r:AstRoot) (acn:AcnTypes.AcnAstResolved) = 
    let PrintInitFunc () = 
        let sName = t.GetCName r.TypePrefix
        let sStar = (TypeStar t.Type r)
        let newt = RemoveWithComponents t.Type r
        let initVal = Asn1Values.GetDefaultValueByType (Some 1I) newt m r
        let bIsString = match (Ast.GetActualType t.Type r).Kind with
                        | IA5String | NumericString -> true
                        | _                         -> false
        let sVal = c_variables.PrintAsn1Value initVal t.Type false (t.Name.Value,0) m r
        c_src.PrintInitialize sName sStar sVal bIsString
    let initContent =  PrintInitFunc ()
    let valContent =   c_validate.PrintTypeAss t m f r acn
    let equalContent = if r.GenerateEqualFunctions then (c_equal.PrintTypeAss t m r) else ""
    let EncFunc = function
        | UPER  -> [c_uper.EmitTypeAss t m r Encode; c_uper.EmitTypeAss t m r Decode] 
        | ACN   -> [c_acn.EmitUpdate_param_functions t m r acn; c_acn.EmitTypeAss t m r acn Encode;  c_acn.EmitTypeAss t m r acn Decode]
        | BER   -> []
        | XER   -> [c_XER.EmitTypeAss t m r Encode; c_XER.EmitTypeAss t m r Decode]
    let encFunctions = r.Encodings |> List.map EncFunc |> List.collect(fun x -> x)
    let content = [initContent;valContent; equalContent] @ encFunctions |> List.filter(fun s -> s<>"")
    content.StrJoin("\n\n")


let PrintValueAss (v:ValueAssignment) (m:Asn1Module) (f:Asn1File) (r:AstRoot) = 
    let sName = v.c_name
    let sTypeDecl= c_h.PrintTypeDeclaration v.Type [m.Name.Value; v.Name.Value] r
    let sVal = c_variables.PrintAsn1Value v.Value v.Type true (v.Name.Value,0) m r
    c_src.PrintUnnamedVariable sTypeDecl sName sVal


let PrintFile (fileIdx:int) (f:Asn1File) outDir newFileExt (r:AstRoot) (acn:AcnTypes.AcnAstResolved)  =
    let fileNameNoExtUpper = f.FileNameWithoutExtension
    let allImportedModules = f.Modules |> Seq.collect(fun m -> m.Imports) |> Seq.map(fun imp -> imp.Name.Value) |> Seq.distinct
    let includedModules  = seq {   
        for file in r.Files do
            if file.FileName <> f.FileName then
                if file.Modules |> Seq.exists (fun m -> allImportedModules |> Seq.exists(fun x -> x = m.Name.Value)) then
                    yield file.FileNameWithoutExtension } |> Seq.toList 
    let sortedTas = c_h.SortTypeAssignments f r acn |> Seq.toList
    let tases = sortedTas |> Seq.map(fun (m,tas) -> PrintTypeAss tas m f r acn ) |> Seq.toList
    let vases= seq {
                for m in f.Modules do
                    for vas in m.ValueAssignments do
                        yield (PrintValueAss vas m f r) 
                } |> Seq.toList

    
    let PrintUnnamedVariable (t:Asn1Type, v:Asn1Value, typePath, tasName,  m, r) = 
        let sName =  Ast.GetValueID v
        let sTypeDecl= c_h.PrintTypeDeclaration t typePath r
        let sVal = c_variables.PrintAsn1Value v t false (tasName,0) m r
        c_src.PrintUnnamedVariable sTypeDecl sName sVal

    let unnamedVariables =
        seq {
            for (m,tas) in sortedTas do
                let curPath = [m.Name.Value; tas.Name.Value]
                let t = RemoveWithComponents tas.Type r
                for (t, typePath) in GetMySelfAndChildrenWithPath t curPath do
                    for (newT,v, newPath) in GetTypeValues t typePath do
                        match v.Kind with
                        | OctetStringValue(_)   | BitStringValue(_)     -> yield (newT,v, newPath, tas.Name.Value, m, r )
                        | _                                             -> ()
        } |> Seq.toList |> Seq.distinctBy(fun (_,v,_,_,_,_) -> Ast.GetValueID v) |> Seq.map PrintUnnamedVariable |> Seq.toArray
    let soMappingFunctionsModule = r.mappingFunctionsModule |> Option.map(fun s -> if s.EndsWith(".h") then s.Substring(0, s.Length-2 ) else s)
    let content = c_src.main fileNameNoExtUpper unnamedVariables tases vases soMappingFunctionsModule
    let fileName = Path.Combine(outDir, (f.FileNameWithoutExtension+newFileExt))
    let content = FsUtils.replaceErrorCodes content "ERR_INSUFFICIENT_DATA" 1 fileIdx 1
    let content = FsUtils.replaceErrorCodes content "ERR_INCORRECT_PER_STREAM" 2 fileIdx 1
    let content = FsUtils.replaceErrorCodes content "ERR_INVALID_CHOICE_ALTERNATIVE" 3 fileIdx 1
    let content = FsUtils.replaceErrorCodes content "ERR_INVALID_ENUM_VALUE" 4 fileIdx 1
    File.WriteAllText(fileName, content.Replace("\r",""))


let DoWork (r:AstRoot) (acn:AcnTypes.AcnAstResolved) outDir  =
    c_h.DoWork r acn outDir ".h" 
    r.Files |> Seq.iteri(fun i f -> PrintFile i f outDir ".c" r acn )   


let EmmitDefaultACNGrammar (r:AstRoot) outDir  =
    let printTas (tas: TypeAssignment) =
        tas.Name.Value + "[]"
    let printModule (m:Asn1Module) =
        let arrTases = m.TypeAssignments |> Seq.map printTas
        c_src.PrintDefaultAcnModule m.Name.Value arrTases "::="
    let printFile (f:Asn1File) =
        let fileName = f.FileNameWithoutExtension + ".acn"
        if (System.IO.File.Exists fileName) then
            System.Console.Error.WriteLine("File {0} already exists. Creation of default ASN.1 grammar abandoned", fileName);
        else
            let content = f.Modules |> Seq.map printModule |> Seq.StrJoin "\n"
            let fileName = Path.Combine(outDir, fileName)
            File.WriteAllText(fileName, content.Replace("\r",""))
    r.Files |> Seq.iter printFile