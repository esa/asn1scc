module DAstExportToXml

open System
open System.Numerics
open System.IO
open System.Xml.Linq
open FsUtils
open CommonTypes
open AbstractMacros
open Asn1AcnAstUtilFunctions;
open DAst
open DastFold
open DAstUtilFunctions

let private xname s = System.Xml.Linq.XName.Get(s)
let private xnameNs str ns = System.Xml.Linq.XName.Get(str, ns)

let private xsiUrl = "http://www.w3.org/2001/XMLSchema-instance"
let private xsi = XNamespace.Get xsiUrl
let private customWsSchemaLocation = "asn1sccAst.xsd"

let exportOptionalElement (tagName:string) (content:string option) =
    match content with
    | Some cnt -> XElement(xname tagName, (XCData cnt))
    | None     -> null

let exportElement (tagName:string) (cnt:string) =
    XElement(xname tagName, (XCData cnt))



let private exportType (r:AstRoot) (t:Asn1Type) =
    foldAsn1Type
        t
        ()
        (fun t ti us ->
                    XElement(xname "INTEGER",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
//                        (exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
//                        (exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq)
                        ), us )

        (fun t ti us ->
                    XElement(xname "REAL",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
//                        (exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
//                        (exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq)
                        ), us )

        (fun t ti us ->
                    XElement(xname "IA5STRING",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
//                        (exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
//                        (exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq)
                        ), us )

        (fun t ti us ->
                    XElement(xname "OctetString",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
  //                      (exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
  //                      (exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq)
                        ), us )

        (fun t ti us ->
                    XElement(xname "Null",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
//                        (exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
//                        (exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq)
                        ), us )

        (fun t ti us ->
                    XElement(xname "BitString",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
//                        (exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
//                        (exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq)
                        ), us )

        (fun t ti us ->
                    XElement(xname "Boolean",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
//                        (exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
//                        (exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq)
                        ), us )

        (fun t ti us ->
                    XElement(xname "Enumerated",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
//                        (exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
//                        (exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq)
                        ), us )

        (fun t ti us ->
                    XElement(xname "ObjectIdentifier",
                        XAttribute(xname "id", t.id.AsString),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
                        ), us )
        (fun t ti us ->
                    XElement(xname "TIME",
                        XAttribute(xname "id", t.id.AsString),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind)
                        ), us )

        (fun t ti (child,us) ->
                     XElement(xname "SEQUENCEOF",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind),
                        child
                        //(exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
                        //(exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq)
                        ), us )
        (fun t ti ch (chType, us) ->
                    XElement(xname "SEQUENCE_COMPONENT",
                        XAttribute(xname "Name", ch.Name.Value),
                        (ExportToXml.exportOptionality ch.Optionality ),
                        chType), us )
        (fun t ti ch us ->
                    XElement(xname "ACN_COMPONENT",
                        XAttribute(xname "Name", ch.Name.Value),
                        (XAttribute(xname "acnMaxSizeInBits", ch.Type.acnMaxSizeInBits )),
                        (XAttribute(xname "acnMinSizeInBits", ch.Type.acnMinSizeInBits ))
                        ), us )

        (fun t ti (children,us) ->
                    XElement(xname "SEQUENCE",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind),
                        //(exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
                        //(exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq),
                        children
                        ), us )

        (fun t ti ch (chType, us) ->
                    XElement(xname "CHOICE_ALTERNATIVE",
                        XAttribute(xname "Name", ch.Name.Value),
                        XAttribute(xname "PresentWhenName", ch.presentWhenName (Some ch.chType.typeDefinitionOrReference) Ada),
                        XAttribute(xname "AdaName", (ch._ada_name)),
                        XAttribute(xname "CName", (ch._c_name)),
                        XAttribute(xname "ScalaName", (ch._scala_name)),
                        XAttribute(xname "AcnPresentWhenCont", (ch.acnPresentWhenConditions |> List.map(fun pwc -> sprintf "%s = %s" pwc.relativePath.AsString pwc.valueAsString) |> Seq.StrJoin ", ")),
                        chType), us )

        (fun t ti (children, us) ->
                    XElement(xname "CHOICE",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "newTypedefName2", (ti.baseInfo.typeDef.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName)),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind),
                        XAttribute(xname "ancEncClass", ti.ancEncClass.ToString()),

                        //(exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
                        //(exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq),
                        children
                        ), us )

        (fun t ti (baseType,us) ->
                    XElement(xname "REFERENCE_TYPE",
                        XAttribute(xname "id", t.id.AsString),
                        //(match t.parInfoData with Some pi-> (XAttribute(xname "parentData.typedefName",pi.parentData.typedefName)) | None -> null),
//                        XAttribute(xname "typeDefinition.name", ti.typeDefinition.name),
//                        XAttribute(xname "typeDefinition.typeDefinitionBodyWithinSeq", ti.typeDefinition.typeDefinitionBodyWithinSeq),
                        XAttribute(xname "newTypedefName", (t.typeDefinitionOrReference.longTypedefName2 true)),
                        XAttribute(xname "typeDefinitionOrReference", (match t.typeDefinitionOrReference with ReferenceToExistingDefinition r -> "ReferenceToExistingDefinition" | TypeDefinition td -> "TypeDefinition" )),
                        XAttribute(xname "Module", ti.baseInfo.modName.Value),
                        XAttribute(xname "TypeAssignment", ti.baseInfo.tasName.Value),
                        XAttribute(xname "newTypedefName2", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName),
                        XAttribute(xname "newTypedefName2_kind", t.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].kind),
                        //(exportElement "CompleteDefinition" ti.typeDefinition.completeDefinition),
                        //(exportOptionalElement "CompleteDefinitionWithinSeq" ti.typeDefinition.completeDefinitionWithinSeq),
                        (match ti.baseInfo.acnArguments with
                            | []   -> []
                            | args -> [XElement(xname "AcnArguments", (args |> List.map ExportToXml.exprtRefTypeArgument) )]),
                        baseType), us )

        (fun o newKind  ->
            let a = o.FT_TypeDefinition.[CommonTypes.ProgrammingLanguage.ActiveLanguages.Head].typeName
            let b = o.typeDefinitionOrReference.longTypedefName2 false
            match a = b with
            | true  -> ()
            | false -> ()//printfn "@@@@@@@@@@ %s FE='%s' B='%s'" o.id.AsString a b
            newKind)
    |> fst

let private exportTas (r:AstRoot) (tas:TypeAssignment) =
    XElement(xname "TypeAssignment",
        XAttribute(xname "Name", tas.Name.Value),
        (exportType r tas.Type)
    )



let private exportModule (r:AstRoot) (m:Asn1Module) =
    XElement(xname "Module",
        XAttribute(xname "Name", m.Name.Value),
        XElement(xname "TypeAssignments", m.TypeAssignments |> List.map  (exportTas r))
    )

let exportFile (r:AstRoot) (deps:Asn1AcnAst.AcnInsertedFieldDependencies) (fileName:string) =
    let wsRoot =
        XElement(xname "AstRoot",
            XAttribute(XNamespace.Xmlns + "xsi", xsi),
            XAttribute(xnameNs "noNamespaceSchemaLocation" xsiUrl, customWsSchemaLocation),
            XAttribute(xname "rename_policy", (sprintf "%A" r.args.renamePolicy)),
            r.Files |>
            List.map(fun f ->
                    XElement(xname "Asn1File",
                        XAttribute(xname "FileName", f.FileName),
                        XElement(xname "Modules",
                            f.Modules |> List.map  (exportModule r))
                    )),
            (ExportToXml.exportAcnDependencies deps)
            )


    let dec = new XDeclaration("1.0", "utf-8", "true")
    let doc =new XDocument(dec)
    doc.AddFirst wsRoot
    doc.Save(fileName)
