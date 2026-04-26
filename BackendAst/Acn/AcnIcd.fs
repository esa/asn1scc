module AcnIcd

open FsUtils
open CommonTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions


let md5 = System.Security.Cryptography.MD5.Create()

let createIcdTas (r:Asn1AcnAst.AstRoot) (id:ReferenceToType) (icdAux:IcdArgAux) (td:FE_TypeDefinition) (typeDefinition:TypeDefinitionOrReference) nMinBytesInACN nMaxBytesInACN hasAcnDefinition =
    let icdRows, compositeChildren = icdAux.rowsFunc "" "" [];
    let icdTas =
        {
            IcdTypeAss.typeId = id
            tasInfo = id.tasInfo
            asn1Link = None;
            acnLink = None;
            name =
                match icdAux.name with
                | Some n -> n
                | None   -> td.asn1Name
            kind = icdAux.baseAsn1Kind;
            canBeEmbedded  = icdAux.canBeEmbedded
            createRowsFunc = icdAux.rowsFunc
            comments =
                let asn1Comments =
                    match id.tasInfo with
                    | None -> []
                    | Some tasInfo ->
                        match r.typeAssignmentsMap.TryFind (tasInfo.modName, tasInfo.tasName) with
                        | None -> []
                        | Some ts -> ts.Comments |> Seq.toList

                asn1Comments@icdAux.commentsForTas
            rows  = icdRows
            compositeChildren = compositeChildren
            minLengthInBytes = nMinBytesInACN;
            maxLengthInBytes = nMaxBytesInACN
            hasAcnDefinition = hasAcnDefinition
            hash = "" // will be calculated later
        }
    let icdHash = CalculateIcdHash.calcIcdTypeAssHash icdTas
    {icdTas with hash = icdHash}
