module AcnHelpers

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language


let foldMap = Asn1Fold.foldMap

let callBaseTypeFunc (lm:LanguageMacros) = lm.uper.call_base_type_func
let callSuperclassFunc (lm:LanguageMacros) = lm.uper.call_superclass_func

let sparkAnnotations (lm:LanguageMacros)  = lm.acn.sparkAnnotations

let THREE_DOTS = {IcdRow.fieldName = ""; comments = []; sPresent="";sType= IcdPlainType ""; sConstraint=None; minLengthInBits = 0I; maxLengthInBits=0I;sUnits=None; rowType = IcdRowType.ThreeDOTs; idxOffset = None}

/// Assign the "No" (index) column of a parent SEQUENCE/CHOICE's flattened rows.
/// Every real row advances a single top-level counter; SubItemRow rows (the
/// collapsed SEQUENCE OF elements of roadmap D2) keep their own element index
/// (1 .. N) so the renderer can print "topIndex.elementIndex", and ThreeDOTs
/// rows stay unnumbered.  A table without collapsed SEQUENCE OFs is numbered
/// exactly as the previous `List.mapi (i+1)` did (there are no top-level
/// ThreeDOTs rows in SEQUENCE/CHOICE tables), so existing tables are unchanged.
let renumberIcdRows (rows: IcdRow list) : IcdRow list =
    rows |> List.mapFold (fun topIdx rw ->
        match rw.rowType with
        | IcdRowType.SubItemRow -> rw, topIdx
        | IcdRowType.ThreeDOTs  -> rw, topIdx
        | IcdRowType.FieldRow
        | IcdRowType.ReferenceToCompositeTypeRow
        | IcdRowType.LengthDeterminantRow
        | IcdRowType.PresentDeterminantRow
        | IcdRowType.PaddingRow ->
            let n = topIdx + 1
            {rw with idxOffset = Some n}, n) 0 |> fst

let getAcnDeterminantName = AcnCreateFromAntlr.getAcnDeterminantName

/// Join the per-constraint ASN.1 strings of a type into the optional text
/// shown in the ICD "Constraint" column (None when the type is unconstrained).
let constraintsToIcdStr (constraintsAsn1Str: string list) : string option =
    match (constraintsAsn1Str |> Seq.StrJoin "").Trim() with
    | ""  -> None
    | s   -> Some s

/// Named-type identity of embedded reference children (roadmap A4): when a
/// type resolves a reference to a named TAS, its ICD field row shows the
/// referenced type's name in the type column, e.g. "INTEGER (I32)" or
/// "OCTET STRING (Checksum)".  Only FieldRows of embeddable types are
/// suffixed - determinant/padding rows keep their labels and composites keep
/// their identity through their own linked table.
let icdAuxAddNamedTypeSuffix (soTasName: string option) (icdAux: IcdArgAux) : IcdArgAux =
    match soTasName with
    | None -> icdAux
    | Some tasName ->
        match icdAux.canBeEmbedded with
        | false -> icdAux
        | true  ->
            let patchRow (rw:IcdRow) =
                match rw.rowType with
                | IcdRowType.FieldRow ->
                    match rw.sType with
                    | IcdPlainType label -> {rw with sType = IcdPlainType $"%s{label} (%s{tasName})"}
                    | TypeHash _         -> rw
                | IcdRowType.ReferenceToCompositeTypeRow
                | IcdRowType.LengthDeterminantRow
                | IcdRowType.PresentDeterminantRow
                | IcdRowType.PaddingRow
                | IcdRowType.SubItemRow
                | IcdRowType.ThreeDOTs -> rw
            let rowsFunc fieldName sPresent comments =
                let rows, comp = icdAux.rowsFunc fieldName sPresent comments
                rows |> List.map patchRow, comp
            {icdAux with rowsFunc = rowsFunc}

/// Max size in bits of a type's own ICD field row (roadmap B3): the type's
/// acnMaxSizeInBits minus the align-to-next padding that AcnEncodingClasses
/// folds into the max.  The padding is presented as a dedicated PaddingRow
/// (prepended by the wrappers in AcnAlignment.fs), so the field row itself
/// must show only the field's own encoding size.
let icdMaxSizeWithoutAlignment (acnAlignment: AcnGenericTypes.AcnAlignment option) (acnMaxSizeInBits: System.Numerics.BigInteger) =
    acnMaxSizeInBits - AcnEncodingClasses.getAlignmentSize acnAlignment

let adaptArgument = DAstUPer.adaptArgument
let adaptArgumentValue = DAstUPer.adaptArgumentValue
let joinedOrAsIdentifier = DAstUPer.joinedOrAsIdentifier

/// Build a function-call string and insert extra actual parameters before
/// the closing ");".  For example:
///   "ret = Fn(pVal, pBitStrm, pErrCode, FALSE);"
/// becomes:
///   "ret = Fn(pVal, pBitStrm, pErrCode, FALSE, &det1);"
let insertActualParams (baseFuncCall: string) (extraActualParams: string list) : string =
    if extraActualParams.IsEmpty then
        baseFuncCall
    else
        let insertIdx = baseFuncCall.LastIndexOf(")")
        if insertIdx > 0 then
            baseFuncCall.[..insertIdx-1] + ", " + (extraActualParams |> String.concat ", ") + baseFuncCall.[insertIdx..]
        else
            baseFuncCall
