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

/// The ACN-inserted field that finally provides a determinant: the field itself, or, for
/// an ACN parameter (e.g. inside a closure-converted --acn-v2 function), the field reached
/// by following the RefTypeArgument dependencies upward.
let rec originalDeterminantField (deps: AcnInsertedFieldDependencies) (determinant: Determinant) : Asn1AcnAst.AcnChild option =
    match determinant with
    | AcnChildDeterminant ac -> Some ac
    | AcnParameterDeterminant prm ->
        deps.acnDependencies |> List.tryPick (fun d ->
            match d.dependencyKind with
            | AcnDepRefTypeArgument p when p.id = prm.id -> originalDeterminantField deps d.determinant
            | _ -> None)

/// Items of the ENUMERATED determinant as encoded by the determinant field itself, so that
/// acnEncodeValue reflects the field's own ACN properties (e.g. encode-values given on the
/// field and not on the CHOICE parameter type). Falls back to the given type.
let determinantEnumItems (deps: AcnInsertedFieldDependencies) (determinant: Determinant) (enm: Asn1AcnAst.Enumerated) : Asn1AcnAst.NamedItem list =
    match originalDeterminantField deps determinant with
    | Some { Asn1AcnAst.AcnChild.Type = AcnReferenceToEnumerated fieldEnm } -> fieldEnm.enumerated.items
    | _ -> enm.items

let THREE_DOTS = {IcdRow.fieldName = ""; comments = []; sPresent="";sType= IcdPlainType ""; sConstraint=None; minLengthInBits = 0I; maxLengthInBits=0I;sUnits=None; rowType = IcdRowType.ThreeDOTs; idxOffset = None}

/// The unit of measure written on a reference (`deltaAngle DeltaAngle --{deg}--`) applies to the
/// value rows of the referenced type's embedded rows and replaces their unit: the unit written at
/// the use site is the more specific one (the old ICD pipeline, GenerateUperIcd.getUnits, does the
/// same). Length, presence, padding and choice-index rows carry no unit.
let withUnitsOfMeasure (unitsOfMeasure: string option) (rows: IcdRow list) : IcdRow list =
    match unitsOfMeasure with
    | None   -> rows
    | Some u ->
        rows |> List.map (fun row ->
            match row.rowType with
            | IcdRowType.FieldRow
            | IcdRowType.SubItemRow                  -> {row with sUnits = Some u}
            | IcdRowType.ReferenceToCompositeTypeRow
            | IcdRowType.LengthDeterminantRow
            | IcdRowType.PresentDeterminantRow
            | IcdRowType.PaddingRow
            | IcdRowType.ThreeDOTs                   -> row)

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

/// Justification of a GNATcoverage exemption region in the generated code; the IDs and
/// their proofs are in Docs/statement-coverage-exemptions.md (the C uPER / ACN CHOICE
/// and string templates carry the same COV-001 / COV-002 texts).
let covExemptJustification (id:string) =
    match id with
    | "ASN1SCC-COV-001" -> "ASN1SCC-COV-001: value range enforced by the RTL decode, see Docs/statement-coverage-exemptions.md"
    | "ASN1SCC-COV-003" -> "ASN1SCC-COV-003: every value of the decoded bit width has an item, see Docs/statement-coverage-exemptions.md"
    | "ASN1SCC-COV-004" -> "ASN1SCC-COV-004: an earlier switch of this function on the same selector rejected every other value, see Docs/statement-coverage-exemptions.md"
    | "ASN1SCC-COV-005" -> "ASN1SCC-COV-005: every update of the determinant assigns an item, see Docs/statement-coverage-exemptions.md"
    | "ASN1SCC-COV-008" -> "ASN1SCC-COV-008: the determinant decoded earlier in this function holds an item, see Docs/statement-coverage-exemptions.md"
    | "ASN1SCC-COV-009" -> "ASN1SCC-COV-009: every caller decoded the determinant as an item before the call, see Docs/statement-coverage-exemptions.md"
    | _ -> raise (BugErrorException ("unknown coverage exemption " + id))

/// q/issues.md Issue 106: the initial value of the decode local of an ACN field that a dependent may read while the
/// component holding it is absent (AbsentHolder.fieldsReadWhenHolderAbsent): AcnChild.initExpression, except for a
/// string field, whose initExpression counts one character more than the typedef's array (SIZE max + 1).
let decodeLocalInitExpr (lm: LanguageMacros) (x: AcnChild) : string =
    match x.Type with
    | AcnReferenceToIA5String str -> lm.lg.stringLocalInit (int str.str.maxSize.uper)
    | AcnInteger _ | AcnBoolean _ | AcnNullType _ | AcnReferenceToEnumerated _ -> x.initExpression

/// Coverage step 12e: the key of a CHOICE selector in NestingScope.checkedSelectors / AcnChildUpdate.checkedSelectors
/// (the text of `<p><sAcc>kind` of the C templates; only compared with other keys of the same language).
let choiceSelectorKey (joined:string) (acc:string) = joined + acc + "kind"


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


// Choose the right base scope for folding a dependency path.
// In --acn-v2, a reference type's funcBody may be inlined into a specialized
// function whose runtime pVal does not correspond to the captured type's TAS.
// To generate correct access paths, walk the enclosing scopes (current SEQUENCE
// + ancestors from nestingScope.parents) and pick the DEEPEST whose type id is
// a prefix of depPath.  Fold the remaining nodes from that scope.
//
// Intra-sequence deps resolve to the current SEQUENCE (shortest fold).
// Sibling/outer deps resolve to an ancestor whose scope matches the dep's
// outer TA, giving the correct base pVal for the fold.
//
// The returned path carries the backend names of the SEQUENCE / CHOICE
// children (withBackendChildNames), so that the access path built from it
// matches the generated type definitions.
let rec resolveDepScope (r: Asn1AcnAst.AstRoot) (lm: LanguageMacros) (nestingScope: NestingScope) (pSrcRoot: CodegenScope) (depPath: ReferenceToType) : CodegenScope * ReferenceToType =
    match tryResolveDepScope r lm nestingScope depPath with
    | Some resolved -> resolved
    | None          -> pSrcRoot, withBackendChildNames r lm depPath

/// resolveDepScope without a fallback: None when no enclosing scope of nestingScope is a prefix of depPath.
and tryResolveDepScope (r: Asn1AcnAst.AstRoot) (lm: LanguageMacros) (nestingScope: NestingScope) (depPath: ReferenceToType) : (CodegenScope * ReferenceToType) option =
    let (ReferenceToType depNodes) = depPath
    let (ReferenceToType backendNodes) = withBackendChildNames r lm depPath
    let candidates = nestingScope.parents |> List.map (fun (p, t) -> (p, t.id))
    let matching =
        candidates |> List.tryFind (fun (_, ReferenceToType scopeNodes) ->
            let n = List.length scopeNodes
            n >= 2 && List.length depNodes >= n && List.take n depNodes = scopeNodes)
    match matching with
    | Some (scope, ReferenceToType scopeNodes) ->
        let n = List.length scopeNodes
        let remaining = ReferenceToType (List.take 2 scopeNodes @ List.skip n backendNodes)
        Some (scope, remaining)
    | None -> None

// The scope nodes of a type path with the backend names of the SEQUENCE and
// CHOICE children instead of their ASN.1 names. The two differ when -fp AUTO
// renames a field (EnsureUniqueEnumNames: a keyword, a type name or a module
// name), e.g. Ada "type" -> "MyPDU_type", C "switch" -> "MyPDU_switch".
// A node whose child cannot be found keeps its ASN.1 name.
and withBackendChildNames (r: Asn1AcnAst.AstRoot) (lm: LanguageMacros) (path: ReferenceToType) : ReferenceToType =
    let rec walk (t: Asn1AcnAst.Asn1Type option) (nodes: ScopeNode list) : ScopeNode list =
        match nodes, t |> Option.map (fun t -> t.ActualType.Kind) with
        | [], _ -> []
        | (SEQ_CHILD (name, isOpt)) :: rest, Some (Asn1AcnAst.Sequence s) ->
            match s.children |> List.tryPick (fun c -> match c with Asn1AcnAst.Asn1Child c when c.Name.Value = name -> Some c | _ -> None) with
            | Some c -> SEQ_CHILD (lm.lg.getAsn1ChildBackendName0 c, isOpt) :: walk (Some c.Type) rest
            | None   -> SEQ_CHILD (name, isOpt) :: walk None rest
        | (CH_CHILD (name, preName, chParent)) :: rest, Some (Asn1AcnAst.Choice ch) ->
            match ch.children |> List.tryFind (fun (c: Asn1AcnAst.ChChildInfo) -> c.Name.Value = name) with
            | Some c -> CH_CHILD (lm.lg.getAsn1ChChildBackendName0 c, preName, chParent) :: walk (Some c.Type) rest
            | None   -> CH_CHILD (name, preName, chParent) :: walk None rest
        | SQF :: rest, Some (Asn1AcnAst.SequenceOf sqf) -> SQF :: walk (Some sqf.child) rest
        | n :: rest, _ -> n :: walk None rest
    match path with
    | ReferenceToType ((MD md) :: (TA tas) :: xs) ->
        let tasType = r.typeAssignmentsMap.TryFind (md, tas) |> Option.map (fun ts -> ts.Type)
        ReferenceToType ((MD md) :: (TA tas) :: walk tasType xs)
    | _ -> path
