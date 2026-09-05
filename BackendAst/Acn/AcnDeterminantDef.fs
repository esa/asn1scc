module AcnDeterminantDef

open FsUtils
open CommonTypes
open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open DAst
open DAstUtilFunctions
open Language


let getDeterminantTypeDefinitionBodyWithinSeq (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (det:Determinant) =
    let createPrmAcnInteger (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros)  =
        let Declare_Integer     =  lm.typeDef.Declare_Integer
        Declare_Integer ()

    let createAcnInteger (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (a:Asn1AcnAst.AcnInteger) =
        let intClass = getAcnIntegerClass r.args a
        let stgMacro = DAstTypeDefinition.getIntegerTypeByClass lm intClass
        stgMacro ()

    let createAcnBoolean (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) =
        lm.typeDef.Declare_Boolean ()

    let createAcnNull (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) =
        lm.typeDef.Declare_Null ()

    let getTypeDefinitionName (r:Asn1AcnAst.AstRoot) (lm:LanguageMacros) (id : ReferenceToType) =
        let longName = id.AcnAbsPath.Tail |> Seq.StrJoin "_"
        ToC2(r.args.TypePrefix + longName.Replace("#","elm"))

    match det with
    | AcnChildDeterminant       ch ->
        match ch.Type with
        | Asn1AcnAst.AcnInteger  a -> createAcnInteger r lm a
        | Asn1AcnAst.AcnNullType _ -> createAcnNull r lm
        | Asn1AcnAst.AcnBoolean  _ -> createAcnBoolean r lm
        | Asn1AcnAst.AcnReferenceToEnumerated a -> ToC2(r.args.TypePrefix + a.tasName.Value)
        | Asn1AcnAst.AcnReferenceToIA5String a ->
            let typeName = ToC2(r.args.TypePrefix + a.tasName.Value)
            lm.lg.qualifyNameWithModule (ToC a.modName.Value) (ToC ch.id.ModName) typeName

    | AcnParameterDeterminant   prm ->
        match prm.asn1Type with
        | AcnGenericTypes.AcnPrmInteger  _       -> createPrmAcnInteger r lm
        | AcnGenericTypes.AcnPrmBoolean  _       -> createAcnBoolean r lm
        | AcnGenericTypes.AcnPrmNullType _       -> createAcnNull r lm
        | AcnGenericTypes.AcnPrmRefType (md,ts)  ->
            getTypeDefinitionName r lm (ReferenceToType [MD md.Value; TA ts.Value])


let getDeterminant_macro (det:Determinant) pri_macro str_macro =
    match det with
    | AcnChildDeterminant ch ->
        match ch.Type with
        | Asn1AcnAst.AcnReferenceToIA5String _ -> str_macro
        | _ -> pri_macro
    | AcnParameterDeterminant prm -> pri_macro

let getDeterminantTypeUpdateMacro (lm:LanguageMacros) (det:Determinant) =
    let MultiAcnUpdate_get_first_init_value_pri     =  lm.acn.MultiAcnUpdate_get_first_init_value_pri
    let MultiAcnUpdate_get_first_init_value_str     =  lm.acn.MultiAcnUpdate_get_first_init_value_str
    getDeterminant_macro det MultiAcnUpdate_get_first_init_value_pri MultiAcnUpdate_get_first_init_value_str

let getDeterminantTypeCheckEqual (lm:LanguageMacros) (det:Determinant) =
    let multiAcnUpdate_checkEqual_pri     =  lm.acn.MultiAcnUpdate_checkEqual_pri
    let multiAcnUpdate_checkEqual_str     =  lm.acn.MultiAcnUpdate_checkEqual_str
    getDeterminant_macro det multiAcnUpdate_checkEqual_pri multiAcnUpdate_checkEqual_str
