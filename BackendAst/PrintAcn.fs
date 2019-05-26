module PrintAcn

open System
open System.IO
open FsUtils
open CommonTypes
open AcnGenericTypes
open AcnCreateFromAntlr



let printAcnParamType (pt:AcnGenericTypes.AcnParamType) =
        match pt with
        |AcnGenericTypes.AcnPrmInteger   _         -> "INTEGER"
        |AcnGenericTypes.AcnPrmBoolean   _         -> "BOOLEAN"
        |AcnGenericTypes.AcnPrmNullType  _         -> "NULL"
        |AcnGenericTypes.AcnPrmRefType    (md,ts)  -> sprintf "%s" ts.Value

let printAcnParam (p:AcnGenericTypes.AcnParameter) =
    stg_acn.PrintParam p.name (printAcnParamType p.asn1Type) 




let rec printTypeEncSpec (t:AcnTypeEncodingSpec) =
    let props = 
        t.acnProperties |>
        List.map(fun p ->
            match p with
            | ENCODING           enc            -> 
                match enc with
                | GP_PosInt             -> stg_acn.PP_Encoding_PosInt()
                | GP_TwosComplement     -> stg_acn.PP_Encoding_TwosComplement()
                | GP_Ascii              -> stg_acn.PP_Encoding_Ascii()
                | GP_BCD                -> stg_acn.PP_Encoding_BCD()
                | GP_IEEE754_32         -> stg_acn.PP_Encoding_IEEE754_32()
                | GP_IEEE754_64         -> stg_acn.PP_Encoding_IEEE754_64()
            | SIZE               siz            -> 
                match siz with
                | GP_Fixed                 size         -> stg_acn.PP_Size_Fixed size.Value
                | GP_NullTerminated                     -> stg_acn.PP_Size_NullTerminated ()
                | GP_SizeDeterminant       relativePath -> stg_acn.PP_Sixe_Fld relativePath.AsString

            | ALIGNTONEXT        al             -> 
                match al with
                | AcnGenericTypes.NextByte    -> stg_acn.PP_Aligment_byte()
                | AcnGenericTypes.NextWord    -> stg_acn.PP_Aligment_word()
                | AcnGenericTypes.NextDWord   -> stg_acn.PP_Aligment_dword()
            | ENCODE_VALUES                     -> stg_acn.PP_EncodeValues ()
            | SAVE_POSITION                     -> stg_acn.PP_SavePosition ()
            | PRESENT_WHEN       prWhenList     -> 
                let prCons =
                    prWhenList |>
                    List.map(fun pw ->
                        match pw with
                        | GP_PresenceBool  repPath          -> stg_acn.PP_PresentWhenConditionBool repPath.AsString
                        | GP_PresenceInt   (repPath, intVal)  -> stg_acn.PP_PresentWhenConditionInt repPath.AsString intVal.Value
                        | GP_PresenceStr   (repPath, strVal)  -> stg_acn.PP_PresentWhenConditionStr repPath.AsString strVal.Value)
                stg_acn.PP_PresentWhen prCons
            | PRESENT_WHEN_EXP acnExp   ->
                let _, debugStr = AcnGenericCreateFromAntlr.printDebug acnExp
                stg_acn.PP_PresentWhen [debugStr]
            | TRUE_VALUE         trueVal        -> stg_acn.PP_TrueValue trueVal.Value
            | FALSE_VALUE        falseVal       -> stg_acn.PP_FalseValue falseVal.Value
            | PATTERN            pattern        -> 
                match pattern with
                | AcnGenericTypes.PATTERN_PROP_BITSTR_VALUE pat    -> stg_acn.PP_NullValue pat.Value
                | AcnGenericTypes.PATTERN_PROP_OCTSTR_VALUE v      -> 
                    let octStr = v |> List.map(fun b -> System.String.Format("{0:X2}", b.Value) ) |> Seq.StrJoin "" 
                    stg_acn.PP_NullValue octStr 
            | CHOICE_DETERMINANT  relPath       -> stg_acn.PP_ChoiceDeterminant relPath.AsString
            | ENDIANNES          endian         -> 
                match endian with
                | AcnGenericTypes.LittleEndianness   -> stg_acn.PP_Endianness_little ()
                | AcnGenericTypes.BigEndianness      -> stg_acn.PP_Endianness_big ()

            | ENUM_SET_VALUE     newVal         -> newVal.Value.ToString()
            | TERMINATION_PATTERN  nullByte     -> nullByte.ToString()
            | MAPPING_FUNCTION   fncName        -> fncName.Value
            | POST_ENCODING_FUNCTION fncName    -> fncName.Value
            | PRE_DECODING_FUNCTION fncName     -> fncName.Value
        )
    
    let children =
        t.children |>
        List.map(fun ch ->
            let sImplMode = 
                match ch.asn1Type with
                | Some chType -> printAcnParamType chType
                | None        -> null
            let chType = printTypeEncSpec ch.childEncodingSpec
            stg_acn.PrintTypeChild ch.name.Value (ch.argumentList |> List.map(fun z -> z.AsString)) sImplMode chType )
    stg_acn.PrintType props children


let printInASignleFile (r:AcnAst) outDir newFile (tasToPrint:TypeAssignmentInfo list)=
    
    let allTasses = r.files |> List.collect(fun f -> f.modules) |> List.collect (fun m -> m.typeAssignments)
    let tasToPrintSet = tasToPrint |> List.map(fun ts -> ts.tasName) |> Set.ofList
    let modulesContent = 
        let tasses = 
            allTasses |> 
            List.filter(fun tas -> tasToPrintSet.Contains tas.name.Value) |>
            List.map(fun tas -> stg_acn.PrintTypeAssignment tas.name.Value (tas.acnParameters |> List.map printAcnParam ) [] (printTypeEncSpec tas.typeEncodingSpec))

        stg_acn.PrintModule "SingleModuleName" tasses


    let outFileName = Path.Combine(outDir, newFile)
    File.WriteAllText(outFileName, modulesContent.Replace("\r",""))




