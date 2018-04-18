module PrintAcn

open System
open System.IO
open FsUtils
open CommonTypes

open AcnCreateFromAntlr



let printAcnParamType (pt:Asn1AcnAst.AcnParamType) =
        match pt with
        | Asn1AcnAst.AcnPrmInteger   _         -> "INTEGER"
        | Asn1AcnAst.AcnPrmBoolean   _         -> "BOOLEAN"
        | Asn1AcnAst.AcnPrmNullType  _         -> "NULL"
        | Asn1AcnAst.AcnPrmRefType    (md,ts)  -> sprintf "%s" ts.Value

let printAcnParam (p:Asn1AcnAst.AcnParameter) =
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
                | Asn1AcnAst.NextByte    -> stg_acn.PP_Aligment_byte()
                | Asn1AcnAst.NextWord    -> stg_acn.PP_Aligment_word()
                | Asn1AcnAst.NextDWord   -> stg_acn.PP_Aligment_dword()
            | ENCODE_VALUES                     -> stg_acn.PP_EncodeValues ()
            | PRESENT_WHEN       prWhenList     -> 
                let prCons =
                    prWhenList |>
                    List.map(fun pw ->
                        match pw with
                        | GP_PresenceBool  repPath          -> stg_acn.PP_PresentWhenConditionBool repPath.AsString
                        | GP_PresenceInt   (repPath, intVal)  -> stg_acn.PP_PresentWhenConditionInt repPath.AsString intVal.Value
                        | GP_PresenceStr   (repPath, strVal)  -> stg_acn.PP_PresentWhenConditionStr repPath.AsString strVal.Value)
                stg_acn.PP_PresentWhen prCons
            | TRUE_VALUE         trueVal        -> stg_acn.PP_TrueValue trueVal.Value
            | FALSE_VALUE        falseVal       -> stg_acn.PP_FalseValue falseVal.Value
            | PATTERN            pattern        -> stg_acn.PP_NullValue pattern.Value
            | CHOICE_DETERMINANT  relPath       -> stg_acn.PP_ChoiceDeterminant relPath.AsString
            | ENDIANNES          endian         -> 
                match endian with
                | Asn1AcnAst.LittleEndianness   -> stg_acn.PP_Endianness_little ()
                | Asn1AcnAst.BigEndianness      -> stg_acn.PP_Endianness_big ()

            | ENUM_SET_VALUE     newVal         -> newVal.Value.ToString()
            | TERMINATION_PATTERN  nullByte     -> nullByte.ToString()
            | MAPPING_FUNCTION   fncName        -> fncName.Value
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




