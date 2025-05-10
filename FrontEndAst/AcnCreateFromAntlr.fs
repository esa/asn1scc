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

module AcnCreateFromAntlr

open System
open System.Linq
open System.Numerics
open Antlr.Acn
open Antlr.Runtime.Tree
open Antlr.Runtime
open CommonTypes
open FsUtils
open FE_TypeDefinition
open AcnGenericCreateFromAntlr


open AcnGenericTypes
open Asn1AcnAst
open Asn1AcnAstUtilFunctions
open Language

let private tryGetProp (props:GenericAcnProperty list) fnPropKind =
    match props |> List.choose fnPropKind  with
    | e::_  -> Some e
    | _     -> None

let private  getEndiannessProperty (props:GenericAcnProperty list) =
    tryGetProp props (fun x -> match x with ENDIANNESS e -> Some e | _ -> None)

let private getIntSizeProperty  errLoc (props:GenericAcnProperty list) =
    match tryGetProp props (fun x -> match x with SIZE e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_Fixed           v)   -> Some(AcnGenericTypes.Fixed (v.Value))
    | Some (GP_NullTerminated   )   ->
        match tryGetProp props (fun x -> match x with TERMINATION_PATTERN e -> Some e | _ -> None) with
        | Some bitPattern    ->
            match bitPattern.Value.Length % 8 <> 0 with
            | true  -> raise(SemanticError(bitPattern.Location, sprintf "termination-pattern value must be a sequence of bytes"  ))
            | false ->
                let ba = bitStringValueToByteArray bitPattern |> Seq.toList
                Some(AcnGenericTypes.IntNullTerminated ba)
        | None      -> Some(AcnGenericTypes.IntNullTerminated ([byte 0]))
    | Some (GP_SizeDeterminant _)   -> raise(SemanticError(errLoc ,"Expecting an Integer value or an ACN constant as value for the size property"))

let private getStringSizeProperty (minSize:BigInteger) (maxSize:BigInteger) errLoc (props:GenericAcnProperty list) =
    match tryGetProp props (fun x -> match x with SIZE e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_Fixed           v)   ->
        match minSize = maxSize with
        | false ->
            let errMessage = sprintf "The size constraints of the ASN.1  allows variable items (%A .. %A). Therefore, you should either remove the size property (in which case the size determinant will be encoded automatically exactly like uPER), or use a an Integer field as size determinant"  minSize maxSize
            raise(SemanticError(errLoc ,errMessage))
        | true  ->
            match minSize = v.Value with
            | true  -> None
            | false -> raise(SemanticError(errLoc , (sprintf "The size property must either be ommited or have the fixed value %A" minSize)))
    | Some (GP_NullTerminated   )   ->
        match tryGetProp props (fun x -> match x with TERMINATION_PATTERN e -> Some e | _ -> None) with
        | Some bitPattern    ->
            match bitPattern.Value.Length % 8 <> 0 with
            | true  -> raise(SemanticError(bitPattern.Location, sprintf "termination-pattern value must be a sequence of bytes"  ))
            | false ->
                let ba = bitStringValueToByteArray bitPattern |> Seq.toList
                Some(AcnGenericTypes.StrNullTerminated ba)
        | None      -> Some(AcnGenericTypes.StrNullTerminated ([byte 0]))
    | Some (GP_SizeDeterminant fld)   -> (Some (AcnGenericTypes.StrExternalField fld))

let private getSizeableSizeProperty (minSize:BigInteger) (maxSize:BigInteger) errLoc (props:GenericAcnProperty list) =
    match tryGetProp props (fun x -> match x with SIZE e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_Fixed           v)   ->
        match minSize = maxSize with
        | false ->
            let errMessage = sprintf "The size constraints of the ASN.1  allows variable items (%A .. %A). Therefore, you should either remove the size property (in which case the size determinant will be encoded automatically exactly like uPER), or use a an Integer field as size determinant"  minSize maxSize
            raise(SemanticError(errLoc ,errMessage))
        | true  ->
            match minSize = v.Value with
            | true  -> None
            | false -> raise(SemanticError(errLoc , (sprintf "The size property must either be ommited or have the fixed value %A" minSize)))
    | Some (GP_SizeDeterminant fld)   -> (Some (SzExternalField fld))
    | Some (GP_NullTerminated   )   ->
        match tryGetProp props (fun x -> match x with TERMINATION_PATTERN e -> Some e | _ -> None) with
        | Some b    -> Some(AcnGenericTypes.SzNullTerminated b)
        | None      -> raise(SemanticError(errLoc , (sprintf "No 'termination-pattern' was provided")))

let private getIntEncodingProperty errLoc (props:GenericAcnProperty list) =
    match tryGetProp props (fun x -> match x with ENCODING e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_PosInt         ) ->  Some (AcnGenericTypes.PosInt)
    | Some (GP_TwosComplement ) ->  Some (AcnGenericTypes.TwosComplement)
    | Some (GP_Ascii          ) ->  Some (AcnGenericTypes.IntAscii)
    | Some (GP_BCD            ) ->  Some (AcnGenericTypes.BCD)
    | Some (GP_IEEE754_32     )
    | Some (GP_IEEE754_64     ) ->   raise(SemanticError(errLoc ,"The encoding property was expected to be one of 'pos-int','twos-complement','BCD' or 'ASCII' "))

let private getMappingFunctionProperty acnErrLoc (props:GenericAcnProperty list) =
    match tryGetProp props (fun x -> match x with MAPPING_FUNCTION (md,fn) -> Some (md,fn) | _ -> None) with
    | None  -> None
    | Some (md,fn)  ->
        let newMd = md |> Option.map(fun m -> {StringLoc.Value = ToC m.Value; Location=m.Location})
        let newFn = {StringLoc.Value = ToC fn.Value; Location=fn.Location}
        Some (AcnGenericTypes.MappingFunction (newMd,newFn))

let private getPostEncodingFunction (props:GenericAcnProperty list) =
    match tryGetProp props (fun x -> match x with POST_ENCODING_FUNCTION (md,fn)-> Some (md,fn) | _ -> None) with
    | None  -> None
    | Some (md,fn)  -> Some (AcnGenericTypes.POST_ENCODING_FUNCTION (md,fn))

let private getPreDecodingFunction  (props:GenericAcnProperty list) =
    match tryGetProp props (fun x -> match x with PRE_DECODING_FUNCTION (md,fn) -> Some (md,fn) | _ -> None) with
    | None  -> None
    | Some (md,fn)  -> Some (AcnGenericTypes.PRE_DECODING_FUNCTION (md,fn))


let private getRealEncodingProperty errLoc (props:GenericAcnProperty list) =
    match tryGetProp props (fun x -> match x with ENCODING e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_PosInt         )
    | Some (GP_TwosComplement )
    | Some (GP_Ascii          )
    | Some (GP_BCD            ) ->  raise(SemanticError(errLoc ,"Invalid encoding property value. Expecting 'IEEE754-1985-32' or 'IEEE754-1985-64'"))
    | Some (GP_IEEE754_32     ) ->  Some (AcnGenericTypes.IEEE754_32)
    | Some (GP_IEEE754_64     ) ->  Some (AcnGenericTypes.IEEE754_64)

let private getStringEncodingProperty errLoc (props:GenericAcnProperty list) =
    match tryGetProp props (fun x -> match x with ENCODING e -> Some e | _ -> None) with
    | None  -> None
    | Some (GP_PosInt         )
    | Some (GP_TwosComplement )
    | Some (GP_IEEE754_32     )
    | Some (GP_IEEE754_64     )
    | Some (GP_BCD            ) ->  raise(SemanticError(errLoc ,"The encoding property was expected to be 'ASCII' or empty"))
    | Some (GP_Ascii          ) -> Some (AcnGenericTypes.StrAscii)

let private substAcnArg (acnParamSubst: Map<string, AcnGenericTypes.RelativePath>) (arg: AcnGenericTypes.RelativePath): AcnGenericTypes.RelativePath =
    match arg with
    | RelativePath [] -> arg
    | RelativePath (hd :: rest) ->
        acnParamSubst.TryFind hd.Value |>
        Option.map (fun subst -> match subst with RelativePath subst -> RelativePath (subst @ rest)) |>
        Option.defaultValue arg

let private substAcnArgs (acnParamSubst: Map<string, AcnGenericTypes.RelativePath>) (acnArgs : AcnGenericTypes.RelativePath list): AcnGenericTypes.RelativePath list =
    acnArgs |> List.map (substAcnArg acnParamSubst)

let private addAcnSubst (acnParamSubst: Map<string, AcnGenericTypes.RelativePath>)
                        (acnParams: AcnParameter list)
                        (acnArgs : AcnGenericTypes.RelativePath list): Map<string, AcnGenericTypes.RelativePath> =
    assert (acnParams.Length = acnArgs.Length)
    let add (curr: Map<string, AcnGenericTypes.RelativePath>) (p: AcnParameter, acnArg: AcnGenericTypes.RelativePath): Map<string, AcnGenericTypes.RelativePath> =
        let substed = substAcnArg curr acnArg
        curr |> Map.add p.name substed

    List.fold add acnParamSubst (List.zip acnParams acnArgs)

let checkIntHasEnoughSpace acnEncodingClass (hasMappingFunction:bool) acnErrLoc0 asn1Min asn1Max =
    let check_ (minEnc : BigInteger) (maxEnc:BigInteger) =
        match minEnc <= asn1Min && asn1Max <= maxEnc with
        | true                              -> ()
        | false  when not (asn1Max <= maxEnc)     ->
            let errMessage = sprintf "The applied ACN encoding does not allow values larger than %A to be encoded, while the corresponding ASN.1 type allows values up to %A" maxEnc asn1Max
            raise(SemanticError(acnErrLoc0, errMessage))
        | false                             ->
            let errMessage = sprintf "The applied ACN encoding does not allow values smaller than %A to be encoded, while the corresponding ASN.1 type allows values up to %A" minEnc asn1Min
            raise(SemanticError(acnErrLoc0, errMessage))
    let checkBinary isUnsigned (lengthInBiths:BigInteger) =
        match isUnsigned  with
        | true              -> check_ 0I (BigInteger.Pow(2I, int lengthInBiths) - 1I)
        | false             -> check_ (-BigInteger.Pow(2I, int (lengthInBiths - 1I))) ((BigInteger.Pow(2I, int (lengthInBiths - 1I))) - 1I)
    let checkAscii isUnsigned (lengthInBiths:BigInteger) =
        let digits = int (lengthInBiths / 8I)
        match isUnsigned  with
        | true              -> check_ 0I (BigInteger.Pow(10I, digits) - 1I)
        | false             -> check_ (-BigInteger.Pow(10I, digits - 1)) ((BigInteger.Pow(10I, digits - 1)) - 1I)
    let checkBCD (lengthInBiths:BigInteger) =
        let digits = int (lengthInBiths / 4I)
        check_ 0I (BigInteger.Pow(10I, digits) - 1I)

    match hasMappingFunction with
    | true  -> ()       //when there is a mapping function we are performing no size check
    | false ->
        match acnEncodingClass with
        |Integer_uPER                                   -> ()
        |PositiveInteger_ConstSize_8                    -> checkBinary true 8I
        |PositiveInteger_ConstSize_big_endian_16        -> checkBinary true 16I
        |PositiveInteger_ConstSize_little_endian_16     -> checkBinary true 16I
        |PositiveInteger_ConstSize_big_endian_32        -> checkBinary true 32I
        |PositiveInteger_ConstSize_little_endian_32     -> checkBinary true 32I
        |PositiveInteger_ConstSize_big_endian_64        -> checkBinary true 64I
        |PositiveInteger_ConstSize_little_endian_64     -> checkBinary true 64I
        |PositiveInteger_ConstSize nBits                -> checkBinary true nBits
        |TwosComplement_ConstSize_8                     -> checkBinary false 8I
        |TwosComplement_ConstSize_big_endian_16         -> checkBinary false 16I
        |TwosComplement_ConstSize_little_endian_16      -> checkBinary false 16I
        |TwosComplement_ConstSize_big_endian_32         -> checkBinary false 32I
        |TwosComplement_ConstSize_little_endian_32      -> checkBinary false 32I
        |TwosComplement_ConstSize_big_endian_64         -> checkBinary false 64I
        |TwosComplement_ConstSize_little_endian_64      -> checkBinary false 64I
        |TwosComplement_ConstSize nBits                 -> checkBinary false nBits
        |ASCII_ConstSize nBits                          -> checkAscii false nBits
        |ASCII_VarSize_NullTerminated _                 -> ()
        |ASCII_UINT_ConstSize nBits                     -> checkAscii false nBits
        |ASCII_UINT_VarSize_NullTerminated _            -> ()
        |BCD_ConstSize nBits                            -> checkBCD nBits
        |BCD_VarSize_NullTerminated _                   -> ()

let private removeTypePrefix (typePrefix : String) (typeName : string)=
    match typePrefix.Length > 0 with
    | false -> typeName
    | true  ->
        match typeName.StartsWith(typePrefix) with
        | false -> typeName
        | true  -> (typeName.Substring(typePrefix.Length))

let getAcnDeterminantName (id : ReferenceToType) =
    match id with
    | ReferenceToType path ->
        match path with
        | (MD mdName)::(TA tasName)::(PRM prmName)::[]   -> ToC2 prmName
        | _ ->
            let longName = id.AcnAbsPath.Tail |> Seq.StrJoin "_"
            ToC2(longName.Replace("#","elem"))


let private mergeInteger (asn1:Asn1Ast.AstRoot) (lms:(ProgrammingLanguage*LanguageMacros) list) (loc:SrcLoc)  (typeAssignmentInfo : AssignmentInfo option) (acnErrLoc: SrcLoc option) (props:GenericAcnProperty list) cons withcons thisTypeCons (tdarg:GetTypeDefinition_arg) (us:Asn1AcnMergeState) =
    let declare_IntegerNoRTL       (l:ProgrammingLanguage)     = (asn1.args.getBasicLang l).declare_IntegerNoRTL
    let declare_PosIntegerNoRTL    (l:ProgrammingLanguage)     = (asn1.args.getBasicLang l).declare_PosIntegerNoRTL

    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let rootCons = cons |> List.filter(fun c -> match c with RangeRootConstraint _  | RangeRootConstraint2 _ -> true | _ -> false)
    let uperRange    = uPER.getIntTypeConstraintUperRange cons  loc
    let thisTypeUperRange = uPER.getIntTypeConstraintUperRange thisTypeCons loc
    let intClass = getIntEncodingClassByUperRange asn1.args thisTypeUperRange
    uPER.getIntTypeConstraintUperRange (cons@withcons)  loc |> ignore
    let acnProperties =
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                IntegerAcnProperties.encodingProp    = getIntEncodingProperty acnErrLoc props
                sizeProp                             = getIntSizeProperty acnErrLoc props
                endiannessProp                       = getEndiannessProperty props
                mappingFunction                      = getMappingFunctionProperty acnErrLoc props
            }
        | None  -> {IntegerAcnProperties.encodingProp = None; sizeProp = None; endiannessProp = None; mappingFunction = None }
    let uperMinSizeInBits, uperMaxSizeInBits =
        match rootCons with
        | []  -> uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes uperRange
        | _   ->
            let mn,mx = uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes uperRange
            1I + mn, 1I + mx

    let isUnsigned = intClass.IsPositive
        (*
        match uperRange with
        | Concrete  (a,b) when a >= 0I && rootCons.IsEmpty-> true      //[a, b]
        | Concrete  _                  -> false
        | NegInf    _                  -> false    //(-inf, b]
        | PosInf   a when a >= 0I  && rootCons.IsEmpty    -> true     //[a, +inf)
        | PosInf  _                    -> false    //[a, +inf)
        | Full    _                    -> false    // (-inf, +inf)
        *)

    let alignment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetIntEncodingClass asn1.args.integerSizeInBytes alignment acnErrLoc0 acnProperties uperMinSizeInBits uperMaxSizeInBits isUnsigned

    let asn1Min, asn1Max =
        match uperRange with
        | Concrete  (a,b)                   -> (a,b)
        | NegInf    b                       -> (asn1.args.SIntMin, b)    //(-inf, b]
        | PosInf   a    when a >= 0I        -> (a, asn1.args.UIntMax)     //[a, +inf)
        | PosInf   a                        -> (a, asn1.args.SIntMax)
        | Full                              -> (asn1.args.SIntMin, asn1.args.SIntMax)


    let rtlFnc =
        match uperRange with
        | Full                        -> Some declare_IntegerNoRTL
        | PosInf (a)  when a=0I       -> Some declare_PosIntegerNoRTL
        | _                           ->
            match typeAssignmentInfo with
            | Some (ValueAssignmentInfo _)  ->
                //this is a case where a new type should have been defined. However, the type is under a value assignment e.g. max-Instr INTEGER (12 .. 504) ::= 12
                //and currently, we do not declare a new type
                match asn1Min >= 0I with
                | true                       -> Some declare_PosIntegerNoRTL
                | false                      -> Some declare_IntegerNoRTL
            | _                             -> None

    checkIntHasEnoughSpace acnEncodingClass acnProperties.mappingFunction.IsSome acnErrLoc0 asn1Min asn1Max
    let typeDef, us1 = getPrimitiveTypeDefinition {tdarg with rtlFnc = rtlFnc} us
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createInteger_u lm (ReferenceToType tdarg.curPath) typeDef intClass uperRange)) |> Map.ofList
    {Integer.acnProperties = acnProperties; cons = cons; withcons = withcons; uperRange = uperRange;uperMinSizeInBits=uperMinSizeInBits;
        uperMaxSizeInBits=uperMaxSizeInBits; acnEncodingClass = acnEncodingClass; intClass=intClass;  acnMinSizeInBits=acnMinSizeInBits;
        acnMaxSizeInBits = acnMaxSizeInBits; isUnsigned= isUnsigned; typeDef=typeDef; defaultInitVal="0"; definitionOrRef=definitionOrRef}, us1

let private mergeReal (asn1: Asn1Ast.AstRoot) (lms:(ProgrammingLanguage*LanguageMacros) list) (loc: SrcLoc) (acnErrLoc: SrcLoc option) (props: GenericAcnProperty list) cons withcons (tdarg: GetTypeDefinition_arg) (us: Asn1AcnMergeState) =
    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let getRtlTypeName  (l:ProgrammingLanguage) = (asn1.args.getBasicLang l).getRealRtlTypeName
    //check for invalid properties
    props |>
    Seq.iter(fun pr ->
        match pr with
        | SIZE  _   -> raise(SemanticError(acnErrLoc0, "Acn property 'size' cannot be applied to REAL types"))
        | _         -> ())

    let acnProperties =
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                RealAcnProperties.encodingProp    = getRealEncodingProperty acnErrLoc props
                endiannessProp                    = getEndiannessProperty props
            }
        | None  -> {RealAcnProperties.encodingProp = None; endiannessProp = None }
    let uperRange    = uPER.getRealTypeConstraintUperRange cons loc
    let uperMaxSizeInBits=(5I+asn1.args.integerSizeInBytes)*8I
    let uperMinSizeInBits=8I
    let alignment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetRealEncodingClass alignment loc acnProperties uperMinSizeInBits uperMaxSizeInBits
    match acnEncodingClass with
    | Real_IEEE754_64_big_endian        when asn1.args.floatingPointSizeInBytes = 4I -> raise(SemanticError(acnErrLoc0, "Acn property 'IEEE754-1985-64' cannot be applied when -fpWordSize  4"))
    | Real_IEEE754_64_little_endian     when asn1.args.floatingPointSizeInBytes = 4I -> raise(SemanticError(acnErrLoc0, "Acn property 'IEEE754-1985-64' cannot be applied when -fpWordSize  4"))
    | _                                                                              -> ()
    let typeDef, us1 = getPrimitiveTypeDefinition {tdarg with rtlFnc = Some getRtlTypeName} us
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createReal_u asn1.args lm (ReferenceToType tdarg.curPath) typeDef acnEncodingClass )) |> Map.ofList

    {Real.acnProperties = acnProperties; cons = cons; withcons = withcons; uperRange=uperRange; uperMaxSizeInBits=uperMaxSizeInBits;
        uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits;
        acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef; definitionOrRef=definitionOrRef; defaultInitVal="0"}, us1


let private mergeObjectIdentifier (asn1: Asn1Ast.AstRoot) (lms:(ProgrammingLanguage*LanguageMacros) list) (relativeId: bool) (loc: SrcLoc) (acnErrLoc: SrcLoc option) (props: GenericAcnProperty list) cons withcons (tdarg: GetTypeDefinition_arg) (us: Asn1AcnMergeState) =
    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let getRtlTypeName  (l:ProgrammingLanguage) = (asn1.args.getBasicLang l).getObjectIdentifierRtlTypeName relativeId

    //check for invalid properties
    props |> Seq.iter(fun pr -> raise(SemanticError(acnErrLoc0, "Acn property cannot be applied to OBJECT IDENTIFIER types")))

    let acnProperties =  {ObjectIdTypeAcnProperties.sizeProperties = None; itemProperties = None }

    let lengthSize = 16I //+++ SOS, must be valiader
    let compUperMinSizeInBits, compUperMaxSizeInBits = 8I, 16I*8I//uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes Full
    let uperMaxSizeInBits= compUperMaxSizeInBits*asn1.args.objectIdentifierMaxLength + lengthSize
    let uperMinSizeInBits= lengthSize
    let alignment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnMinSizeInBits, acnMaxSizeInBits= uperMinSizeInBits, uperMaxSizeInBits
    let typeDef, us1 = getPrimitiveTypeDefinition {tdarg with rtlFnc = Some getRtlTypeName} us
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createObjectIdentifier_u lm (ReferenceToType tdarg.curPath) typeDef)) |> Map.ofList
    {ObjectIdentifier.acnProperties = acnProperties; cons = cons; withcons = withcons; relativeObjectId=relativeId; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef; definitionOrRef=definitionOrRef}, us1

let private mergeTimeType (asn1: Asn1Ast.AstRoot) (lms:(ProgrammingLanguage*LanguageMacros) list) (timeClass: TimeTypeClass) (loc: SrcLoc) (acnErrLoc: SrcLoc option) (props: GenericAcnProperty list) cons withcons (tdarg: GetTypeDefinition_arg) (us: Asn1AcnMergeState) =
    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let getRtlTypeName  (l:ProgrammingLanguage) = (asn1.args.getBasicLang l).getTimeRtlTypeName timeClass

    //check for invalid properties
    props |> Seq.iter(fun pr -> raise(SemanticError(acnErrLoc0, "Acn property cannot be applied to TIME types")))


    let uperMaxSizeInBits= 1I
    let uperMinSizeInBits= 400I  //+++
    let acnMinSizeInBits, acnMaxSizeInBits= uperMinSizeInBits, uperMaxSizeInBits
    let typeDef, us1 = getPrimitiveTypeDefinition {tdarg with rtlFnc = Some getRtlTypeName} us
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createTimeType_u lm (ReferenceToType tdarg.curPath) typeDef timeClass)) |> Map.ofList
    {TimeType.timeClass = timeClass; uperMaxSizeInBits=uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef; cons = cons; withcons = withcons; definitionOrRef=definitionOrRef }, us1

type EnmStrGetTypeDefinition_arg =
    | EnmStrGetTypeDefinition_arg of GetTypeDefinition_arg
    | AcnPrmGetTypeDefinition of ((ScopeNode list)* string*string)

let isCharacterAllowedByAlphabetConstrains (cons:IA5StringConstraint list) (b:byte) =
    let ch = System.Convert.ToChar b
    let isCharacterAllowedByAlphabetConstrain (c:IA5StringConstraint)  =
        let con_or      _ e1 e2 _ s =  e1 || e2, s
        let con_and     _ e1 e2 s =  e1 && e2, s
        let con_not     _ e  s =  not e, s
        let con_except  _ e1 e2 s =  e1 && (not e2), s
        let con_root    _ e s = e,s
        let con_root2   _ e1 e2  s =  e1 || e2, s

        let foldRangeCharCon  (c:CharTypeConstraint)   =
            Asn1Fold.foldRangeTypeConstraint
                con_or con_and con_not con_except con_root con_root2
                (fun _ (v:string)  s  -> v.Contains ch ,s)
                (fun _ v1 v2  minIsIn maxIsIn s   ->
                    let ret =
                        match minIsIn, maxIsIn with
                        | true, true    -> v1 <= ch && ch <= v2
                        | true, false   -> v1 <= ch && ch < v2
                        | false, true   -> v1 < ch && ch <= v2
                        | false, false  -> v1 < ch && ch < v2
                    ret, s)
                (fun _ v1 minIsIn s   -> (if minIsIn then (v1 <= ch) else (v1 < ch)), s)
                (fun _ v2 maxIsIn s   -> (if maxIsIn then (ch<=v2) else (ch<v2)), s)
                c
                0 |> fst

        Asn1Fold.foldStringTypeConstraint2
            con_or con_and con_not con_except con_root con_root2
            (fun _ _  s           -> true ,s)
            (fun _ _ s            -> true, s)
            (fun _ alphcon s      -> foldRangeCharCon alphcon,s)
            c
            0 |> fst
    cons |> Seq.forall isCharacterAllowedByAlphabetConstrain

let private mergeStringType (asn1: Asn1Ast.AstRoot) (lms:(ProgrammingLanguage*LanguageMacros) list) (t: Asn1Ast.Asn1Type option) (loc: SrcLoc) (acnErrLoc: SrcLoc option) (props: GenericAcnProperty list) cons withcons defaultCharSet isNumeric (tdarg: EnmStrGetTypeDefinition_arg) (us: Asn1AcnMergeState) =
    let acnErrLoc0 = match acnErrLoc with Some a -> a | None -> loc
    let sizeUperRange = uPER.getStringSizeUperRange cons loc
    let sizeUperAcnRange = uPER.getStringSizeUperRange (cons@withcons) loc
    let uperCharSet = uPER.getSrtingAlphaUperRange cons defaultCharSet loc
    let uminSize, umaxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange
    let aminSize, amaxSize = uPER.getSizeMinAndMaxValue loc sizeUperAcnRange
    let minSize = {SIZE.uper = uminSize; acn = aminSize }
    let maxSize = {SIZE.uper = umaxSize; acn = amaxSize }
    let acnProperties =
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                StringAcnProperties.encodingProp    = getStringEncodingProperty acnErrLoc props
                sizeProp                            = getStringSizeProperty minSize.acn maxSize.acn acnErrLoc props
            }
        | None  -> {StringAcnProperties.encodingProp = None; sizeProp = None }

    let charSize =  GetNumberOfBitsForNonNegativeInteger (BigInteger (uperCharSet.Length-1))
    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize.uper maxSize.uper charSize
    let acnUperMinSizeInBits, acnUperMaxSizeInBits = uPER.getSizeableTypeSize minSize.acn maxSize.acn charSize
    let alignment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)

    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetStringEncodingClass alignment loc acnProperties acnUperMinSizeInBits acnUperMaxSizeInBits minSize.acn maxSize.acn uperCharSet

    match acnEncodingClass with
    | Acn_Enc_String_uPER                                _                -> ()
    | Acn_Enc_String_uPER_Ascii                          _                -> ()
    | Acn_Enc_String_Ascii_Null_Terminated                (_, nullChars)     ->
        let errMsg = "The termination-pattern defines a character which belongs to the allowed values of the ASN.1 type. Use another value in the termination-pattern or apply different constraints in the ASN.1 type."
        match nullChars |> Seq.filter(fun c -> c <> 0uy) |> Seq.tryFind (isCharacterAllowedByAlphabetConstrains cons) with
        | Some _ -> raise(SemanticError(acnErrLoc0, errMsg))
        | None   -> ()
    | Acn_Enc_String_Ascii_External_Field_Determinant       (_,relativePath) -> ()
    | Acn_Enc_String_CharIndex_External_Field_Determinant   (_,relativePath) -> ()

    let typeDef, us1 =
        match tdarg with
        | EnmStrGetTypeDefinition_arg tdarg   -> getStringTypeDefinition tdarg us
        | AcnPrmGetTypeDefinition (curPath, md, ts)   ->
            let lanDefs, us1 =
                ProgrammingLanguage.ActiveLanguages |> foldMap (fun us l ->
                    let ib = asn1.args.getBasicLang l
                    let itm, ns = registerStringTypeDefinition us (l,ib) (ReferenceToType curPath) (FEI_Reference2OtherType (ReferenceToType [MD md; TA ts]))
                    (l,itm), ns) us
            lanDefs |> Map.ofList, us1
    let id =
        match tdarg with
        | EnmStrGetTypeDefinition_arg tdarg   -> ReferenceToType tdarg.curPath
        | AcnPrmGetTypeDefinition (curPath, md, ts)   -> ReferenceToType curPath
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createString_u lm id typeDef uperCharSet minSize maxSize)) |> Map.ofList

    {StringType.acnProperties = acnProperties; cons = cons; withcons = withcons; minSize=minSize; maxSize =maxSize;
        uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; uperCharSet=uperCharSet;
        acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits;
        acnMaxSizeInBits = acnMaxSizeInBits;isNumeric=isNumeric; typeDef=typeDef; definitionOrRef=definitionOrRef}, us1

let private mergeOctetStringType (asn1: Asn1Ast.AstRoot)
                                 (lms:(ProgrammingLanguage*LanguageMacros) list)                         
                                 (loc: SrcLoc)
                                 (acnErrLoc: SrcLoc option)
                                 (props: GenericAcnProperty list)
                                 cons
                                 withcons
                                 (tdarg: GetTypeDefinition_arg)
                                 (acnParamSubst: Map<string, AcnGenericTypes.RelativePath>)
                                 (us: Asn1AcnMergeState) =
    let sizeUperRange = uPER.getOctetStringUperRange cons loc
    let sizeUperAcnRange = uPER.getOctetStringUperRange (cons@withcons) loc

    let uminSize, umaxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange
    let aminSize, amaxSize = uPER.getSizeMinAndMaxValue loc sizeUperAcnRange
    let minSize = {SIZE.uper = uminSize; acn = aminSize }
    let maxSize = {SIZE.uper = umaxSize; acn = amaxSize }
    let hasNCount = minSize.uper <> maxSize.uper

    //let minSize, maxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange
    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize.uper maxSize.uper 8I
    let acnUperMinSizeInBits, acnUperMaxSizeInBits = uPER.getSizeableTypeSize minSize.acn maxSize.acn 8I
    //let fixAsn1Size = match minSize = maxSize with true -> Some minSize | false -> None

    let acnProperties =
        match acnErrLoc with
        | Some acnErrLoc    -> {SizeableAcnProperties.sizeProp = getSizeableSizeProperty minSize.acn maxSize.acn acnErrLoc props}
        | None              -> {SizeableAcnProperties.sizeProp = None}
    let sizeDetArg = acnProperties.sizeProp |> Option.bind (fun p ->
        match p with
        | SzExternalField f -> Some f
        | SzNullTerminated _ -> None
    )
    let acnArgsSubsted = substAcnArgs acnParamSubst (sizeDetArg |> Option.toList)

    let alignment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetOctetStringEncodingClass alignment loc acnProperties acnUperMinSizeInBits acnUperMaxSizeInBits minSize.acn maxSize.acn hasNCount

    let typeDef, us1 = getSizeableTypeDefinition tdarg us
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createOctetString_u lm (ReferenceToType tdarg.curPath) typeDef minSize maxSize)) |> Map.ofList
    {OctetString.acnProperties = acnProperties; cons = cons; withcons = withcons; minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; acnArgs = acnArgsSubsted; typeDef=typeDef; definitionOrRef=definitionOrRef}, us1

let private mergeBitStringType (asn1:Asn1Ast.AstRoot)
                               (lms:(ProgrammingLanguage*LanguageMacros) list)
                               (namedBitList: NamedBit0 list)
                               (loc:SrcLoc)
                               (acnErrLoc: SrcLoc option)
                               (props:GenericAcnProperty list)
                               cons
                               withcons
                               (tdarg:GetTypeDefinition_arg)
                               (acnParamSubst: Map<string, AcnGenericTypes.RelativePath>)
                               (us:Asn1AcnMergeState) =
    let newNamedBitList =
        namedBitList |> List.map(fun nb ->
            let resolvedValue =
                match nb._value with
                | IDV_IntegerValue       intVal     -> intVal.Value
                | IDV_DefinedValue   (mdVal, refVal) -> Asn1Ast.GetValueAsInt (Asn1Ast.GetBaseValue mdVal refVal asn1) asn1
            {NamedBit1.Name = nb.Name;  _value = nb._value; resolvedValue = resolvedValue; Comments = nb.Comments})

    let sizeUperRange = uPER.getBitStringUperRange cons loc
    let sizeUperAcnRange = uPER.getBitStringUperRange (cons@withcons) loc
    //let minSize, maxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange

    let uminSize, umaxSize = uPER.getSizeMinAndMaxValue loc sizeUperRange
    let aminSize, amaxSize = uPER.getSizeMinAndMaxValue loc sizeUperAcnRange
    let minSize = {SIZE.uper = uminSize; acn = aminSize }
    let maxSize = {SIZE.uper = umaxSize; acn = amaxSize }
    let hasNCount = minSize.uper <> maxSize.uper
    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize.uper maxSize.uper 1I
    let acnUperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize.acn maxSize.acn 1I
    //let fixAsn1Size = match minSize = maxSize with true -> Some minSize | false -> None

    let acnProperties =
        match acnErrLoc with
        | Some acnErrLoc    -> { SizeableAcnProperties.sizeProp  = getSizeableSizeProperty minSize.acn maxSize.acn acnErrLoc props}
        | None              -> {SizeableAcnProperties.sizeProp = None }
    let sizeDetArg = acnProperties.sizeProp |> Option.bind (fun p ->
        match p with
        | SzExternalField f -> Some f
        | SzNullTerminated _ -> None
    )
    let acnArgsSubsted = substAcnArgs acnParamSubst (sizeDetArg |> Option.toList)

    let alignment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetBitStringEncodingClass alignment loc acnProperties acnUperMinSizeInBits uperMaxSizeInBits minSize.acn maxSize.acn hasNCount

    let typeDef, us1 = getSizeableTypeDefinition tdarg us
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createBitString_u lm (ReferenceToType tdarg.curPath) typeDef minSize maxSize newNamedBitList)) |> Map.ofList
    {BitString.acnProperties = acnProperties; cons = cons; withcons = withcons; minSize=minSize; maxSize =maxSize;
        uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;
        acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; acnArgs = acnArgsSubsted; typeDef=typeDef; namedBitList = newNamedBitList; definitionOrRef=definitionOrRef}, us1

let private mergeNullType (args: CommandLineSettings) (lms:(ProgrammingLanguage*LanguageMacros) list) (acnErrLoc: SrcLoc option) (props: GenericAcnProperty list) (tdarg: GetTypeDefinition_arg) (us: Asn1AcnMergeState) =
    let getRtlTypeName  (l:ProgrammingLanguage) = (args.getBasicLang l).getNullRtlTypeName
    let acnProperties =
        match acnErrLoc with
        | Some acnErrLoc    -> { NullTypeAcnProperties.encodingPattern  = tryGetProp props (fun x -> match x with PATTERN e -> Some e | _ -> None); savePosition = props |> Seq.exists(fun z -> match z with SAVE_POSITION -> true | _ -> false )}
        | None              -> {NullTypeAcnProperties.encodingPattern = None; savePosition = false }

    let alignment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetNullEncodingClass alignment loc acnProperties
    let typeDef, us1 = getPrimitiveTypeDefinition {tdarg with rtlFnc = Some getRtlTypeName} us
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createNull_u lm (ReferenceToType tdarg.curPath) typeDef)) |> Map.ofList
    {NullType.acnProperties = acnProperties; uperMaxSizeInBits = 0I; uperMinSizeInBits=0I;
        acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits;
        typeDef=typeDef; defaultInitVal="0"; definitionOrRef=definitionOrRef}, us1

let createAcnBooleanProperties (props: GenericAcnProperty list) (acnErrLoc: SrcLoc option) =
    match acnErrLoc with
    | Some acnErrLoc    ->
        let trueValue = tryGetProp props (fun x -> match x with TRUE_VALUE e -> Some e | _ -> None)
        let falseValue = tryGetProp props (fun x -> match x with FALSE_VALUE e -> Some e | _ -> None)
        match trueValue, falseValue with
        | Some tv, Some fv  ->
            //if both true and false values are defined, then the length of the values must be the same and greater than 0
            if tv.Value.Length = 0 || fv.Value.Length = 0 then
                raise(SemanticError(acnErrLoc, "The length of the 'true-value' and 'false-value' properties must be greater than 0"))
            else if tv.Value.Length <> fv.Value.Length then
                raise(SemanticError(acnErrLoc, "The length of the 'true-value' and 'false-value' properties must be the same"))
            else if tv.Value = fv.Value then
                raise(SemanticError(acnErrLoc, "The 'true-value' and 'false-value' properties must have different values"))
            else
                {BooleanAcnProperties.encodingPattern  = Some( TrueFalseValueEncoding(tv, fv))}
        | Some tv, None    ->
            if tv.Value.Length = 0 then
                raise(SemanticError(acnErrLoc, "The length of the 'true-value' property must be greater than 0"))
            else
                {BooleanAcnProperties.encodingPattern = Some( TrueValueEncoding(tv))}
        | None, Some fv    ->
            if fv.Value.Length = 0 then
                raise(SemanticError(acnErrLoc, "The length of the 'false-value' property must be greater than 0"))
            else
                {BooleanAcnProperties.encodingPattern = Some( FalseValueEncoding(fv))}
        | None, None        -> {BooleanAcnProperties.encodingPattern = None }
    | None              -> {BooleanAcnProperties.encodingPattern = None }

let private mergeBooleanType (args: CommandLineSettings) (lms:(ProgrammingLanguage*LanguageMacros) list) (acnErrLoc: SrcLoc option) (props: GenericAcnProperty list) cons withcons (tdarg: GetTypeDefinition_arg) (us: Asn1AcnMergeState)=
    let getRtlTypeName  (l:ProgrammingLanguage) = (args.getBasicLang l).getBoolRtlTypeName
    let size =
        match acnErrLoc with
        | Some acnErrLoc    ->
            match tryGetProp props (fun x -> match x with SIZE e -> Some e | _ -> None) with
            | None                -> None
            | Some (GP_Fixed v)   -> Some(v.Value)
            | Some _              -> raise(SemanticError(acnErrLoc ,"Expecting an Integer value or an ACN constant as value for the size property"))
        | None                    -> None

    let alignWithZeros (bitVal: StringLoc) =
        match size with
        | None      -> bitVal
        | Some sz  when sz >= bitVal.Value.Length.AsBigInt ->
            let zeros = new String('0', ((int sz) - bitVal.Value.Length)  )
            {StringLoc.Value = zeros + bitVal.Value; Location = bitVal.Location}
        | Some sz          ->
            let errMsg = sprintf "The size of the pattern '%s' is greater than the encoding size (%d)" bitVal.Value (int sz)
            raise(SemanticError(bitVal.Location ,errMsg))

    let acnProperties = createAcnBooleanProperties props acnErrLoc
    let alignment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetBooleanEncodingClass alignment loc acnProperties
    let typeDef, us1 = getPrimitiveTypeDefinition {tdarg with rtlFnc = Some getRtlTypeName} us
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createBoolean_u lm (ReferenceToType tdarg.curPath) typeDef)) |> Map.ofList
    {Boolean.acnProperties = acnProperties; cons = cons; withcons = withcons;uperMaxSizeInBits = 1I; uperMinSizeInBits=1I;
        acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; typeDef=typeDef; defaultInitVal="false"; definitionOrRef=definitionOrRef}, us1

let private mergeEnumerated (asn1: Asn1Ast.AstRoot) (lms:(ProgrammingLanguage*LanguageMacros) list) (items: Asn1Ast.NamedItem list) (originalLocation: SrcLoc option, loc: SrcLoc)
    (acnErrLoc: SrcLoc option) (acnType: AcnTypeEncodingSpec option) (props: GenericAcnProperty list) cons withcons (tdarg: EnmStrGetTypeDefinition_arg) (us: Asn1AcnMergeState) =
    let loc =
        match originalLocation with
        | None  -> loc
        | Some l -> l
    let encodeValues =
        match tryGetProp props (fun x -> match x with ENCODE_VALUES -> Some true | _ -> None) with
        | Some  true    -> true
        | _             -> false
    let typeDef, proposedEnmName, us1 =
        match tdarg with
        | EnmStrGetTypeDefinition_arg tdarg   ->
            let proposedEnmName =
                ProgrammingLanguage.ActiveLanguages |> List.map(fun l ->
                    l, FE_TypeDefinition.getProposedTypeDefName us l (ReferenceToType tdarg.enmItemTypeDefPath) |> fst) |> Map.ofList

            let typeDef, us1 = getEnumeratedTypeDefinition tdarg us
            typeDef, proposedEnmName, us1
        | AcnPrmGetTypeDefinition (curPath, md, ts)   ->
            let proposedEnmName =
                ProgrammingLanguage.ActiveLanguages |> List.map(fun l ->
                    l, FE_TypeDefinition.getProposedTypeDefName us l (ReferenceToType [MD md; TA ts]) |> fst) |> Map.ofList
            let lanDefs, us1 =
                ProgrammingLanguage.ActiveLanguages |> foldMap (fun us l ->
                    let ib = asn1.args.getBasicLang l
                    let itm, ns = registerEnumeratedTypeDefinition us (l,ib) (ReferenceToType curPath) (FEI_Reference2OtherType (ReferenceToType [MD md; TA ts]))
                    (l,itm), ns) us
            lanDefs |> Map.ofList, proposedEnmName, us1

    let allocatedValuesToAllEnumItems (namedItems:Asn1Ast.NamedItem list) =
        let createAsn1ValueByBigInt biVal = {Asn1Ast.Asn1Value.Kind = Asn1Ast.IntegerValue (IntLoc.ByValue biVal); Asn1Ast.Asn1Value.moduleName="";  Asn1Ast.Location = emptyLocation; Asn1Ast.id = ReferenceToValue([],[])}
        let allocated   = namedItems |> List.filter(fun x -> x._value.IsSome)
        let unallocated = namedItems |> List.filter(fun x -> x._value.IsNone)
        let rec allocateItems (unallocated:Asn1Ast.NamedItem list) (allocated:Asn1Ast.NamedItem list) potentialValue: Asn1Ast.NamedItem list =
            match unallocated with
            | []    -> allocated
            |x::xs  ->
                let rec getValue (allocated:Asn1Ast.NamedItem list) (vl:BigInteger) =
                    match allocated |> Seq.exists(fun ni -> match ni._value with Some value -> vl = (Asn1Ast.GetValueAsInt value asn1) | None -> false) with
                    | false -> vl
                    | true  -> getValue allocated (vl + 1I)
                let vl = getValue allocated potentialValue
                let newAllocatedItems = allocated@[{x with _value = Some (createAsn1ValueByBigInt vl)} ]
                allocateItems xs newAllocatedItems (vl + 1I)
        let newItems = allocateItems unallocated allocated 0I |> List.sortBy(fun ni -> namedItems |> Seq.findIndex(fun x -> x.Name.Value = ni.Name.Value) )
        newItems

    let mapItem (i:int) (itm:Asn1Ast.NamedItem) =
        let definitionValue = Asn1Ast.GetValueAsInt itm._value.Value asn1
        let c_name, s_name, a_name =
            match asn1.args.renamePolicy with
            | AlwaysPrefixTypeName      ->
                let typeName0 lang =
                    match ProgrammingLanguage.ActiveLanguages |> Seq.exists ((=) lang) with
                    | true -> proposedEnmName[lang]
                    | false -> ""
                    (*
                    let langTypeDef = typeDef.[lang]
                    let rec aux (langTypeDef:FE_EnumeratedTypeDefinition) =
                        match langTypeDef.kind with
                        | NonPrimitiveNewTypeDefinition         -> langTypeDef.typeName
                        | NonPrimitiveNewSubTypeDefinition sub  -> aux sub
                        | NonPrimitiveReference2OtherType       -> langTypeDef.typeName
                    aux langTypeDef
                    *)
                let c_tpname = removeTypePrefix  asn1.args.TypePrefix (typeName0 C)
                let s_tpname = removeTypePrefix  asn1.args.TypePrefix (typeName0 Scala)
                let a_tpname = removeTypePrefix  asn1.args.TypePrefix (typeName0 Ada)
                c_tpname + "_" + itm.c_name, s_tpname + "_" + itm.scala_name, a_tpname + "_" + itm.ada_name
            | _     ->
                asn1.args.TypePrefix + itm.c_name, asn1.args.TypePrefix + itm.scala_name, asn1.args.TypePrefix + itm.ada_name

        match acnType with
        | None  ->
            let acnEncodeValue = (BigInteger i)
            {NamedItem.Name = itm.Name; Comments = itm.Comments; c_name = c_name; scala_name = s_name;  ada_name = a_name; definitionValue = definitionValue; acnEncodeValue = acnEncodeValue}
        | Some acnType ->
            let acnEncodeValue =
                match tryGetProp props (fun x -> match x with ENCODE_VALUES -> Some true | _ -> None) with
                | Some _    ->
                    match acnType.children |> Seq.tryFind(fun z -> z.name.Value = itm.Name.Value) with
                    | Some acnNameItem    ->
                        match tryGetProp acnNameItem.childEncodingSpec.acnProperties (fun x -> match x with ENUM_SET_VALUE newIntVal -> Some newIntVal | _ -> None) with
                        | Some intVal   -> intVal.Value
                        | None          -> definitionValue
                    | None      -> definitionValue
                | None      -> (BigInteger i)
            {NamedItem.Name = itm.Name; Comments = itm.Comments; c_name = c_name; scala_name = s_name; ada_name = a_name; definitionValue = definitionValue; acnEncodeValue = acnEncodeValue}

    let items0, userDefinedValues =
        match items |> Seq.exists (fun nm -> nm._value.IsSome) with
        | false -> allocatedValuesToAllEnumItems items, false
        | true -> allocatedValuesToAllEnumItems items, true
    let uperSizeInBits = GetNumberOfBitsForNonNegativeInteger(BigInteger((Seq.length items) - 1))
    let items = items0|> List.mapi mapItem

    let acnProperties =
        match acnErrLoc with
        | Some acnErrLoc    ->
            {
                IntegerAcnProperties.encodingProp    = getIntEncodingProperty acnErrLoc props
                sizeProp                             = getIntSizeProperty  acnErrLoc props
                endiannessProp                       = getEndiannessProperty props
                mappingFunction                      = getMappingFunctionProperty acnErrLoc props
            }
        | None  -> {IntegerAcnProperties.encodingProp = None; sizeProp = None; endiannessProp = None; mappingFunction = None }

    let alignment = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetEnumeratedEncodingClass asn1.args.integerSizeInBytes items alignment loc acnProperties uperSizeInBits uperSizeInBits encodeValues

    let validItems = items |> List.filter (Asn1Fold.isValidValueGeneric cons (fun a b -> a = b.Name.Value)) |> List.sortBy(fun x -> x.definitionValue)

    match validItems with
    | [] -> raise(SemanticError(loc, (sprintf "The constraints defined for this type do not allow any value" )))
    | _  -> ()
    let id =
        match tdarg with
        | EnmStrGetTypeDefinition_arg tdarg   -> ReferenceToType tdarg.curPath
        | AcnPrmGetTypeDefinition (curPath, md, ts)   -> ReferenceToType curPath
    let definitionOrRef = 
        lms |> List.map(fun (l,lm) ->
           (l, DAstTypeDefinition.createEnumerated_u asn1.args lm id typeDef items validItems)) |> Map.ofList
    {Enumerated.acnProperties = acnProperties; items=items; validItems=validItems; cons = cons; withcons = withcons;uperMaxSizeInBits = uperSizeInBits;
        uperMinSizeInBits=uperSizeInBits;encodeValues=encodeValues; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits=acnMinSizeInBits;
        acnMaxSizeInBits = acnMaxSizeInBits;userDefinedValues=userDefinedValues; typeDef=typeDef; definitionOrRef=definitionOrRef}, us1

let rec private mergeAcnEncodingSpecs (thisType:AcnTypeEncodingSpec option) (baseType:AcnTypeEncodingSpec option) =
    match thisType, baseType with
    | None, None    -> None
    | None, Some x -> Some x
    | Some x, None -> Some x
    | Some thisType, Some baseType ->
        let mergedProperties = thisType.acnProperties@baseType.acnProperties
        let mergedChildren =
            let thisTypeNames = thisType.children |> List.map(fun x -> x.name)
            let baseTypeNames =
                baseType.children |>
                List.filter(fun x ->
                    match thisType.children with
                    | []    -> true                             //if this type has children specification then use this type children
                    | _     -> false (*x.asn1Type.IsNone*)) |>  //otherwise use base
                List.map(fun x -> x.name)
            let combinedNames = thisTypeNames@baseTypeNames |> Seq.distinctBy(fun x -> x.Value) |> Seq.toList
            combinedNames |>
                List.choose(fun nm ->
                    let e1 = thisType.children |> Seq.tryFind(fun x -> x.name = nm)
                    let e2 = baseType.children |> Seq.tryFind(fun x -> x.name = nm)
                    match e1, e2 with
                    | None, None    -> None
                    | None, Some x  -> Some x
                    | Some x, None  -> Some x
                    | Some thisChild, Some baseChild    ->
                        match mergeAcnEncodingSpecs (Some thisChild.childEncodingSpec) (Some baseChild.childEncodingSpec) with
                        | Some combinedEncodingSpec  ->
                            Some ({name = nm; childEncodingSpec = combinedEncodingSpec; asn1Type = thisChild.asn1Type; argumentList = thisChild.argumentList; comments=thisChild.comments})
                        | None                      -> None)

        Some {AcnTypeEncodingSpec.acnProperties = mergedProperties; children = mergedChildren; loc = thisType.loc; comments = thisType.comments; position=thisType.position; antlrSubTree=thisType.antlrSubTree}

(*
    match asn1Type with
    | AcnTypes.Integer   -> {Asn1Type.Kind = Ast.Integer; Constraints=[]; Location=emptyLocation; AcnProperties=acnChild.Properties}
    | AcnTypes.Boolean   -> {Asn1Type.Kind = Ast.Boolean; Constraints=[]; Location=emptyLocation; AcnProperties=acnChild.Properties}
    | AcnTypes.NullType  -> {Asn1Type.Kind = Ast.NullType; Constraints=[]; Location=emptyLocation; AcnProperties=acnChild.Properties}
    | AcnTypes.RefTypeCon(md,ts)  -> {Asn1Type.Kind = Ast.ReferenceType(md,ts, false); Constraints=[]; Location=emptyLocation; AcnProperties=acnChild.Properties}

*)

(*Removes Type inclusion and RangeConstraint_MIN_MAX constraints*)
let rec fixConstraint (asn1:Asn1Ast.AstRoot) (c:Asn1Ast.Asn1Constraint) =
    let intersect c1 cs =
        //let str = (c1::cs) |> List.map(fun (z:Asn1Ast.Asn1Constraint) -> z.Asn1Con) |> Seq.StrJoin (" | ")
        let str (a:Asn1Ast.Asn1Constraint) (b:Asn1Ast.Asn1Constraint) =
            sprintf "(%s) | (%s)" a.Asn1Con b.Asn1Con
        cs |> List.fold(fun fc cc -> (Asn1Ast.IntersectionConstraint (str fc cc, fc,cc))) c1
    let mergeConstraints (cc:Asn1Ast.Asn1Constraint list)  constConstr =
        match cc with
        | []        -> []
        | x1::[]    -> [constConstr x1]
        | x1::xs    ->
            let sc = intersect x1 xs
            [constConstr sc]
    let mergeConstraints2 (c1:Asn1Ast.Asn1Constraint list) (c2:Asn1Ast.Asn1Constraint list) constConstr =
        match c1,c2 with
        | [], []    -> []
        | _::_, []  -> c1
        | [], _::_  -> c2
        | x1::xs1, x2::xs2      -> [constConstr (intersect x1 xs1)  (intersect x2 xs2) ]

    Asn1Ast.foldConstraint
        (fun s v            -> [Asn1Ast.SingleValueConstraint(s, v)] )
        (fun s a b b1 b2    -> [Asn1Ast.RangeConstraint(s, a,b,b1,b2)])
        (fun s a b1         -> [Asn1Ast.RangeConstraint_val_MAX (s, a,b1)])
        (fun s a b1         -> [Asn1Ast.RangeConstraint_MIN_val (s, a,b1)])
        (fun ()         -> [])
        (fun s md ts      ->
            let actTypeAllCons = Asn1Ast.GetActualTypeByNameAllConsIncluded md ts asn1
            actTypeAllCons.Constraints |> List.collect (fixConstraint asn1))
        (fun s sc         -> mergeConstraints sc (fun c -> Asn1Ast.SizeConstraint (s, c)) )
        (fun s sc         -> mergeConstraints sc (fun c -> Asn1Ast.AlphabetConstraint (s, c)) )
        (fun s sc l       -> mergeConstraints sc (fun c -> Asn1Ast.WithComponentConstraint(s, c, l)) )
        (fun s ni cons    ->
            match cons with
            | None          -> [{ni with Constraint = None}]
            | Some cons     -> cons |> List.map (fun c -> {ni with Constraint = Some c}))
        (fun s nameItems      -> [Asn1Ast.WithComponentsConstraint (s, nameItems |> List.collect id)])
        (fun s c1 c2 b        -> mergeConstraints2 c1 c2 (fun c1 c2 -> Asn1Ast.UnionConstraint   (s, c1,c2,b)))
        (fun s c1 c2          -> mergeConstraints2 c1 c2 (fun c1 c2 -> Asn1Ast.IntersectionConstraint  (s, c1,c2)))
        (fun s sc             -> mergeConstraints sc (fun c -> Asn1Ast.AllExceptConstraint (s,c)) )
        (fun s c1 c2          ->
            match c1,c2 with
            | [], []    -> []
            | _::_, []  -> c1
            | [], _::_  -> mergeConstraints c2 (fun c -> Asn1Ast.AllExceptConstraint (s, c))
            | _, _      -> mergeConstraints2 c1 c2 (fun c1 c2 -> Asn1Ast.ExceptConstraint   (s, c1,c2)))
        (fun s sc             -> mergeConstraints sc (fun c -> Asn1Ast.RootConstraint (s, c)) )
        (fun s c1 c2          -> mergeConstraints2 c1 c2 (fun c1 c2 -> Asn1Ast.RootConstraint2   (s, c1,c2)))
        c

let rec private mapAcnParamTypeToAcnAcnInsertedType (asn1:Asn1Ast.AstRoot) (lms:(ProgrammingLanguage*LanguageMacros) list) (acn:AcnAst) (p:AcnParamType) (props:GenericAcnProperty list) (curPath : ScopeNode list) (us:Asn1AcnMergeState) =
    let  acnAlignment     = tryGetProp props (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    match p with
    | AcnPrmInteger acnErrLoc ->
        let acnProperties = {IntegerAcnProperties.encodingProp = getIntEncodingProperty acnErrLoc props; sizeProp = getIntSizeProperty acnErrLoc props; endiannessProp = getEndiannessProperty props; mappingFunction = getMappingFunctionProperty acnErrLoc props}
        let encProp =
            match acnProperties.encodingProp with
            | Some e -> e
            | None   -> raise(SemanticError(acnErrLoc, "Mandatory ACN property 'encoding' is missing"))
        let isUnsigned, uperRange =
            match encProp with
            | PosInt            -> true, PosInf(0I)
            | TwosComplement    -> false, Full
            | IntAscii          -> false, Full
            | BCD               -> true, PosInf(0I)
        let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetIntEncodingClass asn1.args.integerSizeInBytes acnAlignment acnErrLoc acnProperties 0I 0I isUnsigned

        let checkIntHasEnoughSpace asn1Min asn1Max =
            checkIntHasEnoughSpace acnEncodingClass acnProperties.mappingFunction.IsSome acnErrLoc asn1Min asn1Max
        let  intClass = getIntEncodingClassByUperRange asn1.args uperRange
        AcnInteger ({AcnInteger.acnProperties=acnProperties; acnAlignment=acnAlignment; acnEncodingClass = acnEncodingClass;
            Location = acnErrLoc; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; cons=[]; withcons=[];isUnsigned=isUnsigned;
            uperRange= uperRange; intClass=intClass; checkIntHasEnoughSpace=checkIntHasEnoughSpace; inheritInfo=None; defaultValue="0"}), us

    | AcnPrmBoolean  acnErrLoc ->
        let acnProperties = createAcnBooleanProperties props (Some acnErrLoc)

        let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetBooleanEncodingClass acnAlignment acnErrLoc acnProperties
        AcnBoolean ({AcnBoolean.acnProperties=acnProperties; acnAlignment=acnAlignment; Location = acnErrLoc; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; defaultValue="false"}), us
    | AcnPrmNullType acnErrLoc ->
        let acnProperties = { NullTypeAcnProperties.encodingPattern  = tryGetProp props (fun x -> match x with PATTERN e -> Some e | _ -> None); savePosition = props |> Seq.exists(fun z -> match z with SAVE_POSITION -> true | _ -> false )}
        let acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetNullEncodingClass acnAlignment acnErrLoc acnProperties
        AcnNullType ({AcnNullType.acnProperties=acnProperties; acnAlignment=acnAlignment; Location = acnErrLoc; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits; defaultValue="0"}), us
    | AcnPrmRefType (md,ts)->
        let asn1Type0 = Asn1Ast.GetBaseTypeByName md ts asn1
        let baseProps =
            acn.files |>
            List.collect(fun f -> f.modules) |>
            List.filter(fun acm -> acm.name.Value = md.Value) |>
            List.collect(fun acm -> acm.typeAssignments) |>
            List.filter(fun act -> act.name.Value = ts.Value) |>
            List.collect(fun act -> act.typeEncodingSpec.acnProperties)
        let props =
            match props with
            | []    -> baseProps
            | _     -> props
        match asn1Type0.Kind with
        | Asn1Ast.Enumerated nmItems    ->
            let cons =  asn1Type0.Constraints |> List.collect (fixConstraint asn1) |> List.map (ConstraintsMapping.getEnumConstraint asn1 asn1Type0)
            let enumerated, ns = mergeEnumerated asn1 lms nmItems (None, ts.Location) (Some ts.Location) (Some {AcnTypeEncodingSpec.acnProperties = props; children = []; loc=ts.Location; comments = []; position=(ts.Location, ts.Location); antlrSubTree=None}) props cons [] (AcnPrmGetTypeDefinition (curPath,md.Value,ts.Value)) us
            AcnReferenceToEnumerated({AcnReferenceToEnumerated.modName = md; tasName = ts; enumerated = enumerated; acnAlignment= acnAlignment}), ns
        | Asn1Ast.IA5String
        | Asn1Ast.NumericString  ->
            let isNumeric = (asn1Type0.Kind = Asn1Ast.NumericString)
            let cons =  asn1Type0.Constraints |> List.collect (fixConstraint asn1) |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 asn1Type0)
            let defaultCharSet = [|for i in 0..127 -> System.Convert.ToChar(i) |]
            let str, ns = mergeStringType asn1 lms None ts.Location (Some ts.Location) props cons [] defaultCharSet isNumeric (AcnPrmGetTypeDefinition (curPath,md.Value,ts.Value)) us
            AcnReferenceToIA5String({AcnReferenceToIA5String.modName = md; tasName = ts; str = str; acnAlignment= acnAlignment}), ns
        | Asn1Ast.Integer       ->
            let cons =  asn1Type0.Constraints |> List.collect (fixConstraint asn1) |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 asn1Type0)
            let uperRange    = uPER.getIntTypeConstraintUperRange cons  ts.Location
            let uperMinSizeInBits, uperMaxSizeInBits = uPER.getRequiredBitsForIntUperEncoding asn1.args.integerSizeInBytes uperRange
            let acnProperties = {IntegerAcnProperties.encodingProp = getIntEncodingProperty ts.Location props; sizeProp = getIntSizeProperty ts.Location props; endiannessProp = getEndiannessProperty props; mappingFunction = getMappingFunctionProperty ts.Location props}
            let isUnsigned =
                match acnProperties.encodingProp with
                | Some encProp ->
                    match encProp with
                    | PosInt            -> true
                    | TwosComplement    -> false
                    | IntAscii          -> false
                    | BCD               -> true
                | None         ->
                    match acnProperties.sizeProp.IsNone && acnProperties.endiannessProp.IsNone with
                    | true  ->
                        match uperRange with
                        | Concrete  (a,b) when a >= 0I -> true      //[a, b]
                        | Concrete  _                  -> false
                        | NegInf    _                  -> false    //(-inf, b]
                        | PosInf   a when a >= 0I  -> true     //[a, +inf)
                        | PosInf  _                    -> false    //[a, +inf)
                        | Full                         -> false    // (-inf, +inf)
                    | false -> raise(SemanticError(ts.Location, "Mandatory ACN property 'encoding' is missing"))

            let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits  =
                AcnEncodingClasses.GetIntEncodingClass asn1.args.integerSizeInBytes acnAlignment ts.Location acnProperties uperMinSizeInBits uperMaxSizeInBits isUnsigned

            let checkIntHasEnoughSpace asn1Min asn1Max =
                checkIntHasEnoughSpace acnEncodingClass acnProperties.mappingFunction.IsSome ts.Location asn1Min asn1Max

            let  intClass = getIntEncodingClassByUperRange asn1.args uperRange
            AcnInteger ({AcnInteger.acnProperties=acnProperties; acnAlignment=acnAlignment; acnEncodingClass = acnEncodingClass;  Location = ts.Location; acnMinSizeInBits=acnMinSizeInBits; acnMaxSizeInBits = acnMaxSizeInBits;cons=cons;withcons=[];isUnsigned=isUnsigned; uperRange= uperRange; intClass=intClass; defaultValue="0"; checkIntHasEnoughSpace=checkIntHasEnoughSpace; inheritInfo=Some {InheritanceInfo.modName=md.Value; tasName=ts.Value;hasAdditionalConstraints=false;}}), us
        | _                               ->
            let newParma  =
                match asn1Type0.Kind with
                //| Asn1Ast.Integer           -> AcnPrmInteger ts.Location
                | Asn1Ast.NullType          -> AcnPrmNullType ts.Location
                | Asn1Ast.Boolean           -> AcnPrmBoolean ts.Location
                | Asn1Ast.ReferenceType rf  -> AcnPrmRefType (rf.modName, rf.tasName)
                | _                         -> raise(SemanticError(ts.Location, "Invalid type for ACN inserted type"))
            let baseTypeAcnProps =
                match tryFindAcnTypeByName md ts acn with
                | None      -> []
                | Some x    -> x.typeEncodingSpec.acnProperties
            mapAcnParamTypeToAcnAcnInsertedType asn1 lms acn newParma (props@baseTypeAcnProps) curPath us


let rec mapAnyConstraint (asn1:Asn1Ast.AstRoot) (t:Asn1Ast.Asn1Type) (cons:Asn1Ast.Asn1Constraint list) =
    let fixConstraint  = (fixConstraint asn1)
    match t.Kind with
    | Asn1Ast.Integer                  ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 t) |> List.map IntegerTypeConstraint
    | Asn1Ast.Real                     ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getRealTypeConstraint asn1 t) |> List.map RealTypeConstraint
    | Asn1Ast.ObjectIdentifier
    | Asn1Ast.RelativeObjectIdentifier         ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getObjectIdConstraint asn1 t) |> List.map ObjectIdConstraint
    | Asn1Ast.TimeType   tc      ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getTimeConstraint asn1 t) |> List.map TimeConstraint
    | Asn1Ast.IA5String                ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t) |> List.map IA5StringConstraint
    | Asn1Ast.NumericString            ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t) |> List.map IA5StringConstraint
    | Asn1Ast.OctetString              ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getOctetStringConstraint asn1 t) |> List.map OctetStringConstraint
    | Asn1Ast.BitString    namedBitList            ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBitStringConstraint asn1 t) |> List.map BitStringConstraint
    | Asn1Ast.NullType                 ->  [NullConstraint]
    | Asn1Ast.Boolean                  ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBoolConstraint asn1 t) |> List.map BoolConstraint
    | Asn1Ast.Enumerated  items        ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getEnumConstraint asn1 t) |> List.map EnumConstraint
    | Asn1Ast.SequenceOf  chType       ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getSequenceOfConstraint asn1 t chType) |> List.map SequenceOfConstraint
    | Asn1Ast.Sequence    children     ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getSeqConstraint asn1 t children) |> List.map SeqConstraint
    | Asn1Ast.Choice      children     ->
        cons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getChoiceConstraint asn1 t children) |> List.map ChoiceConstraint
    | Asn1Ast.ReferenceType rf    ->
        let oldBaseType  = Asn1Ast.GetBaseTypeByName rf.modName rf.tasName asn1
        mapAnyConstraint asn1 oldBaseType cons

let rec private mergeType  (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (typeIdsSet : Map<String,int>) (lms:(ProgrammingLanguage*LanguageMacros) list) 
                           (m:Asn1Ast.Asn1Module) 
                           (t:Asn1Ast.Asn1Type) 
                           (curPath : ScopeNode list)
                           (typeDefPath : ScopeNode list)
                           (enmItemTypeDefPath : ScopeNode list)
                           (acnType:AcnTypeEncodingSpec option)
                           (originalLocation : SrcLoc option)
                           (refTypeCons:Asn1Ast.Asn1Constraint list)      // constraints applied to this type originating from reference types --> uPER visible
                           (withCons:Asn1Ast.Asn1Constraint list)         // constraints applied to this type originating from with component and  with components --> non uPER visible
                           (acnArgs : AcnGenericTypes.RelativePath list)
                           (acnParamSubst: Map<string, AcnGenericTypes.RelativePath>)
                           (acnParameters   : AcnParameter list)
                           (inheritInfo : InheritanceInfo option)
                           (typeAssignmentInfo : AssignmentInfo option)
                           (referencedBy : TypeAssignmentInfo list)
                           (caller : TypeAssignmentInfo option)
                           (us:Asn1AcnMergeState) : (Asn1Type*Asn1AcnMergeState)=

    let maxAlignmentOf (aligns: AcnAlignment option list): AcnAlignment option =
      aligns |> List.maxBy (fun a -> a |> Option.map (fun a -> a.nbBits) |> Option.defaultValue 0I)

    let acnProps =
        match acnType with
        | None      -> []
        | Some x    -> x.acnProperties
    let acnErrLoc = acnType |> Option.map(fun x -> x.loc)
    let combinedProperties = acnProps
    let alignment = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
    let allCons = t.Constraints@refTypeCons@withCons
    let debug = ReferenceToType curPath
    //if debug.AsString.EndsWith "ALPHA-DELETE-DIAGNOSTIC-PARAMETER-REPORT-STRUCTURES-GENERIC" then
    //    printfn "%s" debug.AsString
    //if debug.AsString = "RW90-DATAVIEW.UART-Config.timeout" then
    //    printfn "%s" debug.AsString

    let tfdArg = {GetTypeDefinition_arg.asn1TypeKind = t.Kind; loc = t.Location; curPath = curPath; typeDefPath = typeDefPath; enmItemTypeDefPath = enmItemTypeDefPath; inheritInfo = inheritInfo; typeAssignmentInfo = typeAssignmentInfo; rtlFnc = None; blm=asn1.args.blm}
    let fixConstraint = fixConstraint asn1

    let asn1Kind, kindState =
        match t.Kind with
        | Asn1Ast.Integer                  ->
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 t)
            let thisTypeCons = t.Constraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIntegerTypeConstraint asn1 t)
            let o, us1  = mergeInteger asn1 lms t.Location typeAssignmentInfo  acnErrLoc combinedProperties cons wcons thisTypeCons tfdArg us
            Integer o, us1
        | Asn1Ast.Real                     ->
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getRealTypeConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getRealTypeConstraint asn1 t)
            let o, us1 = mergeReal asn1 lms t.Location acnErrLoc combinedProperties cons wcons tfdArg us
            Real o, us1
        | Asn1Ast.ObjectIdentifier
        | Asn1Ast.RelativeObjectIdentifier         ->
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getObjectIdConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getObjectIdConstraint asn1 t)
            let o, us1 = mergeObjectIdentifier asn1 lms (t.Kind=Asn1Ast.RelativeObjectIdentifier) t.Location acnErrLoc combinedProperties cons wcons tfdArg us
            ObjectIdentifier o, us1
        | Asn1Ast.TimeType   tc      ->
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getTimeConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getTimeConstraint asn1 t)
            let o, us1 = mergeTimeType asn1 lms tc t.Location acnErrLoc combinedProperties cons wcons tfdArg us
            TimeType o, us1
        | Asn1Ast.IA5String                ->
            let defaultCharSet = [|for i in 0..127 -> System.Convert.ToChar(i) |]
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let o, us1 = mergeStringType asn1 lms (Some t) t.Location acnErrLoc combinedProperties cons wcons defaultCharSet false (EnmStrGetTypeDefinition_arg tfdArg) us
            IA5String o , us1
        | Asn1Ast.NumericString            ->
            let defaultCharSet = [| ' ';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9'|]

            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getIA5StringConstraint asn1 t)
            let o, us1 = mergeStringType asn1 lms (Some t) t.Location acnErrLoc combinedProperties cons wcons defaultCharSet true (EnmStrGetTypeDefinition_arg tfdArg) us
            NumericString o, us1
        | Asn1Ast.OctetString              ->
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getOctetStringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getOctetStringConstraint asn1 t)
            let o, us1 = mergeOctetStringType asn1 lms t.Location acnErrLoc combinedProperties cons wcons tfdArg acnParamSubst us
            OctetString o, us1
        | Asn1Ast.BitString    namedBitList            ->
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBitStringConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBitStringConstraint asn1 t)
            let o, us1 = mergeBitStringType asn1 lms namedBitList t.Location acnErrLoc combinedProperties cons wcons tfdArg acnParamSubst us
            BitString o, us1
        | Asn1Ast.NullType                 ->
            let constraints = []
            let o, us1 = mergeNullType asn1.args lms acnErrLoc combinedProperties tfdArg us
            NullType o, us1
        | Asn1Ast.Boolean                  ->
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBoolConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getBoolConstraint asn1 t)
            let o, us1 = mergeBooleanType asn1.args lms acnErrLoc combinedProperties cons wcons tfdArg us
            Boolean o, us1
        | Asn1Ast.Enumerated  items        ->
            let cons =  t.Constraints@refTypeCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getEnumConstraint asn1 t)
            let wcons = withCons |> List.collect fixConstraint |> List.map (ConstraintsMapping.getEnumConstraint asn1 t)
            let o, us1 = mergeEnumerated asn1 lms items (originalLocation, t.Location) acnErrLoc acnType combinedProperties cons wcons (EnmStrGetTypeDefinition_arg tfdArg) us
            Enumerated o, us1
        | Asn1Ast.SequenceOf  chType       ->
            let childWithCons = allCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint (_,w,_) -> Some w| _ -> None)
            let myVisibleConstraints = t.Constraints@refTypeCons
            let myNonVisibleConstraints = withCons

            let cons =  myVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getSequenceOfConstraint asn1 t chType)
            let wcons = myNonVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getSequenceOfConstraint asn1 t chType)

            let childEncSpec, acnArgs, sizeDetArg =
                match acnType with
                | None          -> None, [], None
                | Some acnType  ->
                    let sizeDetArg = acnType.acnProperties |> List.tryFindMap (fun prop ->
                        match prop with
                        | SIZE (GP_SizeDeterminant det) -> Some det
                        | _ -> None)
                    match acnType.children with
                    | []    -> None, [], sizeDetArg
                    | c1::[] -> Some c1.childEncodingSpec, c1.argumentList, sizeDetArg
                    | c1::c2::_      -> raise(SemanticError(c1.name.Location, (sprintf "%s Unexpected field name" c2.name.Value)))

            let acnArgsSubsted = substAcnArgs acnParamSubst (acnArgs @ Option.toList sizeDetArg)

            let typeDef, us1 = getSizeableTypeDefinition tfdArg us
            let newChType, us2  = mergeType asn1 acn typeIdsSet lms m chType (curPath@[SQF]) (typeDefPath@[SQF]) (enmItemTypeDefPath@[SQF]) childEncSpec None [] childWithCons acnArgs acnParamSubst [] None None  referencedBy caller us1

            let sizeUperRange = uPER.getSequenceOfUperRange cons t.Location
            let sizeUperAcnRange = uPER.getSequenceOfUperRange (cons@wcons) t.Location

            let uminSize, umaxSize = uPER.getSizeMinAndMaxValue t.Location sizeUperRange
            let aminSize, amaxSize = uPER.getSizeMinAndMaxValue t.Location sizeUperAcnRange
            let minSize = {SIZE.uper = uminSize; acn = aminSize }
            let maxSize = {SIZE.uper = umaxSize; acn = amaxSize }
            let hasNCount = minSize.uper <> maxSize.uper

            let acnProperties =
                match acnErrLoc with
                | Some acnErrLoc    -> { SizeableAcnProperties.sizeProp  = getSizeableSizeProperty minSize.acn maxSize.acn acnErrLoc combinedProperties}
                | None              -> {SizeableAcnProperties.sizeProp = None }

            let uperMinSizeInBits, _ = uPER.getSizeableTypeSize minSize.uper maxSize.uper newChType.uperMinSizeInBits
            let _, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize.uper maxSize.uper newChType.uperMaxSizeInBits

            let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetSequenceOfEncodingClass alignment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize.acn maxSize.acn newChType.acnMinSizeInBits newChType.acnMaxSizeInBits hasNCount
            //   (acnAlignment : AcnGenericTypes.AcnAlignment option) (child : Asn1AcnAst.Asn1Type) (childType: DAst.Asn1Type)
            let maxAlignment = maxAlignmentOf [alignment; newChType.maxAlignment]

            let definitionOrRef = 
                lms |> List.map(fun (l,lm) ->
                   (l, DAstTypeDefinition.createSequenceOf_u lm (ReferenceToType curPath) typeDef acnMinSizeInBits acnMaxSizeInBits minSize maxSize acnEncodingClass alignment maxAlignment newChType)) |> Map.ofList

            let newKind = {SequenceOf.child=newChType; acnProperties   = acnProperties; cons = cons; withcons = wcons;minSize=minSize; maxSize =maxSize; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits=uperMinSizeInBits; acnEncodingClass = acnEncodingClass;  acnMinSizeInBits = acnMinSizeInBits; acnMaxSizeInBits=acnMaxSizeInBits; acnArgs=acnArgsSubsted; typeDef=typeDef; definitionOrRef=definitionOrRef; maxAlignment=maxAlignment}
            SequenceOf newKind, us2
        | Asn1Ast.Sequence children ->
            let childrenNameConstraints = allCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint (_,w) -> Some w| _ -> None) |> List.collect id
            let myVisibleConstraints = refTypeCons@t.Constraints
            let myNonVisibleConstraints = withCons

            let cons =  myVisibleConstraints|> List.collect fixConstraint |> List.map (ConstraintsMapping.getSeqConstraint asn1 t children)
            let wcons = myNonVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getSeqConstraint asn1 t children)

            let typeDef, us1 = getSequenceTypeDefinition tfdArg us

            let mergeChild (cc:ChildSpec option) (c:Asn1Ast.ChildInfo) (us:Asn1AcnMergeState)  =
                let childNamedConstraints = childrenNameConstraints |> List.filter(fun x -> x.Name = c.Name)
                let childWithCons = childNamedConstraints |> List.choose(fun nc -> nc.Constraint)
                let asn1OptionalityFromWithComponents =
                    childNamedConstraints |>
                    List.choose(fun nc ->
                        match nc.Mark with
                        | Asn1Ast.NoMark            -> None
                        | Asn1Ast.MarkPresent       -> Some AlwaysPresent
                        | Asn1Ast.MarkAbsent        -> Some AlwaysAbsent
                        | Asn1Ast.MarkOptional      -> Some (Optional ({Optional.defaultValue = None; acnPresentWhen= None})) ) |>
                    Seq.distinct |> Seq.toList

                let newOptionality =
                    match c.Optionality with
                    | None  ->
                        match asn1OptionalityFromWithComponents with
                        | []  -> ()
                        | _   -> raise(SemanticError(c.Name.Location, (sprintf "component %s is not optional to apply ALWAYS PRESENT or ALWAYS ABSENT constraints" c.Name.Value)))
                        None
                    | Some Asn1Ast.AlwaysAbsent  ->
                        match asn1OptionalityFromWithComponents with
                        | []          -> Some AlwaysAbsent
                        | newOpt::_   -> Some newOpt
                    | Some Asn1Ast.AlwaysPresent  ->
                        match asn1OptionalityFromWithComponents with
                        | []  -> Some AlwaysPresent
                        | newOpt::_   -> Some newOpt
                    | Some (Asn1Ast.Optional opt) ->
                        match asn1OptionalityFromWithComponents with
                        | []
                        | (Optional _)::_   ->
                            Some (Optional {Optional.defaultValue = opt.defaultValue |> Option.map (ValuesMapping.mapValue asn1 c.Type) ; acnPresentWhen = None})
                        | x::_              -> Some x

                let newOptionality =
                    let acnPresentWhenConditions =
                        match cc with
                        | None      -> []
                        | Some cc   ->
                            match tryGetProp cc.childEncodingSpec.acnProperties (fun x -> match x with PRESENT_WHEN e -> Some e | _ -> None) with
                            | None      ->
                                match tryGetProp cc.childEncodingSpec.acnProperties (fun x -> match x with PRESENT_WHEN_EXP e -> Some e | _ -> None) with
                                | None  -> []
                                | Some acnExp -> [PresenceWhenBoolExpression acnExp]
                            | Some lst  ->
                                lst |>
                                List.iter( fun gp ->
                                    match gp with
                                    | GP_PresenceBool l -> ()
                                    | GP_PresenceInt (l,v) ->
                                        let errMsg = sprintf "expecting a single boolean ACN field, or single ACN boolean parameter or a boolean expression composed by ASN.1 fields. The present-when attribute in the form %s==%A can be used only in choice alternatives" l.AsString v.Value
                                        raise(SemanticError(l.location, errMsg))
                                    | GP_PresenceStr (l,s) ->
                                        let errMsg = sprintf "expecting a single boolean ACN field, or single ACN boolean parameter or a boolean expression composed by ASN.1 fields. The present-when attribute in the form %s==%s can be used only in choice alternatives" l.AsString s.Value
                                        raise(SemanticError(l.location, errMsg))         )
                                lst |> List.choose(fun gp -> match gp with GP_PresenceBool l -> Some (PresenceWhenBool l) | _ -> None)
                    let checkForPresentWhenConditions () =
                        match acnPresentWhenConditions with
                        | []    -> ()
                        | _     -> raise(SemanticError(cc.Value.name.Location, (sprintf "present-when attribute cannot be applied here since component %s is not optional" cc.Value.name.Value)))

                    match newOptionality with
                    | None
                    | Some AlwaysAbsent
                    | Some AlwaysPresent  ->
                        checkForPresentWhenConditions ()
                        newOptionality
                    | Some (Optional opt)   ->
                        let acnPresentWhen =
                            match acnPresentWhenConditions with
                            | []        -> None
                            | x::_      -> Some x
                        Some (Optional {Optional.defaultValue = opt.defaultValue ; acnPresentWhen = acnPresentWhen})

                let isOptional =
                    match newOptionality with
                    | None  -> false
                    | Some AlwaysAbsent
                    | Some (Optional _)
                    | Some AlwaysPresent -> true

                match cc with
                | None      ->
                    let newChild, us1 = mergeType asn1 acn typeIdsSet lms m c.Type (curPath@[SEQ_CHILD (c.Name.Value, isOptional)]) (typeDefPath@[SEQ_CHILD (c.Name.Value, isOptional)]) (enmItemTypeDefPath@[SEQ_CHILD (c.Name.Value, isOptional)]) None None [] childWithCons [] acnParamSubst [] None None referencedBy caller us
                    Asn1Child ({Asn1Child.Name = c.Name; _c_name = c.c_name; _scala_name = c.scala_name; _ada_name = c.ada_name; Type = newChild; Optionality = newOptionality; asn1Comments = c.Comments |> Seq.toList; acnComments=[]}), us1
                | Some cc   ->
                    match cc.asn1Type with
                    | None  ->
                        let newChild, us1 = mergeType asn1 acn typeIdsSet lms m c.Type (curPath@[SEQ_CHILD (c.Name.Value, isOptional)]) (typeDefPath@[SEQ_CHILD (c.Name.Value, isOptional)]) (enmItemTypeDefPath@[SEQ_CHILD (c.Name.Value, isOptional)]) (Some cc.childEncodingSpec) None [] childWithCons cc.argumentList acnParamSubst [] None None referencedBy caller us
                        Asn1Child ({Asn1Child.Name = c.Name; _c_name = c.c_name; _scala_name = c.scala_name; _ada_name = c.ada_name; Type = newChild; Optionality = newOptionality; asn1Comments = c.Comments |> Seq.toList; acnComments = cc.comments}), us1
                    | Some xx  ->
                        let newType, us1 = mapAcnParamTypeToAcnAcnInsertedType asn1 lms acn xx cc.childEncodingSpec.acnProperties  (curPath@[SEQ_CHILD (c.Name.Value, isOptional)]) us
                        let id = ReferenceToType(curPath@[SEQ_CHILD (c.Name.Value, isOptional)])
                        let c_name = getAcnDeterminantName id
                        AcnChild({AcnChild.Name = c.Name; c_name = c_name; id = id; Type = newType; Comments = cc.comments |> Seq.toArray}), us1

            let mergedChildren, chus =
                match acnType with
                | None            -> children |> foldMap (fun st ch -> mergeChild None ch st ) us1
                | Some acnEncSpec ->
                    match acnEncSpec.children with
                    | []            -> children |> foldMap (fun st ch -> mergeChild None ch st) us1
                    | acnChildren   ->
                        // MAKE SURE ACN CHILDREN ARE SUPERSET OF ASN1 CHILDREN !!!!!
                        children |> List.filter(fun c -> not (acnChildren |> List.exists(fun c2 -> c2.name = c.Name))) |> List.iter(fun c -> raise(SemanticError(acnEncSpec.loc, (sprintf "No ACN encoding specification was provided for component %s" c.Name.Value)))  )
                        //detect acn inserted children which already defined in ASN.1
                        acnChildren |>
                        List.choose(fun c -> match c.asn1Type with None -> None | Some pt -> (Some (c,pt))) |>
                        List.choose(fun (acnC,pt) ->
                            match children |> List.tryFind(fun asn1C -> asn1C.Name = acnC.name) with
                            | None -> None
                            | Some asn1C -> Some (asn1C, acnC, pt)) |>
                        List.iter(fun (asn1C, acnC, pt) ->
                            raise(SemanticError(acnC.name.Location, (sprintf "Component '%s' cannot be defined as an ACN inserted field. Remove the type '%s' from the ACN file or remove th component from the ANS.1 file" acnC.name.Value (pt.ToString()))))
                        )

                        acnChildren |>
                        foldMap(fun st acnChild ->
                            match children |> Seq.tryFind (fun a -> a.Name = acnChild.name) with
                            | Some x -> mergeChild (Some acnChild) x st
                            | None   ->
                                match acnChild.asn1Type with
                                | Some xx ->
                                    let newType, nest = mapAcnParamTypeToAcnAcnInsertedType asn1 lms acn xx acnChild.childEncodingSpec.acnProperties (curPath@[SEQ_CHILD (acnChild.name.Value, false)]) st
                                    let id = ReferenceToType(curPath@[SEQ_CHILD (acnChild.name.Value, false)])
                                    let c_name = getAcnDeterminantName id
                                    AcnChild({AcnChild.Name = acnChild.name; id = id; c_name = c_name; Type = newType; Comments = acnChild.comments |> Seq.toArray}), nest
                                | None ->
                                    raise(SemanticError(acnChild.name.Location, (sprintf "invalid name %s" acnChild.name.Value)))) us1

            let uperBitMaskSize      = children |> Seq.filter(fun c -> c.Optionality.IsSome) |> Seq.length |> BigInteger
            let asn1Children     = mergedChildren |> List.choose(fun c -> match c with Asn1Child c -> Some c | AcnChild _ -> None)
            let uperMaxChildrenSize  = asn1Children |> List.map(fun x -> x.Type.uperMaxSizeInBits) |> Seq.sum
            let uperMinChildrenSize  = asn1Children |> List.filter(fun x -> x.Optionality.IsNone) |> List.map(fun x -> x.Type.uperMinSizeInBits) |> Seq.sum

            let alignment = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
            let acnBitMaskSize =
                mergedChildren |>
                List.filter(fun c ->
                    match c.Optionality with
                    | Some AlwaysAbsent                                 -> false
                    | Some (Optional p) when p.acnPresentWhen.IsNone    -> true
                    | _                                                 -> false) |> Seq.length |> BigInteger
            let minChildrenSize =
                mergedChildren |>
                List.map(fun c ->
                    match c.Optionality with
                    | Some (Optional _) -> 0I
                    | _                 -> c.acnMinSizeInBits) |> Seq.sum
            let maxChildrenSize = mergedChildren |> List.map(fun c -> c.acnMaxSizeInBits) |> Seq.sum
            let acnMaxSizeInBits = acnBitMaskSize + maxChildrenSize + AcnEncodingClasses.getAlignmentSize alignment
            let acnMinSizeInBits = acnBitMaskSize + minChildrenSize
            let acnProperties =
                {
                    SequenceAcnProperties.postEncodingFunction = tryGetProp combinedProperties (fun x -> match x with POST_ENCODING_FUNCTION (md,fn) -> Some (PostEncodingFunction (md,fn)) | _ -> None);
                    preDecodingFunction = tryGetProp combinedProperties (fun x -> match x with PRE_DECODING_FUNCTION (md,fn) -> Some (PreDecodingFunction (md,fn)) | _ -> None)
                }

            let acnArgsSubsted = substAcnArgs acnParamSubst acnArgs

            let childrenAlignemts = mergedChildren |> List.map(fun c -> match c with Asn1Child c -> c.Type.maxAlignment | AcnChild c -> c.Type.acnAlignment)
            let maxAlignment = maxAlignmentOf (alignment::childrenAlignemts)

            let definitionOrRef = 
                lms |> List.map(fun (l,lm) ->
                   let acnAlignment     = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
                   (l, DAstTypeDefinition.createSequence_u asn1.args lm typeDef (ReferenceToType curPath) acnAlignment maxAlignment acnProperties acnMinSizeInBits acnMaxSizeInBits mergedChildren)) |> Map.ofList

            Sequence ({Sequence.children = mergedChildren;  acnProperties=acnProperties;  cons=cons; withcons = wcons;uperMaxSizeInBits=uperBitMaskSize+uperMaxChildrenSize; uperMinSizeInBits=uperBitMaskSize+uperMinChildrenSize;acnMaxSizeInBits=acnMaxSizeInBits;acnMinSizeInBits=acnMinSizeInBits; acnArgs=acnArgsSubsted; typeDef=typeDef; definitionOrRef=definitionOrRef; maxAlignment=maxAlignment}), chus
        | Asn1Ast.Choice children ->
            let childrenNameConstraints = t.Constraints@refTypeCons |> List.choose(fun c -> match c with Asn1Ast.WithComponentsConstraint (_,w) -> Some w| _ -> None) |> List.collect id
            let myVisibleConstraints = t.Constraints@refTypeCons
            let myNonVisibleConstraints = withCons

            let cons =  myVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getChoiceConstraint asn1 t children)
            let wcons = myNonVisibleConstraints |> List.collect fixConstraint |> List.map (ConstraintsMapping.getChoiceConstraint asn1 t children)
            let typeDef, us1 = getChoiceTypeDefinition tfdArg us

            let mergeChild (cc:ChildSpec option) (c:Asn1Ast.ChildInfo) (us:Asn1AcnMergeState) =
                let childNamedConstraints = childrenNameConstraints |> List.filter(fun x -> x.Name = c.Name)
                let childWithCons = childNamedConstraints |> List.choose(fun nc -> nc.Constraint)
                let asn1OptionalityFromWithComponents =
                    childNamedConstraints |>
                    List.choose(fun nc ->
                        match nc.Mark with
                        | Asn1Ast.NoMark            -> None
                        | Asn1Ast.MarkPresent       -> Some ChoiceAlwaysPresent
                        | Asn1Ast.MarkAbsent        -> Some ChoiceAlwaysAbsent
                        | Asn1Ast.MarkOptional      -> None ) |>
                    Seq.distinct |> Seq.toList
                let newOptionality =
                    match c.Optionality with
                    | None
                    | Some (Asn1Ast.Optional _)                  ->
                        match asn1OptionalityFromWithComponents with
                        | []          -> None
                        | newOpt::_   -> Some newOpt
                    | Some Asn1Ast.AlwaysAbsent  ->
                        match asn1OptionalityFromWithComponents with
                        | []          -> Some ChoiceAlwaysAbsent
                        | newOpt::_   -> Some newOpt
                    | Some Asn1Ast.AlwaysPresent  ->
                        match asn1OptionalityFromWithComponents with
                        | []          -> Some ChoiceAlwaysPresent
                        | newOpt::_   -> Some newOpt

                let acnPresentWhenConditions =
                    match cc with
                    | None      -> []
                    | Some cc   ->
                        match tryGetProp cc.childEncodingSpec.acnProperties (fun x -> match x with PRESENT_WHEN e -> Some e | _ -> None) with
                        | None      -> []
                        | Some lst  ->
                            lst |>
                            List.choose(fun gp ->
                                match gp with
                                | GP_PresenceInt (p,v) -> Some (PresenceInt (p,v))
                                | GP_PresenceStr (p,v) -> Some (PresenceStr (p,v))
                                | _ -> None)

                let present_when_name =
                    match asn1.args.renamePolicy with
                    | AlwaysPrefixTypeName ->
                        let activeLang =
                            match asn1.args.targetLanguages  with
                            | x1::_ -> x1
                            | []    -> C
                        let typeName0 =
                            let aaa = typeDef.[activeLang]
                            match aaa.kind with
                            | NonPrimitiveNewTypeDefinition         -> typeDef.[activeLang].typeName
                            | NonPrimitiveNewSubTypeDefinition sub  -> sub.typeName
                            | NonPrimitiveReference2OtherType       -> typeDef.[activeLang].typeName


                        let tpName = removeTypePrefix  asn1.args.TypePrefix typeName0
                        tpName + "_" + c.present_when_name
                    | _ ->  c.present_when_name

                match cc with
                | None      ->
                    let newChild, us1 = mergeType asn1 acn typeIdsSet lms m c.Type (curPath@[CH_CHILD (c.Name.Value, present_when_name, "")]) (enmItemTypeDefPath@[CH_CHILD (c.Name.Value, present_when_name, "")]) (typeDefPath@[CH_CHILD (c.Name.Value, present_when_name, "")]) None None [] childWithCons [] acnParamSubst [] None  None  referencedBy caller us
                    {ChChildInfo.Name = c.Name; _c_name = c.c_name; _scala_name = c.scala_name; _ada_name = c.ada_name; Type = newChild; acnPresentWhenConditions = acnPresentWhenConditions; asn1Comments = c.Comments|> Seq.toList; acnComments = []; present_when_name = present_when_name; Optionality = newOptionality}, us1
                | Some cc   ->
                    let enumClassName =
                        match us.args.targetLanguages with
                        | Scala::x -> typeDef[Scala].typeName
                        | _ -> ""
                    let newChild, us1 = mergeType asn1 acn typeIdsSet lms m c.Type (curPath@[CH_CHILD (c.Name.Value, present_when_name, enumClassName)]) (typeDefPath@[CH_CHILD (c.Name.Value, present_when_name, enumClassName)]) (enmItemTypeDefPath@[CH_CHILD (c.Name.Value, present_when_name, enumClassName)]) (Some cc.childEncodingSpec) None [] childWithCons cc.argumentList acnParamSubst [] None  None referencedBy caller us
                    {ChChildInfo.Name = c.Name; _c_name = c.c_name; _scala_name = c.scala_name; _ada_name = c.ada_name; Type  = newChild; acnPresentWhenConditions = acnPresentWhenConditions; asn1Comments = c.Comments |> Seq.toList; acnComments = cc.comments ; present_when_name = present_when_name; Optionality = newOptionality}, us1
            let mergedChildren, chus =
                match acnType with
                | None            -> children |> foldMap (fun st ch -> mergeChild None ch st) us1
                | Some acnEncSpec ->
                    match acnEncSpec.children with
                    | []            -> children |> foldMap (fun st ch -> mergeChild None ch st) us1
                    | acnChildren   ->
                        // MAKE SURE ACN CHILDREN ARE THE SAME OF ASN1 CHILDREN !!!!!
                        let invalidAcnChildren =
                            acnChildren |> List.filter(fun acnChild -> not (children |> List.exists (fun asn1Child -> acnChild.name.Value = asn1Child.Name.Value)) )
                        match invalidAcnChildren with
                        | []    -> ()
                        | acnChild::_     -> raise(SemanticError(acnChild.name.Location, (sprintf "unexpected child name '%s'" acnChild.name.Value)))

                        children |>
                        foldMap(fun st asn1Child ->
                            match acnChildren |> Seq.tryFind(fun a -> a.name.Value = asn1Child.Name.Value) with
                            | Some acnChild -> mergeChild (Some acnChild) asn1Child st
                            | None          -> mergeChild None asn1Child st) us1

            let alwaysPresentChildren = mergedChildren |> List.filter(fun x -> x.Optionality = Some (ChoiceAlwaysPresent))
            match alwaysPresentChildren with
            | []        -> ()
            | x1::[]    -> ()
            | _         -> raise(SemanticError(t.Location,"Only one alternative can be marked as ALWAYS PRESENT"))

            let acnProperties =
                {ChoiceAcnProperties.enumDeterminant = tryGetProp combinedProperties (fun x -> match x with CHOICE_DETERMINANT e -> Some e | _ -> None)}
            let acnLoc = acnType |> Option.map (fun z -> z.loc)
            let indexSize = GetChoiceUperDeterminantLengthInBits(BigInteger(Seq.length children))
            let minChildSize = mergedChildren  |> List.map(fun x -> x.Type.uperMinSizeInBits) |> Seq.min
            let maxChildSize = mergedChildren  |> List.map(fun x -> x.Type.uperMaxSizeInBits) |> Seq.max

            let acnMinSizeInBits, acnMaxSizeInBits = AcnEncodingClasses.GetChoiceEncodingClass  mergedChildren alignment t.Location acnProperties
            // TODO: Voir si cela ne prove pas de dup? + SequenceOf
            // let detArg = acnType |> Option.bind (fun acnType -> acnType.acnProperties |> List.tryFindMap (fun prop ->
            //     match prop with
            //     | CHOICE_DETERMINANT det -> Some det
            //     | _ -> None))
            // let detArgSubsted = detArg |> Option.map (fun detArg -> substAcnArg acnParamSubst detArg)
            let allAcnArgsSubsted = substAcnArgs acnParamSubst acnArgs

            let childrenAlignemts = mergedChildren |> List.map(fun c -> c.Type.maxAlignment)
            let maxAlignment = maxAlignmentOf (alignment::childrenAlignemts)

            let definitionOrRef = 
                lms |> List.map(fun (l,lm) ->
                   let acnAlignment     = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
                   (l, DAstTypeDefinition.createChoice_u asn1.args typeIdsSet lm typeDef (ReferenceToType curPath)  acnProperties acnAlignment maxAlignment acnMinSizeInBits acnMaxSizeInBits mergedChildren)) |> Map.ofList

            Choice ({Choice.children = mergedChildren; acnProperties = acnProperties; cons=cons; withcons = wcons;
                uperMaxSizeInBits=indexSize+maxChildSize; uperMinSizeInBits=indexSize+minChildSize; acnMinSizeInBits =acnMinSizeInBits;
                acnMaxSizeInBits=acnMaxSizeInBits; acnParameters = acnParameters; acnArgs = allAcnArgsSubsted; acnLoc = acnLoc; typeDef=typeDef; definitionOrRef=definitionOrRef; maxAlignment=maxAlignment}), chus

        | Asn1Ast.ReferenceType rf    ->
            let acnArguments = acnArgs
            let oldBaseType  = Asn1Ast.GetBaseTypeByName rf.modName rf.tasName asn1
            let withCompCons = withCons
            let restCons = t.Constraints@refTypeCons
            let acnTypeAssign = tryFindAcnTypeByName rf.modName rf.tasName acn
            let baseTypeAcnParams =
                match acnTypeAssign with
                | None      -> []
                | Some x    -> x.acnParameters

            if not (baseTypeAcnParams.Length = acnArgs.Length) then
                let errMsg = sprintf "ACN parameters and arguments do not match. Expected %d parameters but got %d arguments. Reftype is %s" baseTypeAcnParams.Length acnArgs.Length rf.tasName.Value
                raise(SemanticError(rf.tasName.Location, errMsg))

            let baseTypeAcnEncSpec =
                match acnTypeAssign with
                | None      -> None
                | Some x    -> Some x.typeEncodingSpec
            let mergedAcnEncSpec =
                //if a reference type has a component constraint (i.e. it is actually a SEQUENCE, CHOICE or SEQUENCE OF) then we should not merge the ACN spec
                //We must take the the ACN specification only from this type and not the base type. The reason is that with the WITH COMPONENTS constraints you can
                //change the definition of the type (i.e. make child as always absent).
                match t.Constraints@refTypeCons |> Seq.exists(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> true | Asn1Ast.WithComponentsConstraint _ -> true | _ -> false) with
                | true  -> acnType
                | false -> mergeAcnEncodingSpecs acnType baseTypeAcnEncSpec
            let hasAdditionalConstraints = restCons.Length > 0
            let inheritanceInfo = (Some {InheritanceInfo.modName = rf.modName.Value; tasName = rf.tasName.Value; hasAdditionalConstraints=hasAdditionalConstraints})

            //The current type definition path changes to this referenced type path, if this referenced type has no constraints (with component constraints are ignored)
            let newTypeDefPath =
                match t.Constraints |> List.choose(fun c -> match c with Asn1Ast.WithComponentConstraint _ -> None | Asn1Ast.WithComponentsConstraint _ -> None | _ -> Some c) with
                | []     -> [MD rf.modName.Value; TA rf.tasName.Value]
                | _      -> typeDefPath
            let newEnmItemTypeDefPath = [MD rf.modName.Value; TA rf.tasName.Value]

            let typeDef, us1 = getReferenceTypeDefinition asn1 t {tfdArg with typeDefPath = newTypeDefPath} us
            let hasChildren, hasAcnProps =
                match acnType with
                | None            -> false, false
                | Some acnEncSpec ->
                    let b1 = acnEncSpec.children.Length > 0
                    let b2 =acnEncSpec.acnProperties.Length>0
                    b1,b2

            let newSubst = 
                match baseTypeAcnParams.Length = acnArgs.Length with
                | true -> addAcnSubst acnParamSubst baseTypeAcnParams acnArgs
                | false -> acnParamSubst
            let refTypeAssignInfo = {TypeAssignmentInfo.modName = rf.modName.Value; tasName = rf.tasName.Value}
            let newReferencedBy = referencedBy @ [refTypeAssignInfo]
            let us1a = 
                match caller with
                | None  -> us1
                | Some caller ->
                    {us1 with allDependencies = (caller,refTypeAssignInfo)::us1.allDependencies }
            let resolvedType, us2 = mergeType asn1 acn typeIdsSet lms m oldBaseType curPath newTypeDefPath newEnmItemTypeDefPath mergedAcnEncSpec (Some t.Location) restCons withCompCons acnArgs newSubst baseTypeAcnParams inheritanceInfo typeAssignmentInfo newReferencedBy (Some refTypeAssignInfo) us1a
            let hasExtraConstrainsOrChildrenOrAcnArgs =
                let b1 = hasAdditionalConstraints || hasChildren || acnArguments.Length > 0 || hasAcnProps
                match resolvedType.Kind with
                | ReferenceType baseRef   -> b1 || baseRef.hasExtraConstrainsOrChildrenOrAcnArgs
                | _                       -> b1

            let refCons = mapAnyConstraint asn1 t t.Constraints

            let toByte sizeInBits =
                sizeInBits/8I + (if sizeInBits % 8I = 0I then 0I else 1I)


            let uperMinSizeInBits, uperMaxSizeInBits, acnMinSizeInBits, acnMaxSizeInBits, encodingOptions =
                match rf.refEnc with
                | None          ->
                    resolvedType.uperMinSizeInBits, resolvedType.uperMaxSizeInBits, resolvedType.acnMinSizeInBits, resolvedType.acnMaxSizeInBits, None
                | Some  ContainedInBitString ->
                    let minSize = {SIZE.uper = resolvedType.uperMinSizeInBits; acn = resolvedType.acnMinSizeInBits}
                    let maxSize = {SIZE.uper = resolvedType.uperMaxSizeInBits; acn = resolvedType.acnMaxSizeInBits}
                    let hasNCount = minSize.uper <> maxSize.uper
                    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize.uper maxSize.uper 1I
                    let acnProperties =
                        match acnErrLoc with
                        | Some acnErrLoc    -> { SizeableAcnProperties.sizeProp  = getSizeableSizeProperty minSize.acn maxSize.acn acnErrLoc combinedProperties}
                        | None              -> {SizeableAcnProperties.sizeProp = None }

                    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetBitStringEncodingClass alignment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize.acn maxSize.acn hasNCount

                    uperMinSizeInBits, uperMaxSizeInBits, acnMinSizeInBits, acnMaxSizeInBits, (Some  {EncodeWithinOctetOrBitStringProperties.acnEncodingClass = acnEncodingClass; octOrBitStr = ContainedInBitString; minSize = minSize; maxSize=maxSize})
                | Some  ContainedInOctString  ->
                    let minSize = {SIZE.uper = toByte resolvedType.uperMinSizeInBits; acn = toByte resolvedType.acnMinSizeInBits}
                    let maxSize = {SIZE.uper = toByte resolvedType.uperMaxSizeInBits; acn = toByte resolvedType.acnMaxSizeInBits}
                    let hasNCount = (minSize.uper <> maxSize.uper) || (minSize.acn <> maxSize.acn)
                    let uperMinSizeInBits, uperMaxSizeInBits = uPER.getSizeableTypeSize minSize.uper maxSize.uper 8I

                    let acnProperties =
                        match acnErrLoc with
                        | Some acnErrLoc    -> {SizeableAcnProperties.sizeProp = getSizeableSizeProperty minSize.acn maxSize.acn acnErrLoc combinedProperties}
                        | None              -> {SizeableAcnProperties.sizeProp = None}

                    let acnEncodingClass,  acnMinSizeInBits, acnMaxSizeInBits= AcnEncodingClasses.GetOctetStringEncodingClass alignment loc acnProperties uperMinSizeInBits uperMaxSizeInBits minSize.acn maxSize.acn hasNCount

                    uperMinSizeInBits, uperMaxSizeInBits, acnMinSizeInBits, acnMaxSizeInBits, (Some  {EncodeWithinOctetOrBitStringProperties.acnEncodingClass = acnEncodingClass; octOrBitStr = ContainedInOctString; minSize = minSize; maxSize=maxSize})

            let maxAlignment = maxAlignmentOf [alignment; resolvedType.maxAlignment]

            let definitionOrRef = resolvedType.typeDefinitionOrReference
            let newRef       = {ReferenceType.modName = rf.modName; tasName = rf.tasName; tabularized = rf.tabularized; acnArguments = acnArguments; resolvedType=resolvedType; hasConstraints = hasAdditionalConstraints; typeDef=typeDef; uperMaxSizeInBits = uperMaxSizeInBits; uperMinSizeInBits = uperMinSizeInBits; acnMaxSizeInBits  = acnMaxSizeInBits; acnMinSizeInBits  = acnMinSizeInBits; encodingOptions=encodingOptions; hasExtraConstrainsOrChildrenOrAcnArgs=hasExtraConstrainsOrChildrenOrAcnArgs; refCons = refCons;definitionOrRef=definitionOrRef; maxAlignment=maxAlignment}
            ReferenceType newRef, us2

    {
        Asn1Type.Kind   = asn1Kind
        id              = ReferenceToType curPath
        acnAlignment     = tryGetProp combinedProperties (fun x -> match x with ALIGNTONEXT e -> Some e | _ -> None)
        Location        = t.Location
        acnLocation     = acnErrLoc
        acnParameters   = acnParameters |> List.map(fun prm -> {prm with id = (ReferenceToType (curPath@[(PRM prm.name)]))})
        inheritInfo   = inheritInfo
        typeAssignmentInfo = typeAssignmentInfo
        parameterizedTypeInstance = t.parameterizedTypeInstance
        moduleName      = t.moduleName
        acnEncSpecPosition = acnType |> Option.map (fun x -> x.position)
        acnEncSpecAntlrSubTree  =
            match acnType with
            | None -> None
            | Some st -> st.antlrSubTree
        unitsOfMeasure = t.unitsOfMeasure
        referencedBy    = referencedBy    

    }, kindState

(*
This function calculates the type ids map. 
The maps key is the type id BUT WITHOUT THE MODULE NAME.
The value is the number of times this type id (which is without module name) is used in the ASN.1 files.
It is used by C backend (which doesn't have modules) to generate valid C choice ids for the none case.
*)
let calculateTypeIdsMap (r:Asn1Ast.AstRoot) =
    let rec mapTypeId  (curPath : ScopeNode list)  (t:Asn1Ast.Asn1Type) =
        seq {
            match t.Kind with                           
            | Asn1Ast.SequenceOf        chType          -> 
                yield (ReferenceToType curPath)
                yield! mapTypeId (curPath@[SQF]) chType
            | Asn1Ast.Sequence          children        -> 
                yield (ReferenceToType curPath)
                for c in children do
                    yield! mapTypeId (curPath@[SEQ_CHILD (c.Name.Value, false)]) c.Type
            | Asn1Ast.Choice            children        -> 
                yield (ReferenceToType curPath)
                for c in children do
                    yield! mapTypeId (curPath@[CH_CHILD (c.Name.Value, "", "")]) c.Type
            | Asn1Ast.ReferenceType     referenceType   -> 
                let refType = r.typeAssignmentsMap.[referenceType.modName.Value, referenceType.tasName.Value]
                yield! mapTypeId (curPath) refType.Type
            | _               -> 
                yield (ReferenceToType curPath)
        } |> 
        Seq.toList 

    r.Files |>
    List.collect(fun f -> f.Modules) |>
    List.collect(fun m -> m.TypeAssignments) |>
    List.collect(fun ta -> mapTypeId [TA ta.Name.Value] ta.Type) |>
    List.map(fun tid -> ToC ((tid.AcnAbsPath |> Seq.StrJoin("_")).Replace("#","elem"))) |>
    List.groupBy id |>
    List.map(fun (id, lst) -> (id, Seq.length lst)) |>
    Map.ofList


let private mergeTAS (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (typeIdsSet : Map<String,int>) (lms:(ProgrammingLanguage*LanguageMacros) list) (m:Asn1Ast.Asn1Module) (am:AcnModule option)  (tas:Asn1Ast.TypeAssignment) (us:Asn1AcnMergeState) : (TypeAssignment*Asn1AcnMergeState) =
//    let acnTas =
//        match am with
//        | None  -> None
//        | Some x -> x.typeAssignments |> Seq.tryFind(fun z -> z.name = tas.Name)
    let acnParameters, acnComments  =
            match tas.acnInfo with
            | None -> [], []
            | Some acnTas -> acnTas.acnParameters, acnTas.comments
    let typeEncodingSpec = tas.Type.acnInfo
    let tsInfo = {TypeAssignmentInfo.modName = m.Name.Value; tasName = tas.Name.Value}
    let newType, us1 = mergeType asn1 acn typeIdsSet lms m tas.Type [MD m.Name.Value; TA tas.Name.Value] [MD m.Name.Value; TA tas.Name.Value] [MD m.Name.Value; TA tas.Name.Value] typeEncodingSpec None [] [] [] Map.empty acnParameters None (Some (TypeAssignmentInfo {TypeAssignmentInfo.modName = m.Name.Value; tasName = tas.Name.Value})) [] (Some tsInfo) us
    let newTas =
            {
            TypeAssignment.Name = tas.Name
            c_name = tas.c_name
            scala_name = tas.scala_name
            ada_name = tas.ada_name
            Type = newType
            asn1Comments = tas.Comments |> Seq.toList
            acnComments = acnComments
        }
    newTas, us1
            
let private mergeValueAssignment (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (typeIdsSet : Map<String,int>) (lms:(ProgrammingLanguage*LanguageMacros) list) (m:Asn1Ast.Asn1Module) (am:AcnModule option)  (vas:Asn1Ast.ValueAssignment) (us:Asn1AcnMergeState) : (ValueAssignment * Asn1AcnMergeState)=
    let inheritInfo =
        match vas.Type.Kind with
        | Asn1Ast.ReferenceType rf    -> (Some ({InheritanceInfo.modName = rf.modName.Value; tasName = rf.tasName.Value; hasAdditionalConstraints=false}))//(Some {InheritanceInfo.id = ReferenceToType [MD rf.modName.Value; TA rf.tasName.Value]; hasAdditionalConstraints=false})
        | _                           -> None
    let newType, us1 = mergeType asn1 acn typeIdsSet lms m vas.Type [MD m.Name.Value; VA vas.Name.Value] [MD m.Name.Value; VA vas.Name.Value] [MD m.Name.Value; VA vas.Name.Value] None None [] [] [] Map.empty [] inheritInfo (Some (ValueAssignmentInfo {ValueAssignmentInfo.modName = m.Name.Value; vasName = vas.Name.Value})) [] None us
    let newVas =
        {
            ValueAssignment.Name = vas.Name
            c_name = vas.c_name
            scala_name = vas.scala_name
            ada_name = vas.ada_name
            Type = newType
            Value = ValuesMapping.mapValue asn1 vas.Type vas.Value
        }
    newVas, us1

let private mergeModule (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (typeIdsSet : Map<String,int>) (lms:(ProgrammingLanguage*LanguageMacros) list) (m:Asn1Ast.Asn1Module) (us:Asn1AcnMergeState) : (Asn1Module*Asn1AcnMergeState) =
    let acnModule = acn.files |> Seq.collect(fun f -> f.modules)  |> Seq.tryFind (fun x -> x.name = m.Name)
    let newTases, us1 = m.TypeAssignments |> foldMap (fun st tas -> mergeTAS asn1 acn typeIdsSet lms m acnModule tas st) us
    let newVaes, us2 = m.ValueAssignments |> foldMap (fun st vas -> mergeValueAssignment asn1 acn typeIdsSet lms m acnModule vas st) us1
    let typeAssignmentsMap = newTases |> Seq.map(fun tas -> (tas.Name.Value), tas) |> Map.ofSeq
    let valueAssignmentsMap = newVaes |> Seq.map(fun vas -> (vas.Name.Value), vas) |> Map.ofSeq
    let typeImportMap = 
        seq {
            for imp in m.Imports do
                for typeName in imp.Types do
                    yield typeName.Value, imp
        } |> Map.ofSeq
    let newModule =
        {
            Asn1Module.Name = m.Name
            TypeAssignments = newTases
            ValueAssignments = newVaes
            typeAssignmentsMap = typeAssignmentsMap
            valueAssignmentsMap = valueAssignmentsMap
            typeImportMap = typeImportMap
            Imports  =  m.Imports
            Exports = m.Exports
            Comments = m.Comments
            position = m.position

        }
    newModule, us2

let private mergeFile (asn1:Asn1Ast.AstRoot) (acn:AcnAst) (typeIdsSet : Map<String,int>) (lms:(ProgrammingLanguage*LanguageMacros) list) (f:Asn1Ast.Asn1File) (us:Asn1AcnMergeState): (Asn1File*Asn1AcnMergeState) =
    let newModules, us1 = f.Modules |> foldMap (fun st m -> mergeModule asn1 acn typeIdsSet lms m st) us
    let newFile =
        {
            Asn1File.FileName = f.FileName
            Tokens = f.Tokens
            Modules = newModules
        }
    newFile, us1



//let rec registerPrimitiveTypeDefinition (us:Asn1AcnMergeState) l (id : ReferenceToType) (kind : FE_TypeDefinitionKind) getRtlDefinitionFunc : (FE_PrimitiveTypeDefinition*Asn1AcnMergeState)=
let mergeAsn1WithAcnAst (asn1: Asn1Ast.AstRoot) (lms:(ProgrammingLanguage*LanguageMacros) list) (acn: AcnGenericTypes.AcnAst, acnParseResults: CommonTypes.AntlrParserResult list) =
    let typeIdsSet : Map<String,int> = calculateTypeIdsMap asn1
    let initialState = {Asn1AcnMergeState.allocatedTypeNames = Map.empty; allocatedFE_TypeDefinition = Map.empty; args = asn1.args; temporaryTypesAllocation = Map.empty; allDependencies=[]}
    let state =
        seq {
            for l in ProgrammingLanguage.ActiveLanguages do
                for f in asn1.Files do
                    for m in f.Modules do
                        for tas in m.TypeAssignments do
                            let id = ReferenceToType [MD m.Name.Value; TA tas.Name.Value]
                            let programUnit = ToC m.Name.Value
                            let proposedTypedefName = ToC (asn1.args.TypePrefix + tas.Name.Value)
                            yield (l, id, tas.Type, programUnit, proposedTypedefName)
        } |> Seq.toList
        |> foldMap (fun st (l, id, t, programUnit, proposedTypedefName) ->
            let ib = asn1.args.getBasicLang l
            temporaryRegisterTypeDefinition st (l,ib) id programUnit proposedTypedefName
            //match t.Kind with
            //| Asn1Ast.ReferenceType rf  -> registerAnyTypeDefinition asn1 t st l id (FEI_NewSubTypeDefinition (ReferenceToType [MD rf.modName.Value; TA rf.tasName.Value]))
            //| _                         -> registerAnyTypeDefinition asn1 t st l id FEI_NewTypeDefinition
            ) initialState |> snd
    //let acn = CreateAcnAst acnParseResults
    let files, finalState = asn1.Files |> foldMap (fun st f -> mergeFile asn1 acn typeIdsSet lms f st) state
    let modulesMap = files |> Seq.collect(fun f -> f.Modules) |> Seq.map(fun m -> m.Name.Value, m) |> Map.ofSeq
    let typeAssignmentsMap =  
        seq {
            for f in files do
                for m in f.Modules do
                    for tas in m.TypeAssignments do
                        yield (m.Name.Value, tas.Name.Value), tas
        } |> Map.ofSeq
    {AstRoot.Files = files; args = asn1.args; acnConstants = acn.acnConstants; acnParseResults=acnParseResults; modulesMap = modulesMap; typeAssignmentsMap = typeAssignmentsMap; allDependencies= finalState.allDependencies}, acn
