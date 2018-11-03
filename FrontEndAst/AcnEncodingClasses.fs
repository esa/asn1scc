module AcnEncodingClasses

open System
open System.Numerics
open FsUtils
open Asn1AcnAst
open Asn1Fold
open Asn1AcnAstUtilFunctions
open AcnGenericTypes

let getAlignmentSize (aligment: AcnAligment option) =
    match aligment with
    | None              -> 0I
    | Some NextByte     -> 8I
    | Some NextWord     -> 16I
    | Some NextDWord    -> 32I


let GetIntEncodingClass (integerSizeInBytes:BigInteger) (aligment: AcnAligment option) errLoc (p  : IntegerAcnProperties) (uperMinSizeInBits:BigInteger) (uperMaxSizeInBits:BigInteger) isUnsigned=
    let alignmentSize = getAlignmentSize aligment
    let maxDigitsInInteger =
        match integerSizeInBytes with
        | _ when integerSizeInBytes = 8I && isUnsigned      -> UInt64.MaxValue.ToString().Length
        | _ when integerSizeInBytes = 8I && not(isUnsigned) -> Int64.MaxValue.ToString().Length
        | _ when integerSizeInBytes = 4I && isUnsigned      -> UInt32.MaxValue.ToString().Length
        | _ when integerSizeInBytes = 4I && not(isUnsigned) -> Int32.MaxValue.ToString().Length
        | _                                                 -> raise(SemanticError(errLoc, (sprintf "Unsuported integer size :%A" integerSizeInBytes)))
    let maxDigitsInInteger = BigInteger maxDigitsInInteger

        
    let encClass, minSizeInBits, maxSizeInBits = 
        match p.encodingProp.IsNone && p.sizeProp.IsNone && p.endiannessProp.IsNone with     
        | true -> Integer_uPER, uperMinSizeInBits, uperMaxSizeInBits
        | false  -> 
            let endianess = 
                match p.endiannessProp with
                | Some e -> e
                | None   -> BigEndianness
            let encProp = 
                match p.encodingProp with
                | Some e -> e
                | None   -> raise(SemanticError(errLoc, "Mandatory ACN property 'encoding' is missing"))

            let sizeProp = 
                match p.sizeProp with
                | Some e    -> e
                | None   -> raise(SemanticError(errLoc, "Mandatory ACN property 'size' is missing"))
            let bUINT = isUnsigned

            match encProp, sizeProp, endianess with
            | PosInt, Fixed(fixedSizeInBits) , BigEndianness     when fixedSizeInBits = 8I              ->  PositiveInteger_ConstSize_8, 8I, 8I
            | PosInt, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 16I->  PositiveInteger_ConstSize_big_endian_16, 16I, 16I
            | PosInt, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 16I->  PositiveInteger_ConstSize_little_endian_16, 16I, 16I
            | PosInt, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 32I->  PositiveInteger_ConstSize_big_endian_32, 32I, 32I
            | PosInt, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 32I->  PositiveInteger_ConstSize_little_endian_32, 32I, 32I
            | PosInt, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 64I->  PositiveInteger_ConstSize_big_endian_64, 64I, 64I
            | PosInt, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 64I->  PositiveInteger_ConstSize_little_endian_64, 64I, 64I
            | PosInt, Fixed(fxVal) , BigEndianness            ->  PositiveInteger_ConstSize fxVal, fxVal, fxVal
            | PosInt, IntNullTerminated _, _                    ->  raise(SemanticError(errLoc, "Acn properties pos-int and null-terminated are mutually exclusive"))
            | TwosComplement, _,_              when bUINT       ->  raise(SemanticError(errLoc, "Acn property twos-complement cannot be applied to non negative INTEGER types"))
            | TwosComplement, Fixed(fixedSizeInBits) , BigEndianness      when fixedSizeInBits = 8I  ->  TwosComplement_ConstSize_8, 8I, 8I
            | TwosComplement, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 16I ->  TwosComplement_ConstSize_big_endian_16, 16I, 16I
            | TwosComplement, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 16I ->  TwosComplement_ConstSize_little_endian_16, 16I, 16I
            | TwosComplement, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 32I ->  TwosComplement_ConstSize_big_endian_32, 32I, 32I
            | TwosComplement, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 32I ->  TwosComplement_ConstSize_little_endian_32, 32I, 32I
            | TwosComplement, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 64I ->  TwosComplement_ConstSize_big_endian_64, 64I, 64I
            | TwosComplement, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 64I ->  TwosComplement_ConstSize_little_endian_64, 64I, 64I
            | TwosComplement, Fixed(fxVal) , BigEndianness   ->  TwosComplement_ConstSize fxVal, fxVal, fxVal
            | TwosComplement, IntNullTerminated _, _         ->  raise(SemanticError(errLoc, "Acn properties twos-complement and null-terminated are mutually exclusive"))
            | IntAscii, Fixed(fxVal) , BigEndianness  when fxVal % 8I <> 0I       ->  raise(SemanticError(errLoc, "size value should be multiple of 8"))
            | IntAscii, Fixed(fxVal) , BigEndianness         ->  
                match bUINT with
                | true                                          -> ASCII_UINT_ConstSize fxVal, fxVal, fxVal
                | false                                         -> ASCII_ConstSize  fxVal, fxVal, fxVal
            | BCD, Fixed(fxVal) , BigEndianness   when fxVal % 4I <> 0I       ->  raise(SemanticError(errLoc, "size value should be multiple of 4"))
            | BCD, Fixed(fxVal) , BigEndianness                 ->  BCD_ConstSize fxVal, fxVal, fxVal
            | BCD, IntNullTerminated b, BigEndianness           ->  BCD_VarSize_NullTerminated b, 4I, maxDigitsInInteger*4I
            | IntAscii, IntNullTerminated b, BigEndianness         ->  
                match bUINT with
                | true                                                            -> ASCII_UINT_VarSize_NullTerminated b, 8I, 8I+ maxDigitsInInteger*8I
                | false                                                           -> ASCII_VarSize_NullTerminated b, 8I, 8I+8I+maxDigitsInInteger*8I
            | _, IntNullTerminated _, _                                  ->  raise(SemanticError(errLoc, "null-terminated can be applied only for ASCII or BCD encodings"))
            | _, _ , LittleEndianness                           ->  raise(SemanticError(errLoc, "Little endian can be applied only for fixed size encodings and size must be 16 or 32 or 64"))

    encClass, minSizeInBits+alignmentSize, maxSizeInBits+alignmentSize


let GetEnumeratedEncodingClass (integerSizeInBytes:BigInteger) (items:NamedItem list) (aligment: AcnAligment option) errLoc (p  : IntegerAcnProperties) uperMinSizeInBits uperMaxSizeInBits endodeValues =
    match endodeValues with
    | false -> 
        GetIntEncodingClass integerSizeInBytes aligment errLoc p uperMinSizeInBits uperMaxSizeInBits true
    | true  -> 
        let minVal = items |> List.map(fun x -> x.acnEncodeValue) |> List.min
        let maxVal = items |> List.map(fun x -> x.acnEncodeValue) |> List.max
        let uperSizeForValues = GetNumberOfBitsForNonNegativeInteger(maxVal-minVal)
        GetIntEncodingClass integerSizeInBytes aligment errLoc p uperSizeForValues uperSizeForValues (minVal >= 0I)

(*
 ######                       
 #     # ######   ##   #      
 #     # #       #  #  #      
 ######  #####  #    # #      
 #   #   #      ###### #      
 #    #  #      #    # #      
 #     # ###### #    # ###### 
                              
*)

let GetRealEncodingClass (aligment: AcnAligment option) errLoc (p  : RealAcnProperties) uperMinSizeInBits uperMaxSizeInBits =
    let alignmentSize = getAlignmentSize aligment
    let encClass, minSizeInBits, maxSizeInBits = 
        match p.encodingProp.IsNone && p.endiannessProp.IsNone with     
        | true     -> Real_uPER, uperMinSizeInBits, uperMaxSizeInBits
        | false    ->
            let endianess = 
                match p.endiannessProp with
                | Some e -> e
                | None   -> BigEndianness
            let encProp = 
                match p.encodingProp with
                | Some e -> e
                | None   -> raise(SemanticError(errLoc, "Mandatory ACN property 'encoding' is missing"))
            match encProp, endianess with
            | IEEE754_32, BigEndianness     -> Real_IEEE754_32_big_endian, 32I, 32I
            | IEEE754_64, BigEndianness     -> Real_IEEE754_64_big_endian, 64I, 64I
            | IEEE754_32, LittleEndianness  -> Real_IEEE754_32_little_endian, 32I, 32I
            | IEEE754_64, LittleEndianness  -> Real_IEEE754_64_little_endian, 64I, 64I
    encClass, minSizeInBits+alignmentSize, maxSizeInBits+alignmentSize  


(*
 ###    #    #######  #####                               
  #    # #   #       #     # ##### #####  # #    #  ####  
  #   #   #  #       #         #   #    # # ##   # #    # 
  #  #     # ######   #####    #   #    # # # #  # #      
  #  #######       #       #   #   #####  # #  # # #  ### 
  #  #     # #     # #     #   #   #   #  # #   ## #    # 
 ### #     #  #####   #####    #   #    # # #    #  ####  
*)



let GetStringEncodingClass (aligment: AcnAligment option) errLoc (p  : StringAcnProperties) (uperMinSizeInBits:BigInteger) (uperMaxSizeInBits:BigInteger) (asn1Min:BigInteger) (asn1Max:BigInteger) alphaSet =
    let alignmentSize = getAlignmentSize aligment
    let lengthDeterminantSize = GetNumberOfBitsForNonNegativeInteger (asn1Max-asn1Min)

    let bAsciiEncoding = 
        match p.encodingProp with
        | Some StrAscii     -> true
        | None              -> false
    let charSizeInBits =
        match  bAsciiEncoding with
        | true      -> 8I
        | false     -> 
            let allowedBytes = alphaSet |> Array.map(fun c -> (System.Text.Encoding.ASCII.GetBytes (c.ToString())).[0]) |> Set.ofArray
            GetNumberOfBitsForNonNegativeInteger (BigInteger(alphaSet.Length - 1))

    let encClass, minSizeInBits, maxSizeInBits = 
        match  bAsciiEncoding, p.sizeProp with
        | false, None                                  ->       Acn_Enc_String_uPER charSizeInBits, uperMinSizeInBits, uperMaxSizeInBits
        | false, Some (StrExternalField longField)     ->       Acn_Enc_String_CharIndex_External_Field_Determinant (charSizeInBits, longField) , asn1Min*charSizeInBits,  asn1Max*charSizeInBits
        | false, Some (StrNullTerminated b)            ->       raise(BugErrorException(sprintf "when a string type has the acn property 'size null-terminated' it must also have the acn property 'encoding ASCII'" ))
        | true, None                                   ->       Acn_Enc_String_uPER_Ascii charSizeInBits, lengthDeterminantSize + asn1Min*charSizeInBits, lengthDeterminantSize + asn1Max*charSizeInBits
        | true, Some (StrExternalField longField)      ->       Acn_Enc_String_Ascii_External_Field_Determinant (charSizeInBits, longField), asn1Min*charSizeInBits,  asn1Max*charSizeInBits
        | true, Some (StrNullTerminated b)             ->       Acn_Enc_String_Ascii_Null_Teminated (charSizeInBits, b), asn1Min*charSizeInBits + 8I,  asn1Max*charSizeInBits + 8I

    encClass,  minSizeInBits+alignmentSize, maxSizeInBits+alignmentSize

//banner text from this link
//http://patorjk.com/software/taag/#p=display&v=2&f=ANSI%20Shadow&t=Octet%20String%0A
(*
 ███████╗███████╗ ██████╗ ██╗   ██╗███████╗███╗   ██╗ ██████╗███████╗     ██████╗ ███████╗     ██████╗  ██████╗████████╗███████╗████████╗ ██╗██████╗ ██╗████████╗    ███████╗████████╗██████╗ ██╗███╗   ██╗ ██████╗ 
██╔════╝██╔════╝██╔═══██╗██║   ██║██╔════╝████╗  ██║██╔════╝██╔════╝    ██╔═══██╗██╔════╝    ██╔═══██╗██╔════╝╚══██╔══╝██╔════╝╚══██╔══╝██╔╝██╔══██╗██║╚══██╔══╝    ██╔════╝╚══██╔══╝██╔══██╗██║████╗  ██║██╔════╝ 
███████╗█████╗  ██║   ██║██║   ██║█████╗  ██╔██╗ ██║██║     █████╗      ██║   ██║█████╗      ██║   ██║██║        ██║   █████╗     ██║  ██╔╝ ██████╔╝██║   ██║       ███████╗   ██║   ██████╔╝██║██╔██╗ ██║██║  ███╗
╚════██║██╔══╝  ██║▄▄ ██║██║   ██║██╔══╝  ██║╚██╗██║██║     ██╔══╝      ██║   ██║██╔══╝      ██║   ██║██║        ██║   ██╔══╝     ██║ ██╔╝  ██╔══██╗██║   ██║       ╚════██║   ██║   ██╔══██╗██║██║╚██╗██║██║   ██║
███████║███████╗╚██████╔╝╚██████╔╝███████╗██║ ╚████║╚██████╗███████╗    ╚██████╔╝██║         ╚██████╔╝╚██████╗   ██║   ███████╗   ██║██╔╝   ██████╔╝██║   ██║       ███████║   ██║   ██║  ██║██║██║ ╚████║╚██████╔╝
╚══════╝╚══════╝ ╚══▀▀═╝  ╚═════╝ ╚══════╝╚═╝  ╚═══╝ ╚═════╝╚══════╝     ╚═════╝ ╚═╝          ╚═════╝  ╚═════╝   ╚═╝   ╚══════╝   ╚═╝╚═╝    ╚═════╝ ╚═╝   ╚═╝       ╚══════╝   ╚═╝   ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝ ╚═════╝ 
*)

let GetOctetBitSeqofEncodingClass (aligment: AcnAligment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max internalMinSize internalMaxSize bSequenecOf =
    let alignmentSize = getAlignmentSize aligment
    
    let encClass, minSizeInBits, maxSizeInBits = 
        match  p.sizeProp with
        | None                  -> 
                let minSizeInBits, maxSizeInBits = uPER.getSizeableTypeSize asn1Min asn1Max internalMaxSize
                SZ_EC_uPER, minSizeInBits, maxSizeInBits
        | Some p                -> SZ_EC_ExternalField p, asn1Min*internalMinSize, asn1Max*internalMaxSize

    encClass, minSizeInBits+alignmentSize, maxSizeInBits+alignmentSize

let GetOctetStringEncodingClass (aligment: AcnAligment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max =
    GetOctetBitSeqofEncodingClass aligment errLoc p   uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max 8I 8I false

let GetBitStringEncodingClass (aligment: AcnAligment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max =
    GetOctetBitSeqofEncodingClass aligment errLoc p   uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max 1I 1I false

let GetSequenceOfEncodingClass (aligment: AcnAligment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max internalMinSize internalMaxSize =
    GetOctetBitSeqofEncodingClass aligment errLoc p   uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max internalMinSize internalMaxSize true


let GetNullEncodingClass (aligment: AcnAligment option) errLoc (p  : NullTypeAcnProperties) =
    let alignmentSize = getAlignmentSize aligment
    match  p.encodingPattern with
    | None                                  -> alignmentSize, alignmentSize
    | Some (PATERN_PROP_BITSTR_VALUE p)     -> alignmentSize + p.Value.Length.AsBigInt, alignmentSize  + p.Value.Length.AsBigInt
    | Some (PATERN_PROP_OCTSTR_VALUE p)     -> alignmentSize + (p.Length*8).AsBigInt, alignmentSize  + (p.Length*8).AsBigInt

let GetBooleanEncodingClass (aligment: AcnAligment option) errLoc (p  : BooleanAcnProperties) =
    let alignmentSize = getAlignmentSize aligment
    match  p.encodingPattern with
    | None                      -> alignmentSize + 1I, alignmentSize + 1I
    | Some (TrueValue p)        -> alignmentSize + p.Value.Length.AsBigInt, alignmentSize  + p.Value.Length.AsBigInt
    | Some (FalseValue p)       -> alignmentSize + p.Value.Length.AsBigInt, alignmentSize  + p.Value.Length.AsBigInt



let GetChoiceEncodingClass  (children : ChChildInfo list) (aligment: AcnAligment option) errLoc (p  : ChoiceAcnProperties) =
    let maxChildSize = children |> List.map(fun c -> c.Type.acnMaxSizeInBits) |> Seq.max
    let minChildSize = children |> List.map(fun c -> c.Type.acnMaxSizeInBits) |> Seq.min
    let alignmentSize = getAlignmentSize aligment

    let presenceDeterminentByAcn =
        p.enumDeterminant.IsSome || (children |> Seq.exists(fun z -> not z.acnPresentWhenConditions.IsEmpty))

    match presenceDeterminentByAcn with
    | false      -> 
        let indexSize = GetChoiceUperDeterminantLengthInBits(BigInteger(Seq.length children))
        alignmentSize + indexSize + minChildSize, alignmentSize + indexSize + maxChildSize
    | true   ->
        alignmentSize + minChildSize, alignmentSize + maxChildSize
