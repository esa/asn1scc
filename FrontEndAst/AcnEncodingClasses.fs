module AcnEncodingClasses

open System
open System.Numerics
open FsUtils
open Asn1AcnAst
open Asn1Fold
open Asn1AcnAstUtilFunctions
open AcnGenericTypes

let getAlignmentSize (alignment: AcnAlignment option) =
    match alignment with
    | None              -> 0I
    | Some NextByte     -> 7I
    | Some NextWord     -> 15I
    | Some NextDWord    -> 31I

let alignedToBits (alignment: bigint) (bits: bigint) =
    assert (1I < alignment)
    let rem = bits % alignment
    if rem <> 0I then bits + (alignment - rem)
    else bits
let alignedToByte (b: bigint): bigint = alignedToBits 8I b
let alignedToWord (b: bigint): bigint = alignedToBits 16I b
let alignedToDWord (b: bigint): bigint = alignedToBits 32I b

let alignedTo (alignment: AcnAlignment option) (b: bigint): bigint =
    match alignment with
    | None -> b
    | Some NextByte -> alignedToByte b
    | Some NextWord -> alignedToWord b
    | Some NextDWord -> alignedToDWord b

let GetIntEncodingClass (integerSizeInBytes:BigInteger) (alignment: AcnAlignment option) errLoc (p  : IntegerAcnProperties) (uperMinSizeInBits:BigInteger) (uperMaxSizeInBits:BigInteger) isUnsigned=
    let maxDigitsInInteger =
        match integerSizeInBytes with
        | _ when integerSizeInBytes = 8I && isUnsigned      -> UInt64.MaxValue.ToString().Length
        | _ when integerSizeInBytes = 8I && not(isUnsigned) -> Int64.MaxValue.ToString().Length
        | _ when integerSizeInBytes = 4I && isUnsigned      -> UInt32.MaxValue.ToString().Length
        | _ when integerSizeInBytes = 4I && not(isUnsigned) -> Int32.MaxValue.ToString().Length
        | _                                                 -> raise(SemanticError(errLoc, (sprintf "Unsupported integer size :%A" integerSizeInBytes)))
    let maxDigitsInInteger = BigInteger maxDigitsInInteger


    let encClass, minSizeInBits, maxSizeInBits =
        match p.encodingProp.IsNone && p.sizeProp.IsNone && p.endiannessProp.IsNone with
        | true -> Integer_uPER, uperMinSizeInBits, uperMaxSizeInBits
        | false  ->
            let endianness =
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
            let maxFxVal = integerSizeInBytes*8I
            match encProp, sizeProp, endianness with
            | PosInt, _,_              when not bUINT       ->  raise(SemanticError(errLoc, "Acn property pos-int cannot be applied to signed INTEGER types"))
            | PosInt, Fixed(fixedSizeInBits) , BigEndianness     when fixedSizeInBits = 8I              ->
                match p.endiannessProp with
                | Some BigEndianness ->
                    let errMsg = "endianness property has no effect here.\nLittle/big endian can be applied only for fixed size encodings and size must be 16 or 32 or 64\n"
                    Console.Error.WriteLine(AntlrParse.formatSemanticWarning errLoc errMsg)
                | _                 -> ()
                PositiveInteger_ConstSize_8, 8I, 8I
            | PosInt, Fixed(fixedSizeInBits) , LittleEndianness     when fixedSizeInBits = 8I              ->
                let errMsg = "endianness property has no effect here.\nLittle/big endian can be applied only for fixed size encodings and size must be 16 or 32 or 64\n"
                Console.Error.WriteLine(AntlrParse.formatSemanticWarning errLoc errMsg)
                PositiveInteger_ConstSize_8, 8I, 8I
            | PosInt, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 16I->  PositiveInteger_ConstSize_big_endian_16, 16I, 16I
            | PosInt, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 16I->  PositiveInteger_ConstSize_little_endian_16, 16I, 16I
            | PosInt, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 32I->  PositiveInteger_ConstSize_big_endian_32, 32I, 32I
            | PosInt, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 32I->  PositiveInteger_ConstSize_little_endian_32, 32I, 32I
            | PosInt, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 64I->  PositiveInteger_ConstSize_big_endian_64, 64I, 64I
            | PosInt, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 64I->  PositiveInteger_ConstSize_little_endian_64, 64I, 64I
            | PosInt, Fixed(fxVal) , BigEndianness  when fxVal > maxFxVal ->  raise(SemanticError(errLoc, (sprintf "Size must be less than %A" maxFxVal)))
            | PosInt, Fixed(fxVal) , BigEndianness            ->  PositiveInteger_ConstSize fxVal, fxVal, fxVal
            | PosInt, IntNullTerminated _, _                    ->  raise(SemanticError(errLoc, "Acn properties pos-int and null-terminated are mutually exclusive"))
            | TwosComplement, _,_              when bUINT       ->  raise(SemanticError(errLoc, "Acn property twos-complement cannot be applied to non negative INTEGER types"))
            | TwosComplement, Fixed(fixedSizeInBits) , BigEndianness      when fixedSizeInBits = 8I  ->
                match p.endiannessProp with
                | Some BigEndianness ->
                    let errMsg = "endianness property has no effect here.\nLittle/big endian can be applied only for fixed size encodings and size must be 16 or 32 or 64\n"
                    Console.Error.WriteLine(AntlrParse.formatSemanticWarning errLoc errMsg)
                | _                 -> ()
                TwosComplement_ConstSize_8, 8I, 8I
            | TwosComplement, Fixed(fixedSizeInBits) , LittleEndianness   when fixedSizeInBits = 8I  ->
                let errMsg = "endianness property has no effect here.\nLittle/big endian can be applied only for fixed size encodings and size must be 16 or 32 or 64\n"
                Console.Error.WriteLine(AntlrParse.formatSemanticWarning errLoc errMsg)
                TwosComplement_ConstSize_8, 8I, 8I
            | TwosComplement, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 16I ->  TwosComplement_ConstSize_big_endian_16, 16I, 16I
            | TwosComplement, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 16I ->  TwosComplement_ConstSize_little_endian_16, 16I, 16I
            | TwosComplement, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 32I ->  TwosComplement_ConstSize_big_endian_32, 32I, 32I
            | TwosComplement, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 32I ->  TwosComplement_ConstSize_little_endian_32, 32I, 32I
            | TwosComplement, Fixed(fixedSizeInBits), BigEndianness      when fixedSizeInBits = 64I ->  TwosComplement_ConstSize_big_endian_64, 64I, 64I
            | TwosComplement, Fixed(fixedSizeInBits), LittleEndianness   when fixedSizeInBits = 64I ->  TwosComplement_ConstSize_little_endian_64, 64I, 64I
            | TwosComplement, Fixed(fxVal) , BigEndianness when fxVal > maxFxVal   ->  raise(SemanticError(errLoc, (sprintf "Size must be less than %A" maxFxVal)))
            | TwosComplement, Fixed(fxVal) , BigEndianness   ->  TwosComplement_ConstSize fxVal, fxVal, fxVal
            | TwosComplement, IntNullTerminated _, _         ->  raise(SemanticError(errLoc, "Acn properties twos-complement and null-terminated are mutually exclusive"))
            | IntAscii, Fixed(fxVal) , BigEndianness  when fxVal > maxDigitsInInteger*8I+8I       ->  raise(SemanticError(errLoc, (sprintf "Size must be less than %A" (maxDigitsInInteger*8I+8I))))
            | IntAscii, Fixed(fxVal) , BigEndianness  when fxVal % 8I <> 0I       ->  raise(SemanticError(errLoc, "size value should be multiple of 8"))
            | IntAscii, Fixed(fxVal) , BigEndianness         ->
                match bUINT with
                | true                                          -> ASCII_UINT_ConstSize fxVal, fxVal, fxVal
                | false                                         -> ASCII_ConstSize  fxVal, fxVal, fxVal
            | BCD, Fixed(fxVal) , BigEndianness   when fxVal > maxDigitsInInteger*4I       ->  raise(SemanticError(errLoc, (sprintf "Size must be less than %A" (maxDigitsInInteger*4I))))
            | BCD, Fixed(fxVal) , BigEndianness   when fxVal % 4I <> 0I       ->  raise(SemanticError(errLoc, "size value should be multiple of 4"))
            | BCD, Fixed(fxVal) , BigEndianness                 ->  BCD_ConstSize fxVal, fxVal, fxVal
            | BCD, IntNullTerminated b, BigEndianness           ->  BCD_VarSize_NullTerminated b, 4I, 4I+maxDigitsInInteger*4I
            | IntAscii, IntNullTerminated nullBytes, BigEndianness         ->
                let nullBytesLength = BigInteger (nullBytes.Length*8)
                match bUINT with
                | true                                                            -> ASCII_UINT_VarSize_NullTerminated nullBytes, nullBytesLength, nullBytesLength + maxDigitsInInteger*8I
                | false                                                           -> ASCII_VarSize_NullTerminated nullBytes, nullBytesLength, nullBytesLength+8I+8I+maxDigitsInInteger*8I
            | _, IntNullTerminated _, _                                  ->  raise(SemanticError(errLoc, "null-terminated can be applied only for ASCII or BCD encodings"))
            | _, _ , LittleEndianness                           ->  raise(SemanticError(errLoc, "Little endian can be applied only for fixed size encodings and size must be 16 or 32 or 64"))

    encClass, minSizeInBits, maxSizeInBits + getAlignmentSize alignment


let GetEnumeratedEncodingClass (integerSizeInBytes:BigInteger) (items:NamedItem list) (alignment: AcnAlignment option) errLoc (p  : IntegerAcnProperties) uperMinSizeInBits uperMaxSizeInBits encodeValues =
    match encodeValues with
    | false ->
        GetIntEncodingClass integerSizeInBytes alignment errLoc p uperMinSizeInBits uperMaxSizeInBits true
    | true  ->
        let minVal = items |> List.map(fun x -> x.acnEncodeValue) |> List.min
        let maxVal = items |> List.map(fun x -> x.acnEncodeValue) |> List.max
        let uperSizeForValues = GetNumberOfBitsForNonNegativeInteger(maxVal-minVal)
        GetIntEncodingClass integerSizeInBytes alignment errLoc p uperSizeForValues uperSizeForValues (minVal >= 0I)

(*
 ######
 #     # ######   ##   #
 #     # #       #  #  #
 ######  #####  #    # #
 #   #   #      ###### #
 #    #  #      #    # #
 #     # ###### #    # ######

*)

let GetRealEncodingClass (alignment: AcnAlignment option) errLoc (p  : RealAcnProperties) uperMinSizeInBits uperMaxSizeInBits =
    let encClass, minSizeInBits, maxSizeInBits =
        match p.encodingProp.IsNone && p.endiannessProp.IsNone with
        | true     -> Real_uPER, uperMinSizeInBits, uperMaxSizeInBits
        | false    ->
            let endianness =
                match p.endiannessProp with
                | Some e -> e
                | None   -> BigEndianness
            let encProp =
                match p.encodingProp with
                | Some e -> e
                | None   -> raise(SemanticError(errLoc, "Mandatory ACN property 'encoding' is missing"))
            match encProp, endianness with
            | IEEE754_32, BigEndianness     -> Real_IEEE754_32_big_endian, 32I, 32I
            | IEEE754_64, BigEndianness     -> Real_IEEE754_64_big_endian, 64I, 64I
            | IEEE754_32, LittleEndianness  -> Real_IEEE754_32_little_endian, 32I, 32I
            | IEEE754_64, LittleEndianness  -> Real_IEEE754_64_little_endian, 64I, 64I
    encClass, minSizeInBits, maxSizeInBits + getAlignmentSize alignment


(*
 ###    #    #######  #####
  #    # #   #       #     # ##### #####  # #    #  ####
  #   #   #  #       #         #   #    # # ##   # #    #
  #  #     # ######   #####    #   #    # # # #  # #
  #  #######       #       #   #   #####  # #  # # #  ###
  #  #     # #     # #     #   #   #   #  # #   ## #    #
 ### #     #  #####   #####    #   #    # # #    #  ####
*)



let GetStringEncodingClass (alignment: AcnAlignment option) errLoc (p  : StringAcnProperties) (uperMinSizeInBits:BigInteger) (uperMaxSizeInBits:BigInteger) (asn1Min:BigInteger) (asn1Max:BigInteger) alphaSet =
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
        | true, Some (StrNullTerminated nullChars)             ->       Acn_Enc_String_Ascii_Null_Terminated (charSizeInBits, nullChars), asn1Min*charSizeInBits + (BigInteger (nullChars.Length * 8)),  asn1Max*charSizeInBits + (BigInteger (nullChars.Length * 8))

    encClass, minSizeInBits, maxSizeInBits + getAlignmentSize alignment

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

let GetOctetBitSeqofEncodingClass (alignment: AcnAlignment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max internalMinSize internalMaxSize bOcteOrBitString hasNCount =
    let encClass, minSizeInBits, maxSizeInBits =
        match  p.sizeProp with
        | None                  ->
                match hasNCount with
                | false  -> SZ_EC_FIXED_SIZE, asn1Min*internalMaxSize, asn1Max*internalMaxSize
                | true ->
                    let lenSize  = GetNumberOfBitsForNonNegativeInteger(asn1Max-asn1Min)
                    SZ_EC_LENGTH_EMBEDDED lenSize, asn1Min*internalMaxSize + lenSize, asn1Max*internalMaxSize + lenSize
            //let minSizeInBits, maxSizeInBits = uPER.getSizeableTypeSize asn1Min asn1Max internalMaxSize
            //SZ_EC_uPER, minSizeInBits, maxSizeInBits
        | Some p                ->
            match p with
            | SzExternalField p     -> SZ_EC_ExternalField p, asn1Min*internalMinSize, asn1Max*internalMaxSize
            | SzNullTerminated tp   -> SZ_EC_TerminationPattern tp,  (BigInteger tp.Value.Length) +  asn1Min*internalMinSize, (BigInteger tp.Value.Length) +  asn1Max*internalMaxSize

    encClass, minSizeInBits, maxSizeInBits + getAlignmentSize alignment

let GetOctetStringEncodingClass (alignment: AcnAlignment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max hasNCount =
    GetOctetBitSeqofEncodingClass alignment errLoc p   uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max 8I 8I true hasNCount

let GetBitStringEncodingClass (alignment: AcnAlignment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max hasNCount =
    GetOctetBitSeqofEncodingClass alignment errLoc p   uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max 1I 1I true hasNCount

let GetSequenceOfEncodingClass (alignment: AcnAlignment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max internalMinSize internalMaxSize hasNCount =
    GetOctetBitSeqofEncodingClass alignment errLoc p   uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max internalMinSize internalMaxSize false hasNCount


let GetNullEncodingClass (alignment: AcnAlignment option) errLoc (p  : NullTypeAcnProperties) =
    let sz =
        match p.encodingPattern with
        | None -> 0I
        | Some (PATTERN_PROP_BITSTR_VALUE p) -> p.Value.Length.AsBigInt
        | Some (PATTERN_PROP_OCTSTR_VALUE p) -> (p.Length*8).AsBigInt
    sz, sz + getAlignmentSize alignment

let GetBooleanEncodingClass (alignment: AcnAlignment option) errLoc (p  : BooleanAcnProperties) =
    let sz =
        match p.encodingPattern with
        | None -> 1I
        | Some p -> p.bitValLength.AsBigInt
    sz, sz + getAlignmentSize alignment


let GetChoiceEncodingClass  (children : ChChildInfo list) (alignment: AcnAlignment option) errLoc (p  : ChoiceAcnProperties) =
    let maxChildSize = children |> List.map(fun c -> c.Type.acnMaxSizeInBits) |> Seq.max
    let minChildSize = children |> List.map(fun c -> c.Type.acnMinSizeInBits) |> Seq.min

    let presenceDeterminantByAcn =
        p.enumDeterminant.IsSome || (children |> Seq.exists(fun z -> not z.acnPresentWhenConditions.IsEmpty))

    match presenceDeterminantByAcn with
    | false      ->
        let indexSize = GetChoiceUperDeterminantLengthInBits(BigInteger(Seq.length children))
        indexSize + minChildSize, indexSize + maxChildSize + getAlignmentSize alignment
    | true   ->
        minChildSize, maxChildSize + getAlignmentSize alignment
