module AcnEncodingClasses

open System
open System.Numerics
open FsUtils
open Asn1AcnAst
open Asn1Fold
open Asn1AcnAstUtilFunctions


let getAlignmentSize (aligment: AcnAligment option) =
    match aligment with
    | None              -> 0
    | Some NextByte     -> 8
    | Some NextWord     -> 16
    | Some NextDWord    -> 32


let GetIntEncodingClass (aligment: AcnAligment option) errLoc (p  : IntegerAcnProperties) uperMinSizeInBits uperMaxSizeInBits isUnsigned=
    let alignmentSize = getAlignmentSize aligment
        
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
            | PosInt, Fixed(8) , BigEndianness               ->  PositiveInteger_ConstSize_8, 8, 8
            | PosInt, Fixed(16), BigEndianness               ->  PositiveInteger_ConstSize_big_endian_16, 16, 16
            | PosInt, Fixed(16), LittleEndianness            ->  PositiveInteger_ConstSize_little_endian_16, 16, 16
            | PosInt, Fixed(32), BigEndianness               ->  PositiveInteger_ConstSize_big_endian_32, 32, 32
            | PosInt, Fixed(32), LittleEndianness            ->  PositiveInteger_ConstSize_little_endian_32, 32, 32
            | PosInt, Fixed(64), BigEndianness               ->  PositiveInteger_ConstSize_big_endian_64, 64, 64
            | PosInt, Fixed(64), LittleEndianness            ->  PositiveInteger_ConstSize_little_endian_64, 64, 64
            | PosInt, Fixed(fxVal) , BigEndianness           ->  PositiveInteger_ConstSize fxVal, fxVal, fxVal
            | PosInt, IntNullTerminated _, _                    ->  raise(SemanticError(errLoc, "Acn properties pos-int and null-terminated are mutually exclusive"))
            | TwosComplement, _,_              when bUINT       ->  raise(SemanticError(errLoc, "Acn property twos-complement cannot be applied to non negative INTEGER types"))
            | TwosComplement, Fixed(8) , BigEndianness       ->  TwosComplement_ConstSize_8, 8, 8
            | TwosComplement, Fixed(16), BigEndianness       ->  TwosComplement_ConstSize_big_endian_16, 16, 16
            | TwosComplement, Fixed(16), LittleEndianness    ->  TwosComplement_ConstSize_little_endian_16, 16, 16
            | TwosComplement, Fixed(32), BigEndianness       ->  TwosComplement_ConstSize_big_endian_32, 32, 32
            | TwosComplement, Fixed(32), LittleEndianness    ->  TwosComplement_ConstSize_little_endian_32, 32, 32
            | TwosComplement, Fixed(64), BigEndianness       ->  TwosComplement_ConstSize_big_endian_64, 64, 64
            | TwosComplement, Fixed(64), LittleEndianness    ->  TwosComplement_ConstSize_little_endian_64, 64, 64
            | TwosComplement, Fixed(fxVal) , BigEndianness   ->  TwosComplement_ConstSize fxVal, fxVal, fxVal
            | TwosComplement, IntNullTerminated _, _         ->  raise(SemanticError(errLoc, "Acn properties twos-complement and null-terminated are mutually exclusive"))
            | IntAscii, Fixed(fxVal) , BigEndianness  when fxVal % 8 <> 0       ->  raise(SemanticError(errLoc, "size value should be multiple of 8"))
            | IntAscii, Fixed(fxVal) , BigEndianness         ->  
                match bUINT with
                | true                                          -> ASCII_UINT_ConstSize fxVal, fxVal, fxVal
                | false                                         -> ASCII_ConstSize  fxVal, fxVal, fxVal
            | BCD, Fixed(fxVal) , BigEndianness   when fxVal % 4 <> 0       ->  raise(SemanticError(errLoc, "size value should be multiple of 4"))
            | BCD, Fixed(fxVal) , BigEndianness                 ->  BCD_ConstSize fxVal, fxVal, fxVal
            | BCD, IntNullTerminated b, BigEndianness           ->  BCD_VarSize_NullTerminated b, 4, 19*4
            | IntAscii, IntNullTerminated b, BigEndianness         ->  
                match bUINT with
                | true                                                            -> ASCII_UINT_VarSize_NullTerminated b, 8, 8+8+18*8
                | false                                                           -> ASCII_VarSize_NullTerminated b, 8, 8+8+18*8
            | _, IntNullTerminated _, _                                  ->  raise(SemanticError(errLoc, "null-terminated can be applied only for ASCII or BCD encodings"))
            | _, _ , LittleEndianness                           ->  raise(SemanticError(errLoc, "Little endian can be applied only for fixed size encodings and size must be 16 or 32 or 64"))

    encClass, minSizeInBits+alignmentSize, maxSizeInBits+alignmentSize


let GetEnumeratedEncodingClass (items:NamedItem list) (aligment: AcnAligment option) errLoc (p  : IntegerAcnProperties) uperMinSizeInBits uperMaxSizeInBits endodeValues =
    match endodeValues with
    | false -> 
        GetIntEncodingClass aligment errLoc p uperMinSizeInBits uperMaxSizeInBits true
    | true  -> 
        let minVal = items |> List.map(fun x -> x.acnEncodeValue) |> List.min
        let maxVal = items |> List.map(fun x -> x.acnEncodeValue) |> List.max
        let uperSizeForValues = int (GetNumberOfBitsForNonNegativeInteger(maxVal-minVal))
        GetIntEncodingClass aligment errLoc p uperSizeForValues uperSizeForValues (minVal >= 0I)

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
            | IEEE754_32, BigEndianness     -> Real_IEEE754_32_big_endian, 32, 32
            | IEEE754_64, BigEndianness     -> Real_IEEE754_64_big_endian, 64, 64
            | IEEE754_32, LittleEndianness  -> Real_IEEE754_32_little_endian, 32, 32
            | IEEE754_64, LittleEndianness  -> Real_IEEE754_64_little_endian, 64, 64
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



let GetStringEncodingClass (aligment: AcnAligment option) errLoc (p  : StringAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max alphaSet =
    let alignmentSize = getAlignmentSize aligment
    let lengthDeterminantSize = GetNumberOfBitsForNonNegativeInt (asn1Max-asn1Min)

    let bAsciiEncoding = 
        match p.encodingProp with
        | Some StrAscii     -> true
        | None              -> false
    let charSizeInBits =
        match  bAsciiEncoding with
        | true      -> 8
        | false     -> 
            let allowedBytes = alphaSet |> Array.map(fun c -> (System.Text.Encoding.ASCII.GetBytes (c.ToString())).[0]) |> Set.ofArray
            GetNumberOfBitsForNonNegativeInt (alphaSet.Length - 1)

    let encClass, minSizeInBits, maxSizeInBits = 
        match  bAsciiEncoding, p.sizeProp with
        | false, None                                  ->       Acn_Enc_String_uPER charSizeInBits, uperMinSizeInBits, uperMaxSizeInBits
        | false, Some (StrExternalField longField)     ->       Acn_Enc_String_CharIndex_External_Field_Determinant (charSizeInBits, longField) , asn1Min*charSizeInBits,  asn1Max*charSizeInBits
        | false, Some (StrNullTerminated b)            ->       raise(BugErrorException(sprintf "when a string type has the acn property 'size null-terminated' it must also have the acn property 'encoding ASCII'" ))
        | true, None                                   ->       Acn_Enc_String_uPER_Ascii charSizeInBits, lengthDeterminantSize + asn1Min*charSizeInBits, lengthDeterminantSize + asn1Max*charSizeInBits
        | true, Some (StrExternalField longField)      ->       Acn_Enc_String_Ascii_External_Field_Determinant (charSizeInBits, longField), asn1Min*charSizeInBits,  asn1Max*charSizeInBits
        | true, Some (StrNullTerminated b)             ->       Acn_Enc_String_Ascii_Null_Teminated (charSizeInBits, b), asn1Min*charSizeInBits + 8,  asn1Max*charSizeInBits + 8

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
    GetOctetBitSeqofEncodingClass aligment errLoc p   uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max 8 8 false

let GetBitStringEncodingClass (aligment: AcnAligment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max =
    GetOctetBitSeqofEncodingClass aligment errLoc p   uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max 1 1 false

let GetSequenceOfEncodingClass (aligment: AcnAligment option) errLoc (p  : SizeableAcnProperties) uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max internalMinSize internalMaxSize =
    GetOctetBitSeqofEncodingClass aligment errLoc p   uperMinSizeInBits uperMaxSizeInBits asn1Min asn1Max internalMinSize internalMaxSize true


let GetNullEncodingClass (aligment: AcnAligment option) errLoc (p  : NullTypeAcnProperties) =
    let alignmentSize = getAlignmentSize aligment
    match  p.encodingPattern with
    | None                  -> alignmentSize, alignmentSize
    | Some p                -> alignmentSize + p.Value.Length, alignmentSize  + p.Value.Length

let GetBooleanEncodingClass (aligment: AcnAligment option) errLoc (p  : BooleanAcnProperties) =
    let alignmentSize = getAlignmentSize aligment
    match  p.encodingPattern with
    | None                      -> alignmentSize + 1, alignmentSize + 1
    | Some (TrueValue p)        -> alignmentSize + p.Value.Length, alignmentSize  + p.Value.Length
    | Some (FalseValue p)       -> alignmentSize + p.Value.Length, alignmentSize  + p.Value.Length



let GetChoiceEncodingClass  (children : ChChildInfo list) (aligment: AcnAligment option) errLoc (p  : ChoiceAcnProperties) =
    let maxChildSize = children |> List.map(fun c -> c.Type.acnMaxSizeInBits) |> Seq.max
    let minChildSize = children |> List.map(fun c -> c.Type.acnMaxSizeInBits) |> Seq.min
    let alignmentSize = getAlignmentSize aligment

    match p.enumDeterminant with
    | None      -> 
        let indexSize = int (GetNumberOfBitsForNonNegativeInteger(BigInteger(Seq.length children)))
        alignmentSize + indexSize + minChildSize, alignmentSize + indexSize + maxChildSize
    | Some _    ->
        alignmentSize + minChildSize, alignmentSize + maxChildSize
