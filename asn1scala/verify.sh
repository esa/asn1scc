stainless-dotty src/main/scala/asn1scala/asn1jvm.scala  \
src/main/scala/asn1scala/asn1jvm_Verification.scala  \
src/main/scala/asn1scala/asn1jvm_Helper.scala \
src/main/scala/asn1scala/asn1jvm_Bitstream.scala \
src/main/scala/asn1scala/asn1jvm_Codec.scala \
src/main/scala/asn1scala/asn1jvm_Codec_ACN.scala \
--config-file=stainless.conf \
-D-parallel=12 \
--watch \
--functions=\
enc_IA5String_CharIndex_External_Field_Determinant,\
dec_IA5String_CharIndex_External_Field_Determinant,\
dec_Int_TwosComplement_ConstSize_little_endian_64,\
dec_Real_IEEE754_32_big_endian,\
dec_Real_IEEE754_32_little_endian,\
dec_Real_IEEE754_64_big_endian,\
dec_Real_IEEE754_64_little_endian,\
milbus_encode,\
milbus_decode\
BitStream_ReadBitPattern,\
dec_IA5String_CharIndex_Internal_Field_Determinant,\
dec_String_CharIndex_private,\
enc_String_CharIndex_private,\
GetCharIndex,\
initACNCodec,\
reader,\
readPrefixLemma_TODO,\
dec_Int_PositiveInteger_ConstSize_big_endian_16_prefixLemma,\
#dec_Int_PositiveInteger_ConstSize_big_endian_32_prefixLemma,\
#dec_Int_PositiveInteger_ConstSize_big_endian_64_prefixLemma,\
#dec_Int_PositiveInteger_ConstSize_little_endian_16_prefixLemma,\
#dec_Int_PositiveInteger_ConstSize_little_endian_32_prefixLemma,\
#dec_Int_PositiveInteger_ConstSize_little_endian_64_prefixLemma,\
#enc_Int_PositiveInteger_ConstSize,\
#enc_Int_PositiveInteger_ConstSize,\
#resetAt,\
#withMovedByteIndex,\
#withMovedBitIndex,\
#isPrefixOf,\
#enc_Int_PositiveInteger_ConstSize_8,\
#enc_Int_PositiveInteger_ConstSize_big_endian_16,\
#enc_Int_PositiveInteger_ConstSize_big_endian_32,\
#enc_Int_PositiveInteger_ConstSize_big_endian_64,\
#dec_Int_PositiveInteger_ConstSize,\
#dec_Int_PositiveInteger_ConstSize_pure,\
#dec_Int_PositiveInteger_ConstSize_8,\
#dec_Int_PositiveInteger_ConstSize_big_endian_16,\
#dec_Int_PositiveInteger_ConstSize_big_endian_16_pure,\
#dec_Int_PositiveInteger_ConstSize_big_endian_32,\
#dec_Int_PositiveInteger_ConstSize_big_endian_32_pure,\
#dec_Int_PositiveInteger_ConstSize_big_endian_64,\
#dec_Int_PositiveInteger_ConstSize_big_endian_64_pure,\
#dec_Int_PositiveInteger_ConstSize_little_endian_16_pure,\
#dec_Int_PositiveInteger_ConstSize_little_endian_32_pure,\
#dec_Int_PositiveInteger_ConstSize_little_endian_64_pure,\
#enc_Int_TwosComplement_ConstSize,\
#enc_Int_TwosComplement_ConstSize_8,\
#enc_Int_TwosComplement_ConstSize_big_endian_16,\
#enc_Int_TwosComplement_ConstSize_big_endian_32,\
#enc_Int_TwosComplement_ConstSize_big_endian_64,\
#dec_Int_TwosComplement_ConstSize,\
#dec_Int_TwosComplement_ConstSize_pure,\
#dec_Int_TwosComplement_ConstSize_8,\
#dec_Int_TwosComplement_ConstSize_big_endian_16,\
#dec_Int_TwosComplement_ConstSize_big_endian_32,\
#dec_Int_TwosComplement_ConstSize_big_endian_64,\
#BitStream_ReadBitPattern,\
#enc_Real_IEEE754_32_big_endian,\
#enc_Real_IEEE754_32_little_endian,\
#enc_Real_IEEE754_64_big_endian,\
#enc_Real_IEEE754_64_little_endian,\
#dec_Real_IEEE754_32_big_endian,\
#dec_Real_IEEE754_32_little_endian,\
#dec_Real_IEEE754_64_big_endian,\
#dec_Real_IEEE754_64_little_endian,\
#enc_String_CharIndex_private,\
#enc_IA5String_CharIndex_External_Field_Determinant,\
#dec_String_CharIndex_private,\
#dec_IA5String_CharIndex_External_Field_Determinant,\
#dec_IA5String_CharIndex_Internal_Field_Determinant,\
#milbus_encode,\
#milbus_decode\
$1