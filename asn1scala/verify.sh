stainless-dotty src/main/scala/asn1scala/asn1jvm.scala  \
src/main/scala/asn1scala/asn1jvm_Verification.scala  \
src/main/scala/asn1scala/asn1jvm_Helper.scala \
src/main/scala/asn1scala/asn1jvm_Bitstream.scala \
src/main/scala/asn1scala/asn1jvm_Codec.scala \
--config-file=stainless.conf \
-D-parallel=8 \
--watch \
#--functions=\
#encodeOctetString,\
#encodeOctetString_fragmentation,\
#encodeOctetString_fragmentation_innerMostWhile,\
#encodeOctetString_fragmentation_innerWhile,\
#decodeBitString_while,\
#encodeOctetString_no_length,\
#decodeBitString,\
#encodeBitString,\
#encodeBitString_while,\
#decodeOctetString,\
#decodeConstrainedWholeNumber,\
#decodeOctetString,\
#decodeOctetString_fragmentation_innerMostWhile,\
#decodeOctetString_fragmentation_innerWhile,\
#decodeOctetString_fragmentation,\
#decodeRealFromBitStream,\
#encodeOctetString_fragmentation_innerWhile,\
#lemmaValidateOffsetBitsBytesEquiv,\
#appendNBits,\
#ByteStream_Init,\
#runtimeAssert,\
#writeToStdErr,\
#ByteStream_AttachBuffer,\
#ByteStream_GetLength,\
#BitString_equal,\
#reader,\
#decodeUnconstrainedWholeNumber_prefixLemma_helper,\
#decodeUnconstrainedWholeNumber_prefixLemma,\
#decodeConstrainedPosWholeNumber_prefixLemma,\
#resetAt,\
#withMovedByteIndex,\
#withMovedBitIndex,\
#bufLength,\
#isPrefixOf,\
#encodeUnsignedInteger,\
#decodeUnsignedInteger,\
#decodeUnsignedIntegerPure,\
#encodeConstrainedPosWholeNumber,\
#decodeConstrainedPosWholeNumber,\
#decodeConstrainedPosWholeNumberPure,\
#encodeConstrainedWholeNumber,\
#decodeConstrainedWholeNumber,\
#decodeConstrainedWholeNumberPure,\
#decodeConstrainedWholeNumberInt,\
#decodeConstrainedWholeNumberShort,\
#decodeConstrainedWholeNumberByte,\
#encodeSemiConstrainedWholeNumber,\
#decodeSemiConstrainedWholeNumber,\
#encodeSemiConstrainedPosWholeNumber,\
#decodeSemiConstrainedPosWholeNumber,\
#encodeUnconstrainedWholeNumber,\
#decodeUnconstrainedWholeNumber,\
#decodeUnconstrainedWholeNumberPure,\
#encodeReal,\
#encodeRealBitString,\
#decodeReal,\
#decodeRealBitString,\
#encodeOctetString,\
#encodeOctetString_no_length,\
#decodeOctetString_no_length,\
#encodeOctetString_fragmentation,\
#lemmaAdvanceBitIndexLessMaintainOffset,\
#lemmaGetBitCountUnsigned7FFFEquals15,\
#lemmaGetBitCountUnsignedFFEqualsEight,\
#encodeBitString,\
#encodeBitString_while,\
#decodeOctetString,\
#decodeBitString,\
$1