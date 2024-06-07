stainless-dotty src/main/scala/asn1scala/asn1jvm.scala  \
src/main/scala/asn1scala/asn1jvm_Verification.scala  \
src/main/scala/asn1scala/asn1jvm_Helper.scala \
src/main/scala/asn1scala/asn1jvm_Bitstream.scala \
src/main/scala/asn1scala/asn1jvm_Codec.scala \
src/main/scala/asn1scala/asn1jvm_Codec_ACN.scala \
--config-file=stainless.conf \
-D-parallel=5 \
--watch \
--functions=\
lemmaReadBitsThenGetListIsSameAsGetList,\
lemmaBitStreamReadBitsIntoListFromBitIndexPlusOneIsTail,\
lemmaSameBitContentListThenCheckByteArrayBitContent,\
appendBitsMSBFirst,\
readBits,\
readBitsLoop,\
##reader,\
##byteArrayBitContentToList,\
##appendBitsMSBFirstLoop,\
##appendBitFromByte,\
##checkByteArrayBitContent,\
##BitStream.appendBit,\
#readerFrom,\
#appendBitOne,\
#appendBitZero,\
#appendNBits,\
#appendNBitsLoop,\
#remainingBitsBitIndexLemma,\
#validateOffsetBitsDifferenceLemma,\
#validateOffsetBitsIneqLemma,\
#validateOffsetBitsWeakeningLemma,\
#validateOffsetBytesFromBitIndexLemma,\
#validateOffsetBitsContentIrrelevancyLemma,\
#readBitPrefixLemma,\
#checkBitsLoopPrefixLemma,\
#lemmaIsPrefixRefl,\
#lemmaIsPrefixTransitive,\
#checkBitsLoop,\
#checkBitsLoopPure,\
#appendNOneBits,\
#appendNZeroBits,\
#appendBitFromByte,\
#appendBitsLSBFirst,\
#appendBitsLSBFirstWhile,\
#appendBitsLSBFirstLoopTR,\
#readNLeastSignificantBitsLoop,\
#appendNLeastSignificantBits,\
#appendNLeastSignificantBitsLoop,\
#readNLeastSignificantBitsLoopPrefixLemma,\
#readBit,\
#readBitsLoop,\
#readNBitsLSBFirst,\
#readNBitsLSBFirstPure,\
#readNBitsLSBFirstsLoop,\
#readNBitsLSBFirstsLoopPure,\
#lemmaReadNBitsLSBFirstsLoopIsCorrect,\
$1
