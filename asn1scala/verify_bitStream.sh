stainless-dotty \
src/main/scala/asn1scala/asn1jvm.scala  \
src/main/scala/asn1scala/asn1jvm_Verification.scala  \
src/main/scala/asn1scala/asn1jvm_Helper.scala \
src/main/scala/asn1scala/asn1jvm_Bitstream.scala \
src/main/scala/asn1scala/asn1jvm_Vector.scala \
--config-file=stainless.conf \
-D-parallel=5 \
$1
