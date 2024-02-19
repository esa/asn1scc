package asn1scala


/**
 * Get an instance of a PER coded bitstream
 * @param count of elements in underlying buffer
 * @return PER coded bitstream
 */
def initPERCodec(count: Int): PER = {
   PER(Codec(BitStream(Array.fill(count)(0))))
}

case class PER private [asn1scala](base: Codec) {
   export base.*
}
