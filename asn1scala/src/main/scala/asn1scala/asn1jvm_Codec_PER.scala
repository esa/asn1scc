package asn1scala


/**
 * Get an instance of a PER coded bitstream
 * @param count of elements in underlaying buffer
 * @return PER coded bitstream
 */
def initPERCodec(count: Int): PER = {
   PER(BitStream(Array.fill(count)(0), count.toLong * 8))
}

case class PER(bitStream: BitStream) extends Codec { }
