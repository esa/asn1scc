package asn1scala

// all bits of the integer
val MASK_BYTE       = 0xFF
val MASK_BYTE_L     = 0xFFL
val MASK_SHORT_L    = 0xFF_FFL
val MASK_INT_L      = 0xFF_FF_FF_FFL

// MSBs (neg bits of the integer)
val MASK_MSB_BYTE   = 0x80L
val MASK_MSB_INT    = 0x80_00_00_00L

// pos bits of the integer
val MASK_POS_BYTE   = 0x7FL
val MASK_POS_INT    = 0x7F_FF_FF_FFL

/*
* Meths to upcast unsigned integer data types on the JVM
*/
extension (ub: UByte) {
    def unsignedToLong: Long = ub & MASK_BYTE_L
    def unsignedToInt: Int = ub & MASK_BYTE
}

extension (us: UShort) {
    def unsignedToLong: Long = us & MASK_SHORT_L
}

extension (ui: UInt) {
    def unsignedToLong: Long = ui & MASK_INT_L
}

extension (i: Int) {
    def toUnsignedByte: UByte = {
        require(i >= 0 && i <= MASK_BYTE)

        if(i == MASK_MSB_BYTE)
            (-MASK_MSB_BYTE).toByte
        else if ((i & MASK_MSB_BYTE) == MASK_MSB_BYTE)
            ((i & MASK_POS_BYTE) - MASK_MSB_BYTE).toByte
        else
            i.toByte
    }
}

extension (l: Long) {
    def toUnsignedInt: UInt = {
        require(l >= 0 && l <= MASK_INT_L)

        if(l == MASK_MSB_INT)
            (-MASK_MSB_INT).toInt
        else if ((l & MASK_MSB_INT) == MASK_MSB_INT)
            ((l & MASK_POS_INT) - MASK_MSB_INT).toInt
        else
            l.toInt
    }
}

extension (b: Byte) {
    def >>>>(i: Int): Byte = {
        require(i >= 0 && i <= 8)
        ((b.toInt & MASK_BYTE) >>> i).toUnsignedByte
    }

    def <<<<(i: Int): Byte = {
        require(i >= 0 && i <= 8)
        ((b.toInt << i) & MASK_BYTE).toUnsignedByte
    }
}
