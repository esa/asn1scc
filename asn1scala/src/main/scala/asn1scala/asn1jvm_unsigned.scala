package asn1scala


/*
* Meths to upcast unsigned integer data types on the JVM
*/
extension (ub: UByte) {
    def unsignedToLong: Long = ub & 0xFFL
}

extension (us: UShort) {
    def unsignedToLong: Long = us & 0xFF_FFL
}

extension (ui: UInt) {
    def unsignedToLong: Long = ui & 0xFF_FF_FF_FFL
}

extension (i: Int) {
    def unsignedToByte: UByte = {
        require(i >= 0 && i <= 0xFF)
        if ((i & 0x80) == 0x80)
            ((i & 0x7F) - 0x80).toByte
        else
            i.toByte
    }
}