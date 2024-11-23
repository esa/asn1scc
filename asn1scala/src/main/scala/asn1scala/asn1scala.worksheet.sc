opaque type ULong = Long
object ULong {
    @inline def fromRaw(u: Long): ULong = u
}
extension (l: ULong) {
    @inline def toRaw: Long = l

    // @ignore
    // inline def ==(r: Int): Boolean = {
    //     scala.compiletime.requireConst(r)
    //     l == r.toLong.toRawULong
    // }
}

val NO_OF_BITS_IN_LONG = 64

def GetBitCountUnsigned(vv: ULong): Int = {
   val v = vv.toRaw

   if v < 0 then
      return NO_OF_BITS_IN_LONG

   if v == 0 then
      return 0

   var i = 0
   var l = v
   (while i < NO_OF_BITS_IN_LONG - 1 && l != 0 do
      l >>>= 1
      i += 1
   )
   i
}

GetBitCountUnsigned(0xFF)

(0x4002) / 0x4000