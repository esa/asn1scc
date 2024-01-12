package asn1scala

import stainless.lang.StaticChecks.assert
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}

val FAILED_READ_ERR_CODE = 5400

/**
 * Get an instance of a ACN coded bitstream
 * @param count of elements in underlaying buffer
 * @return ACN coded bitstream
 */
def initACNCodec(count: Int): ACN = {
   ACN(BitStream(Array.fill(count)(0)))
}

case class ACN(bitStream: BitStream) extends Codec {

   /*ACN Integer functions*/
   def enc_Int_PositiveInteger_ConstSize(intVal: ULong, encodedSizeInBits: Int): Unit = {
      if encodedSizeInBits == 0 then
         return

      /* Get number of bits*/
      val nBits: Int = GetNumberOfBitsForNonNegativeInteger(intVal)
      /* put required zeros*/
      // TODO what if nBits > encodedSizeInBits ??
      appendNBitZero(encodedSizeInBits - nBits)
      /*Encode number */
      encodeNonNegativeInteger(intVal)
   }

   def enc_Int_PositiveInteger_ConstSize_8(intVal: ULong): Unit = {
      appendByte(intVal.toByte)
   }

   def enc_Int_PositiveInteger_ConstSize_big_endian_B(intVal: ULong, size: Int): Unit = {
      val tmp: ULong = intVal
      var mask: ULong = 0xFF
      mask <<= (size - 1) * 8

      var i: Int = 0
      while i < size do
         val byteToEncode: Byte = ((tmp & mask) >>> ((size - i - 1) * 8)).toByte
         appendByte(byteToEncode)
         mask >>>= 8
         i += 1
   }

   def enc_Int_PositiveInteger_ConstSize_big_endian_16(intVal: ULong): Unit = {
      enc_Int_PositiveInteger_ConstSize_big_endian_B(intVal, NO_OF_BYTES_IN_JVM_SHORT)
   }

   def enc_Int_PositiveInteger_ConstSize_big_endian_32(intVal: ULong): Unit = {
      enc_Int_PositiveInteger_ConstSize_big_endian_B(intVal, NO_OF_BYTES_IN_JVM_INT)
   }

   def enc_Int_PositiveInteger_ConstSize_big_endian_64(intVal: ULong): Unit = {
      enc_Int_PositiveInteger_ConstSize_big_endian_B(intVal, NO_OF_BYTES_IN_JVM_LONG)
   }

   def enc_Int_PositiveInteger_ConstSize_little_endian_N(intVal: ULong, size: Int): Unit = {
      var tmp: ULong = intVal

      var i: Int = 0
      while i < size do
         val byteToEncode: Byte = tmp.toByte
         appendByte(byteToEncode)
         tmp >>>= 8
         i += 1
   }

   def enc_Int_PositiveInteger_ConstSize_little_endian_16(intVal: ULong): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_N(intVal, 2)
   }

   def enc_Int_PositiveInteger_ConstSize_little_endian_32(intVal: ULong): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_N(intVal, 4)
   }

   def enc_Int_PositiveInteger_ConstSize_little_endian_64(intVal: ULong): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_N(intVal, NO_OF_BYTES_IN_JVM_LONG)
   }

   def dec_Int_PositiveInteger_ConstSize(encodedSizeInBits: Int): ULong = {
      decodeNonNegativeInteger(encodedSizeInBits)
   }

   def dec_Int_PositiveInteger_ConstSize_8(): ULong = {
      readByte().unsignedToInt
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_N(sizeInBytes: Int): ULong = {
      var ret: ULong = 0

      var i: Int = 0
      (while i < sizeInBytes do
         decreases(sizeInBytes < i)
         ret <<= 8
         ret |= readByte().unsignedToInt
         i += 1
      ).invariant(true) // TODO invariant

      ret
   }

   // TODO remove those and call dec_Int_PositiveInteger_ConstSize_big_endian_N directly
   def dec_Int_PositiveInteger_ConstSize_big_endian_16(): ULong = {
      dec_Int_PositiveInteger_ConstSize_big_endian_N(NO_OF_BYTES_IN_JVM_SHORT)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_32(): ULong = {
      dec_Int_PositiveInteger_ConstSize_big_endian_N(NO_OF_BYTES_IN_JVM_INT)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_64(): ULong = {
      dec_Int_PositiveInteger_ConstSize_big_endian_N(NO_OF_BYTES_IN_JVM_LONG)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_N(sizeInBytes: Int): ULong = {
      var ret: ULong = 0
      var tmp: ULong = 0 // TODO is this var even needed?

      var i: Int = 0
      (while i < sizeInBytes do
         decreases(sizeInBytes - i)
         tmp = readByte().unsignedToInt
         tmp <<= i * 8
         ret |= tmp
         i += 1
      ).invariant(true) // TODO invariant

      ret
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_16(): ULong = {
      dec_Int_PositiveInteger_ConstSize_little_endian_N(2)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_32(): ULong = {
      dec_Int_PositiveInteger_ConstSize_little_endian_N(4)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_64(): ULong = {
      dec_Int_PositiveInteger_ConstSize_little_endian_N(NO_OF_BYTES_IN_JVM_LONG)
   }

   def encode_UnsignedInteger(v: ULong, nBytes: Byte): Unit = {
      val MAX_BYTE_MASK = 0xFF00000000000000L
      assert(nBytes <= 8)

      var vv: ULong = v << (NO_OF_BYTES_IN_JVM_LONG * 8 - nBytes * 8)

      var i: Int = 0
      while i < nBytes do
         val byteToEncode: Byte = ((vv & MAX_BYTE_MASK) >>> ((NO_OF_BYTES_IN_JVM_LONG - 1) * 8)).toByte
         appendByte(byteToEncode)
         vv <<= 8
         i += 1
   }

   def enc_Int_PositiveInteger_VarSize_LengthEmbedded(intVal: ULong): Unit = {
      val nBytes: Byte = GetLengthForEncodingUnsigned(intVal).toByte

      /* encode length */
      appendByte(nBytes)
      /* Encode integer data*/
      encode_UnsignedInteger(intVal, nBytes)
   }

   def dec_Int_PositiveInteger_VarSize_LengthEmbedded(): ULong = {
      var v: ULong = 0

      val nBytes = readByte()
      var i: Int = 0

      (while i < nBytes do
         decreases(nBytes - i)
         v = (v << 8) | readByte().unsignedToInt
         i += 1
      ).invariant(true) // TODO invariant

      v
   }


   def enc_Int_TwosComplement_ConstSize(intVal: Long, encodedSizeInBits: Int): Unit = {
      if intVal >= 0 then
         appendNBitZero(encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger(intVal))
         encodeNonNegativeInteger(intVal)

      else
         appendNBitOne(encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger(-intVal - 1))
         encodeNonNegativeIntegerNeg(-intVal - 1)
   }


   def enc_Int_TwosComplement_ConstSize_8(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_8(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_big_endian_16(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_big_endian_16(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_big_endian_32(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_big_endian_32(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_big_endian_64(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_big_endian_64(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_little_endian_16(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_16(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_little_endian_32(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_32(int2uint(intVal))
   }

   def enc_Int_TwosComplement_ConstSize_little_endian_64(intVal: Long): Unit = {
      enc_Int_PositiveInteger_ConstSize_little_endian_64(int2uint(intVal))
   }

   def dec_Int_TwosComplement_ConstSize(encodedSizeInBits: Int): Long = {
      val valIsNegative = peekBit()

      val nBytes: Int = encodedSizeInBits / 8
      val rstBits: Int = encodedSizeInBits % 8

      var pIntVal: Long = if valIsNegative then Long.MaxValue else 0

      var i: Int = 0
      (while i < nBytes do
         decreases(nBytes - i)
         pIntVal = (pIntVal << 8) | (readByte().unsignedToInt)
         i += 1
         ).invariant(true) // TODO invariant

      if rstBits > 0 then
          pIntVal = (pIntVal << rstBits) | (readPartialByte(rstBits.toByte) & 0xFF)

      pIntVal
   }


   def dec_Int_TwosComplement_ConstSize_8(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_8(), 1)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_16(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_big_endian_16(), NO_OF_BYTES_IN_JVM_SHORT)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_32(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_big_endian_32(), NO_OF_BYTES_IN_JVM_INT)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_64(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_big_endian_64(), NO_OF_BYTES_IN_JVM_LONG)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_16(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_little_endian_16(), NO_OF_BYTES_IN_JVM_SHORT)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_32(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_little_endian_32(), NO_OF_BYTES_IN_JVM_INT)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_64(): Long = {
      uint2int(dec_Int_PositiveInteger_ConstSize_little_endian_64(), NO_OF_BYTES_IN_JVM_LONG)
   }

   def enc_Int_TwosComplement_VarSize_LengthEmbedded(intVal: Long): Unit = {
      val nBytes: Byte = GetLengthForEncodingSigned(intVal).toByte

      /* encode length */
      appendByte(nBytes)
      /* Encode integer data*/
      encode_UnsignedInteger(int2uint(intVal), nBytes)
   }

   def dec_Int_TwosComplement_VarSize_LengthEmbedded(): Long = {
      var v: ULong = 0
      var isNegative: Boolean = false

      val nBytes = readByte()

      var i: Int = 0
      (while i < nBytes do
         decreases(nBytes - i)
         val ub = readByte()

         if i == 0 && (ub.unsignedToInt) > 0 then
            v = Long.MaxValue
            isNegative = true

         v = (v << 8) | (ub & 0xFF)
         i += 1
      ).invariant(true) // TODO invariant

      if isNegative then
         -(~v) - 1      // TODO fixme
      else
         v
   }

   //return values is in nibbles
   def get_Int_Size_BCD(intVal: ULong): Int = {
      var intVar = intVal
      var ret: Int = 0
      while intVar > 0 do
         intVar /= 10
         ret += 1

      ret
   }


   def enc_Int_BCD_ConstSize(intVal: ULong, encodedSizeInNibbles: Int): Unit = {
      var intVar = intVal
      var totalNibbles: Int = 0
      val tmp: Array[UByte] = Array.fill(100)(0)

      assert(100 >= encodedSizeInNibbles)

      while intVar > 0 do
         tmp(totalNibbles) = (intVar % 10).asInstanceOf[UByte]
         totalNibbles += 1
         intVar /= 10

      assert(encodedSizeInNibbles >= totalNibbles)

      var i: Int = encodedSizeInNibbles - 1
      while i >= 0 do
         appendPartialByte(tmp(i).toByte, 4)
         i -= 1
   }


   def dec_Int_BCD_ConstSize(encodedSizeInNibbles: Int): ULong = {
      var l: ULong = 0

      var encodedSizeInNibblesVar = encodedSizeInNibbles

      (while encodedSizeInNibblesVar > 0 do
         decreases(encodedSizeInNibblesVar)

         l *= 10
         l += readPartialByte(4)
         encodedSizeInNibblesVar -= 1
      ).invariant(true) // TODO invariant
      l
   }


   def enc_Int_BCD_VarSize_LengthEmbedded(intVal: ULong): Unit = {
      val nNibbles: Int = get_Int_Size_BCD(intVal)
      /* encode length */
      appendByte(nNibbles.toByte)

      /* Encode Number */
      enc_Int_BCD_ConstSize(intVal, nNibbles)
   }


   def dec_Int_BCD_VarSize_LengthEmbedded(): ULong = {
       dec_Int_BCD_ConstSize(readByte())
   }


   //encoding puts an 'F' at the end
   def enc_Int_BCD_VarSize_NullTerminated(intVal: ULong): Unit = {

      val nNibbles: Int = get_Int_Size_BCD(intVal)

      /* Encode Number */
      enc_Int_BCD_ConstSize(intVal, nNibbles)

      appendPartialByte(0xF, 4)
   }

   def dec_Int_BCD_VarSize_NullTerminated(): ULong = {
      var l: ULong = 0

      while true do
          val digit = readPartialByte(4)
          if (digit > 9)
            return l

          l *= 10
          l += digit

      l
   }

   def enc_UInt_ASCII_ConstSize(intVal: ULong, encodedSizeInBytes: Int): Unit = {
      var intVar = intVal
      var totalNibbles: Int = 0
      val tmp: Array[UByte] = Array.fill(100)(0)

      assert(100 >= encodedSizeInBytes)

      while intVar > 0 do
         tmp(totalNibbles) = (intVar % 10).asInstanceOf[UByte]
         totalNibbles += 1
         intVar /= 10

      assert(encodedSizeInBytes >= totalNibbles)

      var i = encodedSizeInBytes - 1
      while i >= 0 do
         appendByte((tmp(i) + CHAR_ZERO).toByte)
         i -= 1
   }


   def enc_SInt_ASCII_ConstSize(intVal: Long, encodedSizeInBytes: Int): Unit = {
      val absIntVal: ULong = if intVal >= 0 then intVal else -intVal

      /* encode sign */
      appendByte(if intVal >= 0 then CHAR_PLUS else CHAR_MINUS)

      enc_UInt_ASCII_ConstSize(absIntVal, encodedSizeInBytes - 1)
   }

   def dec_UInt_ASCII_ConstSize(encodedSizeInBytes: Int): ULong = {
      var encodedSizeInBytesVar = encodedSizeInBytes
      var ret: ULong = 0

      (while encodedSizeInBytesVar > 0 do
         decreases(encodedSizeInBytesVar)
         val digit = readByte()

         assert(digit >= CHAR_ZERO && digit <= CHAR_NINE)

         ret *= 10
         ret += (digit.toInt - CHAR_ZERO).toByte

         encodedSizeInBytesVar -= 1
      ).invariant(true) // TODO invariant

      ret
   }

   def dec_SInt_ASCII_ConstSize(encodedSizeInBytes: Int): Long = {
      val digit = readByte()

      var sign: Int = 1
      if digit == CHAR_PLUS then
         sign = 1
      else if digit == CHAR_MINUS then
         sign = -1
      else
         assert(false)

      sign * dec_UInt_ASCII_ConstSize(encodedSizeInBytes - 1)
   }


   def getIntegerDigits(intVal: ULong): (Array[Byte], Byte) = {
      var intVar = intVal
      val digitsArray100: Array[Byte] = Array.fill(100)(0)
      val reversedDigitsArray: Array[Byte] = Array.fill(100)(0)
      var totalDigits: Byte = 0


      if intVar > 0 then
         while intVar > 0 && totalDigits < 100 do
            reversedDigitsArray(totalDigits) = (CHAR_ZERO + (intVar % 10)).toByte
            totalDigits = (totalDigits + 1).toByte
            intVar /= 10

         var i: Int = totalDigits - 1
         while i >= 0 do
            digitsArray100(totalDigits - 1 - i) = reversedDigitsArray(i)
            i -= 1

      else
         digitsArray100(0) = CHAR_ZERO
         totalDigits = 1

      (digitsArray100, totalDigits)
   }


   def enc_SInt_ASCII_VarSize_LengthEmbedded(intVal: Long): Unit = {
      val absIntVal: ULong = if intVal >= 0 then intVal else -intVal
      val (digitsArray100, nChars) = getIntegerDigits(absIntVal)

      /* encode length, plus 1 for sign */
      appendByte((nChars + 1).toByte)

      /* encode sign */
      appendByte(if intVal >= 0 then CHAR_PLUS else CHAR_MINUS)

      /* encode digits */
      var i: Int = 0
      while i < 100 && digitsArray100(i) != 0x0 do
         appendByte(digitsArray100(i))
         i += 1
   }

   def enc_UInt_ASCII_VarSize_LengthEmbedded(intVal: ULong): Unit = {
      val (digitsArray100, nChars) = getIntegerDigits(intVal)

      /* encode length */
      appendByte(nChars)
      /* encode digits */
      var i: Int = 0
      while i < 100 && digitsArray100(i) != 0x0 do
         appendByte(digitsArray100(i))
         i += 1
   }


   def dec_UInt_ASCII_VarSize_LengthEmbedded(): ULong = dec_UInt_ASCII_ConstSize(readByte())
   def dec_SInt_ASCII_VarSize_LengthEmbedded(): Long = dec_SInt_ASCII_ConstSize(readByte())

   def enc_UInt_ASCII_VarSize_NullTerminated(intVal: ULong, null_characters: Array[Byte], null_characters_size: Int): Unit = {
      val (digitsArray100, nChars) = getIntegerDigits(intVal)

      var i: Int = 0 // TODO: size_t?
      while i < 100 && digitsArray100(i) != 0x0 do
         appendByte(digitsArray100(i))
         i += 1

      i = 0
      while i < null_characters_size do
         appendByte(null_characters(i))
         i += 1
   }

   def enc_SInt_ASCII_VarSize_NullTerminated(intVal: Long, null_characters: Array[Byte], null_characters_size: Int): Unit = {
      val absValue: ULong = if intVal >= 0 then intVal else -intVal
      appendByte(if intVal >= 0 then CHAR_PLUS else CHAR_MINUS)

      enc_UInt_ASCII_VarSize_NullTerminated(absValue, null_characters, null_characters_size)
   }

   def dec_UInt_ASCII_VarSize_NullTerminated(null_characters: Array[Byte], null_characters_size: Int): ULong = {
      var digit: Byte = 0
      var ret: ULong = 0
      val tmp: Array[Byte] = Array.fill(10)(0)

      val sz: Int = if null_characters_size < 10 then null_characters_size else 10

      //read null_character_size characters into the tmp buffer
      var j: Int = 0
      (while j < null_characters_size do
         decreases(null_characters_size - j)
         tmp(j) = readByte()
         j += 1
      ).invariant(true) // TODO invariant

      var i: Long = 0
      while !arraySameElements(null_characters, tmp) do
         digit = tmp(0)
         i += 1

         j = 0
         while j < null_characters_size - 1 do
            tmp(j) = tmp(j + 1)
            j += 1

         tmp(null_characters_size - 1) = readByte()

         digit = (digit - CHAR_ZERO).toByte

         ret *= 10
         ret += digit

      ret
   }


   def dec_SInt_ASCII_VarSize_NullTerminated(null_characters: Array[Byte], null_characters_size: Int): Long = {
      var isNegative: Boolean = false

      val digit = readByte()
      assert(digit == CHAR_MINUS || digit == CHAR_PLUS)
      if digit == CHAR_MINUS then
         isNegative = true

      val ul = dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size)
      if isNegative then -ul else ul
   }


   /* Boolean Decode */
   // TODO move to codec?
   def BitStream_ReadBitPattern(patternToRead: Array[Byte], nBitsToRead: Int): Boolean = {
      val nBytesToRead: Int = nBitsToRead / 8
      val nRemainingBitsToRead: Int = nBitsToRead % 8

      var pBoolValue: Boolean = true
      var i: Int = 0
      (while i < nBytesToRead do
         decreases(nBytesToRead - i)
         if readByte() != patternToRead(i) then
            pBoolValue = false
         i += 1
      ).invariant(true) // TODO

      if nRemainingBitsToRead > 0 then
         if readPartialByte(nRemainingBitsToRead.toByte) != ((patternToRead(nBytesToRead) & 0xFF) >>> (8 - nRemainingBitsToRead)) then
            pBoolValue = false

      pBoolValue
   }

   // TODO move to codec?
   def BitStream_ReadBitPattern_ignore_value(nBitsToRead: Int): Unit = {
      // TODO replace implementation with readBits(nBitsToRead)?
      val nBytesToRead: Int = nBitsToRead / 8
      val nRemainingBitsToRead: Int = nBitsToRead % 8

      var i: Int = 0
      (while i < nBytesToRead do
         decreases(nBytesToRead - i)
         readByte()
         i += 1
      ).invariant(true) // TODO invariant

      if nRemainingBitsToRead > 0 then
         readPartialByte(nRemainingBitsToRead.toByte)
   }

   /* Real encoding functions */
   def enc_Real_IEEE754_32_big_endian(realValue: Float): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(NO_OF_BYTES_IN_JVM_FLOAT).putFloat(realValue).array

      var i: Int = 0
      while i < NO_OF_BYTES_IN_JVM_FLOAT do
         appendByte(b(i))
         i += 1
   }

   def enc_Real_IEEE754_32_little_endian(realValue: Float): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(NO_OF_BYTES_IN_JVM_FLOAT).putFloat(realValue).array

      var i: Int = NO_OF_BYTES_IN_JVM_FLOAT - 1
      while i >= 0 do
         appendByte(b(i))
         i -= 1
   }

   def enc_Real_IEEE754_64_big_endian(realValue: Double): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(NO_OF_BYTES_IN_JVM_DOUBLE).putDouble(realValue).array

      var i: Int = 0
      while i < NO_OF_BYTES_IN_JVM_DOUBLE do
         appendByte(b(i))
         i += 1
   }

   def enc_Real_IEEE754_64_little_endian(realValue: Double): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(NO_OF_BYTES_IN_JVM_DOUBLE).putDouble(realValue).array

      var i: Int = NO_OF_BYTES_IN_JVM_DOUBLE - 1
      while i >= 0 do
         appendByte(b(i))
         i -= 1
   }

   /* Real decoding functions */
   def dec_Real_IEEE754_32_big_endian(): Float = {
      var ret: Int = 0
      var i: Int = 1

      assert(NO_OF_BYTES_IN_JVM_INT == NO_OF_BYTES_IN_JVM_FLOAT)

      (while i <= NO_OF_BYTES_IN_JVM_INT do
         decreases(NO_OF_BYTES_IN_JVM_INT - i)
         ret |= readByte().unsignedToInt << (NO_OF_BYTES_IN_JVM_INT - i) * NO_OF_BITS_IN_BYTE
         i += 1
      ).invariant(true) // TODO

      java.lang.Float.intBitsToFloat(ret)
   }

   def dec_Real_IEEE754_32_little_endian(): Float = {
      var ret: Int = 0
      var i: Int = 0

      assert(NO_OF_BYTES_IN_JVM_INT == NO_OF_BYTES_IN_JVM_FLOAT)

      (while i < NO_OF_BYTES_IN_JVM_INT do
         decreases(NO_OF_BYTES_IN_JVM_INT - i)
         ret |= readByte().unsignedToInt << i * NO_OF_BITS_IN_BYTE
         i += 1
      ).invariant(true) // TODO

      java.lang.Float.intBitsToFloat(ret)
   }

   def dec_Real_IEEE754_64_big_endian(): Double = {
      var ret: Long = 0
      var i: Int = 1

      assert(NO_OF_BYTES_IN_JVM_LONG == NO_OF_BYTES_IN_JVM_DOUBLE)

      (while i <= NO_OF_BYTES_IN_JVM_LONG do
         decreases(NO_OF_BYTES_IN_JVM_LONG - i)
         ret |= readByte().unsignedToLong << (NO_OF_BYTES_IN_JVM_LONG - i) * NO_OF_BITS_IN_BYTE
         i += 1
      ).invariant(true) // TODO

      java.lang.Double.longBitsToDouble(ret)
   }

   def dec_Real_IEEE754_64_little_endian(): Double = {
      var ret: Long = 0
      var i: Int = 0

      assert(NO_OF_BYTES_IN_JVM_LONG == NO_OF_BYTES_IN_JVM_DOUBLE)

      (while i < NO_OF_BYTES_IN_JVM_LONG do
         decreases(NO_OF_BYTES_IN_JVM_LONG - i)
         ret |= readByte().unsignedToLong << i * NO_OF_BITS_IN_BYTE
         i += 1
      ).invariant(true) // TODO

      java.lang.Double.longBitsToDouble(ret)
   }

   /* String functions*/
   def enc_String_Ascii_FixSize(max: Long, strVal: Array[ASCIIChar]): Unit = {
      var i: Long = 0
      while i < max do
         appendByte(strVal(i.toInt))
         i += 1
   }

   def enc_String_Ascii_private(max: Long, strVal: Array[ASCIIChar]): Long = {
      var i: Long = 0
      while (i < max) && (strVal(i.toInt) != CHAR_0000) do
         appendByte(strVal(i.toInt))
         i += 1

      i
   }

   def enc_String_Ascii_Null_Teminated(max: Long, null_character: Byte, strVal: Array[ASCIIChar]): Unit = {
      enc_String_Ascii_private(max, strVal)
      appendByte(null_character.toByte)
   }

   def enc_String_Ascii_Null_Teminated_mult(max: Long, null_character: Array[Byte], null_character_size: Int, strVal: Array[ASCIIChar]): Unit = {
      enc_String_Ascii_private(max, strVal)
      var i: Int = 0
      while i < null_character_size do
         appendByte(null_character(i))
         i += 1
   }


   def enc_String_Ascii_External_Field_Determinant(max: Long, strVal: Array[ASCIIChar]): Unit = {
      enc_String_Ascii_private(max, strVal)
   }

   def enc_String_Ascii_Internal_Field_Determinant(max: Long, min: Long, strVal: Array[ASCIIChar]): Unit = {
      val strLen: Int = strVal.length
      encodeConstrainedWholeNumber(if strLen <= max then strLen else max, min, max)
      enc_String_Ascii_private(max, strVal)
   }

   def enc_String_CharIndex_FixSize(max: Long, allowedCharSet: Array[Byte], strVal: Array[ASCIIChar]): Unit = {
      var i: Int = 0
      while i < max do
         val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
         encodeConstrainedWholeNumber(charIndex, 0, allowedCharSet.length - 1)
         i += 1
   }

   def enc_String_CharIndex_private(max: Long, allowedCharSet: Array[Byte], strVal: Array[ASCIIChar]): Long = {
      var i: Int = 0
      while (i < max) && (strVal(i) != CHAR_0000) do
         val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
         encodeConstrainedWholeNumber(charIndex, 0, allowedCharSet.length - 1)
         i += 1

      i
   }


   def enc_String_CharIndex_External_Field_Determinant(max: Long, allowedCharSet: Array[Byte], strVal: Array[ASCIIChar]): Unit = {
      enc_String_CharIndex_private(max, allowedCharSet, strVal)
   }

   def enc_String_CharIndex_Internal_Field_Determinant(max: Long, allowedCharSet: Array[Byte], min: Long, strVal: Array[ASCIIChar]): Unit = {
      val strLen: Int = strVal.length
      encodeConstrainedWholeNumber(if strLen <= max then strLen else max, min, max)
      enc_String_CharIndex_private(max, allowedCharSet, strVal)
   }


   def enc_IA5String_CharIndex_External_Field_Determinant(max: Long, strVal: Array[ASCIIChar]): Unit = {
      val allowedCharSet: Array[ASCIIChar] = Array(
         0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
         0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
         0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
         0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
         0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
         0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
         0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
         0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
         0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
         0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
         0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
         0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
         0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
      )

      enc_String_CharIndex_private(max, allowedCharSet, strVal)
   }

   def enc_IA5String_CharIndex_Internal_Field_Determinant(max: Long, min: Long, strVal: Array[ASCIIChar]): Unit = {
      val allowedCharSet: Array[ASCIIChar] = Array(
         0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
         0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
         0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
         0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
         0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
         0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
         0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
         0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
         0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
         0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
         0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
         0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
         0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
      )
      val strLen: Int = strVal.length
      encodeConstrainedWholeNumber(if strLen <= max then strLen else max, min, max)
      enc_String_CharIndex_private(max, allowedCharSet, strVal)
   }


   def dec_String_Ascii_private(max: Long, charactersToDecode: Long): Array[ASCIIChar] = {
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0)
      var i: Int = 0
      (while i < charactersToDecode do
         decreases(charactersToDecode - i)
          strVal(i) = readByte()
         i += 1
      ).invariant(true) // TODO

      strVal
   }


   def dec_String_Ascii_FixSize(max: Long): Array[ASCIIChar] = {
      dec_String_Ascii_private(max, max)
   }

   def dec_String_Ascii_Null_Teminated(max: Long, null_character: ASCIIChar): Array[ASCIIChar] = {
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0)
      var endReached = false
      var i: Int = 0
      (while i <= max && endReached do
         val decodedCharacter = readByte()
         if decodedCharacter != null_character then
            strVal(i) = decodedCharacter
            i += 1
         else
            strVal(i) = 0x0
            endReached = true
      ).invariant(true) // TODO

      strVal
   }

   def dec_String_Ascii_Null_Teminated_mult(max: Long, null_character: Array[ASCIIChar], null_character_size: Int): Array[ASCIIChar] = {
      val tmp: Array[Byte] = Array.fill(null_character_size)(0)
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0)
      //read null_character_size characters into the tmp buffer

      var j: Int = 0
      (while j < null_character_size do
         decreases(null_character_size - j)
         tmp(j) = readByte()
         j += 1
      ).invariant(true) // TODO

      var i: Int = 0
      while i <= max && !arraySameElements(null_character, tmp) do
         strVal(i) = tmp(0)
         i += 1
         j = 0
         (while j < null_character_size - 1 do
            decreases(null_character_size - j)
            tmp(j) = tmp(j + 1)
            j += 1
         ).invariant(true) // TODO
         tmp(null_character_size - 1) = readByte()

      strVal(i) = 0x0

      assert(arraySameElements(null_character, tmp))

      strVal
   }


   def dec_String_Ascii_External_Field_Determinant(max: Long, extSizeDeterminatFld: Long): Array[ASCIIChar] = {
      dec_String_Ascii_private(max, if extSizeDeterminatFld <= max then extSizeDeterminatFld else max)
   }

   def dec_String_Ascii_Internal_Field_Determinant(max: Long, min: Long): Array[ASCIIChar] = {
      val nCount = decodeConstrainedWholeNumber(min, max)
      dec_String_Ascii_private(max, if nCount <= max then nCount else max)
   }

   def dec_String_CharIndex_private(max: Long, charactersToDecode: Long, allowedCharSet: Array[Byte]): Array[ASCIIChar] = {
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0)
      var i: Int = 0
      (while i < charactersToDecode do
         decreases(charactersToDecode - i)
         strVal(i) = allowedCharSet(decodeConstrainedWholeNumber(0, allowedCharSet.length - 1).toInt)
         i += 1
      ).invariant(true) // TODO

      strVal
   }

   def dec_String_CharIndex_FixSize(max: Long, allowedCharSet: Array[ASCIIChar]): Array[ASCIIChar] = {
      dec_String_CharIndex_private(max, max, allowedCharSet)
   }

   def dec_String_CharIndex_External_Field_Determinant(max: Long, allowedCharSet: Array[ASCIIChar], extSizeDeterminatFld: Long): Array[ASCIIChar] = {
      dec_String_CharIndex_private(max, if extSizeDeterminatFld <= max then extSizeDeterminatFld else max, allowedCharSet)
   }


   def dec_String_CharIndex_Internal_Field_Determinant(max: Long, allowedCharSet: Array[ASCIIChar], min: Long): Array[ASCIIChar] = {
      val nCount = decodeConstrainedWholeNumber(min, max)
      dec_String_CharIndex_private(max, if nCount <= max then nCount else max, allowedCharSet)
   }


   def dec_IA5String_CharIndex_External_Field_Determinant(max: Long, extSizeDeterminatFld: Long): Array[ASCIIChar] = {
      val allowedCharSet: Array[ASCIIChar] = Array(
         0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
         0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
         0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
         0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
         0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
         0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
         0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
         0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
         0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
         0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
         0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
         0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
         0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
      )
      dec_String_CharIndex_private(max, if extSizeDeterminatFld <= max then extSizeDeterminatFld else max, allowedCharSet)
   }

   def dec_IA5String_CharIndex_Internal_Field_Determinant(max: Long, min: Long): Array[ASCIIChar] = {
      val allowedCharSet: Array[ASCIIChar] = Array(
         0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09,
         0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10, 0x11, 0x12, 0x13,
         0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
         0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
         0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31,
         0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
         0x3C, 0x3D, 0x3E, 0x3F, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45,
         0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
         0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
         0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60, 0x61, 0x62, 0x63,
         0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D,
         0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
         0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F
      )
      val nCount = decodeConstrainedWholeNumber(min, max)
      dec_String_CharIndex_private(max, if nCount <= max then nCount else max, allowedCharSet)
   }


   /* Length Determinant functions*/
   def enc_Length(lengthValue: ULong, lengthSizeInBits: Int): Unit = {
      /* encode length */
      enc_Int_PositiveInteger_ConstSize(lengthValue, lengthSizeInBits)
   }

   def dec_Length(lengthSizeInBits: Int): ULong = {
      dec_Int_PositiveInteger_ConstSize(lengthSizeInBits)
   }

   def milbus_encode(v: Long): Long = {
      if v == 32 then 0 else v
   }

   def milbus_decode(v: Long): Long = {
      if v == 0 then 32 else v
   }

   def dec_Int_PositiveInteger_ConstSizeUInt8(encodedSizeInBits: Int): UByte = dec_Int_PositiveInteger_ConstSize(encodedSizeInBits).toByte
   def dec_Int_PositiveInteger_ConstSizeUInt16(encodedSizeInBits: Int): UShort = dec_Int_PositiveInteger_ConstSize(encodedSizeInBits).toShort
   def dec_Int_PositiveInteger_ConstSizeUInt32(encodedSizeInBits: Int): UInt = dec_Int_PositiveInteger_ConstSize(encodedSizeInBits).toInt
   def dec_Int_PositiveInteger_ConstSize_8UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_8().toByte
   def dec_Int_PositiveInteger_ConstSize_big_endian_16UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_big_endian_16().toShort
   def dec_Int_PositiveInteger_ConstSize_big_endian_16UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_big_endian_16().toByte
   def dec_Int_PositiveInteger_ConstSize_big_endian_32UInt32(): UInt = dec_Int_PositiveInteger_ConstSize_big_endian_32().toInt
   def dec_Int_PositiveInteger_ConstSize_big_endian_32UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_big_endian_32().toShort
   def dec_Int_PositiveInteger_ConstSize_big_endian_32UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_big_endian_32().toByte
   def dec_Int_PositiveInteger_ConstSize_big_endian_64UInt32(): UInt = dec_Int_PositiveInteger_ConstSize_big_endian_64().toInt
   def dec_Int_PositiveInteger_ConstSize_big_endian_64UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_big_endian_64().toShort
   def dec_Int_PositiveInteger_ConstSize_big_endian_64UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_big_endian_64().toByte
   def dec_Int_PositiveInteger_ConstSize_little_endian_16UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_little_endian_16().toShort
   def dec_Int_PositiveInteger_ConstSize_little_endian_16UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_little_endian_16().toByte
   def dec_Int_PositiveInteger_ConstSize_little_endian_32UInt32(): UInt = dec_Int_PositiveInteger_ConstSize_little_endian_32().toInt
   def dec_Int_PositiveInteger_ConstSize_little_endian_32UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_little_endian_32().toShort
   def dec_Int_PositiveInteger_ConstSize_little_endian_32UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_little_endian_32().toByte
   def dec_Int_PositiveInteger_ConstSize_little_endian_64UInt32(): UInt = dec_Int_PositiveInteger_ConstSize_little_endian_64().toInt
   def dec_Int_PositiveInteger_ConstSize_little_endian_64UInt16(): UShort = dec_Int_PositiveInteger_ConstSize_little_endian_64().toShort
   def dec_Int_PositiveInteger_ConstSize_little_endian_64UInt8(): UByte = dec_Int_PositiveInteger_ConstSize_little_endian_64().toByte
   def dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt8(): UByte = dec_Int_PositiveInteger_VarSize_LengthEmbedded().toByte
   def dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt16(): UShort = dec_Int_PositiveInteger_VarSize_LengthEmbedded().toShort
   def dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt32(): UInt = dec_Int_PositiveInteger_VarSize_LengthEmbedded().toInt
   def dec_Int_TwosComplement_ConstSizeInt8(encodedSizeInBits: Int): Byte = dec_Int_TwosComplement_ConstSize(encodedSizeInBits).toByte
   def dec_Int_TwosComplement_ConstSizeInt16(encodedSizeInBits: Int): Short = dec_Int_TwosComplement_ConstSize(encodedSizeInBits).toShort
   def dec_Int_TwosComplement_ConstSizeInt32(encodedSizeInBits: Int): Int = dec_Int_TwosComplement_ConstSize(encodedSizeInBits).toInt
   def dec_Int_TwosComplement_ConstSize_8Int8(): Byte = dec_Int_TwosComplement_ConstSize_8().toByte
   def dec_Int_TwosComplement_ConstSize_big_endian_16Int16(): Short = dec_Int_TwosComplement_ConstSize_big_endian_16().toShort
   def dec_Int_TwosComplement_ConstSize_big_endian_16Int8(): Byte = dec_Int_TwosComplement_ConstSize_big_endian_16().toByte
   def dec_Int_TwosComplement_ConstSize_big_endian_32Int32(): Int = dec_Int_TwosComplement_ConstSize_big_endian_32().toInt
   def dec_Int_TwosComplement_ConstSize_big_endian_32Int16(): Short = dec_Int_TwosComplement_ConstSize_big_endian_32().toShort
   def dec_Int_TwosComplement_ConstSize_big_endian_32Int8(): Byte = dec_Int_TwosComplement_ConstSize_big_endian_32().toByte
   def dec_Int_TwosComplement_ConstSize_big_endian_64Int32(): Int = dec_Int_TwosComplement_ConstSize_big_endian_64().toInt
   def dec_Int_TwosComplement_ConstSize_big_endian_64Int16(): Short = dec_Int_TwosComplement_ConstSize_big_endian_64().toShort
   def dec_Int_TwosComplement_ConstSize_big_endian_64Int8(): Byte = dec_Int_TwosComplement_ConstSize_big_endian_64().toByte
   def dec_Int_TwosComplement_ConstSize_little_endian_16Int16(): Short = dec_Int_TwosComplement_ConstSize_little_endian_16().toShort
   def dec_Int_TwosComplement_ConstSize_little_endian_16Int8(): Byte = dec_Int_TwosComplement_ConstSize_little_endian_16().toByte
   def dec_Int_TwosComplement_ConstSize_little_endian_32Int32(): Int = dec_Int_TwosComplement_ConstSize_little_endian_32().toInt
   def dec_Int_TwosComplement_ConstSize_little_endian_32Int16(): Short = dec_Int_TwosComplement_ConstSize_little_endian_32().toShort
   def dec_Int_TwosComplement_ConstSize_little_endian_32Int8(): Byte = dec_Int_TwosComplement_ConstSize_little_endian_32().toByte
   def dec_Int_TwosComplement_ConstSize_little_endian_64Int32(): Int = dec_Int_TwosComplement_ConstSize_little_endian_64().toInt
   def dec_Int_TwosComplement_ConstSize_little_endian_64Int16(): Short = dec_Int_TwosComplement_ConstSize_little_endian_64().toShort
   def dec_Int_TwosComplement_ConstSize_little_endian_64Int8(): Byte = dec_Int_TwosComplement_ConstSize_little_endian_64().toByte
   def dec_Int_TwosComplement_VarSize_LengthEmbeddedInt8(): Byte = dec_Int_TwosComplement_VarSize_LengthEmbedded().toByte
   def dec_Int_TwosComplement_VarSize_LengthEmbeddedInt16(): Short = dec_Int_TwosComplement_VarSize_LengthEmbedded().toShort
   def dec_Int_TwosComplement_VarSize_LengthEmbeddedInt32(): Int = dec_Int_TwosComplement_VarSize_LengthEmbedded().toInt
   def dec_Int_BCD_ConstSizeUInt8(encodedSizeInNibbles: Int): UByte = dec_Int_BCD_ConstSize(encodedSizeInNibbles).toByte
   def dec_Int_BCD_ConstSizeUInt16(encodedSizeInNibbles: Int): UShort = dec_Int_BCD_ConstSize(encodedSizeInNibbles).toShort
   def dec_Int_BCD_ConstSizeUInt32(encodedSizeInNibbles: Int): UInt = dec_Int_BCD_ConstSize(encodedSizeInNibbles).toInt
   def dec_Int_BCD_VarSize_LengthEmbeddedUInt8(): UByte = dec_Int_BCD_VarSize_LengthEmbedded().toByte
   def dec_Int_BCD_VarSize_LengthEmbeddedUInt16(): UShort = dec_Int_BCD_VarSize_LengthEmbedded().toShort
   def dec_Int_BCD_VarSize_LengthEmbeddedUInt32(): UInt = dec_Int_BCD_VarSize_LengthEmbedded().toInt
   def dec_Int_BCD_VarSize_NullTerminatedUInt8(): UByte = dec_Int_BCD_VarSize_NullTerminated().toByte
   def dec_Int_BCD_VarSize_NullTerminatedUInt16(): UShort = dec_Int_BCD_VarSize_NullTerminated().toShort
   def dec_Int_BCD_VarSize_NullTerminatedUInt32(): UInt = dec_Int_BCD_VarSize_NullTerminated().toInt
   def dec_SInt_ASCII_ConstSizeInt8(encodedSizeInBytes: Int): Byte = dec_SInt_ASCII_ConstSize(encodedSizeInBytes).toByte
   def dec_SInt_ASCII_ConstSizeInt16(encodedSizeInBytes: Int): Short = dec_SInt_ASCII_ConstSize(encodedSizeInBytes).toShort
   def dec_SInt_ASCII_ConstSizeInt32(encodedSizeInBytes: Int): Int = dec_SInt_ASCII_ConstSize(encodedSizeInBytes).toInt
   def dec_SInt_ASCII_VarSize_LengthEmbeddedInt8(): Byte = dec_SInt_ASCII_VarSize_LengthEmbedded().toByte
   def dec_SInt_ASCII_VarSize_LengthEmbeddedInt16(): Short = dec_SInt_ASCII_VarSize_LengthEmbedded().toShort
   def dec_SInt_ASCII_VarSize_LengthEmbeddedInt32(): Int = dec_SInt_ASCII_VarSize_LengthEmbedded().toInt
   def dec_SInt_ASCII_VarSize_NullTerminatedInt8(null_characters: Array[Byte], null_characters_size: Int): Byte =
      dec_SInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toByte
   def dec_SInt_ASCII_VarSize_NullTerminatedInt16(null_characters: Array[Byte], null_characters_size: Int): Short =
      dec_SInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toShort
   def dec_SInt_ASCII_VarSize_NullTerminatedInt32(null_characters: Array[Byte], null_characters_size: Int): Int =
      dec_SInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toInt
   def dec_UInt_ASCII_ConstSizeUInt8(encodedSizeInBytes: Int): UByte = dec_UInt_ASCII_ConstSize(encodedSizeInBytes).toByte
   def dec_UInt_ASCII_ConstSizeUInt16(encodedSizeInBytes: Int): UShort = dec_UInt_ASCII_ConstSize(encodedSizeInBytes).toShort
   def dec_UInt_ASCII_ConstSizeUInt32(encodedSizeInBytes: Int): UInt = dec_UInt_ASCII_ConstSize(encodedSizeInBytes).toInt
   def dec_UInt_ASCII_VarSize_LengthEmbeddedUInt8(): UByte = dec_UInt_ASCII_VarSize_LengthEmbedded().toByte
   def dec_UInt_ASCII_VarSize_LengthEmbeddedUInt16(): UShort = dec_UInt_ASCII_VarSize_LengthEmbedded().toShort
   def dec_UInt_ASCII_VarSize_LengthEmbeddedUInt32(): UInt = dec_UInt_ASCII_VarSize_LengthEmbedded().toInt
   def dec_UInt_ASCII_VarSize_NullTerminatedUInt8(null_characters: Array[Byte], null_characters_size: Int): UByte =
      dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toByte
   def dec_UInt_ASCII_VarSize_NullTerminatedUInt16(null_characters: Array[Byte], null_characters_size: Int): UShort =
      dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toShort
   def dec_UInt_ASCII_VarSize_NullTerminatedUInt32(null_characters: Array[Byte], null_characters_size: Int): UInt =
      dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size).toInt
}
