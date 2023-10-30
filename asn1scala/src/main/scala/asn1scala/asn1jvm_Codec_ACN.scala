package asn1scala

import stainless.lang.StaticChecks.assert
import stainless.lang.{None, Option, Some}

val FAILED_READ_ERR_CODE = 5400

// TODO remove / replace by invariant
def CHECK_BIT_STREAM(pBitStrm: BitStream): Unit = {
   assert(pBitStrm.currentByte.toLong * 8 + pBitStrm.currentBit <= pBitStrm.buf.length.toLong * 8)
}

/**
 * Get an instance of a ACN coded bitstream
 * @param count of elements in underlaying buffer
 * @return ACN coded bitstream
 */
def initACNCodec(count: Int): ACN = {
   ACN(BitStream(Array.fill(count)(0)))
}

case class ACN(bitStream: BitStream) extends Codec {

   def alignToByte(): Unit = {
      if bitStream.currentBit != 0 then
         bitStream.currentBit = 0
         bitStream.currentByte += 1
         CHECK_BIT_STREAM(bitStream)
   }

   def alignToShort(): Unit = {
      alignToByte()
      bitStream.currentByte = ((bitStream.currentByte +
         (NO_OF_BYTES_IN_JVM_SHORT - 1)) / NO_OF_BYTES_IN_JVM_SHORT) * NO_OF_BYTES_IN_JVM_SHORT
      CHECK_BIT_STREAM(bitStream)
   }

   def alignToInt(): Unit = {
      alignToByte()
      bitStream.currentByte = ((bitStream.currentByte +
         (NO_OF_BYTES_IN_JVM_INT - 1)) / NO_OF_BYTES_IN_JVM_INT) * NO_OF_BYTES_IN_JVM_INT
      CHECK_BIT_STREAM(bitStream)
   }

   /*ACN Integer functions*/
   def enc_Int_PositiveInteger_ConstSize(intVal: ULong, encodedSizeInBits: Int): Unit = {
      if encodedSizeInBits == 0 then
         return

      /* Get number of bits*/
      val nBits: Int = GetNumberOfBitsForNonNegativeInteger(intVal)
      /* put required zeros*/
      // TODO what if nBits > encodedSizeInBits ??
      bitStream.appendNBitZero(encodedSizeInBits - nBits)
      /*Encode number */
      encodeNonNegativeInteger(intVal)

      CHECK_BIT_STREAM(bitStream)
   }

   def enc_Int_PositiveInteger_ConstSize_8(intVal: ULong): Unit = {
      bitStream.appendByte0(intVal.toByte)
      CHECK_BIT_STREAM(bitStream)
   }

   def enc_Int_PositiveInteger_ConstSize_big_endian_B(intVal: ULong, size: Int): Unit = {
      val tmp: ULong = intVal
      var mask: ULong = 0xFF
      mask <<= (size - 1) * 8

      var i: Int = 0
      while i < size do
         val ByteToEncode: Byte = ((tmp & mask) >>> ((size - i - 1) * 8)).toByte
         bitStream.appendByte0(ByteToEncode)
         mask >>>= 8
         i += 1

      CHECK_BIT_STREAM(bitStream)
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
         val ByteToEncode: Byte = tmp.toByte
         bitStream.appendByte0(ByteToEncode)
         tmp >>>= 8
         i += 1

      CHECK_BIT_STREAM(bitStream)
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


   def dec_Int_PositiveInteger_ConstSize(encodedSizeInBits: Int): Option[ULong] = {
      decodeNonNegativeInteger(encodedSizeInBits) match
         case None() => None()
         case Some(ul) => Some(ul)
   }


   def dec_Int_PositiveInteger_ConstSize_8(): Option[ULong] = {
      bitStream.readByte() match
         case None() => None()
         case Some(ub) => Some(ub & 0xFF)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_N(SizeInBytes: Int): Option[ULong] = {
      var ret: ULong = 0

      var i: Int = 0
      while i < SizeInBytes do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) =>
               ret <<= 8
               ret |= (ub & 0xFF)
         i += 1

      Some(ret)
   }

   // TODO remove those and call dec_Int_PositiveInteger_ConstSize_big_endian_N directly
   def dec_Int_PositiveInteger_ConstSize_big_endian_16(): Option[ULong] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_N(NO_OF_BYTES_IN_JVM_SHORT)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_32(): Option[ULong] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_N(NO_OF_BYTES_IN_JVM_INT)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_64(): Option[ULong] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_N(NO_OF_BYTES_IN_JVM_LONG)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_N(SizeInBytes: Int): Option[ULong] = {
      var ret: ULong = 0
      var tmp: ULong = 0

      var i: Int = 0
      while i < SizeInBytes do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) =>
               tmp = ub & 0xFF
               tmp <<= i * 8
               ret |= tmp
         i += 1

      Some(ret)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_16(): Option[ULong] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_N(2)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_32(): Option[ULong] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_N(4)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_64(): Option[ULong] = {
      val ret = dec_Int_PositiveInteger_ConstSize_little_endian_N(NO_OF_BYTES_IN_JVM_LONG)
      bitStream.currentByte += (8 - NO_OF_BYTES_IN_JVM_LONG)
      ret
   }


   def encode_UnsignedInteger(v: ULong, nBytes: Byte): Unit = {
      val MAX_BYTE_MASK = 0xFF00000000000000L
      assert(nBytes <= 8)

      var vv: ULong = v << (NO_OF_BYTES_IN_JVM_LONG * 8 - nBytes * 8)

      var i: Int = 0
      while i < nBytes do
         val ByteToEncode: Byte = ((vv & MAX_BYTE_MASK) >>> ((NO_OF_BYTES_IN_JVM_LONG - 1) * 8)).toByte
         bitStream.appendByte0(ByteToEncode)
         vv <<= 8
         i += 1
   }


   def enc_Int_PositiveInteger_VarSize_LengthEmbedded(intVal: ULong): Unit = {
      val nBytes: Byte = GetLengthInBytesOfUInt(intVal).toByte

      /* encode length */
      bitStream.appendByte0(nBytes)
      /* Encode integer data*/
      encode_UnsignedInteger(intVal, nBytes)

      CHECK_BIT_STREAM(bitStream)
   }

   def dec_Int_PositiveInteger_VarSize_LengthEmbedded(): Option[ULong] = {
      var v: ULong = 0

      bitStream.readByte() match
         case None() => return None()
         case Some(nBytes) =>
            var i: Int = 0
            while i < nBytes do
               bitStream.readByte() match
                  case None() => return None()
                  case Some(ub) =>
                     v = (v << 8) | (ub & 0xFF)
               i += 1

      Some(v)
   }


   def enc_Int_TwosComplement_ConstSize(intVal: Long, encodedSizeInBits: Int): Unit = {
      if intVal >= 0 then
         bitStream.appendNBitZero(encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger(intVal))
         encodeNonNegativeInteger(intVal)

      else
         bitStream.appendNBitOne(encodedSizeInBits - GetNumberOfBitsForNonNegativeInteger(-intVal - 1))
         encodeNonNegativeIntegerNeg(-intVal - 1, true)

      CHECK_BIT_STREAM(bitStream)
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

   def dec_Int_TwosComplement_ConstSize(encodedSizeInBits: Int): Option[Long] = {
      val valIsNegative: Boolean = bitStream.peekBit()
      val nBytes: Int = encodedSizeInBits / 8
      val rstBits: Int = encodedSizeInBits % 8

      var pIntVal: Long = if valIsNegative then Long.MaxValue else 0

      var i: Int = 0
      while i < nBytes do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) =>
               pIntVal = (pIntVal << 8) | (ub & 0xFF)
         i += 1

      if rstBits > 0 then
         bitStream.readPartialByte(rstBits.toByte) match
            case None() => return None()
            case Some(ub) =>
               pIntVal = (pIntVal << rstBits) | (ub & 0xFF)

      Some(pIntVal)
   }


   def dec_Int_TwosComplement_ConstSize_8(): Option[Long] = {
      dec_Int_PositiveInteger_ConstSize_8() match
         case None() => None()
         case Some(ul) => Some(uint2int(ul, 1))
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_16(): Option[Long] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_16() match
         case None() => None()
         case Some(ul) => Some(uint2int(ul, 2))
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_32(): Option[Long] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_32() match
         case None() => None()
         case Some(ul) => Some(uint2int(ul, 4))
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_64(): Option[Long] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_64() match
         case None() => None()
         case Some(ul) => Some(uint2int(ul, NO_OF_BYTES_IN_JVM_LONG))
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_16(): Option[Long] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_16() match
         case None() => None()
         case Some(ul) => Some(uint2int(ul, 2))
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_32(): Option[Long] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_32() match
         case None() => None()
         case Some(ul) => Some(uint2int(ul, 4))
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_64(): Option[Long] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_64() match
         case None() => None()
         case Some(ul) => Some(uint2int(ul, NO_OF_BYTES_IN_JVM_LONG))
   }


   def enc_Int_TwosComplement_VarSize_LengthEmbedded(intVal: Long): Unit = {
      val nBytes: Byte = GetLengthInBytesOfSInt(intVal).toByte

      /* encode length */
      bitStream.appendByte0(nBytes)
      /* Encode integer data*/
      encode_UnsignedInteger(int2uint(intVal), nBytes)

      CHECK_BIT_STREAM(bitStream)
   }


   def dec_Int_TwosComplement_VarSize_LengthEmbedded(): Option[Long] = {
      var v: ULong = 0
      var isNegative: Boolean = false

      bitStream.readByte() match
         case None() => None()
         case Some(nBytes) =>
            var i: Int = 0
            while i < nBytes do
               bitStream.readByte() match
                  case None() => return None()
                  case Some(ub) =>
                     if i == 0 && (ub & 0x80) > 0 then
                        v = Long.MaxValue
                        isNegative = true

                     v = (v << 8) | (ub & 0xFF)
                     i += 1

            if isNegative then
               Some(-(~v) - 1)
            else
               Some(v)
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
         bitStream.appendPartialByte(tmp(i).toByte, 4, false)
         i -= 1

      CHECK_BIT_STREAM(bitStream)
   }


   def dec_Int_BCD_ConstSize(encodedSizeInNibbles: Int): Option[ULong] = {
      var ret: ULong = 0

      var encodedSizeInNibblesVar = encodedSizeInNibbles
      while encodedSizeInNibblesVar > 0 do
         bitStream.readPartialByte(4) match
            case None() => return None()
            case Some(digit) =>
               ret *= 10
               ret += digit
         encodedSizeInNibblesVar -= 1

      Some(ret)
   }


   def enc_Int_BCD_VarSize_LengthEmbedded(intVal: ULong): Unit = {
      val nNibbles: Int = get_Int_Size_BCD(intVal)
      /* encode length */
      bitStream.appendByte0(nNibbles.toByte)

      /* Encode Number */
      enc_Int_BCD_ConstSize(intVal, nNibbles)

      CHECK_BIT_STREAM(bitStream)
   }


   def dec_Int_BCD_VarSize_LengthEmbedded(): Option[ULong] = {
      bitStream.readByte() match
         case None() => None()
         case Some(nNibbles) => dec_Int_BCD_ConstSize(nNibbles)
   }


   //encoding puts an 'F' at the end
   def enc_Int_BCD_VarSize_NullTerminated(intVal: ULong): Unit = {

      val nNibbles: Int = get_Int_Size_BCD(intVal)

      /* Encode Number */
      enc_Int_BCD_ConstSize(intVal, nNibbles)

      bitStream.appendPartialByte(0xF, 4, false)

      CHECK_BIT_STREAM(bitStream)
   }

   def dec_Int_BCD_VarSize_NullTerminated(): Option[ULong] = {
      var ret: ULong = 0

      while true do
         bitStream.readPartialByte(4) match
            case None() => return None()
            case Some(digit) =>
               if (digit > 9)
                  return Some(ret)

               ret *= 10
               ret += digit

      Some(ret)
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
         bitStream.appendByte0((tmp(i) + '0').toByte)
         i -= 1

      CHECK_BIT_STREAM(bitStream)
   }


   def enc_SInt_ASCII_ConstSize(intVal: Long, encodedSizeInBytes: Int): Unit = {
      val absIntVal: ULong = if intVal >= 0 then intVal else -intVal

      /* encode sign */
      bitStream.appendByte0(if intVal >= 0 then '+' else '-')

      enc_UInt_ASCII_ConstSize(absIntVal, encodedSizeInBytes - 1)
   }

   def dec_UInt_ASCII_ConstSize(encodedSizeInBytes: Int): Option[ULong] = {
      var encodedSizeInBytesVar = encodedSizeInBytes
      var ret: ULong = 0

      while encodedSizeInBytesVar > 0 do
         bitStream.readByte() match
            case None() => return None()
            case Some(digit) =>
               assert(digit >= '0' && digit <= '9')

               ret *= 10
               ret += (digit.toInt - '0').toByte

         encodedSizeInBytesVar -= 1

      Some(ret)
   }

   def dec_SInt_ASCII_ConstSize(encodedSizeInBytes: Int): Option[Long] = {
      bitStream.readByte() match
         case None() => None()
         case Some(digit) =>
            var sign: Int = 1
            if digit == '+' then
               sign = 1
            else if digit == '-' then
               sign = -1
            else
               assert(false)

            dec_UInt_ASCII_ConstSize(encodedSizeInBytes - 1) match
               case None() => None()
               case Some(ul) => Some(sign * ul)
   }


   def getIntegerDigits(intVal: ULong): (Array[Byte], Byte) = {
      var intVar = intVal
      val digitsArray100: Array[Byte] = Array.fill(100)(0)
      val reversedDigitsArray: Array[Byte] = Array.fill(100)(0)
      var totalDigits: Byte = 0


      if intVar > 0 then
         while intVar > 0 && totalDigits < 100 do
            reversedDigitsArray(totalDigits) = ('0' + (intVar % 10)).toByte
            totalDigits = (totalDigits + 1).toByte
            intVar /= 10

         var i: Int = totalDigits - 1
         while i >= 0 do
            digitsArray100(totalDigits - 1 - i) = reversedDigitsArray(i)
            i -= 1

      else
         digitsArray100(0) = '0'
         totalDigits = 1

      (digitsArray100, totalDigits)
   }


   def enc_SInt_ASCII_VarSize_LengthEmbedded(intVal: Long): Unit = {
      val absIntVal: ULong = if intVal >= 0 then intVal else -intVal
      val (digitsArray100, nChars) = getIntegerDigits(absIntVal)

      /* encode length, plus 1 for sign */
      bitStream.appendByte0((nChars + 1).toByte)

      /* encode sign */
      bitStream.appendByte0(if intVal >= 0 then '+' else '-')

      /* encode digits */
      var i: Int = 0
      while i < 100 && digitsArray100(i) != 0x0 do
         bitStream.appendByte0(digitsArray100(i))
         i += 1

      CHECK_BIT_STREAM(bitStream)
   }

   def enc_UInt_ASCII_VarSize_LengthEmbedded(intVal: ULong): Unit = {
      val (digitsArray100, nChars) = getIntegerDigits(intVal)

      /* encode length */
      bitStream.appendByte0(nChars)
      /* encode digits */
      var i: Int = 0
      while i < 100 && digitsArray100(i) != 0x0 do
         bitStream.appendByte0(digitsArray100(i))
         i += 1

      CHECK_BIT_STREAM(bitStream)
   }


   def dec_UInt_ASCII_VarSize_LengthEmbedded(): Option[ULong] = {
      bitStream.readByte() match
         case None() => None()
         case Some(nChars) => dec_UInt_ASCII_ConstSize(nChars)
   }

   def dec_SInt_ASCII_VarSize_LengthEmbedded(): Option[Long] = {
      bitStream.readByte() match
         case None() => None()
         case Some(nChars) => dec_SInt_ASCII_ConstSize(nChars)
   }


   def enc_UInt_ASCII_VarSize_NullTerminated(intVal: ULong, null_characters: Array[Byte], null_characters_size: Int): Unit = {
      val (digitsArray100, nChars) = getIntegerDigits(intVal)

      var i: Int = 0 // TODO: size_t?
      while i < 100 && digitsArray100(i) != 0x0 do
         bitStream.appendByte0(digitsArray100(i))
         i += 1

      i = 0
      while i < null_characters_size do
         bitStream.appendByte0(null_characters(i))
         i += 1

      CHECK_BIT_STREAM(bitStream)
   }

   def enc_SInt_ASCII_VarSize_NullTerminated(intVal: Long, null_characters: Array[Byte], null_characters_size: Int): Unit = {
      val absValue: ULong = if intVal >= 0 then intVal else -intVal
      bitStream.appendByte0(if intVal >= 0 then '+' else '-')

      enc_UInt_ASCII_VarSize_NullTerminated(absValue, null_characters, null_characters_size)
   }

   def dec_UInt_ASCII_VarSize_NullTerminated(null_characters: Array[Byte], null_characters_size: Int): Option[ULong] = {
      var digit: Byte = 0
      var ret: ULong = 0
      val tmp: Array[Byte] = Array.fill(10)(0)

      val sz: Int = if null_characters_size < 10 then null_characters_size else 10

      //read null_character_size characters into the tmp buffer
      var j: Int = 0
      while j < null_characters_size do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => tmp(j) = ub
         j += 1

      var i: Long = 0
      while !null_characters.sameElements(tmp) do
         digit = tmp(0)
         i += 1

         j = 0
         while j < null_characters_size - 1 do
            tmp(j) = tmp(j + 1)
            j += 1

         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => tmp(null_characters_size - 1) = ub

         digit = (digit - '0').toByte

         ret *= 10
         ret += digit

      Some(ret)
   }


   def dec_SInt_ASCII_VarSize_NullTerminated(null_characters: Array[Byte], null_characters_size: Int): Option[Long] = {
      var isNegative: Boolean = false

      bitStream.readByte() match
         case None() => None()
         case Some(digit) =>
            assert(digit == '-' || digit == '+')
            if digit == '-' then
               isNegative = true

            dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size) match
               case None() => None()
               case Some(ul) => Some(if isNegative then -ul else ul)
   }


   /* Boolean Decode */
   // TODO move to codec?
   def BitStream_ReadBitPattern(patternToRead: Array[Byte], nBitsToRead: Int): Option[Boolean] = {
      val nBytesToRead: Int = nBitsToRead / 8
      val nRemainingBitsToRead: Int = nBitsToRead % 8

      var pBoolValue: Boolean = true
      var i: Int = 0
      while i < nBytesToRead do
         bitStream.readByte() match
            case None() => return None()
            case Some(curByte) =>
               if curByte != patternToRead(i) then
                  pBoolValue = false
         i += 1

      if nRemainingBitsToRead > 0 then
         bitStream.readPartialByte(nRemainingBitsToRead.toByte) match
            case None() => return None()
            case Some(curByte) =>
               if curByte != ((patternToRead(nBytesToRead) & 0xFF) >>> (8 - nRemainingBitsToRead)) then
                  pBoolValue = false

      Some(pBoolValue)
   }

   // TODO move to codec?
   def BitStream_ReadBitPattern_ignore_value(nBitsToRead: Int): Either[ErrorCode, Int] = {
      val nBytesToRead: Int = nBitsToRead / 8
      val nRemainingBitsToRead: Int = nBitsToRead % 8

      var i: Int = 0
      while i < nBytesToRead do
         bitStream.readByte() match
            case None() => return Left(FAILED_READ_ERR_CODE)
            case Some(_) => i += 1

      if nRemainingBitsToRead > 0 then
         if bitStream.readPartialByte(nRemainingBitsToRead.toByte).isEmpty then
            return Left(FAILED_READ_ERR_CODE)

      Right(0)
   }


   /*Real encoding functions*/
   def enc_Real_IEEE754_32_big_endian(realValue: Float): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(4).putFloat(realValue).array

      var i: Int = 0
      while i < 4 do
         bitStream.appendByte0(b(i))
         i += 1
   }

   def dec_Real_IEEE754_32_big_endian(): Option[Double] = {
      val b: Array[Byte] = Array.fill(4)(0)
      var i: Int = 0
      while i < 4 do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => b(i) = ub
         i += 1

      val dat1 = BigInt(b).toInt
      Some(java.lang.Float.intBitsToFloat(dat1).toDouble)
   }

   def dec_Real_IEEE754_32_big_endian_fp32(): Option[Float] = {
      val b: Array[Byte] = Array.fill(4)(0)
      var i: Int = 0
      while i < 4 do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => b(i) = ub
         i += 1

      val dat1 = BigInt(b).toInt
      Some(java.lang.Float.intBitsToFloat(dat1))
   }


   def enc_Real_IEEE754_64_big_endian(realValue: Double): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(8).putDouble(realValue).array

      var i: Int = 0
      while i < 8 do
         bitStream.appendByte0(b(i))
         i += 1
   }

   def dec_Real_IEEE754_64_big_endian(): Option[Double] = {
      val b: Array[Byte] = Array.fill(8)(0)
      var i: Int = 0
      while i < 8 do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => b(i) = ub
         i += 1

      val dat1 = BigInt(b).toLong
      Some(java.lang.Double.longBitsToDouble(dat1))
   }


   def enc_Real_IEEE754_32_little_endian(realValue: Double): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(4).putFloat(realValue.toFloat).array

      var i: Int = 3
      while i >= 0 do
         bitStream.appendByte0(b(i))
         i -= 1
   }

   def dec_Real_IEEE754_32_little_endian(): Option[Double] = {
      dec_Real_IEEE754_32_little_endian_fp32() match
         case None() => None()
         case Some(f) => Some(f.toDouble)
   }

   def dec_Real_IEEE754_32_little_endian_fp32(): Option[Float] = {
      val b: Array[Byte] = Array.fill(4)(0)
      var i: Int = 3
      while i >= 0 do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => b(i) = ub
               i -= 1

      val dat1 = BigInt(b).toInt
      Some(java.lang.Float.intBitsToFloat(dat1))
   }

   def enc_Real_IEEE754_64_little_endian(realValue: Double): Unit = {
      val b: Array[Byte] = java.nio.ByteBuffer.allocate(8).putDouble(realValue).array

      var i: Int = 7
      while i >= 0 do
         bitStream.appendByte0(b(i))
         i -= 1
   }

   def dec_Real_IEEE754_64_little_endian(): Option[Double] = {
      val b: Array[Byte] = Array.fill(8)(0)
      var i: Int = 7
      while i >= 0 do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => b(i) = ub
               i -= 1

      val dat1 = BigInt(b).toLong
      Some(java.lang.Double.longBitsToDouble(dat1))
   }


   /* String functions*/
   def enc_String_Ascii_FixSize(max: Long, strVal: Array[ASCIIChar]): Unit = {
      var i: Long = 0
      while i < max do
         bitStream.appendByte(strVal(i.toInt), false)
         i += 1
   }

   def enc_String_Ascii_private(max: Long, strVal: Array[ASCIIChar]): Long = {
      var i: Long = 0
      while (i < max) && (strVal(i.toInt) != '\u0000') do
         bitStream.appendByte(strVal(i.toInt), false)
         i += 1

      i
   }

   def enc_String_Ascii_Null_Teminated(max: Long, null_character: Byte, strVal: Array[ASCIIChar]): Unit = {
      enc_String_Ascii_private(max, strVal)
      bitStream.appendByte(null_character.toByte, false)
   }

   def enc_String_Ascii_Null_Teminated_mult(max: Long, null_character: Array[Byte], null_character_size: Int, strVal: Array[ASCIIChar]): Unit = {
      enc_String_Ascii_private(max, strVal)
      var i: Int = 0
      while i < null_character_size do
         bitStream.appendByte(null_character(i), false)
         i += 1
   }


   def enc_String_Ascii_External_Field_Determinant(max: Long, strVal: Array[ASCIIChar]): Unit = {
      enc_String_Ascii_private(max, strVal)
   }

   def enc_String_Ascii_Internal_Field_Determinant(max: Long, min: Long, strVal: Array[ASCIIChar]): Unit = {
      val strLen: Int = strVal.length
      encodeConstraintWholeNumber(if strLen <= max then strLen else max, min, max)
      enc_String_Ascii_private(max, strVal)
   }

   def enc_String_CharIndex_FixSize(max: Long, allowedCharSet: Array[Byte], strVal: Array[ASCIIChar]): Unit = {
      var i: Int = 0
      while i < max do
         val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
         encodeConstraintWholeNumber(charIndex, 0, allowedCharSet.length - 1)
         i += 1
   }

   def enc_String_CharIndex_private(max: Long, allowedCharSet: Array[Byte], strVal: Array[ASCIIChar]): Long = {
      var i: Int = 0
      while (i < max) && (strVal(i) != '\u0000') do
         val charIndex: Int = GetCharIndex(strVal(i), allowedCharSet)
         encodeConstraintWholeNumber(charIndex, 0, allowedCharSet.length - 1)
         i += 1

      i
   }


   def enc_String_CharIndex_External_Field_Determinant(max: Long, allowedCharSet: Array[Byte], strVal: Array[ASCIIChar]): Unit = {
      enc_String_CharIndex_private(max, allowedCharSet, strVal)
   }

   def enc_String_CharIndex_Internal_Field_Determinant(max: Long, allowedCharSet: Array[Byte], min: Long, strVal: Array[ASCIIChar]): Unit = {
      val strLen: Int = strVal.length
      encodeConstraintWholeNumber(if strLen <= max then strLen else max, min, max)
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
      encodeConstraintWholeNumber(if strLen <= max then strLen else max, min, max)
      enc_String_CharIndex_private(max, allowedCharSet, strVal)
   }


   def dec_String_Ascii_private(max: Long, charactersToDecode: Long): Option[Array[ASCIIChar]] = {
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0)
      var i: Int = 0
      while i < charactersToDecode do
         bitStream.readByte() match
            case None() => return None()
            case Some(decodedCharacter) =>
               strVal(i) = decodedCharacter
         i += 1
      Some(strVal)
   }


   def dec_String_Ascii_FixSize(max: Long): Option[Array[ASCIIChar]] = {
      dec_String_Ascii_private(max, max)
   }

   def dec_String_Ascii_Null_Teminated(max: Long, null_character: ASCIIChar): Option[Array[ASCIIChar]] = {
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0)
      var i: Int = 0
      while i <= max do
         bitStream.readByte() match
            case None() => return None()
            case Some(decodedCharacter) =>
               if decodedCharacter != null_character then
                  strVal(i) = decodedCharacter
                  i += 1
               else
                  strVal(i) = 0x0
                  return Some(strVal)

      None()

   }

   def dec_String_Ascii_Null_Teminated_mult(max: Long, null_character: Array[ASCIIChar], null_character_size: Int): Option[Array[ASCIIChar]] = {
      val sz: Int = if null_character_size < 10 then null_character_size else 10
      val tmp: Array[Byte] = Array.fill(10)(0)
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0)
      //read null_character_size characters into the tmp buffer
      var j: Int = 0
      while j < null_character_size do
         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => tmp(j) = ub
         j += 1


      var i: Int = 0
      while i <= max && !null_character.sameElements(tmp) do
         strVal(i) = tmp(0)
         i += 1
         j = 0
         while j < null_character_size - 1 do
            tmp(j) = tmp(j + 1)
            j += 1

         bitStream.readByte() match
            case None() => return None()
            case Some(ub) => tmp(null_character_size - 1) = ub

      strVal(i) = 0x0

      if !null_character.sameElements(tmp) then
         return None()

      Some(strVal)
   }


   def dec_String_Ascii_External_Field_Determinant(max: Long, extSizeDeterminatFld: Long): Option[Array[ASCIIChar]] = {
      dec_String_Ascii_private(max, if extSizeDeterminatFld <= max then extSizeDeterminatFld else max)
   }

   def dec_String_Ascii_Internal_Field_Determinant(max: Long, min: Long): Option[Array[ASCIIChar]] = {
      BitStream_DecodeConstraintWholeNumber(min, max) match
         case None() => None()
         case Some(nCount) =>
            dec_String_Ascii_private(max, if nCount <= max then nCount else max)
   }

   def dec_String_CharIndex_private(max: Long, charactersToDecode: Long, allowedCharSet: Array[Byte]): Option[Array[ASCIIChar]] = {
      val strVal: Array[ASCIIChar] = Array.fill(max.toInt + 1)(0)
      var i: Int = 0
      while i < charactersToDecode do
         BitStream_DecodeConstraintWholeNumber(0, allowedCharSet.length - 1) match
            case None() => return None()
            case Some(charIndex) =>
               strVal(i) = allowedCharSet(charIndex.toInt)
         i += 1

      Some(strVal)
   }

   def dec_String_CharIndex_FixSize(max: Long, allowedCharSet: Array[ASCIIChar]): Option[Array[ASCIIChar]] = {
      dec_String_CharIndex_private(max, max, allowedCharSet)
   }

   def dec_String_CharIndex_External_Field_Determinant(max: Long, allowedCharSet: Array[ASCIIChar], extSizeDeterminatFld: Long): Option[Array[ASCIIChar]] = {
      dec_String_CharIndex_private(max, if extSizeDeterminatFld <= max then extSizeDeterminatFld else max, allowedCharSet)
   }


   def dec_String_CharIndex_Internal_Field_Determinant(max: Long, allowedCharSet: Array[ASCIIChar], min: Long): Option[Array[ASCIIChar]] = {
      BitStream_DecodeConstraintWholeNumber(min, max) match
         case None() => None()
         case Some(nCount) =>
            dec_String_CharIndex_private(max, if nCount <= max then nCount else max, allowedCharSet)
   }


   def dec_IA5String_CharIndex_External_Field_Determinant(max: Long, extSizeDeterminatFld: Long): Option[Array[ASCIIChar]] = {
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

   def dec_IA5String_CharIndex_Internal_Field_Determinant(max: Long, min: Long): Option[Array[ASCIIChar]] = {
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
      BitStream_DecodeConstraintWholeNumber(min, max) match
         case None() => None()
         case Some(nCount) =>
            dec_String_CharIndex_private(max, if nCount <= max then nCount else max, allowedCharSet)
   }


   /* Length Determinant functions*/
   def enc_Length(lengthValue: ULong, lengthSizeInBits: Int): Unit = {
      /* encode length */
      enc_Int_PositiveInteger_ConstSize(lengthValue, lengthSizeInBits)
   }

   def dec_Length(lengthSizeInBits: Int): Option[ULong] = {
      dec_Int_PositiveInteger_ConstSize(lengthSizeInBits)
   }

   def milbus_encode(v: Long): Long = {
      if v == 32 then 0 else v
   }

   def milbus_decode(v: Long): Long = {
      if v == 0 then 32 else v
   }

   def dec_Int_PositiveInteger_ConstSizeUInt8(encodedSizeInBits: Int): Option[UByte] = {
      dec_Int_PositiveInteger_ConstSize(encodedSizeInBits) match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_PositiveInteger_ConstSizeUInt16(encodedSizeInBits: Int): Option[UShort] = {
      dec_Int_PositiveInteger_ConstSize(encodedSizeInBits) match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_PositiveInteger_ConstSizeUInt32(encodedSizeInBits: Int): Option[UInt] = {
      dec_Int_PositiveInteger_ConstSize(encodedSizeInBits) match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_PositiveInteger_ConstSize_8UInt8(): Option[UByte] = {
      dec_Int_PositiveInteger_ConstSize_8() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_16UInt16(): Option[UShort] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_16() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_16UInt8(): Option[UByte] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_16() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_32UInt32(): Option[UInt] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }


   def dec_Int_PositiveInteger_ConstSize_big_endian_32UInt16(): Option[UShort] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_32UInt8(): Option[UByte] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_64UInt32(): Option[UInt] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_64UInt16(): Option[UShort] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_PositiveInteger_ConstSize_big_endian_64UInt8(): Option[UByte] = {
      dec_Int_PositiveInteger_ConstSize_big_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_16UInt16(): Option[UShort] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_16() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_16UInt8(): Option[UByte] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_16() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_32UInt32(): Option[UInt] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_32UInt16(): Option[UShort] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_32UInt8(): Option[UByte] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_64UInt32(): Option[UInt] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_64UInt16(): Option[UShort] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_PositiveInteger_ConstSize_little_endian_64UInt8(): Option[UByte] = {
      dec_Int_PositiveInteger_ConstSize_little_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt8(): Option[UByte] = {
      dec_Int_PositiveInteger_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt16(): Option[UShort] = {
      dec_Int_PositiveInteger_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_PositiveInteger_VarSize_LengthEmbeddedUInt32(): Option[UInt] = {
      dec_Int_PositiveInteger_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_TwosComplement_ConstSizeInt8(encodedSizeInBits: Int): Option[Byte] = {
      dec_Int_TwosComplement_ConstSize(encodedSizeInBits) match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_TwosComplement_ConstSizeInt16(encodedSizeInBits: Int): Option[Short] = {
      dec_Int_TwosComplement_ConstSize(encodedSizeInBits) match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_TwosComplement_ConstSizeInt32(encodedSizeInBits: Int): Option[Int] = {
      dec_Int_TwosComplement_ConstSize(encodedSizeInBits) match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_TwosComplement_ConstSize_8Int8(): Option[Byte] = {
      dec_Int_TwosComplement_ConstSize_8() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_16Int16(): Option[Short] = {
      dec_Int_TwosComplement_ConstSize_big_endian_16() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_16Int8(): Option[Byte] = {
      dec_Int_TwosComplement_ConstSize_big_endian_16() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_32Int32(): Option[Int] = {
      dec_Int_TwosComplement_ConstSize_big_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_32Int16(): Option[Short] = {
      dec_Int_TwosComplement_ConstSize_big_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_32Int8(): Option[Byte] = {
      dec_Int_TwosComplement_ConstSize_big_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }


   def dec_Int_TwosComplement_ConstSize_big_endian_64Int32(): Option[Int] = {
      dec_Int_TwosComplement_ConstSize_big_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_64Int16(): Option[Short] = {
      dec_Int_TwosComplement_ConstSize_big_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_TwosComplement_ConstSize_big_endian_64Int8(): Option[Byte] = {
      dec_Int_TwosComplement_ConstSize_big_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_16Int16(): Option[Short] = {
      dec_Int_TwosComplement_ConstSize_little_endian_16() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_16Int8(): Option[Byte] = {
      dec_Int_TwosComplement_ConstSize_little_endian_16() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_32Int32(): Option[Int] = {
      dec_Int_TwosComplement_ConstSize_little_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_32Int16(): Option[Short] = {
      dec_Int_TwosComplement_ConstSize_little_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_32Int8(): Option[Byte] = {
      dec_Int_TwosComplement_ConstSize_little_endian_32() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_64Int32(): Option[Int] = {
      dec_Int_TwosComplement_ConstSize_little_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_64Int16(): Option[Short] = {
      dec_Int_TwosComplement_ConstSize_little_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_TwosComplement_ConstSize_little_endian_64Int8(): Option[Byte] = {
      dec_Int_TwosComplement_ConstSize_little_endian_64() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_TwosComplement_VarSize_LengthEmbeddedInt8(): Option[Byte] = {
      dec_Int_TwosComplement_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_TwosComplement_VarSize_LengthEmbeddedInt16(): Option[Short] = {
      dec_Int_TwosComplement_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_TwosComplement_VarSize_LengthEmbeddedInt32(): Option[Int] = {
      dec_Int_TwosComplement_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_BCD_ConstSizeUInt8(encodedSizeInNibbles: Int): Option[UByte] = {
      dec_Int_BCD_ConstSize(encodedSizeInNibbles) match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_BCD_ConstSizeUInt16(encodedSizeInNibbles: Int): Option[UShort] = {
      dec_Int_BCD_ConstSize(encodedSizeInNibbles) match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_BCD_ConstSizeUInt32(encodedSizeInNibbles: Int): Option[UInt] = {
      dec_Int_BCD_ConstSize(encodedSizeInNibbles) match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_BCD_VarSize_LengthEmbeddedUInt8(): Option[UByte] = {
      dec_Int_BCD_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_BCD_VarSize_LengthEmbeddedUInt16(): Option[UShort] = {
      dec_Int_BCD_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_BCD_VarSize_LengthEmbeddedUInt32(): Option[UInt] = {
      dec_Int_BCD_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_Int_BCD_VarSize_NullTerminatedUInt8(): Option[UByte] = {
      dec_Int_BCD_VarSize_NullTerminated() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_Int_BCD_VarSize_NullTerminatedUInt16(): Option[UShort] = {
      dec_Int_BCD_VarSize_NullTerminated() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_Int_BCD_VarSize_NullTerminatedUInt32(): Option[UInt] = {
      dec_Int_BCD_VarSize_NullTerminated() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_SInt_ASCII_ConstSizeInt8(encodedSizeInBytes: Int): Option[Byte] = {
      dec_SInt_ASCII_ConstSize(encodedSizeInBytes) match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_SInt_ASCII_ConstSizeInt16(encodedSizeInBytes: Int): Option[Short] = {
      dec_SInt_ASCII_ConstSize(encodedSizeInBytes) match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_SInt_ASCII_ConstSizeInt32(encodedSizeInBytes: Int): Option[Int] = {
      dec_SInt_ASCII_ConstSize(encodedSizeInBytes) match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_SInt_ASCII_VarSize_LengthEmbeddedInt8(): Option[Byte] = {
      dec_SInt_ASCII_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_SInt_ASCII_VarSize_LengthEmbeddedInt16(): Option[Short] = {
      dec_SInt_ASCII_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_SInt_ASCII_VarSize_LengthEmbeddedInt32(): Option[Int] = {
      dec_SInt_ASCII_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_SInt_ASCII_VarSize_NullTerminatedInt8(null_characters: Array[Byte], null_characters_size: Int): Option[Byte] = {
      dec_SInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size) match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_SInt_ASCII_VarSize_NullTerminatedInt16(null_characters: Array[Byte], null_characters_size: Int): Option[Short] = {
      dec_SInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size) match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_SInt_ASCII_VarSize_NullTerminatedInt32(null_characters: Array[Byte], null_characters_size: Int): Option[Int] = {
      dec_SInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size) match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_UInt_ASCII_ConstSizeUInt8(encodedSizeInBytes: Int): Option[UByte] = {
      dec_UInt_ASCII_ConstSize(encodedSizeInBytes) match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_UInt_ASCII_ConstSizeUInt16(encodedSizeInBytes: Int): Option[UShort] = {
      dec_UInt_ASCII_ConstSize(encodedSizeInBytes) match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_UInt_ASCII_ConstSizeUInt32(encodedSizeInBytes: Int): Option[UInt] = {
      dec_UInt_ASCII_ConstSize(encodedSizeInBytes) match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_UInt_ASCII_VarSize_LengthEmbeddedUInt8(): Option[UByte] = {
      dec_UInt_ASCII_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_UInt_ASCII_VarSize_LengthEmbeddedUInt16(): Option[UShort] = {
      dec_UInt_ASCII_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_UInt_ASCII_VarSize_LengthEmbeddedUInt32(): Option[UInt] = {
      dec_UInt_ASCII_VarSize_LengthEmbedded() match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }

   def dec_UInt_ASCII_VarSize_NullTerminatedUInt8(null_characters: Array[Byte], null_characters_size: Int): Option[UByte] = {
      dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size) match
         case None() => None()
         case Some(v) => Some(v.toByte)
   }

   def dec_UInt_ASCII_VarSize_NullTerminatedUInt16(null_characters: Array[Byte], null_characters_size: Int): Option[UShort] = {
      dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size) match
         case None() => None()
         case Some(v) => Some(v.toShort)
   }

   def dec_UInt_ASCII_VarSize_NullTerminatedUInt32(null_characters: Array[Byte], null_characters_size: Int): Option[UInt] = {
      dec_UInt_ASCII_VarSize_NullTerminated(null_characters, null_characters_size) match
         case None() => None()
         case Some(v) => Some(v.toInt)
   }
}
