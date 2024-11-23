package asn1scala

import asn1scala.BitStream.bitIndex
import stainless.*
import stainless.lang.{None => None, ghost => ghostExpr, Option => Option, _}
import stainless.collection.*

def byteToBinaryString(b: Byte): String = 
  val s = (0 to 7).map(i => if ((b & (1 << i)) != 0) "1" else "0").mkString
  s

def bitStreamToString(b: BitStream): String = 
  val bi = BitStream.bitIndex(b.buf.length, b.currentByte, b.currentBit)
  val res = s"Buf: ${b.buf.toList.map(byteToBinaryString).mkString(" ")}\n"
 
  res + s"BuffLength: ${b.buf.length}\ncurrentByte: ${b.currentByte}\ncurrentBit: ${b.currentBit}\nBitIndex: ${bi}\n"
@main def Main = 
  // val b1 = BitStream(Array[Byte](0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00), 0, 0)
  // println(s"bitStream before appendBit: \n${bitStreamToString(b1)}")
  // b1.appendBit(true)
  // println(s"bitStream after appendBit: \n${bitStreamToString(b1)}")
  // b1.appendBit(true)
  // println(s"bitStream after appendBit: \n${bitStreamToString(b1)}")

  // // Test lemma

  // val arr = new Array[Byte](536870912)
  // val bs = BitStream(arr, 0, 1)
  // val nBits = 4672978248L
  // val expected = false
  // val from = 4639423817L

  // BitStream.checkBitsLoopPrefixLemma(bs, nBits, expected, from)


  // ----------------------------------------------------------------------------
  // val n = 64
  // val v = Long.MaxValue
  // println(s"bitStream before appendBits: \n${bitStreamToString(b1)}")
  // b1.appendBitsLSBFirst(v, n)
  // println(s"bitStream after appendBits: \n${bitStreamToString(b1)}")
  // b1.moveBitIndex(-n)
  // println(s"bitStream after moveBitIndex: \n${bitStreamToString(b1)}")

  // val res: Long = b1.readNBitsLSBFirst(n)
  // assert(res == v )
  // println(s"Read $n bits: ${res}\n")
  // println(s"bitStream after readNBits: \n${bitStreamToString(b1)}")

  // ----------------------------------------------------------------------------
  // val n = 8 * 5
  // val raw = Array[Byte](7, 18, 112, 76, 87)
  // val ar = UByte.fromArrayRaws(raw)
  // println(s"UByte: ${raw.map(byteToBinaryString).mkString(" ")}")
  // println(s"bitStream before appendBitsMSBFirst: \n${bitStreamToString(b1)}")
  // b1.appendBitsMSBFirst(ar, n)
  // println(s"bitStream after appendBitsMSBFirst: \n${bitStreamToString(b1)}")

  // b1.moveBitIndex(-n)
  // println(s"bitStream after moveBitIndex: \n${bitStreamToString(b1)}")

  // val res = b1.readBits(n)
  // println(s"Read $n bits: ${res.map(ub => byteToBinaryString(ub.toRaw)).mkString(" ")}\n")

  // for (resB, origB) <- res.zip(ar) do
  //   assert(resB == origB)

  // DEBBUG appendBitsMSBFirst
  // val bitStream1 = BitStream(Array(-82, -81, 2, 2), 0, 0)
  // val bitStream2 = BitStream(Array(-18, -82, 1, 1), 0, 1)
  // val base = BitStream(Array(-82, -81, 2, 2), 3, 2)
  // val thiss = BitStream(new Array[Byte](2147), 1, 0)
  // val listBits = Cons[Boolean](true, Cons[Boolean](false, Nil[Boolean]()))
  // val nBits = 2

  // println(s"bitStream1 \n${bitStreamToString(bitStream1)}")
  // println(s"bitStream2 \n${bitStreamToString(bitStream2)}")
  // println(s"read two bits from bitStream1: ${bitStream1.readBit()} ${bitStream1.readBit()}")
  // println(s"Computed ListBits on bitStream1: ${thiss.bitStreamReadBitsIntoList(bitStream1, nBits)}")
  // println(s"Computed ListBits on bitStream2: ${thiss.bitStreamReadBitsIntoList(bitStream2, nBits - 1)}")

   // DEBUG readByteArray
  val bitStream1 = BitStream(new Array(2147483586), 2147483584, 0)
  val i = 1
  val to = 2
  val thiss = bitStream1
  thiss.buf.update(2147483584, 123)
  thiss.buf.update(2147483585, 43)
  val arr = new Array[UByte](1073737727)

  println(s"bit index = ${bitIndex(thiss.buf.length, thiss.currentByte, thiss.currentBit)}")
  
  thiss.readByteArrayLoop(arr, i, to)
  
  println(s"bit index = ${bitIndex(thiss.buf.length, thiss.currentByte, thiss.currentBit)}")
  println(s"arr[0] = ${arr(0).toRaw}\narr[1] = ${arr(1).toRaw}\narr[2] = ${arr(2).toRaw}")
  




