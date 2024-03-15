package asn1scala

import stainless.*
import stainless.lang.*
import stainless.collection.*
import stainless.annotation.{wrapping => _, *}
import stainless.proof.*
import stainless.math.*
import StaticChecks.*

@pure
def arraySameElements[T](a1: Array[T], a2: Array[T]): Boolean =
  a1.length == a2.length && arrayRangesEqOffset(a1, a2, 0, a1.length, 0)

@pure
def arrayRangesEqOffset[T](a1: Array[T], a2: Array[T], fromA1: Int, toA1: Int, fromA2: Int): Boolean = {
  require(0 <= fromA1 && fromA1 <= toA1)
  require(toA1 <= a1.length)
  require(0 <= fromA2 && fromA2 <= a2.length - (toA1 - fromA1))
  decreases(toA1 - fromA1)
  if (fromA1 == toA1) true
  else a1(fromA1) == a2(fromA2) && arrayRangesEqOffset(a1, a2, fromA1 + 1, toA1, fromA2 + 1)
}

def arrayCopyOffset[T](@pure src: Array[T], dst: Array[T], fromSrc: Int, toSrc: Int, fromDst: Int): Unit = {
  require(0 <= fromSrc && fromSrc <= toSrc)
  require(toSrc <= src.length)
  require(0 <= fromDst && fromDst <= dst.length - (toSrc - fromSrc))
  decreases(toSrc - fromSrc)

  if (fromSrc < toSrc) {
    dst(fromDst) = src(fromSrc)
    arrayCopyOffset(src, dst, fromSrc + 1, toSrc, fromDst + 1)
  }
}

def arrayCopyOffsetLen[T](@pure src: Array[T], dst: Array[T], fromSrc: Int, fromDst: Int, len: Int): Unit = {
  require(0 <= len && len <= src.length && len <= dst.length)
  require(0 <= fromSrc && fromSrc <= src.length - len)
  require(0 <= fromDst && fromDst <= dst.length - len)
  arrayCopyOffset(src, dst, fromSrc, fromSrc + len, fromDst)
}

@pure
def arrayBitIndices(fromBit: Long, toBit: Long): (Int, Int, Int, Int) = {
  require(0 <= fromBit && fromBit <= toBit && toBit <= 8 * Int.MaxValue.toLong)
  val arrPrefixStart = (fromBit / 8 + (if (fromBit % 8 == 0) 0 else 1)).toInt
  val arrPrefixEnd = (toBit / 8).toInt
  val fromBitIx = (fromBit / 8).toInt
  val toBitIx = (toBit / 8).toInt
  (arrPrefixStart, arrPrefixEnd, fromBitIx, toBitIx)
}

//def copyToArray[T](src: Array[T], dst: Array[T], startInDst: Int, len: Int): Unit = {
//   require(0 <= len && len <= src.length)
//   require(0 <= startInDst && startInDst <= src.length - len)
//   require(src.length <= dst.length)
//   arrayCopyOffset(src, dst, 0, len, startInDst)
//}

def byteRangesEq(b1: Byte, b2: Byte, from: Int, to: Int): Boolean = {
  require(0 <= from && from <= to && to <= 7)
  ((b1 & MASK_C(to) & MASK_B(8 - from)) & 0xFF) == ((b2 & MASK_C(to) & MASK_B(8 - from)) & 0xFF)
}

@pure
def arrayRangesEq[T](a1: Array[T], a2: Array[T], from: Int, to: Int): Boolean = {
  require(0 <= from && from <= to)
  require(a1.length <= a2.length)
  require(to <= a1.length)
  decreases(to - from)
  if (from == to) true
  else a1(from) == a2(from) && arrayRangesEq(a1, a2, from + 1, to)
}

@pure @opaque @inlineOnce @ghost
def arrayUpdatedAtPrefixLemma[T](a: Array[T], at: Int, v: T): Unit = {
  require(0 <= at && at < a.length)

  @opaque @inlineOnce @ghost
  def rec(i: Int): Unit = {
    require(0 <= i && i <= at)
    require(arrayRangesEq(a, snapshot(a).updated(at, v), i, at))
    decreases(i)
    if (i == 0) ()
    else rec(i - 1)
  }.ensuring { _ =>
    arrayRangesEq(a, snapshot(a).updated(at, v), 0, at)
  }

  rec(at)
}.ensuring { _ =>
  arrayRangesEq(a, snapshot(a).updated(at, v), 0, at)
}

@ghost @pure @opaque @inlineOnce
def arrayRangesEqSlicedLemma[T](a1: Array[T], a2: Array[T], from: Int, to: Int, fromSlice: Int, toSlice: Int): Unit = {
  require(0 <= from && from <= to)
  require(a1.length <= a2.length)
  require(to <= a1.length)
  require(from <= fromSlice && fromSlice <= toSlice && toSlice <= to)
  require(arrayRangesEq(a1, a2, from, to))

  @opaque @inlineOnce
  def rec(i: Int): Unit = {
    require(fromSlice <= i && i <= to)
    require(arrayRangesEq(a1, a2, i, to)) // the original predicate we are unfolding
    require((i <= toSlice) ==> arrayRangesEq(a1, a2, i, toSlice)) // the resulting predicate we are folding
    decreases(i)
    if (i == fromSlice) ()
    else {
      arrayRangesEqImpliesEq(a1, a2, from, i - 1, to)
      rec(i - 1)
    }
  }.ensuring(_ => arrayRangesEq(a1, a2, fromSlice, toSlice))

  rec(to)
}.ensuring(_ => arrayRangesEq(a1, a2, fromSlice, toSlice))

@pure @opaque @inlineOnce @ghost
def arrayRangesEqImpliesEq[T](a1: Array[T], a2: Array[T], from: Int, at: Int, to: Int): Unit = {
  require(0 <= from && from <= to)
  require(a1.length <= a2.length)
  require(to <= a1.length)
  require(from <= at && at < to)
  require(arrayRangesEq(a1, a2, from, to))

  @opaque @inlineOnce @ghost
  def rec(i: Int): Unit = {
    require(from <= i && i <= at)
    require(arrayRangesEq(a1, a2, i, to))
    decreases(to - i)
    if (i == at) ()
    else rec(i + 1)
  }.ensuring { _ =>
    a1(at) == a2(at)
  }

  rec(from)
}.ensuring(_ => a1(at) == a2(at))

@pure @opaque @inlineOnce @ghost
def arrayRangesEqAppend[T](a1: Array[T], a2: Array[T], from: Int, to: Int) = {
  require(0 <= from && from <= to)
  require(a1.length <= a2.length)
  require(to < a1.length)
  require(arrayRangesEq(a1, a2, from, to))
  require(a1(to) == a2(to))

  @opaque @inlineOnce
  def rec(i: Int): Unit = {
    require(from <= i && i <= to)
    require(arrayRangesEq(a1, a2, i, to + 1))
    decreases(i)
    if (i == from) ()
    else {
      arrayRangesEqImpliesEq(a1, a2, from, i - 1, to)
      rec(i - 1)
    }
  }.ensuring { _ =>
    arrayRangesEq(a1, a2, from, to + 1)
  }

  rec(to)
}.ensuring(_ => arrayRangesEq(a1, a2, from, to + 1))

@pure @opaque @inlineOnce @ghost
def arrayRangesEqTransitive[T](a1: Array[T], a2: Array[T], a3: Array[T], from: Int, mid: Int, to: Int): Unit = {
  require(0 <= from && from <= mid && mid <= to)
  require(a1.length <= a2.length && a2.length <= a3.length)
  require(mid <= a1.length && to <= a2.length)
  require(arrayRangesEq(a1, a2, from, mid))
  require(arrayRangesEq(a2, a3, from, to))

  @opaque @inlineOnce @ghost
  def rec(i: Int): Unit = {
    require(from <= i && i <= mid)
    require(arrayRangesEq(a1, a2, i, mid))
    require(arrayRangesEq(a2, a3, i, to))
    require(arrayRangesEq(a1, a3, from, i))
    decreases(to - i)
    if (i == mid) ()
    else {
      arrayRangesEqAppend(a1, a3, from, i)
      rec(i + 1)
    }
  }.ensuring { _ =>
    arrayRangesEq(a1, a3, from, mid)
  }
  rec(from)
}.ensuring(_ => arrayRangesEq(a1, a3, from, mid))

@pure
def arrayBitRangesEq(a1: Array[Byte], a2: Array[Byte], fromBit: Long, toBit: Long): Boolean = {
  require(a1.length <= a2.length)
  require(0 <= fromBit && fromBit <= toBit && toBit <= a1.length.toLong * 8)
  require(fromBit < a1.length.toLong * 8)
  (fromBit < toBit) ==> {
    val (arrPrefixStart, arrPrefixEnd, fromBitIx, toBitIx) = arrayBitIndices(fromBit, toBit)
    val restFrom = (fromBit % 8).toInt
    val restTo = (toBit % 8).toInt
    ((arrPrefixStart < arrPrefixEnd) ==> arrayRangesEq(a1, a2, arrPrefixStart, arrPrefixEnd)) && {
      if (fromBitIx == toBitIx) {
        byteRangesEq(a1(fromBitIx), a2(fromBitIx), restFrom, restTo)
      } else {
        byteRangesEq(a1(fromBitIx), a2(fromBitIx), restFrom, 7) &&
        ((restTo != 0) ==> byteRangesEq(a1(toBitIx), a2(toBitIx), 0, restTo))
      }
    }
  }
}

@ghost @pure @opaque @inlineOnce
def arrayBitRangesEqSlicedLemma(a1: Array[Byte], a2: Array[Byte], fromBit: Long, toBit: Long, fromSlice: Long, toSlice: Long): Unit = {
  require(a1.length <= a2.length)
  require(0 <= fromBit && fromBit <= toBit && toBit <= a1.length.toLong * 8)
  require(fromBit < a1.length.toLong * 8)
  require(arrayBitRangesEq(a1, a2, fromBit, toBit))
  require(fromBit <= fromSlice && fromSlice <= toSlice && toSlice <= toBit)
  require(fromSlice < a1.length.toLong * 8)

  if (fromSlice == toSlice) ()
  else {
    val (arrPrefixStart, arrPrefixEnd, fromBitIx, toBitIx) = arrayBitIndices(fromBit, toBit)
    val restFrom = (fromBit % 8).toInt
    val restTo = (toBit % 8).toInt

    val (arrPrefixSliceStart, arrPrefixSliceEnd, fromSliceIx, toSliceIx) = arrayBitIndices(fromSlice, toSlice)
    val restFromSlice = (fromSlice % 8).toInt
    val restToSlice = (toSlice % 8).toInt

    if (arrPrefixSliceStart < arrPrefixSliceEnd) {
      arrayRangesEqSlicedLemma(a1, a2, arrPrefixStart, arrPrefixEnd, arrPrefixSliceStart, arrPrefixSliceEnd)
    }
    assert {
      if (fromBitIx == toBitIx) {
        byteRangesEq(a1(fromBitIx), a2(fromBitIx), restFrom, restTo)
      } else {
        byteRangesEq(a1(fromBitIx), a2(fromBitIx), restFrom, 7) &&
        ((restTo != 0) ==> byteRangesEq(a1(toBitIx), a2(toBitIx), 0, restTo))
      }
    }

    if (fromBitIx < fromSliceIx && fromSliceIx < toBitIx) {
      arrayRangesEqImpliesEq(a1, a2, arrPrefixStart, fromSliceIx, arrPrefixEnd)
    }
    if (toSliceIx < toBitIx && fromBitIx < toSliceIx) {
      arrayRangesEqImpliesEq(a1, a2, arrPrefixStart, toSliceIx, arrPrefixEnd)
    }

    if (fromSliceIx == toSliceIx) {
      assert(byteRangesEq(a1(fromSliceIx), a2(fromSliceIx), restFromSlice, restToSlice))
    } else {
      assert(byteRangesEq(a1(fromSliceIx), a2(fromSliceIx), restFromSlice, 7))
      assert(((restToSlice != 0) ==> byteRangesEq(a1(toSliceIx), a2(toSliceIx), 0, restToSlice)))
    }
  }
}.ensuring(_ => arrayBitRangesEq(a1, a2, fromSlice, toSlice))
