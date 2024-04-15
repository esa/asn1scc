package asn1scala

import stainless.*
import stainless.lang.*
import stainless.collection.*
import stainless.annotation.{wrapping => _, *}
import stainless.proof.*
import stainless.math.*
import StaticChecks.*

@pure
@inlineOnce
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
} ensuring ( _ => old(dst).length == dst.length)

def arrayCopyOffsetLen[T](@pure src: Array[T], dst: Array[T], fromSrc: Int, fromDst: Int, len: Int): Unit = {
  require(0 <= len && len <= src.length && len <= dst.length)
  require(0 <= fromSrc && fromSrc <= src.length - len)
  require(0 <= fromDst && fromDst <= dst.length - len)
  arrayCopyOffset(src, dst, fromSrc, fromSrc + len, fromDst)
} ensuring ( _ => old(dst).length == dst.length)

@pure
def arrayBitIndices(fromBit: Long, toBit: Long): (Int, Int, Int, Int) = {
  require(0 <= fromBit && fromBit <= toBit && toBit <= 8 * Int.MaxValue.toLong)
  val arrPrefixStart = (fromBit / 8 + (if (fromBit % 8 == 0) 0 else 1)).toInt
  val arrPrefixEnd = (toBit / 8).toInt
  val fromBitIx = (fromBit / 8).toInt
  val toBitIx = (toBit / 8).toInt
  (arrPrefixStart, arrPrefixEnd, fromBitIx, toBitIx)
}

def byteRangesEq(b1: Byte, b2: Byte, from: Int, to: Int): Boolean = {
  require(0 <= from && from <= to && to <= 8)
  from == to || ((b1 & MASK_C(to) & MASK_B(8 - from)) & 0xFF) == ((b2 & MASK_C(to) & MASK_B(8 - from)) & 0xFF)
}

// at = bit index, starting from MSB
def bitEqAt(b1: Byte, b2: Byte, at: Int): Boolean = {
  require(0 <= at && at <= 7)
  val mask = 1 << (7 - at)
  (b1 & mask) == (b2 & mask)
}

@pure
def bitAt(arr: Array[Byte], at: Long): Boolean = {
  require(0 <= at && at < arr.length.toLong * 8)
  val byteIx = (at / 8).toInt
  val bitIx = (at % 8).toInt
  (arr(byteIx) & BitAccessMasks(bitIx)) != 0
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
def arrayRangesEqReflexiveLemma[T](a: Array[T]) = {
  def rec(i: Int): Unit = {
    require(0 <= i && i <= a.length)
    require(arrayRangesEq(a, snapshot(a), i, a.length))
    decreases(i)
    if (i == 0) ()
    else rec(i - 1)
  }.ensuring(_ => arrayRangesEq(a, snapshot(a), 0, a.length))
  rec(a.length)
}.ensuring(_ => arrayRangesEq(a, snapshot(a), 0, a.length))

@pure @opaque @inlineOnce @ghost
def arrayRangesEqSymmetricLemma[T](a1: Array[T], a2: Array[T], from: Int, to: Int) = {
  require(0 <= from && from <= to && to <= a1.length)
  require(a1.length == a2.length)
  require(arrayRangesEq(a1, a2, from, to))

  def rec(i: Int): Unit = {
    require(from <= i && i <= to)
    require(arrayRangesEq(a2, a1, i, to))
    decreases(i)
    if (i == from) ()
    else {
      arrayRangesEqImpliesEq(a1, a2, from, i - 1, to)
      rec(i - 1)
    }
  }.ensuring(_ => arrayRangesEq(a2, a1, from, to))

  rec(to)
}.ensuring(_ => arrayRangesEq(a2, a1, from, to))

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
  (fromBit < toBit) ==> {
    val (arrPrefixStart, arrPrefixEnd, fromBitIx, toBitIx) = arrayBitIndices(fromBit, toBit)
    val restFrom = (fromBit % 8).toInt
    val restTo = (toBit % 8).toInt
    ((arrPrefixStart < arrPrefixEnd) ==> arrayRangesEq(a1, a2, arrPrefixStart, arrPrefixEnd)) && {
      if (fromBitIx == toBitIx) {
        byteRangesEq(a1(fromBitIx), a2(fromBitIx), restFrom, restTo)
      } else {
        byteRangesEq(a1(fromBitIx), a2(fromBitIx), restFrom, 8) &&
        ((restTo != 0) ==> byteRangesEq(a1(toBitIx), a2(toBitIx), 0, restTo))
      }
    }
  }
}

@pure @opaque @inlineOnce @ghost
def arrayBitRangesEqReflexiveLemma(a: Array[Byte]) = {
  def rec(i: Long): Unit = {
    require(0 <= i && i <= a.length.toLong * 8L)
    require(arrayBitRangesEq(a, snapshot(a), i, a.length * 8L))
    decreases(i)
    if (i == 0) ()
    else rec(i - 1)
  }.ensuring(_ => arrayBitRangesEq(a, snapshot(a), 0, a.length.toLong * 8L))
  rec(a.length.toLong * 8L)
}.ensuring(_ => arrayBitRangesEq(a, snapshot(a), 0, a.length.toLong * 8L))

@pure @opaque @inlineOnce @ghost
def arrayBitRangesEqPrepend(a1: Array[Byte], a2: Array[Byte], from: Long, to: Long) = {
  require(0 < from && from <= to)
  require(a1.length == a2.length)
  require(to <= a1.length.toLong * 8L)
  require(arrayBitRangesEq(a1, a2, from, to))
  require(bitAt(a1, from - 1) == bitAt(a2, from - 1))
}.ensuring(_ => arrayBitRangesEq(a1, a2, from - 1, to))

@pure @opaque @inlineOnce @ghost
def arrayBitRangesEqDrop1(a1: Array[Byte], a2: Array[Byte], from: Long, to: Long) = {
  require(0 <= from && from < to)
  require(a1.length == a2.length)
  require(to <= a1.length.toLong * 8L)
  require(arrayBitRangesEq(a1, a2, from, to))
}.ensuring(_ => arrayBitRangesEq(a1, a2, from + 1, to))

@pure @opaque @inlineOnce @ghost
def arrayBitRangesEqAppend(a1: Array[Byte], a2: Array[Byte], from: Long, to: Long) = {
  require(0 <= from && from <= to)
  require(a1.length == a2.length)
  require(to < a1.length.toLong * 8L)
  require(arrayBitRangesEq(a1, a2, from, to))
  require(bitAt(a1, to) == bitAt(a2, to))

  @opaque @inlineOnce
  def rec(i: Long): Unit = {
    require(from <= i && i <= to)
    require(arrayBitRangesEq(a1, a2, i, to + 1))
    decreases(i)
    if (i == from) ()
    else {
      arrayBitRangesEqImpliesEq(a1, a2, from, i - 1, to)
      arrayBitRangesEqPrepend(a1, a2, i, to + 1)
      rec(i - 1)
    }
  }.ensuring { _ =>
    arrayBitRangesEq(a1, a2, from, to + 1)
  }

  rec(to)
}.ensuring(_ => arrayBitRangesEq(a1, a2, from, to + 1))

@pure @opaque @inlineOnce @ghost
def arrayBitRangesUpdatedAtLemma(a: Array[Byte], at: Long, b: Boolean): Unit = {
  require(0 <= at && at < a.length.toLong * 8L)

  val byteIx = (at / 8).toInt
  val bitIx = (at % 8).toInt
  val newB = stainless.math.wrapping { ((a(byteIx) & ~BitAccessMasks(bitIx)) | (if (b) BitAccessMasks(bitIx) else 0)).toByte }

  {
    @opaque @inlineOnce @ghost
    def rec(i: Long): Unit = {
      require(0 <= i && i <= at)
      require(arrayBitRangesEq(a, snapshot(a).updated(byteIx, newB), i, at))
      decreases(i)
      if (i == 0) ()
      else rec(i - 1)
    }.ensuring { _ =>
      arrayBitRangesEq(a, snapshot(a).updated(byteIx, newB), 0, at)
    }

    rec(at)
  }.ensuring { _ =>
    arrayBitRangesEq(a, snapshot(a).updated(byteIx, newB), 0, at)
  }
}

@pure @opaque @inlineOnce @ghost
def arrayBitRangesEqTransitive(a1: Array[Byte], a2: Array[Byte], a3: Array[Byte], from: Long, mid: Long, to: Long): Unit = {
  require(0 <= from && from <= mid && mid <= to)
  require(a1.length == a2.length && a2.length == a3.length)
  require(mid <= a1.length.toLong * 8L && to <= a2.length.toLong * 8L)
  require(arrayBitRangesEq(a1, a2, from, mid))
  require(arrayBitRangesEq(a2, a3, from, to))

  @opaque @inlineOnce @ghost
  def rec(i: Long): Unit = {
    require(from <= i && i <= mid)
    require(arrayBitRangesEq(a1, a2, i, mid))
    require(arrayBitRangesEq(a2, a3, i, to))
    require(arrayBitRangesEq(a1, a3, from, i))
    decreases(to - i)
    if (i == mid) ()
    else {
      arrayBitRangesEqAppend(a1, a3, from, i)
      arrayBitRangesEqDrop1(a1, a2, i, mid)
      arrayBitRangesEqDrop1(a2, a3, i, to)
      rec(i + 1)
    }
  }.ensuring { _ =>
    arrayBitRangesEq(a1, a3, from, mid)
  }
  rec(from)
}.ensuring(_ => arrayBitRangesEq(a1, a3, from, mid))


@ghost @pure @opaque @inlineOnce
def arrayBitEqImpliesRangesEqLemma(a: Array[Byte]): Unit = {
  @ghost @pure @opaque @inlineOnce
  def rec(i: Long): Unit = {
    require(0 <= i && i <= a.length.toLong * 8)
    require(arrayBitRangesEq(a, snapshot(a), i, a.length.toLong * 8))
    decreases(i)
    if (i == 0) ()
    else rec(i - 1)
  }.ensuring(_ => arrayBitRangesEq(a, snapshot(a), 0, a.length.toLong * 8))
  rec(a.length.toLong * 8)
}.ensuring(_ => arrayBitRangesEq(a, snapshot(a), 0, a.length.toLong * 8))

@pure @opaque @inlineOnce @ghost
def arrayBitRangesEqImpliesEq(a1: Array[Byte], a2: Array[Byte], from: Long, at: Long, to: Long): Unit = {
  require(0 <= from && from <= to)
  require(a1.length == a2.length)
  require(to <= a1.length.toLong * 8)
  require(from <= at && at < to)
  require(arrayBitRangesEq(a1, a2, from, to))

  @pure @opaque @inlineOnce @ghost
  def rec(i: Long): Unit = {
    require(from <= i && i <= at)
    require(arrayBitRangesEq(a1, a2, i, to))
    decreases(to - i)
    if (i == at) ()
    else rec(i + 1)
  }.ensuring { _ =>
    bitAt(a1, at) == bitAt(a2, at)
  }
  rec(from)
}.ensuring(_ => bitAt(a1, at) == bitAt(a2, at))

@pure @opaque @inlineOnce @ghost
def arrayBitRangesEqImpliesBitEq(a1: Array[Byte], a2: Array[Byte], from: Long, atByte: Int, to: Long): Unit = {
  require(0 <= from && from <= to)
  require(a1.length == a2.length)
  require(to <= a1.length.toLong * 8)
  require((from + 7) / 8 <= atByte && atByte < to / 8)
  require(arrayBitRangesEq(a1, a2, from, to))

  @pure @opaque @inlineOnce @ghost
  def rec(i: Long): Unit = {
    require((from + 7) / 8 <= i && i <= atByte.toLong * 8L)
    require(arrayBitRangesEq(a1, a2, i, to))
    decreases(to - i)
    if (i / 8 == atByte) ()
    else rec(i + 1)
  }.ensuring { _ =>
    a1(atByte) == a2(atByte)
  }
  rec(from)
}.ensuring(_ => a1(atByte) == a2(atByte))

@ghost @pure @opaque @inlineOnce
def arrayBitRangesEqSlicedLemma(a1: Array[Byte], a2: Array[Byte], fromBit: Long, toBit: Long, fromSlice: Long, toSlice: Long): Unit = {
  require(a1.length <= a2.length)
  require(0 <= fromBit && fromBit <= toBit && toBit <= a1.length.toLong * 8)
  require(fromBit <= a1.length.toLong * 8)
  require(arrayBitRangesEq(a1, a2, fromBit, toBit))
  require(fromBit <= fromSlice && fromSlice <= toSlice && toSlice <= toBit)
  require(fromSlice <= a1.length.toLong * 8)

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
