package asn1scala

import stainless.*
import stainless.lang.*
import stainless.collection.*
import stainless.annotation.{wrapping => _, *}
import stainless.proof.*
import stainless.math.*
import StaticChecks.*

val masksc: Array[Byte] = Array(
    0x00,  //  / 0000 0000 /
    -0x80,  //  / 1000 0000 /
    -0x40,  //  / 1100 0000 /
    -0x20,  //  / 1110 0000 /
    -0x10,  //  / 1111 0000 /
    -0x08,  //  / 1111 1000 /
    -0x04,  //  / 1111 1100 /
    -0x02,  //  / 1111 1110 /
)

def arrayRangesEqOffset[T](a1: Array[T], a2: Array[T], fromA1: Int, toA1: Int, fromA2: Int): Boolean = {
    require(0 <= fromA1 && fromA1 <= toA1)
    require(toA1 <= a1.length)
    require(0 <= fromA2 && fromA2 <= a2.length - (toA1 - fromA1))
    decreases(toA1 - fromA1)
    if (fromA1 == toA1) true
    else a1(fromA1) == a2(fromA2) && arrayRangesEqOffset(a1, a2, fromA1 + 1, toA1, fromA2 + 1)
}

def arrayCopyOffset[T](src: Array[T], dst: Array[T], fromSrc: Int, toSrc: Int, fromDst: Int): Unit = {
    require(0 <= fromSrc && fromSrc <= toSrc)
    require(toSrc <= src.length)
    require(0 <= fromDst && fromDst <= dst.length - (toSrc - fromSrc))
    decreases(toSrc - fromSrc)

    if (fromSrc < toSrc) {
        dst(fromDst) = src(fromSrc)
        arrayCopyOffset(src, dst, fromSrc + 1, toSrc, fromDst + 1)
    }
}

def arrayCopyOffsetLen[T](src: Array[T], dst: Array[T], fromSrc: Int, fromDst: Int, len: Int): Unit = {
    require(0 <= len && len <= src.length && len <= dst.length)
    require(0 <= fromSrc && fromSrc <= src.length - len)
    require(0 <= fromDst && fromDst <= dst.length - len)
    arrayCopyOffset(src, dst, fromSrc, fromSrc + len, fromDst)
}

def copyToArray[T](src: Array[T], dst: Array[T], startInDst: Int, len: Int): Unit = {
    require(0 <= len && len <= src.length)
    require(0 <= startInDst && startInDst <= src.length - len)
    require(src.length <= dst.length)
    arrayCopyOffset(src, dst, 0, len, startInDst)
}

def arraySameElements[T](a1: Array[T], a2: Array[T]): Boolean =
    a1.length == a2.length && arrayRangesEqOffset(a1, a2, 0, a1.length, 0)

// TODO: Reimplement in terms of arrayRangesEqOffset
def arrayPrefix[T](a1: Array[T], a2: Array[T], from: Int, to: Int): Boolean = {
    require(0 <= from && from <= to)
    require(a1.length <= a2.length)
    require(to <= a1.length)
    decreases(to - from)
    if (from == to) true
    else a1(from) == a2(from) && arrayPrefix(a1, a2, from + 1, to)
}
@opaque @inlineOnce
def arrayUpdatedAtPrefixLemma[T](a: Array[T], at: Int, v: T): Unit = {
    require(0 <= at && at < a.length)

    @opaque @inlineOnce
    def rec(i: Int): Unit = {
        require(0 <= i && i <= at)
        require(arrayPrefix(a, freshCopy(a).updated(at, v), i, at))
        decreases(i)
        if (i == 0) ()
        else rec(i - 1)
    }.ensuring { _ =>
        arrayPrefix(a, freshCopy(a).updated(at, v), 0, at)
    }

    rec(at)
}.ensuring { _ =>
    arrayPrefix(a, freshCopy(a).updated(at, v), 0, at)
}

def arrayBitIndices(fromBit: Long, toBit: Long): (Int, Int, Int, Int) = {
    require(0 <= fromBit && fromBit <= toBit && toBit <= 8 * Int.MaxValue.toLong)
    val arrPrefixStart = (fromBit / 8 + (if (fromBit % 8 == 0) 0 else 1)).toInt
    val arrPrefixEnd = (toBit / 8).toInt
    val fromBitIx = (fromBit / 8).toInt
    val toBitIx = (toBit / 8).toInt
    (arrPrefixStart, arrPrefixEnd, fromBitIx, toBitIx)
}

def bytePrefix(b1: Byte, b2: Byte, from: Int, to: Int): Boolean = {
    require(0 <= from && from <= to && to <= 7)
    ((b1 & masksc(to) & masksb(8 - from)) & 0xFF) == ((b2 & masksc(to) & masksb(8 - from)) & 0xFF)
}

def arrayBitPrefix(a1: Array[Byte], a2: Array[Byte], fromBit: Long, toBit: Long): Boolean = {
    require(a1.length <= a2.length)
    require(0 <= fromBit && fromBit <= toBit && toBit <= a1.length.toLong * 8)
    require(fromBit < a1.length.toLong * 8)
    (fromBit < toBit) ==> {
        val (arrPrefixStart, arrPrefixEnd, fromBitIx, toBitIx) = arrayBitIndices(fromBit, toBit)
        val restFrom = (fromBit % 8).toInt
        val restTo = (toBit % 8).toInt
        ((arrPrefixStart < arrPrefixEnd) ==> arrayPrefix(a1, a2, arrPrefixStart, arrPrefixEnd)) && {
            if (fromBitIx == toBitIx) {
                bytePrefix(a1(fromBitIx), a2(fromBitIx), restFrom, restTo)
            } else {
                bytePrefix(a1(fromBitIx), a2(fromBitIx), restFrom, 7) &&
                    ((restTo != 0) ==> bytePrefix(a1(toBitIx), a2(toBitIx), 0, restTo))
            }
        }
    }
}

@opaque @inlineOnce
def arrayPrefixImpliesEq[T](a1: Array[T], a2: Array[T], from: Int, at: Int, to: Int): Unit = {
    require(0 <= from && from <= to)
    require(a1.length <= a2.length)
    require(to <= a1.length)
    require(from <= at && at < to)
    require(arrayPrefix(a1, a2, from, to))

    @opaque @inlineOnce
    def rec(i: Int): Unit = {
        require(from <= i && i <= at)
        require(arrayPrefix(a1, a2, i, to))
        decreases(to - i)
        if (i == at) ()
        else rec(i + 1)
    }.ensuring { _ =>
        a1(at) == a2(at)
    }

    rec(from)
}.ensuring(_ => a1(at) == a2(at))

@opaque @inlineOnce
def arrayPrefixAppend[T](a1: Array[T], a2: Array[T], from: Int, to: Int) = {
    require(0 <= from && from <= to)
    require(a1.length <= a2.length)
    require(to < a1.length)
    require(arrayPrefix(a1, a2, from, to))
    require(a1(to) == a2(to))

    @opaque @inlineOnce
    def rec(i: Int): Unit = {
        require(from <= i && i <= to)
        require(arrayPrefix(a1, a2, i, to + 1))
        decreases(i)
        if (i == from) ()
        else {
            arrayPrefixImpliesEq(a1, a2, from, i - 1, to)
            rec(i - 1)
        }
    }.ensuring { _ =>
        arrayPrefix(a1, a2, from, to + 1)
    }

    rec(to)
}.ensuring(_ => arrayPrefix(a1, a2, from, to + 1))

@opaque @inlineOnce
def arrayPrefixTransitive[T](a1: Array[T], a2: Array[T], a3: Array[T], from: Int, mid: Int, to: Int): Unit = {
    require(0 <= from && from <= mid && mid <= to)
    require(a1.length <= a2.length && a2.length <= a3.length)
    require(mid <= a1.length && to <= a2.length)
    require(arrayPrefix(a1, a2, from, mid))
    require(arrayPrefix(a2, a3, from, to))

    @opaque @inlineOnce
    def rec(i: Int): Unit = {
        require(from <= i && i <= mid)
        require(arrayPrefix(a1, a2, i, mid))
        require(arrayPrefix(a2, a3, i, to))
        require(arrayPrefix(a1, a3, from, i))
        decreases(to - i)
        if (i == mid) ()
        else {
            arrayPrefixAppend(a1, a3, from, i)
            rec(i + 1)
        }
    }.ensuring { _ =>
        arrayPrefix(a1, a3, from, mid)
    }
    rec(from)
}.ensuring(_ => arrayPrefix(a1, a3, from, mid))

sealed trait OptionMut[@mutable A]
case class NoneMut[@mutable A]() extends OptionMut[A]
case class SomeMut[@mutable A](v: A) extends OptionMut[A]
