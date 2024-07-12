package asn1scala

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import StaticChecks._

case class Vector[T](@pure @extern underlying: scala.collection.immutable.Vector[T]) {
  @ghost @pure @extern
  def list: List[T] = List.fromScala(underlying.toList)

  @pure @extern
  def size: Int = {
    underlying.size
  }.ensuring(_ == list.isize)

  @pure
  def length: Int = size

  @pure @extern
  def apply(i: Int): T = {
    require(0 <= i && i < size)
    underlying(i)
  }.ensuring(_ == list.iapply(i))

  @pure @extern
  def :+(t: T): Vector[T] = {
    Vector(underlying :+ t)
  }.ensuring(res => res.list == list :+ t && res.size == (if (size == Int.MaxValue) Int.MaxValue else size + 1))

  @pure @extern
  def +:(t: T): Vector[T] = {
    Vector(t +: underlying)
  }.ensuring(res => res.list == t :: list && res.size == (if (size == Int.MaxValue) Int.MaxValue else size + 1))

  def append(t: T): Vector[T] = this :+ t

  def indexOf(elem: T): Int = {
    def rec(i: Int): Int = {
      require(0 <= i && i <= length)
      decreases(length - i)
      if (i == length) -1
      else if (this(i) == elem) i
      else rec(i + 1)
    }.ensuring(res => -1 <= res && res < length)
    rec(0)
  }.ensuring(res => -1 <= res && res < length)

  def indexOfOrLength(elem: T): Int = {
    val ix = indexOf(elem)
    if (ix == -1) length else ix
  }.ensuring(res => 0 <= res && res <= length)

  @pure @extern
  def toScala: scala.collection.immutable.Vector[T] = underlying
}
object Vector {
  @pure @extern @opaque @inlineOnce
  def listEqImpliesEq[T](v1: Vector[T], v2: Vector[T]): Unit = {
    require(v1.list == v2.list)
  }.ensuring(_ => v1 == v2)

  @pure @extern @opaque @inlineOnce
  def listApplyEqVecApply[T](v: Vector[T], i: Int): Unit = {
    require(0 <= i && i < v.size)
  }.ensuring(_ => v.list.iapply(i) == v(i))

  @pure @extern
  def fill[T](n: Int)(t: T): Vector[T] = {
    Vector(scala.collection.immutable.Vector.fill(n)(t))
  }.ensuring(_.list == List.ifill(n)(t))

  @pure @extern
  def empty[T]: Vector[T] = {
    Vector(scala.collection.immutable.Vector.empty[T])
  }.ensuring(_.list == Nil[T]())

  @pure @extern
  def fromList[T](l: List[T]): Vector[T] = {
    def rec(l: List[T], v: Vector[T]): Vector[T] = {
      l match {
        case Nil() => v
        case Cons(x, xs) => rec(xs, v :+ x)
      }
    }
    rec(l, Vector.empty)
  }.ensuring(_.list == l)

  @pure @extern
  def fromScala[T](v: scala.collection.immutable.Vector[T]): Vector[T] = Vector(v)
}