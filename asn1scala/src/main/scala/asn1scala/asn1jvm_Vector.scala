package asn1scala

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import StaticChecks._

case class Vector[T](@pure @extern underlying: scala.collection.immutable.Vector[T]) {
  @pure @extern
  def list: List[T] = List.fromScala(underlying.toList)

  @pure @extern
  def size: Int = {
    underlying.size
  }.ensuring(_ == list.isize)

  @pure @extern
  def apply(i: Int): T = {
    require(0 <= i && i < size)
    underlying(i)
  }.ensuring(_ == list.iapply(i))

  @pure @extern
  def :+(t: T): Vector[T] = {
    Vector(underlying :+ t)
  }.ensuring(_.list == list :+ t)

  def append(t: T): Vector[T] = this :+ t
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
  def fromScala[T](v: scala.collection.immutable.Vector[T]): Vector[T] = {
    Vector(v)
  }
}