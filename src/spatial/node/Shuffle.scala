package spatial.node

import argon._
import argon.node._
import forge.tags._
import spatial.lang._

abstract class ShuffleOp[A:Bits] extends Primitive[Tup2[Bit, A]] {
  val A: Bits[A] = Bits[A]

  def in: Tup2[Bit, A]
}

abstract class ShuffleOpVec[A:Bits](implicit val vT: Type[Vec[Tup2[Bit, A]]]) extends Primitive[Vec[Tup2[Bit, A]]] {
  val A: Bits[A] = Bits[A]

  def in: Seq[Sym[Tup2[Bit, A]]]
}

@op case class ShuffleCompress[A:Bits](
  in: Tup2[Bit, A]
) extends ShuffleOp[A]

@op case class ShuffleCompressVec[A:Bits](
  in: Seq[Sym[Tup2[Bit, A]]],
)(implicit val vA: Type[Vec[Tup2[Bit, A]]]) extends ShuffleOpVec[A]

