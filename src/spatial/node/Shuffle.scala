package spatial.node

import argon._
import argon.node._
import forge.tags._
import spatial.lang._

//abstract class VecOp[A:Bits](

@op case class ShuffleCompress[A:Bits](
  data: Bits[A],
  mask: Bit
) extends Primitive[A] {
  val A: Bits[A] = Bits[A]
}

@op case class ShuffleCompressVec[A:Bits](
  data: Seq[Sym[A]],
  mask: Seq[Bit]
)(implicit val vA: Type[Vec[A]]) extends Primitive[Vec[A]] {
  val A: Bits[A] = Bits[A]
}
