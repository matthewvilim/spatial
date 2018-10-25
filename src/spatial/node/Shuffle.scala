package spatial.node

import argon._
import argon.node._
import forge.tags._
import spatial.lang._

//@op case class ShuffleCompress[A:Bits](data: Bits[A], mask: Bit) extends Primitive[A]

//@op case class ShuffleCompress[A:Bits](data: Seq[Sym[A]], mask: Seq[Bit])(implicit val vA: Type[Vec[A]]) extends Primitive[A]
@op case class ShuffleCompress[A:Bits](data: Bits[Vec[A]], mask: Vec[Bit])(implicit val tV: Vec[A]) extends Primitive[Vec[A]]
