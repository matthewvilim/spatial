package spatial.lang
package memories

import forge.tags._
import core._
import spatial.node._

import scala.collection.mutable

case class FIFO[A:Bits]() extends LocalMem[A,FIFO] {
  override type I = mutable.Queue[AI]

  override def fresh: FIFO[A] = new FIFO[A]
}
object FIFO {
  private lazy val types = new mutable.HashMap[Bits[_],FIFO[_]]()
  implicit def tp[A:Bits]: FIFO[A] = types.getOrElseUpdate(tbits[A], (new FIFO[A]).asType).asInstanceOf[FIFO[A]]

  @api def apply[A:Bits](depth: I32): FIFO[A] = stage(FIFONew(depth))
}