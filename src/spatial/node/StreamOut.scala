package spatial.node

import argon._
import forge.tags.op
import spatial.lang._

@op case class StreamOutNew[A:Bits](bus: Bus) extends MemAlloc[A,StreamOut] {
  def dims: Seq[I32] = Seq(I32(1))
}

@op case class StreamOutWrite[A:Bits](
    mem:  StreamOut[A],
    data: Bits[A],
    ens:  Set[Bit])
  extends Enqueuer[A]

/** A banked write of a vector of elements to an StreamOut.
  * @param mem the StreamOut being written
  * @param data the vector of data being written to the StreamOut
  * @param enss the set of enables for each vector element
  */
@op case class StreamOutBankedWrite[A:Bits](
  mem:  StreamOut[A],
  data: Seq[Sym[A]],
  enss: Seq[Set[Bit]]
)(implicit vT: Type[Vec[A]])
  extends BankedEnqueue[A]
