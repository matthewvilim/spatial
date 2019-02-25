package spatial.node

import argon._
import argon.node._
import forge.tags._
import spatial.lang._

@op case class MergeBufferNew[A:Bits](ways: I32, par: I32) extends MemAlloc[A,MergeBuffer] {
  def dims = Seq(par)
}

@op case class MergeBufferEnq[A:Bits](mem: MergeBuffer[A], way: Int, data: Bits[A], ens: Set[Bit]) extends Enqueuer[A]
@op case class MergeBufferDeq[A:Bits](mem: MergeBuffer[A], ens: Set[Bit]) extends Dequeuer[A,A]

@op case class MergeBufferBound[A:Bits](mem: MergeBuffer[A], way: Int, bound: Bits[I32], ens: Set[Bit]) extends Resetter[A]
@op case class MergeBufferInit[A:Bits](mem: MergeBuffer[A], init: Bit, ens: Set[Bit]) extends Resetter[A]

@op case class MergeBufferBankedEnq[A:Bits](
    mem:  MergeBuffer[A],
    way:  Int,
    data: Seq[Sym[A]],
    enss: Seq[Set[Bit]])
  extends BankedEnqueue[A]

@op case class MergeBufferBankedDeq[A:Bits](
    mem:  MergeBuffer[A],
    enss: Seq[Set[Bit]]
    )(implicit val vA: Type[Vec[A]])
  extends BankedDequeue[A]

