package spatial.metadata

import argon._
import spatial.node._
import emul.FixedPoint

package object retiming {

  implicit class RetimingOps(s: Sym[_]) {
    def getReduceCycle: Option[Cycle] = metadata[Cycle](s)
    def isInCycle: Boolean = getReduceCycle.isDefined
    def reduceCycle: Cycle = metadata[Cycle](s).getOrElse{ throw new Exception(s"No cycle known for $s") }
    def reduceCycle_=(cycle: Cycle): Unit = metadata.add(s, cycle)

    def delayDefined: Boolean = metadata[FullDelay](s).map(_.latency).isDefined
    def fullDelay: Double = metadata[FullDelay](s).map(_.latency).getOrElse(0.0)
    def fullDelay_=(d: Double): Unit = metadata.add(s, FullDelay(d))

    def trace: Sym[_] = s match {
      case Op(DelayLine(_,data)) => data.trace
      case _ => s
    }

    def traceToInt: Int = s.trace match {
      case Const(c: FixedPoint) => c.toInt
      case _ => throw new Exception(s"Cannot trace $s (${s.trace}) to an Int")
    }

  }

  implicit object ValueDelayOrdering extends Ordering[ValueDelay] {
    override def compare(x: ValueDelay, y: ValueDelay): Int = implicitly[Ordering[Int]].compare(y.delay,x.delay)
  }


}
