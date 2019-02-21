package spatial.codegen.pirgen

import argon._
import spatial.lang._
import spatial.node._
import spatial.metadata.bounds.Expect

trait PIRGenCounter extends PIRCodegen {

  override protected def genAccel(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case CounterNew(start,end,step,Expect(par)) => 
      state(lhs)(src"Counter(par=${par}).min($start).step($step).max($end)")
    case CounterChainNew(ctrs)          => 
      state(lhs, tp=Some("List[Counter]"))(src"$ctrs")
    case ForeverNew()                   => 
      state(lhs)(src"Counter(par=1, isForever=true)")
    case _ => super.genAccel(lhs, rhs)
  }

}
