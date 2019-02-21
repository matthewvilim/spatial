package spatial.codegen.pirgen

import argon._
import spatial.lang._
import spatial.node._
import spatial.metadata.memory._

trait PIRGenStream extends PIRCodegen {

  override protected def genAccel(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@StreamInNew(bus)  =>
      stateMem(lhs, "FIFO()", None)
      bus match {
        case bus:DRAMBus[_] =>
        case bus =>
          emit(src"streamIn($lhs)")
      }

    case op@StreamOutNew(bus) =>
      stateMem(lhs, "FIFO()", None)
      bus match {
        case bus:DRAMBus[_] =>
        case bus =>
          emit(src"streamOut($lhs)")
      }

    case op@StreamInBankedRead(strm, ens) =>
      stateRead(lhs, strm, None, None, ens)
      val Def(StreamInNew(bus)) = strm
      bus match {
        case BurstAckBus =>
          val count = s"countAck_$lhs"
          emit(src"""val $count = CountAck().input($lhs)""")
          emit(src"""MemWrite().setMem(argOut().name("$count")).data($count)""")
        case bus =>
      }

    case StreamOutBankedWrite(strm, data, ens) =>
      stateWrite(lhs, strm, None, None, data, ens)

    case _ => super.genAccel(lhs, rhs)
  }

  override protected def genHost(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case op@StreamInNew(bus)  =>
      genInAccel(lhs, rhs)
    case op@StreamOutNew(bus) =>
      genInAccel(lhs, rhs)
    case _ => super.genHost(lhs, rhs)
  }

}
