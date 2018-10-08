package fringe.templates.mag

import chisel3._
import chisel3.util._
import fringe.templates.counters.{UpDownCtr, CounterChainOpcode, CounterChainCore}
import fringe.templates.memory.{FFRAM,SRAM}
import fringe.utils.MuxN
import fringe.utils.log2Up

class FIFOIO[T <: Data](t: T, depth: Int) extends Bundle {
  val in = Flipped(Decoupled(t))
  val out = Decoupled(t)
  val count = UInt(log2Up(depth + 1).W)

  val banks = Vec(depth, new Bundle {
    val wdata = Flipped(Valid(t))
    val rdata = Valid(t)
  })

  override def cloneType(): this.type = new FIFOIO(t, depth).asInstanceOf[this.type]
}

class FIFO[T <: Data](val t: T, val depth: Int, val banked: Boolean = false) extends Module {
  assert(isPow2(depth))

  val w = t.getWidth
  val addrWidth = log2Up(depth)

  val io = IO(new FIFOIO(t, depth))

  val mem = if (w == 1 || banked) {
    val m = Module(new FFRAM(t, depth))
    m.io.banks.zip(io.banks).foreach { case (a, b) =>
      a.wdata := b.wdata
      b.wdata.bits := a.wdata.bits
    }
  } else {
    val m = Module(new SRAM(t, depth, "VIVADO_SELECT"))
    m.io.flow := true.B
    m
  }

}

