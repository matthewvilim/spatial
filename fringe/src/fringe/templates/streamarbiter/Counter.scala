package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

class FringeCounter(val w: Int) extends Module {
  val io = IO(new Bundle {
    val max = Input(UInt(w.W))
    val stride = Input(UInt(w.W))
    val out = Output(UInt(w.W))
    val next = Output(UInt(w.W))
    val last = Output(Bool())
    val reset = Input(Bool())
    val enable = Input(Bool())
    val saturate = Input(Bool())
    val done = Output(Bool())
  })

  val reg = Module(new FringeFF(UInt(w.W)))
  val init = 0.U(w.W)
  reg.io.init := init
  reg.io.enable := io.reset | io.enable

  val count = Cat(0.U(1.W), reg.io.out)
  val newval = count + io.stride
  val isMax = newval >= io.max
  val next = Mux(isMax, Mux(io.saturate, count, init), newval)
  when (io.reset) {
    reg.io.in := init
  } .otherwise {
    reg.io.in := next
  }

  io.last := isMax
  io.out := count
  io.next := next
  io.done := io.enable & isMax
}
