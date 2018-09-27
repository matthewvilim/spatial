package fringe.utils

import chisel3._

/** Converts a rising edge to a 1-cycle pulse. */
class Pulser() extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(Bool())
  })

  // val commandReg = Reg(Bits(1.W), io.in, 0.U)
  val commandReg = RegNext(io.in, false.B)
  io.out := io.in & (commandReg ^ io.in)
}
