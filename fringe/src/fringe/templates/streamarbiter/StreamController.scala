package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._
import fringe._

class StreamControllerIO(dramStream: DRAMStream) extends Bundle {
  val enable = Bool()
  val dram = dramStream.cloneType()
}

abstract class StreamController(
  val dramStream: DRAMStream
) extends Module {
  val io: StreamControllerIO
}
