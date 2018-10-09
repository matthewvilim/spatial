package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._

class DRAMArbiter(dramStream: DRAMStream, streamCount: Int) extends Module {
  val io = IO(new Bundle {
    val in = Vec(streamCount, dramStream)
    val out = dramStream
  })
}
