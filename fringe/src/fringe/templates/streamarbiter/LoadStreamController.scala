package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._
import fringe._

class LoadStreamControllerIO(dramStream: DRAMStream, loadStream: LoadStream) extends StreamControllerIO(dramStream) {
  val load = loadStream.cloneType
}

class LoadStreamController(
  dramStream: DRAMStream,
  loadStream: LoadStream
) extends StreamController(dramStream) (
  val io = IO(new LoadStreamControllerIO(dramStream, loadStream)
}
