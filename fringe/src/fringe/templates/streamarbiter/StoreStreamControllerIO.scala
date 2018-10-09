package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._
import fringe._

class StoreStreamControllerIO(dramStream: DRAMStream, storeStream: StoreStream) extends StreamControllerIO(dramStream) {
  val store = storeStream.cloneType()
}

class StoreStreamController(
  dramStream: DRAMStream,
  storeStream: StoreStream
) extends StreamController(dramStream) {
  val io = IO(new StoreStreamControllerIO(dramStream, storeStream)
}
