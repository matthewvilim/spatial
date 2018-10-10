package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._
import fringe._

class DRAMStreamControllerIO(dramStream: DRAMStream) extends Bundle {
  val dram = dramStream.cloneType()
}

abstract class DRAMStreamController(
  dramStream: DRAMStream
) extends Module {
  val io: DRAMStreamControllerIO
}

class StreamControllerLoadIO(dramStream: DRAMStream, loadStream: LoadStream) extends DRAMStreamControllerIO(dramStream) {
  val load = loadStream.cloneType
}

class DRAMStreamControllerLoad(
  dramStream: DRAMStream,
  loadStream: LoadStream
) extends DRAMStreamController(dramStream) (
  val io = IO(new StreamControllerLoadIO(dramStream, loadStream)

  val rdata = Module(new FIFOWidthConvert(external_w, io.dram.rresp.bits.rdata.size, info.w, info.v, d))
  rdata.io.enq := io.dram.rresp.bits.rdata
  rdata.io.enqVld := io.enable & io.dram.rresp.valid

  io.dram.rresp.ready := io.enable & ~rdata.io.full

  loadStream.cmd.ready := io.enable & io.sizeCounterDone


}

class DRAMStreamControllerStoreIO(dramStream: DRAMStream, storeStream: StoreStream) extends DRAMStreamControllerIO(dramStream) {
  val store = storeStream.cloneType()
}

class DRAMStreamControllerStore(
  dramStream: DRAMStream,
  storeStream: StoreStream
) extends DRAMStreamController(dramStream) {
  val io = IO(new DRAMStreamControllerStoreIO(dramStream, storeStream)
}
