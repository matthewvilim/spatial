package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._
import fringe._

abstract class StreamController(
  dramStream: DRAMStream
) extends Module {
  class StreamControllerIO extends Bundle {
    val dram = dramStream.cloneType()
  }

  val io: StreamControllerIO
}

class StreamControllerLoad(
  dramStream: DRAMStream,
  app: LoadStream
) extends StreamController(dramStream) {

  class StreamControllerLoadIO extends StreamControllerIO {
    val load = app.cloneType
  }

  val io = IO(new StreamControllerLoadIO)

  /*
  val rdata = Module(new FIFOWidthConvert(external_w, io.dram.rresp.bits.rdata.size, info.w, info.v, d))
  rdata.io.enq := io.dram.rresp.bits.rdata
  rdata.io.enqVld := io.enable & io.dram.rresp.valid

  io.dram.rresp.ready := io.enable & ~rdata.io.full

  loadStream.cmd.ready := io.enable & io.sizeCounterDone
  */


}

class StreamControllerStore(
  dramStream: DRAMStream,
  app: StoreStream
) extends StreamController(dramStream) {

  class StreamControllerStoreIO extends StreamControllerIO {
    val store = app.cloneType()
  }

  val io = IO(new StreamControllerStoreIO)
}

class StreamControllerGather(
  dramStream: DRAMStream,
  app: GatherStream
) extends StreamController(dramStream) {

  class StreamControllerGatherIO extends StreamControllerIO {
    val store = app.cloneType()
  }

  val io = IO(new StreamControllerGatherIO)
}

class StreamControllerScatter(
  dramStream: DRAMStream,
  app: ScatterStream
) extends StreamController(dramStream) {

  class StreamControllerScatterIO extends StreamControllerIO {
    val store = app.cloneType()
  }

  val io = IO(new StreamControllerScatterIO)
}
