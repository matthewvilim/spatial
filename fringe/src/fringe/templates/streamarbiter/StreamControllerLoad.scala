package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._
import fringe._

class StreamControllerLoadIO(dramStream: DRAMStream, loadStream: LoadStream) extends StreamControllerIO(dramStream) {
  val load = loadStream.cloneType
}

class StreamControllerLoad(
  dramStream: DRAMStream,
  loadStream: LoadStream
) extends StreamController(dramStream) (
  val io = IO(new StreamControllerLoadIO(dramStream, loadStream)

  val rdata = Module(new FIFOWidthConvert(external_w, io.dram.rresp.bits.rdata.size, info.w, info.v, d))
  rdata.io.enq := io.dram.rresp.bits.rdata
  rdata.io.enqVld := io.enable & io.dram.rresp.valid

  io.dram.rresp.ready := io.enable & ~rdata.io.full

  loadStream.cmd.ready := io.enable & io.sizeCounterDone


}
