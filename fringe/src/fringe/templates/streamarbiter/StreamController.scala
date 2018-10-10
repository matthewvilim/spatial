package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._
import fringe._

abstract class StreamController(
  info: StreamParInfo,
  depth: Int,
  dramStream: DRAMStream
) extends Module {
  class StreamControllerIO extends Bundle {
    val dram = dramStream.cloneType()
  }

  val io: StreamControllerIO
  val externalW = globals.target.external_w
  val externalV = globals.target.external_v
}

class StreamControllerLoad(
  info: StreamParInfo,
  depth: Int,
  dramStream: DRAMStream,
  app: LoadStream
) extends StreamController(info, depth, dramStream) {

  class StreamControllerLoadIO extends StreamControllerIO {
    val load = app.cloneType
  }

  val io = IO(new StreamControllerLoadIO)

  val cmd = Module(new FIFO(app.cmd.bits, depth))
  cmd.io.in.valid := io.load.cmd.valid
  io.load.cmd.ready := cmd.io.in.ready
  cmd.io.out.ready := io.dram.cmd.ready
  io.dram.cmd.valid := cmd.io.out.valid
  
  io.dram.cmd.bits.addr := cmd.io.out.bits.addr
  io.dram.cmd.bits.rawAddr := cmd.io.out.bits.addr
  io.dram.cmd.bits.size := cmd.io.out.size
  io.dram.cmd.bits.isWr := false.B

  val rdata = Module(new FIFOWidthConvert(externalW, externalV, info.w, info.v, depth))
  rdata.io.in.bits := io.dram.rresp.bits.rdata
  rdata.io.in.valid := io.dram.rresp.valid
  io.dram.rresp.ready := rdata.io.in.ready

  io.load.rdata.valid := rdata.io.out.valid
  io.load.rdata.bits := rdata.io.out.bits
  rdata.io.out.ready := io.load.rdata.ready
}

class StreamControllerStore(
  info: StreamParInfo,
  depth: Int,
  dramStream: DRAMStream,
  app: StoreStream
) extends StreamController(info, depth, dramStream) {

  class StreamControllerStoreIO extends StreamControllerIO {
    val store = app.cloneType()
  }

  val io = IO(new StreamControllerStoreIO)

  val cmd = Module(new FIFO(app.cmd.bits, depth))
  cmd.io.in.valid := io.store.cmd.valid
  io.store.cmd.ready := cmd.io.in.ready
  cmd.io.out.ready := io.dram.cmd.ready
  io.dram.cmd.valid := cmd.io.out.valid
  
  io.dram.cmd.bits.addr := cmd.io.out.addr
  io.dram.cmd.bits.rawAddr := cmd.io.out.addr
  io.dram.cmd.bits.size := cmd.io.out.size
  io.dram.cmd.bits.isWr := true.B

  val wdata = Module(new FIFOWidthConvert(info.w, info.v, externalW, externalV, depth))
  wdata.io.in.valid := io.store.wdata.valid
  wdata.io.in.bits.data := io.store.wdata.bits
  wdata.io.in.bits.strobe := io.store.wstrb.bits
  io.store.wdata.ready := wdata.io.in.ready
  io.store.wstrb.ready := wdata.io.in.ready

  io.dram.wdata.valid := wdata.io.out.valid
  io.dram.wdata.bits.wdata := wdata.io.out.data
  io.dram.wdata.bits.wstrb := wdata.io.out.strobe
  wdata.io.out.ready := io.dram.wdata.ready

  val wresp = Module(new FIFO(Bool(), depth))
  wresp.io.in.valid := io.dram.wresp.valid
  wresp.io.in.bits := io.dram.wresp.valid
  io.dram.wresp.ready := wresp.io.in.ready

  io.store.wresp.valid := wresp.io.out.valid
  io.store.wresp.bits := wresp.io.out.bits
  wresp.io.out.ready := io.store.wresp.ready
}

class StreamControllerGather(
  info: StreamParInfo,
  depth: Int,
  dramStream: DRAMStream,
  app: GatherStream
) extends StreamController(info, depth, dramStream) {

  class StreamControllerGatherIO extends StreamControllerIO {
    val store = app.cloneType()
  }

  val io = IO(new StreamControllerGatherIO)
}

class StreamControllerScatter(
  info: StreamParInfo,
  depth: Int,
  dramStream: DRAMStream,
  app: ScatterStream
) extends StreamController(info, depth, dramStream) {

  class StreamControllerScatterIO extends StreamControllerIO {
    val store = app.cloneType()
  }

  val io = IO(new StreamControllerScatterIO)
}
