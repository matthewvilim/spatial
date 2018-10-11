package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._
import fringe.globals._
import fringe.templates.axi4._

class StreamArbiter(
  val loadStreamInfo: List[StreamParInfo],
  val storeStreamInfo: List[StreamParInfo],
  val gatherStreamInfo: List[StreamParInfo],
  val scatterStreamInfo: List[StreamParInfo],
  val axiParams: AXI4BundleParameters,
  val isDebugChannel: Boolean = false
) extends Module {
  val numStreams = loadStreamInfo.size + storeStreamInfo.size + gatherStreamInfo.size + scatterStreamInfo.size
  val streamTagWidth = log2Ceil(numStreams)
  assert(streamTagWidth <= (new DRAMCommandTag).streamId.getWidth)

  val numDebugs = 500

  val axiLiteParams = new AXI4BundleParameters(64, 512, 1)

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val reset = Input(Bool())
    val app = new AppStreams(loadStreamInfo, storeStreamInfo, gatherStreamInfo, scatterStreamInfo)
    val dram = new DRAMStream(EXTERNAL_W, EXTERNAL_V)
    val debugSignals = Output(Vec(numDebugs, UInt(32.W)))

    // AXI Debuggers
    val TOP_AXI = new AXI4Probe(axiLiteParams)
    val DWIDTH_AXI = new AXI4Probe(axiLiteParams)
    val PROTOCOL_AXI = new AXI4Probe(axiLiteParams)
    val CLOCKCONVERT_AXI = new AXI4Probe(axiLiteParams)
  })

  val loadControllers = loadStreamInfo.zipWithIndex.map { case (s, i) =>
    val load = io.app.loads(i)
    val m = Module(new StreamControllerLoad(s, io.dram, load))
    m.io.load <> load
    m
  }

  val storeControllers = storeStreamInfo.zipWithIndex.map { case (s, i) =>
    val store = io.app.stores(i)
    val m = Module(new StreamControllerStore(s, io.dram, store))
    m.io.store <> store
    m
  }

  val gatherControllers = gatherStreamInfo.zipWithIndex.map { case (s, i) =>
    val gather = io.app.gathers(i)
    val m = Module(new StreamControllerGather(s, io.dram, gather))
    m.io.gather <> gather
    m
  }

  val scatterControllers = scatterStreamInfo.zipWithIndex.map { case (s, i) =>
    val scatter = io.app.scatters(i)
    val m = Module(new StreamControllerScatter(s, io.dram, scatter))
    m.io.scatter <> scatter
    m
  }

  val dramArbiter = Module(new DRAMArbiter(io.dram, numStreams))
  dramArbiter.io.dram <> io.dram
  dramArbiter.io.app <> Vec(
    loadControllers.map { _.io.dram } ++
    gatherControllers.map { _.io.dram } ++
    storeControllers.map { _.io.dram } ++
    scatterControllers.map { _.io.dram }
  )

}
