package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._

class StreamArbiter(
  val w: Int,
  val d: Int,
  val v: Int,
  val loadStreamInfo: List[StreamParInfo],
  val storeStreamInfo: List[StreamParInfo],
  val gatherStreamInfo: List[StreamParInfo],
  val scatterStreamInfo: List[StreamParInfo],
  val numOutstandingBursts: Int,
  val burstSizeBytes: Int,
  val axiParams: AXI4BundleParameters,
  val isDebugChannel: Boolean = false
) extends Module {
  val numStreams = loadStreamInfo.size + storeStreamInfo.size + gatherStreamInfo.size + scatterStreamInfo.size
  val streamTagWidth = log2Up(numStreams)
  assert(streamTagWidth <= (new DRAMCommandTag).streamId.getWidth)

  val axiLiteParams = new AXI4BundleParameters(64, 512, 1)

  val io = IO(new Bundle {
    val enable = Input(Bool())
    val reset = Input(Bool())
    val app = new AppStreams(loadStreamInfo, storeStreamInfo, gatherStreamInfo, scatterStreamInfo)
    val dram = new DRAMStream(w, v)
    val debugSignals = Output(Vec(numDebugs, UInt(w.W)))

    // AXI Debuggers
    val TOP_AXI = new AXI4Probe(axiLiteParams)
    val DWIDTH_AXI = new AXI4Probe(axiLiteParams)
    val PROTOCOL_AXI = new AXI4Probe(axiLiteParams)
    val CLOCKCONVERT_AXI = new AXI4Probe(axiLiteParams)
  })

  val loadControllers = io.app.loads.foreach { load =>
    val m = Module(new StreamControllerLoad(io.dram, load))
    m.io.dram <> load
  }

}
