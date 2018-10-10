package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._
import fringe.templates.axi4._

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
  val streamTagWidth = log2Ceil(numStreams)
  assert(streamTagWidth <= (new DRAMCommandTag).streamId.getWidth)

  val numDebugs = 500

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

  val loadControllers = loadStreamInfo.zipWithIndex.map { case (s, i) =>
    val load = io.app.loads(i)
    val m = Module(new StreamControllerLoad(s, d, io.dram, load))
    m.io.dram <> load
    m
  }

  val storeControllers = storeStreamInfo.zipWithIndex.map { case (s, i) =>
    val store = io.app.stores(i)
    val m = Module(new StreamControllerStore(s, d, io.dram, store))
    m.io.dram <> store
    m
  }

  val dramArbiter = Module(new DRAMArbiter(io.dram, numStreams, burstSizeBytes))
  dramArbiter.io.dram <> io.dram
  dramArbiter.io.app <> Vec(loadControllers.map { _.io.dram } ++ storeControllers.map { _.io.dram })

}
