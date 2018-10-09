package fringe.templates.mag

import chisel3._
import chisel3.util.{Cat, Valid, PriorityEncoder}
import fringe._
import fringe.templates.axi4._
import fringe.templates.counters.{FringeCounter, UpDownCtr}
import fringe.templates.memory.{FringeFF, SRFF}
import fringe.utils.{log2Up, vecWidthConvert}
import fringe.utils.{MuxN, risingEdge}

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.language.reflectiveCalls

class BurstAddr(addrWidth: Int, w: Int, burstSizeBytes: Int) extends Bundle {
  val bits = UInt(addrWidth.W)
  def burstTag = bits(bits.getWidth - 1, log2Up(burstSizeBytes))
  def burstOffset = bits(log2Up(burstSizeBytes) - 1, 0)
  def burstAddr = Cat(burstTag, 0.U(log2Up(burstSizeBytes).W))
  def wordOffset = bits(log2Up(burstSizeBytes) - 1, if (w == 8) 0 else log2Up(w/8))

  override def cloneType(): this.type = {
    new BurstAddr(addrWidth, w, burstSizeBytes).asInstanceOf[this.type]
  }
}

class MAGCore(
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

  val numRdataDebug = 0
  val numRdataWordsDebug = 16
  val numWdataDebug = 0
  val numWdataWordsDebug = 16
  val numDebugs = 500
  val maxBurstsPerCmd = 256
  val maxBytesPerCmd = maxBurstsPerCmd * burstSizeBytes

  val sgDepth = 16

  val loadOrder = loadStreamInfo ++ gatherStreamInfo
  val storeOrder = storeStreamInfo ++ scatterStreamInfo
  val streamOrder = loadOrder ++ storeOrder
  val numStreams = streamOrder.size

  def loadID(s: StreamParInfo) = loadOrder.indexOf(s)
  def storeID(s: StreamParInfo) = storeOrder.indexOf(s)
  def streamID(s: StreamParInfo) = streamOrder.indexOf(s)

  val streamTagWidth = log2Up(numStreams)

  val external_w = globals.target.external_w
  val external_v = globals.target.external_v

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


  // debug registers
  def debugCounter(en: Bool) = {
    val c = Module(new FringeCounter(64))
    c.io.reset := io.reset
    c.io.saturate := false.B
    c.io.max := ~(0.U(w.W))
    c.io.stride := 1.U
    c.io.enable := en
    c
  }

  def debugFF[T<:Data](sig: T, en: UInt) = {
    val in = sig match {
      case v: Vec[_] => v.asInstanceOf[Vec[UInt]].reverse.reduce { Cat(_,_) }
      case u: UInt => u
    }

    val ff = Module(new FringeFF(UInt(sig.getWidth.W)))
    ff.io.init := Cat("hBADF".U, dbgCount.U)
    ff.io.in := in
    ff.io.enable := en
    ff
  }

  var dbgCount = 0
  val signalLabels = ListBuffer[String]()
  def connectDbgSig(sig: UInt, label: String): Unit = {
    if (globals.enableDebugRegs) {
      if (isDebugChannel) {
        io.debugSignals(dbgCount) := sig
        val padded_label = if (label.length < 55) {label + "."*(55-label.length)} else label
        signalLabels.append(padded_label)
        dbgCount += 1
      }
    }
  }

  val addrWidth = io.app.loads(0).cmd.bits.addrWidth
  val sizeWidth = io.app.loads(0).cmd.bits.sizeWidth

  val cmd = new AppCommandDense
  val cmdArbiter = Module(new FIFOArbiter(cmd, d, numStreams))
  val cmdFifos = List.fill(numStreams) { Module(new FIFO(cmd, d)) }
  cmdArbiter.io.fifo.zip(cmdFifos).foreach { case (io, f) => io <> f.io }

  cmdArbiter.io.forceTag.valid := false.B

  connectDbgSig(debugFF(chisel3.util.Cat(0x7F.U(32.W), io.app.loads.head.cmd.bits.addr(31,0)), io.app.loads.head.cmd.valid ).io.out, "[ACCEL] Last load addr enqueued")  

  val sizeCounter = Module(new FringeCounter(sizeWidth)) // Must be able to capture any size command
  val cmds = io.app.loads.map { _.cmd } ++ io.app.stores.map {_.cmd }
  cmdArbiter.io.enq.zip(cmds) foreach {case (enq, cmd) => 
    // enq(0).addr := chisel3.util.Cat(0x7f.U(32.W), cmd.bits.addr(31,0))
    // enq(0).isWr := cmd.bits.isWr
    // enq(0).isSparse := cmd.bits.isSparse
    // enq(0).size := cmd.bits.size
    enq := cmd.bits 
  }
  cmdArbiter.io.enqVld.zip(cmds) foreach {case (enqVld, cmd) => enqVld := cmd.valid }
  cmdArbiter.io.full.zip(cmds) foreach { case (full, cmd) => cmd.ready := ~full }

  val cmdHead = cmdArbiter.io.deq

  val cmdAddr = Wire(new BurstAddr(addrWidth, w, burstSizeBytes))
  cmdAddr.bits := cmdHead.addr + sizeCounter.io.out

  // don't issue commands if a response that cycle has the same tag to avoid edge cases in the coalescing logic
  val cmdReadConflict = io.dram.rresp.valid & io.dram.rresp.bits.tag.asUInt === io.dram.cmd.bits.tag.asUInt
  val cmdRead = io.enable & cmdArbiter.io.deqReady & ~cmdHead.isWr & ~cmdReadConflict
  val cmdWrite = io.enable & cmdArbiter.io.deqReady & cmdHead.isWr

  val rrespTag = io.dram.rresp.bits.tag
  val wrespTag = io.dram.wresp.bits.tag

  val isSparseMux = Module(new MuxN(Bool(), numStreams))
  val wdataReady = io.dram.wdata.ready
  val burstCounter = Module(new FringeCounter(16)) // 9 bits would capture AXI max cmd of 256 on Xilinx, set to 16 to be safe
  val burstTagCounter = Module(new FringeCounter(log2Up(numOutstandingBursts)))
  val dramReadySeen = Wire(Bool())

  // Size counter: Controls the size of requests issued based on target-specific 'maxSize' parameter.
  // max: size in bursts
  // stride: maxSize
  // en: dramReady & dramValid
  // dram.cmd.bits.size := Mux(sizeCounter.io.done, sizeCounter.io.max - sizeCounter.io.out, sizeCounter.io.max)
  // Deq cmdFifo when sizeCounter wraps, NOT when burstCounter wraps
  sizeCounter.io.reset := false.B // TODO: When should this reset
  sizeCounter.io.saturate := false.B
  sizeCounter.io.max := Mux(isSparseMux.io.out, 1.U, cmdHead.size)
  sizeCounter.io.stride := maxBytesPerCmd.U

  val cmdCooldown =  Module(new FringeFF(Bool()))
  val burstCounterDoneLatch = Module(new FringeFF(Bool()))
  val sizeCounterDoneLatch = Module(new FringeFF(Bool()))
  sizeCounterDoneLatch.io.init := false.B
  sizeCounterDoneLatch.io.in := true.B
  sizeCounterDoneLatch.io.enable := Mux(isSparseMux.io.out, false.B, sizeCounter.io.done)
  sizeCounterDoneLatch.io.reset := burstCounterDoneLatch.io.out // | (io.dram.rresp.valid & io.dram.rresp.ready))  // Assumes burst counter will finish after sizeCounter, potential hazard

  val rrespReadyMux = Module(new MuxN(Bool(), loadOrder.size))
  rrespReadyMux.io.sel := rrespTag.streamId
  io.dram.rresp.ready := rrespReadyMux.io.out

  val burstCounterMaxLatch = Module(new FringeFF(UInt(io.dram.cmd.bits.size.getWidth.W)))
  val wdataMux = Module(new MuxN(Valid(io.dram.wdata.bits), storeOrder.size))
  wdataMux.io.sel := cmdArbiter.io.tag - loadOrder.size.U
  wdataMux.io.ins.zipWithIndex.foreach { case (in,i) =>
    in.bits.wlast := in.valid & (burstCounter.io.out === Mux(burstCounterMaxLatch.io.enable, io.dram.cmd.bits.size - 1.U, burstCounterMaxLatch.io.out - 1.U))
  }
  val wdataValid = wdataMux.io.out.valid

  val cmdDeqValidMux = Module(new MuxN(Bool(), numStreams))
  cmdDeqValidMux.io.sel := cmdArbiter.io.tag

  isSparseMux.io.sel := cmdArbiter.io.tag

  val dramCmdMux = Module(new MuxN(Valid(io.dram.cmd.bits), numStreams))
  dramCmdMux.io.sel := cmdArbiter.io.tag
  dramCmdMux.io.ins.zipWithIndex.foreach { case (i, id) =>
    i.bits.addr := cmdAddr.burstAddr
    i.bits.rawAddr := cmdAddr.bits
    val tag = Wire(new DRAMCommandTag)
    tag.streamId := cmdArbiter.io.tag
    i.bits.tag := tag
    val size = Wire(new BurstAddr(cmdHead.size.getWidth, w, burstSizeBytes))
    size.bits := Mux(isSparseMux.io.out, cmdHead.size, Mux(sizeCounter.io.done, cmdHead.size - sizeCounter.io.out, maxBytesPerCmd.U))
    i.bits.size := size.burstTag + (size.burstOffset != 0.U)
    i.bits.isWr := cmdHead.isWr
    if (id < loadOrder.length && id == 0) {
      connectDbgSig(debugFF(dramCmdMux.io.out.bits.tag.streamId, dramCmdMux.io.out.valid & !dramCmdMux.io.out.bits.isWr ).io.out, "[FRINGE] Last load streamId (tag) sent")
      connectDbgSig(debugFF(dramCmdMux.io.out.bits.addr, dramCmdMux.io.out.valid & !dramCmdMux.io.out.bits.isWr ).io.out, "[FRINGE] Last load addr sent")
      connectDbgSig(debugFF(dramCmdMux.io.out.bits.size, dramCmdMux.io.out.valid & !dramCmdMux.io.out.bits.isWr ).io.out, "[FRINGE] Last load size sent")
    } else if (id == loadOrder.length) {
      connectDbgSig(debugFF(cmdArbiter.io.tag, cmdWrite ).io.out, "[FRINGE] Last store streamId (tag) sent")
      connectDbgSig(debugFF(cmdHead.size, cmdWrite ).io.out, "[FRINGE] Last store size sent")
    }

  }

  val wrespReadyMux = Module(new MuxN(Bool(), storeStreamInfo.size))
  wrespReadyMux.io.sel := wrespTag.streamId
  io.dram.wresp.ready := wrespReadyMux.io.out

  val dramReady = io.dram.cmd.ready


  val gatherLoadIssueMux = Module(new MuxN(Bool(), numStreams))
  gatherLoadIssueMux.io.ins.foreach { _ := false.B }
  gatherLoadIssueMux.io.sel := cmdArbiter.io.tag
  val gatherLoadIssue = debugCounter(gatherLoadIssueMux.io.out)

  val gatherLoadSkipMux = Module(new MuxN(Bool(), numStreams))
  gatherLoadSkipMux.io.ins.foreach { _ := false.B }
  gatherLoadSkipMux.io.sel := cmdArbiter.io.tag

  val gatherLoadSkip = debugCounter(gatherLoadSkipMux.io.out)
  if (gatherStreamInfo.size > 0) {
    connectDbgSig(gatherLoadIssue.io.out, "[FRINGE] Gather load issue")
    connectDbgSig(gatherLoadSkip.io.out, "[FRINGE] Gather load skip")
  }

  val scatterLoadIssueMux = Module(new MuxN(Bool(), numStreams))
  scatterLoadIssueMux.io.ins.foreach { _ := false.B }
  scatterLoadIssueMux.io.sel := cmdArbiter.io.tag
  val scatterLoadIssue = debugCounter(scatterLoadIssueMux.io.out)

  val scatterLoadSkipMux = Module(new MuxN(Bool(), numStreams))
  scatterLoadSkipMux.io.ins.foreach { _ := false.B }
  scatterLoadSkipMux.io.sel := cmdArbiter.io.tag
  val scatterLoadSkip = debugCounter(scatterLoadSkipMux.io.out)

  val scatterStoreIssueMux = Module(new MuxN(Bool(), numStreams))
  scatterStoreIssueMux.io.ins.foreach { _ := false.B }
  scatterStoreIssueMux.io.sel := cmdArbiter.io.tag
  val scatterStoreIssue = debugCounter(scatterStoreIssueMux.io.out)

  val scatterStoreSkipMux = Module(new MuxN(Bool(), numStreams))
  scatterStoreSkipMux.io.ins.foreach { _ := false.B }
  scatterStoreSkipMux.io.sel := cmdArbiter.io.tag
  val scatterStoreSkip = debugCounter(scatterStoreSkipMux.io.out)

  if (scatterStreamInfo.size > 0) {
    connectDbgSig(scatterLoadSkip.io.out, "[FRINGE] Scatter load skip")
    connectDbgSig(scatterStoreIssue.io.out, "[FRINGE] Scatter store issue")
    connectDbgSig(scatterLoadIssue.io.out, "[FRINGE] Scatter load issue")
    connectDbgSig(scatterStoreSkip.io.out, "[FRINGE] Scatter store skip")
  }

  val gatherBuffers = gatherStreamInfo.zip(io.app.gathers).map { case (info, stream) =>
    val streamid = streamID(info)
    val loadid = loadID(info)
    val m = Module(new GatherBuffer(info.w, info.v, sgDepth, burstSizeBytes, addrWidth, cmdHead, io.dram.rresp.bits))
    m.io.rresp.valid := io.dram.rresp.valid & (rrespTag.streamId === streamid.U)
    m.io.rresp.bits := io.dram.rresp.bits
    m.io.cmd.valid := cmdRead & (cmdArbiter.io.tag === streamid.U) & dramReady
    m.io.cmd.bits := cmdHead

    gatherLoadIssueMux.io.ins(streamid) := !cmdArbiter.io.empty & cmdDeqValidMux.io.ins(streamid) & dramCmdMux.io.ins(streamid).valid
    gatherLoadSkipMux.io.ins(streamid) := !cmdArbiter.io.empty & cmdDeqValidMux.io.ins(streamid) & !dramCmdMux.io.ins(streamid).valid

    isSparseMux.io.ins(streamid) := true.B

    rrespReadyMux.io.ins(loadid) := true.B
    cmdDeqValidMux.io.ins(streamid) := cmdRead & m.io.in(0).ready & dramReady
    dramCmdMux.io.ins(streamid).valid := cmdRead & m.io.in(0).ready & !m.io.hit
    dramCmdMux.io.ins(streamid).bits.tag.uid := cmdAddr.burstTag

    stream.rdata.bits := m.io.out.bits
    stream.rdata.valid := m.io.out.valid
    m.io.out.ready := stream.rdata.ready
    m
  }

  // TODO: this currently assumes the memory controller hands back data in the order it was requested,
  // but according to the AXI spec the ARID field should be constant to enforce ordering?
  val denseLoadBuffers = loadStreamInfo.zip(io.app.loads).map { case (info, stream) =>
    val streamid = streamID(info)
    val loadid = loadID(info)

    val m = Module(new FIFOWidthConvert(external_w, io.dram.rresp.bits.rdata.size, info.w, info.v, d))
    m.io.enq := io.dram.rresp.bits.rdata
    m.io.enqVld := io.dram.rresp.valid & (rrespTag.streamId === streamid.U)

    rrespReadyMux.io.ins(loadid) := ~m.io.full
//    cmdDeqValidMux.io.ins(streamid) := dramReady
    cmdDeqValidMux.io.ins(streamid) := sizeCounterDoneLatch.io.out

    dramCmdMux.io.ins(streamid).valid := cmdRead
    dramCmdMux.io.ins(streamid).bits.tag.uid := burstTagCounter.io.out

    isSparseMux.io.ins(streamid) := false.B

    stream.rdata.bits := m.io.deq
    stream.rdata.valid := ~m.io.empty
    m.io.deqVld := stream.rdata.ready

    connectDbgSig(debugCounter(m.io.enqVld).io.out, s"[FRINGE DENSELD] rdataFifo $loadid # enqs")
    connectDbgSig(debugCounter(!m.io.empty).io.out, s"[FRINGE DENSELD] rdataFifo $loadid # cycles ~empty (= data valid)")
    connectDbgSig(debugCounter(stream.rdata.ready).io.out, s"[FRINGE DENSELD] load stream $loadid # cycles ready")
    connectDbgSig(debugCounter(!m.io.empty && stream.rdata.ready).io.out, s"[FRINGE DENSELD] load stream $loadid # handshakes")

    connectDbgSig(debugCounter(m.io.empty & m.io.deqVld).io.out, s"[FRINGE DENSELD] number of bad elements (IF =!= 0, LOOK HERE FOR BUGS)")

    val sDeq_latch = Module(new SRFF())
    sDeq_latch.io.input.set := m.io.deqVld
    sDeq_latch.io.input.reset := reset.toBool | io.reset
    sDeq_latch.io.input.asyn_reset := reset.toBool | io.reset

    val sEnq_latch = Module(new SRFF())
    sEnq_latch.io.input.set := m.io.enqVld
    sEnq_latch.io.input.reset := reset.toBool | io.reset
    sEnq_latch.io.input.asyn_reset := reset.toBool | io.reset


    connectDbgSig(debugFF(m.io.deq, !sDeq_latch.io.output.data & risingEdge(m.io.deqVld)).io.out, s"[FRINGE DENSELD] m.io.deq")
    connectDbgSig(debugFF(m.io.enq, !sEnq_latch.io.output.data & risingEdge(m.io.enqVld)).io.out, s"[FRINGE DENSELD] m.io.enq")

    m
  }


  val scatterBuffers = scatterStreamInfo.zipWithIndex.map { case (s, i) =>
    val streamid = streamID(s)
    val storeid = storeID(s)

    val m = Module(new ScatterBuffer(s.w, sgDepth, s.v, burstSizeBytes, addrWidth, sizeWidth, io.dram.rresp.bits))
    val wdata = Module(new FIFOCore(UInt(s.w.W), d, s.v))
    val stream = io.app.scatters(i)

    val write = cmdWrite & (cmdArbiter.io.tag === streamid.U)
    val issueWrite = m.io.complete & (cmdArbiter.io.tag === streamid.U)
    val issueRead = !m.io.complete & write & m.io.fifo.in.ready & !wdata.io.empty & !m.io.hit
    val skipRead = write & m.io.hit & !wdata.io.empty

    isSparseMux.io.ins(streamid) := true.B

    val deqCmd = skipRead | (issueRead & dramReady)
    wdata.io.config.chainRead := false.B
    wdata.io.config.chainWrite := false.B
    wdata.io.enqVld := stream.wdata.valid
    wdata.io.enq := stream.wdata.bits
    wdata.io.deqVld := deqCmd
    stream.wdata.ready := ~wdata.io.full

    cmdDeqValidMux.io.ins(streamid) := deqCmd

    val dramCmd = dramCmdMux.io.ins(streamid)
    val addr = Wire(new BurstAddr(addrWidth, s.w, burstSizeBytes))
    addr.bits := Mux(issueRead, cmdHead.addr, m.io.fifo.out.bits.cmd.addr)
    dramCmd.bits.addr := addr.burstAddr
    dramCmd.bits.rawAddr := addr.bits
    dramCmd.bits.tag.uid := Mux(issueRead, addr.burstTag, m.io.fifo.out.bits.count)
    dramCmd.bits.isWr := issueWrite
    val size = Wire(new BurstAddr(cmdHead.size.getWidth, s.w, burstSizeBytes))
    size.bits := Mux(issueRead, cmdHead.size, m.io.fifo.out.bits.cmd.size)
    dramCmd.bits.size := size.burstTag + (size.burstOffset != 0.U)
    dramCmd.valid := issueRead | (issueWrite & wdataValid & !dramReadySeen)

    scatterLoadIssueMux.io.ins(streamid) := dramCmd.valid & deqCmd & !cmdArbiter.io.empty
    scatterLoadSkipMux.io.ins(streamid)  := !dramCmd.valid & deqCmd & !cmdArbiter.io.empty
    scatterStoreIssueMux.io.ins(streamid) := m.io.complete & m.io.fifo.out.ready
    scatterStoreSkipMux.io.ins(streamid) := deqCmd & !cmdArbiter.io.empty

    m.io.rresp.valid := io.dram.rresp.valid & (rrespTag.streamId === streamid.U)
    m.io.rresp.bits := io.dram.rresp.bits
    m.io.fifo.in.valid := deqCmd
    m.io.fifo.in.bits.data.foreach { _ := wdata.io.deq(0) }
    m.io.fifo.in.bits.cmd := cmdHead
    m.io.fifo.out.ready := burstCounter.io.done

    wdataMux.io.ins(storeid).valid := issueWrite /*& ~cmdCooldown.io.out & ~burstCounterDoneLatch.io.out */
    wdataMux.io.ins(storeid).bits.wdata := vecWidthConvert(m.io.fifo.out.bits.data, w)
    wdataMux.io.ins(storeid).bits.wstrb.zipWithIndex.foreach{case (st, i) => st := true.B}

    val wrespFIFO = Module(new FIFOCore(UInt(32.W), d, 1))
    wrespFIFO.io.enq(0) := io.dram.wresp.bits.tag.uid
    wrespFIFO.io.enqVld := io.dram.wresp.valid & (wrespTag.streamId === streamid.U)
    wrespReadyMux.io.ins(storeid) := ~wrespFIFO.io.full

    val count = Module(new UpDownCtr(32))
    count.io.max := ~(0.U(32.W))
    // send a response after at least 16 sparse writes have completed
    // why does spatial always expect 16 regardless of parallelization factor?
    val sendResp = count.io.out >= 16.U
    val deqRespCount = !wrespFIFO.io.empty & !sendResp
    count.io.strideInc := wrespFIFO.io.deq(0)
    count.io.strideDec := 16.U(32.W)
    count.io.inc := deqRespCount
    count.io.dec := sendResp & stream.wresp.ready

    stream.wresp.bits := sendResp
    stream.wresp.valid := sendResp
    wrespFIFO.io.deqVld := deqRespCount

    m
  }
  // force command arbiter to service scatter buffers when data is waiting to be written back to memory
  scatterStreamInfo.headOption match {
    case Some(s) =>
      val scatterReadys = scatterBuffers.map{ _.io.fifo.out.valid }
      cmdArbiter.io.forceTag.valid := scatterReadys.reduce {_|_}
      cmdArbiter.io.forceTag.bits := PriorityEncoder(scatterReadys) + streamID(s).U
    case None =>
  }

  val denseStoreBuffers = storeStreamInfo.zipWithIndex.map { case (s, i) =>
    val streamid = streamID(s)
    val storeid = storeID(s)
    val m = Module(new FIFOWidthConvert(s.w, s.v, external_w, external_v, d))
    val stream = io.app.stores(i)

    // cmdDeqValidMux.io.ins(streamid) := burstCounter.io.done
    cmdDeqValidMux.io.ins(streamid) := burstCounterDoneLatch.io.out & sizeCounterDoneLatch.io.out //& ~cmdCooldown.io.out

    dramCmdMux.io.ins(streamid).valid := cmdWrite & wdataValid & !dramReadySeen
    dramCmdMux.io.ins(streamid).bits.tag.uid := burstTagCounter.io.out

    isSparseMux.io.ins(streamid) := false.B

    m.io.enqVld := stream.wdata.valid
    m.io.enq := stream.wdata.bits
    m.io.enqStrb := stream.wstrb.bits
    m.io.deqVld := cmdWrite & !m.io.empty & io.dram.wdata.ready & (cmdArbiter.io.tag === streamid.U) & !cmdCooldown.io.out & !burstCounterDoneLatch.io.out

    wdataMux.io.ins(storeid).valid := cmdWrite & !m.io.empty & !cmdCooldown.io.out & !burstCounterDoneLatch.io.out
    wdataMux.io.ins(storeid).bits.wdata := m.io.deq
    wdataMux.io.ins(storeid).bits.wstrb.zipWithIndex.foreach{case (st, i) => st := m.io.deqStrb(i)}
    stream.wdata.ready := ~m.io.full

    val wrespSizeCounter = Module(new FringeCounter(sizeWidth))
    val wrespSizeCounterMaxLatch = Module(new FringeFF(UInt(sizeWidth.W)))
    wrespSizeCounterMaxLatch.io.reset := false.B
    wrespSizeCounterMaxLatch.io.init := 0.U
    wrespSizeCounterMaxLatch.io.in := cmdHead.size
    wrespSizeCounterMaxLatch.io.enable := io.dram.cmd.valid & io.dram.cmd.ready

    wrespSizeCounter.io.reset := false.B
    wrespSizeCounter.io.saturate := false.B
    wrespSizeCounter.io.max := wrespSizeCounterMaxLatch.io.out
    wrespSizeCounter.io.stride := maxBytesPerCmd.U
    wrespSizeCounter.io.enable := io.dram.wresp.valid & (wrespTag.streamId === storeid.U)

    val wrespFIFO = Module(new FIFOCounter(d, 1))
    wrespFIFO.io.enq(0) := io.dram.wresp.valid
    wrespFIFO.io.enqVld := wrespSizeCounter.io.done
    wrespReadyMux.io.ins(storeid) := ~wrespFIFO.io.full
    stream.wresp.bits  := wrespFIFO.io.deq(0)
    stream.wresp.valid := ~wrespFIFO.io.empty
    wrespFIFO.io.deqVld := stream.wresp.ready

    connectDbgSig(debugCounter(wrespFIFO.io.enqVld).io.out, s"[FRINGE DENSEST] wrespFifo $storeid enq")
    connectDbgSig(debugCounter(!wrespFIFO.io.empty).io.out, s"[FRINGE DENSEST] wrespFifo $storeid ~empty (stream.wresp.valid)")
    connectDbgSig(debugCounter(stream.wresp.ready).io.out, s"[FRINGE DENSEST] wrespFifo $storeid deq (stream.wresp.ready)")

    m
  }

  val dramValid = io.dram.cmd.valid

  burstCounterMaxLatch.io.init := Cat("hBADF".U, dbgCount.U)
  burstCounterMaxLatch.io.in := io.dram.cmd.bits.size
  burstCounterMaxLatch.io.enable := dramValid & dramReady
  val burstCounterMax = Mux(dramValid & dramReady, io.dram.cmd.bits.size, burstCounterMaxLatch.io.out)

  burstCounterDoneLatch.io.init := false.B
  burstCounterDoneLatch.io.in := true.B
  burstCounterDoneLatch.io.enable := Mux(isSparseMux.io.out, false.B, burstCounter.io.done)
  burstCounterDoneLatch.io.reset := Mux(burstCounter.io.last, burstCounterDoneLatch.io.out & sizeCounterDoneLatch.io.out, burstCounterDoneLatch.io.out)

  burstCounter.io.max := Mux(io.dram.cmd.bits.isWr, burstCounterMax, 1.U)
  burstCounter.io.stride := 1.U
  burstCounter.io.reset := io.reset
  burstCounter.io.enable := Mux(io.dram.cmd.bits.isWr, wdataValid & wdataReady, dramValid  & dramReady)
  burstCounter.io.saturate := false.B

  sizeCounter.io.enable := dramValid & dramReady
  // sizeCounter.io.enable := Mux(isSparseMux.io.out, false.B, dramValid & dramReady)

  // strictly speaking this isn't necessary, but the DRAM part of the test bench expects unique tags
  // and sometimes apps make requests to the same address so tagging with the address isn't enough to guarantee uniqueness
  burstTagCounter.io.max := (numOutstandingBursts - 1).U
  burstTagCounter.io.stride := 1.U
  burstTagCounter.io.reset := io.reset
  burstTagCounter.io.enable := dramValid  & dramReady
  burstTagCounter.io.saturate := false.B

  val dramReadyFF = Module(new FringeFF(Bool()))
  dramReadyFF.io.init := 0.U
  val dramReadyFFEnabler = Mux(isSparseMux.io.out, burstCounter.io.done, burstCounterDoneLatch.io.out)
  dramReadyFF.io.enable := dramReadyFFEnabler | (dramValid  & io.dram.cmd.bits.isWr)
  dramReadyFF.io.in := Mux(dramReadyFFEnabler, 0.U, dramReady | dramReadySeen)
  dramReadySeen := dramReadyFF.io.out
  cmdArbiter.io.deqVld := cmdDeqValidMux.io.out

  io.dram.wdata.bits.wdata := wdataMux.io.out.bits.wdata
  io.dram.wdata.bits.wlast := wdataMux.io.out.bits.wlast
  io.dram.wdata.bits.wstrb := wdataMux.io.out.bits.wstrb.reverse // .foreach(_ := 1.U)
  io.dram.wdata.valid := wdataMux.io.out.valid

  io.dram.cmd.bits := dramCmdMux.io.out.bits
  // io.dram.cmd.bits.addr := chisel3.util.Cat(0x7F.U(32.W), dramCmdMux.io.out.bits.addr(31,0))
  // io.dram.cmd.bits.size := dramCmdMux.io.out.bits.size
  // io.dram.cmd.bits.rawAddr := chisel3.util.Cat(0x7F.U(32.W), dramCmdMux.io.out.bits.rawAddr(31,0))
  // io.dram.cmd.bits.isWr := dramCmdMux.io.out.bits.isWr
  // io.dram.cmd.bits.tag := dramCmdMux.io.out.bits.tag
  // io.dram.cmd.bits.dramReadySeen := dramCmdMux.io.out.bits.dramReadySeen

  cmdCooldown.io.reset := ~(io.dram.cmd.valid & io.dram.cmd.ready)
  cmdCooldown.io.enable := io.dram.cmd.valid & io.dram.cmd.ready // Enforce one cycle cooldown
  cmdCooldown.io.in := true.B
  cmdCooldown.io.init := 0.U
  io.dram.cmd.valid := dramCmdMux.io.out.valid & Mux(isSparseMux.io.out, true.B, !cmdCooldown.io.out)

  val cycleCount = debugCounter(io.enable)
  connectDbgSig(cycleCount.io.out, "Cycles")

  val rdataEnqCount = debugCounter(io.dram.rresp.valid & io.dram.rresp.ready)
  val wdataCount = debugCounter(io.dram.wdata.valid & io.dram.wdata.ready & io.enable)

  // rdata enq values
  for (i <- 0 until numRdataDebug) {
    connectDbgSig(debugFF(io.dram.cmd.bits.addr, io.dram.rresp.ready & io.dram.rresp.valid & (rdataEnqCount.io.out === (i+42).U) ).io.out, s"[FRINGE] raddr_from_dram${(i+42)}")  
    connectDbgSig(debugFF(io.dram.cmd.bits.size, io.dram.rresp.ready & io.dram.rresp.valid & (rdataEnqCount.io.out === (i+42).U) ).io.out, s"[FRINGE] raddr_from_dram${(i+42)}")  
    for (j <- 0 until numRdataWordsDebug) {
      connectDbgSig(debugFF(io.dram.rresp.bits.rdata(j), io.dram.rresp.ready & io.dram.rresp.valid & (rdataEnqCount.io.out === (i+42).U)).io.out, s"""[FRINGE] rdata_from_dram${(i+42)}_$j""")
    }
  }


  if (io.app.stores.nonEmpty) {
    // wdata enq values
    for (i <- 0 until numWdataDebug) {
      connectDbgSig(debugFF(io.dram.wdata.bits.wstrb, io.dram.wdata.ready & io.dram.wdata.valid & (wdataCount.io.out === (i).U)).io.out, s"""[FRINGE] wstrb_from_dram${(i)}""")
      for (j <- 0 until numWdataWordsDebug) {
        connectDbgSig(debugFF(io.dram.wdata.bits.wdata(j), io.dram.wdata.ready & io.dram.wdata.valid & (wdataCount.io.out === (i).U)).io.out, s"""[FRINGE] wdata_from_dram${(i)}_$j""")
      }
      // connectDbgSig(debugFF(wdataMux.io.out.bits.wdata, io.dram.wdata.ready & io.dram.wdata.valid & (wdataCount.io.out === i.U)).io.out, s"""Actual values on wdata.bits""")
    }
  }

  connectDbgSig(debugCounter(io.enable & dramReady & io.dram.cmd.valid).io.out, "[FRINGE] # DRAM Commands Issued")
  connectDbgSig(debugCounter(io.enable & !cmdArbiter.io.empty & !(dramReady & io.dram.cmd.valid)).io.out, "[FRINGE] Total cycles w/ 1+ cmds queued up")
  connectDbgSig(debugCounter(io.enable & dramReady & io.dram.cmd.valid & !cmdHead.isWr).io.out, "[FRINGE] # Read Commands Sent")
  // Count number of load commands issued from accel per stream
  io.app.loads.zipWithIndex.foreach { case (load, i) =>
    val loadCounter = debugCounter(io.enable & load.cmd.valid)
    val loadCounterHandshake = debugCounter(io.enable & load.cmd.valid & load.cmd.ready)
    connectDbgSig(loadCounterHandshake.io.out, s"[ACCEL] # from load stream $i")
    val signal = s"[FRINGE] # from load stream $i"
    connectDbgSig(debugCounter(io.dram.cmd.valid & dramReady & (cmdArbiter.io.tag === i.U)).io.out, signal)
    connectDbgSig(loadCounter.io.out, s"[ACCEL]  # attempted from Accel load stream (cycles valid) $i")
  }
  connectDbgSig(debugCounter(io.enable & dramReady & io.dram.cmd.valid & cmdHead.isWr).io.out, "# Write Commands Sent")
  // Count number of store commands issued from accel per stream
  io.app.stores.zipWithIndex.foreach { case (store, i) =>
    val storeCounter = debugCounter(io.enable & store.cmd.valid)
    val storeCounterHandshake = debugCounter(io.enable & store.cmd.valid & store.cmd.ready)
    connectDbgSig(storeCounterHandshake.io.out, s"[ACCEL] # from store stream $i")
    val signal = s"[FRINGE] # from store stream ${i}"
    connectDbgSig(debugCounter(io.dram.cmd.valid & dramReady & (cmdArbiter.io.tag === (i+loadStreamInfo.length).U)).io.out, signal)
    connectDbgSig(storeCounter.io.out, s"[ACCEL]  # attempted from Accel store stream (cycles valid) $i")
  }

  connectDbgSig(debugCounter(io.dram.rresp.valid & io.dram.rresp.ready).io.out, "[FRINGE] # Read Responses Acknowledged")
  connectDbgSig(debugCounter(io.enable & io.dram.rresp.valid & !io.dram.rresp.ready).io.out, "[FRINGE] # RResp rejected by ready")
  connectDbgSig(debugCounter(io.enable & !io.dram.rresp.valid & io.dram.rresp.ready).io.out, "[FRINGE] Cycles RResp ready and idle (~valid)")
  loadStreamInfo.indices.foreach{i =>
    val signal = s"[FRINGE]  # from load stream $i"
    connectDbgSig(debugCounter(io.dram.rresp.valid & io.dram.rresp.ready & (rrespTag.streamId === i.U)).io.out, signal)
  }
  connectDbgSig(debugCounter(io.dram.wresp.valid & io.dram.wresp.ready).io.out, "[FRINGE] # Write Responses Acknowledged")
  connectDbgSig(debugCounter(io.enable & io.dram.wresp.valid & !io.dram.wresp.ready).io.out, "[FRINGE] # WResp rejected by ready")
  connectDbgSig(debugCounter(io.enable & !io.dram.wresp.valid & io.dram.wresp.ready).io.out, "[FRINGE] Cycles WResp ready and idle (~valid)")
  storeStreamInfo.indices.foreach{i =>
    val signal = s"[FRINGE]  # from store stream $i"
    connectDbgSig(debugCounter(io.dram.wresp.valid & io.dram.wresp.ready & (wrespTag.streamId === (i+loadStreamInfo.length).U)).io.out, signal)
  }

  denseLoadBuffers.zipWithIndex foreach { case (b,i) =>
    connectDbgSig(debugCounter(b.io.full).io.out, "[FRINGE] (load) fifo converter " + i + " # cycles full")
    connectDbgSig(debugCounter(b.io.almostFull).io.out, "[FRINGE] (load) fifo converter " + i + " # cycles almostFull")
    connectDbgSig(debugCounter(b.io.empty).io.out, "[FRINGE] (load) fifo converter " + i + " # cycles empty")
    connectDbgSig(debugCounter(b.io.almostEmpty).io.out, "[FRINGE] (load) fifo converter " + i + " # cycles almostEmpty")
    connectDbgSig(debugCounter(b.io.enqVld).io.out, "[FRINGE] (load) fifo converter " + i + " # cycles enqVld")
    connectDbgSig(debugCounter(rrespTag.streamId === i.U).io.out, "[FRINGE] (load) fifo converter " + i + " # cycles streamId == " + i)
    connectDbgSig(debugCounter(io.dram.rresp.valid).io.out, "[FRINGE] (load) # cycles rresp == valid")
  }
  connectDbgSig(debugCounter(rrespTag.streamId >= denseLoadBuffers.length.U).io.out, "(load) # cycles streamId >= last")
  connectDbgSig(debugFF(rrespTag.streamId, io.dram.rresp.valid).io.out, "(load) last streamId")
  
  denseStoreBuffers.zipWithIndex.foreach{ case (b,i) =>
    connectDbgSig(debugCounter(b.io.enqVld).io.out, "[FRINGE] (store) fifo converter " + i + " # enq")
    connectDbgSig(debugCounter(!b.io.full).io.out, "[FRINGE] (store) fifo converter " + i + " # cycles ~full (= ready)")
    connectDbgSig(debugCounter(!b.io.full & b.io.enqVld).io.out, s"[FRINGE] (store) fifo converter $i # enq while not full")
    connectDbgSig(debugCounter(b.io.full & b.io.enqVld).io.out, s"[FRINGE] (store) fifo converter $i # enq while full")
    connectDbgSig(debugCounter(b.io.full).io.out, "[FRINGE] (store) fifo converter " + i + " # cycles full")
    connectDbgSig(debugCounter(b.io.almostFull).io.out, "[FRINGE] (store) fifo converter " + i + " # cycles almostFull")
    connectDbgSig(debugCounter(b.io.empty).io.out, "[FRINGE] (store) fifo converter " + i + " # cycles empty")
    connectDbgSig(debugCounter(b.io.almostEmpty).io.out, "[FRINGE] (store) fifo converter " + i + " # cycles almostEmpty")
  }

  connectDbgSig(debugCounter(io.dram.rresp.valid & denseLoadBuffers.map {_.io.enqVld}.reduce{_|_}).io.out, "[FRINGE] RResp valid enqueued somewhere")
  connectDbgSig(debugCounter(io.dram.rresp.valid & io.dram.rresp.ready).io.out, "[FRINGE] Rresp valid and ready")
  connectDbgSig(debugCounter(io.dram.rresp.valid & io.dram.rresp.ready & denseLoadBuffers.map {_.io.enqVld}.reduce{_|_}).io.out, "[FRINGE] Resp valid and ready and enqueued somewhere")
  connectDbgSig(debugCounter(io.dram.rresp.valid & !io.dram.rresp.ready).io.out, "[FRINGE] Resp valid and not ready")

  connectDbgSig(wdataCount.io.out, "[FRINGE] num wdata transferred (wvalid & wready)")

  // Connect AXI loopback debuggers
  // TOP
  connectDbgSig(debugCounter(io.TOP_AXI.ARVALID).io.out, "# cycles TOP ARVALID ")
  connectDbgSig(debugCounter(io.TOP_AXI.ARREADY).io.out, "# cycles TOP ARREADY")
  connectDbgSig(debugCounter(io.TOP_AXI.ARREADY & io.TOP_AXI.ARVALID).io.out, "# cycles TOP ARREADY & ARVALID ")
  connectDbgSig(debugCounter(io.TOP_AXI.AWVALID).io.out, "# cycles TOP AWVALID ")
  connectDbgSig(debugCounter(io.TOP_AXI.AWREADY & io.TOP_AXI.AWVALID).io.out, "# cycles TOP AWREADY & AWVALID ")
  connectDbgSig(debugCounter(io.TOP_AXI.RVALID).io.out, "# cycles TOP RVALID ")
  connectDbgSig(debugCounter(io.TOP_AXI.RREADY & io.TOP_AXI.RVALID).io.out, "# cycles TOP RREADY & RVALID ")
  connectDbgSig(debugCounter(io.TOP_AXI.WVALID).io.out, "# cycles TOP WVALID ")
  connectDbgSig(debugCounter(io.TOP_AXI.WREADY & io.TOP_AXI.WVALID).io.out, "# cycles TOP WREADY & WVALID ")
  connectDbgSig(debugCounter(!io.TOP_AXI.WREADY & io.TOP_AXI.WVALID).io.out, "# cycles TOP ~WREADY & WVALID (forced)" )
  connectDbgSig(debugCounter(!io.TOP_AXI.WREADY).io.out, "# cycles TOP ~WREADY" )
  connectDbgSig(debugCounter(io.TOP_AXI.BVALID).io.out, "# cycles TOP BVALID ")
  connectDbgSig(debugCounter(io.TOP_AXI.BREADY & io.TOP_AXI.BVALID).io.out, "# cycles TOP BREADY & BVALID ")
  connectDbgSig(debugFF(io.TOP_AXI.ARADDR, io.TOP_AXI.ARVALID & io.TOP_AXI.ARREADY).io.out, "Last TOP ARADDR")
  connectDbgSig(debugFF(io.TOP_AXI.ARLEN, io.TOP_AXI.ARVALID & io.TOP_AXI.ARREADY).io.out, "Last TOP ARLEN")
  connectDbgSig(debugFF(io.TOP_AXI.ARSIZE, io.TOP_AXI.ARVALID & io.TOP_AXI.ARREADY).io.out, "Last TOP ARSIZE")
  connectDbgSig(debugFF(io.TOP_AXI.ARID, io.TOP_AXI.ARVALID & io.TOP_AXI.ARREADY).io.out, "Last TOP ARID")
  connectDbgSig(debugFF(io.TOP_AXI.ARBURST, io.TOP_AXI.ARVALID & io.TOP_AXI.ARREADY).io.out, "Last TOP ARBURST")
  // connectDbgSig(debugCounter(io.TOP_AXI.ARLOCK).io.out, "# cycles TOP ARLOCK ")
  connectDbgSig(debugFF(io.TOP_AXI.AWADDR, io.TOP_AXI.AWVALID & io.TOP_AXI.AWREADY).io.out, "Last TOP AWADDR")
  connectDbgSig(debugFF(io.TOP_AXI.AWLEN, io.TOP_AXI.AWVALID & io.TOP_AXI.AWREADY).io.out, "Last TOP AWLEN")
  connectDbgSig(debugFF(io.TOP_AXI.WDATA, io.TOP_AXI.WVALID & io.TOP_AXI.WREADY).io.out, "Last TOP WDATA")
  connectDbgSig(debugFF(io.TOP_AXI.WSTRB, io.TOP_AXI.WVALID & io.TOP_AXI.WREADY).io.out, "Last TOP WSTRB")
  connectDbgSig(debugFF(io.TOP_AXI.WDATA, io.TOP_AXI.WVALID & io.TOP_AXI.WREADY & wdataCount.io.out === 0.U).io.out, "First TOP WDATA")
  connectDbgSig(debugFF(io.TOP_AXI.WSTRB, io.TOP_AXI.WVALID & io.TOP_AXI.WREADY & wdataCount.io.out === 0.U).io.out, "First TOP WSTRB")
  connectDbgSig(debugFF(io.TOP_AXI.WDATA, io.TOP_AXI.WVALID & io.TOP_AXI.WREADY & wdataCount.io.out === 1.U).io.out, "Second TOP WDATA")
  connectDbgSig(debugFF(io.TOP_AXI.WSTRB, io.TOP_AXI.WVALID & io.TOP_AXI.WREADY & wdataCount.io.out === 1.U).io.out, "Second TOP WSTRB")

  // // DWIDTH
  connectDbgSig(debugCounter(io.DWIDTH_AXI.ARVALID).io.out, "# cycles DWIDTH ARVALID ")
  connectDbgSig(debugCounter(io.DWIDTH_AXI.ARREADY).io.out, "# cycles DWIDTH ARREADY ")
  connectDbgSig(debugCounter(io.DWIDTH_AXI.ARREADY & io.DWIDTH_AXI.ARVALID).io.out, "# cycles DWIDTH ARREADY & ARVALID ")
  connectDbgSig(debugCounter(io.DWIDTH_AXI.AWVALID).io.out, "# cycles DWIDTH AWVALID ")
  connectDbgSig(debugCounter(io.DWIDTH_AXI.AWREADY & io.DWIDTH_AXI.AWVALID).io.out, "# cycles DWIDTH ARREADY & AWVALID ")
  connectDbgSig(debugCounter(io.DWIDTH_AXI.RVALID).io.out, "# cycles DWIDTH RVALID ")
  connectDbgSig(debugCounter(io.DWIDTH_AXI.RREADY & io.DWIDTH_AXI.RVALID).io.out, "# cycles DWIDTH RREADY & RVALID ")
  connectDbgSig(debugCounter(io.DWIDTH_AXI.WVALID).io.out, "# cycles DWIDTH WVALID ")
  connectDbgSig(debugCounter(io.DWIDTH_AXI.WREADY & io.DWIDTH_AXI.WVALID).io.out, "# cycles DWIDTH WREADY & WVALID ")
  connectDbgSig(debugCounter(!io.DWIDTH_AXI.WREADY & io.DWIDTH_AXI.WVALID).io.out, "# cycles DWIDTH ~WREADY & WVALID (forced)" )
  connectDbgSig(debugCounter(!io.DWIDTH_AXI.WREADY).io.out, "# cycles DWIDTH ~WREADY" )
  connectDbgSig(debugCounter(io.DWIDTH_AXI.BVALID).io.out, "# cycles DWIDTH BVALID ")
  connectDbgSig(debugCounter(io.DWIDTH_AXI.BREADY & io.DWIDTH_AXI.BVALID).io.out, "# cycles DWIDTH BREADY & BVALID ")
  connectDbgSig(debugFF(io.DWIDTH_AXI.ARADDR, io.DWIDTH_AXI.ARVALID & io.DWIDTH_AXI.ARREADY).io.out, "Last DWIDTH ARADDR")
  connectDbgSig(debugFF(io.DWIDTH_AXI.ARLEN, io.DWIDTH_AXI.ARVALID & io.DWIDTH_AXI.ARREADY).io.out, "Last DWIDTH ARLEN")
  connectDbgSig(debugFF(io.DWIDTH_AXI.ARSIZE, io.DWIDTH_AXI.ARVALID & io.DWIDTH_AXI.ARREADY).io.out, "Last DWIDTH ARSIZE")
  // connectDbgSig(debugFF(io.DWIDTH_AXI.ARID, io.DWIDTH_AXI.ARVALID & io.DWIDTH_AXI.ARREADY).io.out, "Last DWIDTH ARID")
  connectDbgSig(debugFF(io.DWIDTH_AXI.ARBURST, io.DWIDTH_AXI.ARVALID & io.DWIDTH_AXI.ARREADY).io.out, "Last DWIDTH ARBURST")
  // connectDbgSig(debugCounter(io.DWIDTH_AXI.ARLOCK).io.out, "# cycles DWIDTH ARLOCK ")
  connectDbgSig(debugFF(io.DWIDTH_AXI.AWADDR, io.DWIDTH_AXI.AWVALID & io.DWIDTH_AXI.AWREADY).io.out, "Last DWIDTH AWADDR")
  connectDbgSig(debugFF(io.DWIDTH_AXI.AWLEN, io.DWIDTH_AXI.AWVALID & io.DWIDTH_AXI.AWREADY).io.out, "Last DWIDTH AWLEN")
  connectDbgSig(debugFF(io.DWIDTH_AXI.WDATA, io.DWIDTH_AXI.WVALID & io.DWIDTH_AXI.WREADY).io.out, "Last DWIDTH WDATA")
  connectDbgSig(debugFF(io.DWIDTH_AXI.WSTRB, io.DWIDTH_AXI.WVALID & io.DWIDTH_AXI.WREADY).io.out, "Last DWIDTH WSTRB")
  connectDbgSig(debugFF(io.DWIDTH_AXI.WDATA, io.DWIDTH_AXI.WVALID & io.DWIDTH_AXI.WREADY & wdataCount.io.out === 0.U).io.out, "First DWIDTH WDATA")
  connectDbgSig(debugFF(io.DWIDTH_AXI.WSTRB, io.DWIDTH_AXI.WVALID & io.DWIDTH_AXI.WREADY & wdataCount.io.out === 0.U).io.out, "First DWIDTH WSTRB")
  connectDbgSig(debugFF(io.DWIDTH_AXI.WDATA, io.DWIDTH_AXI.WVALID & io.DWIDTH_AXI.WREADY & wdataCount.io.out === 1.U).io.out, "Second DWIDTH WDATA")
  connectDbgSig(debugFF(io.DWIDTH_AXI.WSTRB, io.DWIDTH_AXI.WVALID & io.DWIDTH_AXI.WREADY & wdataCount.io.out === 1.U).io.out, "Second DWIDTH WSTRB")

  // // PROTOCOL
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.ARVALID).io.out, "# cycles PROTOCOL ARVALID ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.ARREADY).io.out, "# cycles PROTOCOL ARREADY ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.ARREADY & io.PROTOCOL_AXI.ARVALID).io.out, "# cycles PROTOCOL ARREADY & ARVALID ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.AWVALID).io.out, "# cycles PROTOCOL AWVALID ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.AWREADY & io.PROTOCOL_AXI.AWVALID).io.out, "# cycles PROTOCOL ARREADY & AWVALID ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.RVALID).io.out, "# cycles PROTOCOL RVALID ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.RREADY & io.PROTOCOL_AXI.RVALID).io.out, "# cycles PROTOCOL RREADY & RVALID ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.WVALID).io.out, "# cycles PROTOCOL WVALID ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.WREADY & io.PROTOCOL_AXI.WVALID).io.out, "# cycles PROTOCOL WREADY & WVALID ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.BVALID).io.out, "# cycles PROTOCOL BVALID ")
  // connectDbgSig(debugCounter(io.PROTOCOL_AXI.BREADY & io.PROTOCOL_AXI.BVALID).io.out, "# cycles PROTOCOL BREADY & BVALID ")
  // connectDbgSig(debugFF(io.PROTOCOL_AXI.ARADDR, io.PROTOCOL_AXI.ARVALID & io.PROTOCOL_AXI.ARREADY).io.out, "Last PROTOCOL ARADDR")
  // connectDbgSig(debugFF(io.PROTOCOL_AXI.ARLEN, io.PROTOCOL_AXI.ARVALID & io.PROTOCOL_AXI.ARREADY).io.out, "Last PROTOCOL ARLEN")
  // connectDbgSig(debugFF(io.PROTOCOL_AXI.ARSIZE, io.PROTOCOL_AXI.ARVALID & io.PROTOCOL_AXI.ARREADY).io.out, "Last PROTOCOL ARSIZE")
  // // connectDbgSig(debugFF(io.PROTOCOL_AXI.ARID, io.PROTOCOL_AXI.ARVALID & io.PROTOCOL_AXI.ARREADY).io.out, "Last PROTOCOL ARID")
  // connectDbgSig(debugFF(io.PROTOCOL_AXI.ARBURST, io.PROTOCOL_AXI.ARVALID & io.PROTOCOL_AXI.ARREADY).io.out, "Last PROTOCOL ARBURST")
  // // connectDbgSig(debugCounter(io.PROTOCOL_AXI.ARLOCK).io.out, "# cycles PROTOCOL ARLOCK ")
  // connectDbgSig(debugFF(io.PROTOCOL_AXI.AWADDR, io.PROTOCOL_AXI.AWVALID & io.PROTOCOL_AXI.AWREADY).io.out, "Last PROTOCOL AWADDR")

  // // Clock converter
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.ARVALID).io.out, "# cycles CLOCKCONVERT ARVALID ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.ARREADY).io.out, "# cycles CLOCKCONVERT ARREADY ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.ARREADY & io.CLOCKCONVERT_AXI.ARVALID).io.out, "# cycles CLOCKCONVERT ARREADY & ARVALID ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.AWVALID).io.out, "# cycles CLOCKCONVERT AWVALID ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.AWREADY & io.CLOCKCONVERT_AXI.AWVALID).io.out, "# cycles CLOCKCONVERT ARREADY & AWVALID ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.RVALID).io.out, "# cycles CLOCKCONVERT RVALID ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.RREADY & io.CLOCKCONVERT_AXI.RVALID).io.out, "# cycles CLOCKCONVERT RREADY & RVALID ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.WVALID).io.out, "# cycles CLOCKCONVERT WVALID ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.WREADY & io.CLOCKCONVERT_AXI.WVALID).io.out, "# cycles CLOCKCONVERT WREADY & WVALID ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.BVALID).io.out, "# cycles CLOCKCONVERT BVALID ")
  // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.BREADY & io.CLOCKCONVERT_AXI.BVALID).io.out, "# cycles CLOCKCONVERT BREADY & BVALID ")
  // connectDbgSig(debugFF(io.CLOCKCONVERT_AXI.ARADDR, io.CLOCKCONVERT_AXI.ARVALID & io.CLOCKCONVERT_AXI.ARREADY).io.out, "Last CLOCKCONVERT ARADDR")
  // connectDbgSig(debugFF(io.CLOCKCONVERT_AXI.ARLEN, io.CLOCKCONVERT_AXI.ARVALID & io.CLOCKCONVERT_AXI.ARREADY).io.out, "Last CLOCKCONVERT ARLEN")
  // connectDbgSig(debugFF(io.CLOCKCONVERT_AXI.ARSIZE, io.CLOCKCONVERT_AXI.ARVALID & io.CLOCKCONVERT_AXI.ARREADY).io.out, "Last CLOCKCONVERT ARSIZE")
  // // connectDbgSig(debugFF(io.CLOCKCONVERT_AXI.ARID, io.CLOCKCONVERT_AXI.ARVALID & io.CLOCKCONVERT_AXI.ARREADY).io.out, "Last CLOCKCONVERT ARID")
  // connectDbgSig(debugFF(io.CLOCKCONVERT_AXI.ARBURST, io.CLOCKCONVERT_AXI.ARVALID & io.CLOCKCONVERT_AXI.ARREADY).io.out, "Last CLOCKCONVERT ARBURST")
  // // connectDbgSig(debugCounter(io.CLOCKCONVERT_AXI.ARLOCK).io.out, "# cycles CLOCKCONVERT ARLOCK ")
  // connectDbgSig(debugFF(io.CLOCKCONVERT_AXI.AWADDR, io.CLOCKCONVERT_AXI.AWVALID & io.CLOCKCONVERT_AXI.AWREADY).io.out, "Last CLOCKCONVERT AWADDR")

  if (isDebugChannel) {
    // Print all debugging signals into a header file
    val debugFileName = "cpp/generated_debugRegs.h"
    val debugPW = new PrintWriter(new File(debugFileName))
    debugPW.println(s"""
  #ifndef __DEBUG_REGS_H__
  #define __DEBUG_REGS_H__

  #define NUM_DEBUG_SIGNALS ${signalLabels.size}

  const char *signalLabels[] = {
  """)

    debugPW.println(signalLabels.map { l => s"""\"${l}\"""" }.mkString(", "))
    debugPW.println("};")
    debugPW.println("#endif // __DEBUG_REGS_H__")
    debugPW.close()
  }
}
