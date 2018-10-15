package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._
import fringe.globals._

class Counter(val w: Int) extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val enable = Input(Bool())
    val stride = Input(UInt(w.W))
    val out = Output(UInt(w.W))
  })

  val count = RegInit(0.U)

  val newCount = count + io.stride

  when(io.reset) {
    count := 0.U
  } .elsewhen(io.enable) {
    count := newCount
  }

  io.out := count
}

/*
class AXICommandSplit(dram: DRAMStream) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(dramStream.cloneType)
    val out = dramStream.cloneType
  })
}

class AXICommandIssue(dram: DRAMStream) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(dramStream.cloneType)
    val out = dramStream.cloneType
  })
}
*/

class DRAMArbiter(dramStream: DRAMStream, streamCount: Int) extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val app = Vec(streamCount, Flipped(dramStream.cloneType))
    val dram = dramStream.cloneType
  })

  val appValids = io.app.map { _.cmd.valid }
  val appId = PriorityEncoder(appValids)
  val appDecoder = UIntToOH(appId)
  val appStream = io.app(appId)

  // split commands if they're larger than AXI supports
  val cmdSizeCounter = Module(new Counter(32))
  // track wdata issues within a split command so we know when to send wlast
  val wdataCounter = Module(new Counter(32))
  // issue write commands only once even if we need to issue multiple wdata
  val writeIssued = RegInit(false.B)

  val dramCmdIssue = io.enable & io.dram.cmd.valid & io.dram.cmd.ready
  val dramWriteIssue = io.enable & io.dram.wdata.valid & io.dram.wdata.ready

  val writeCmd = appStream.cmd.bits.isWr

  val cmdSizeRemaining = appStream.cmd.bits.size - cmdSizeCounter.io.out
  val maxSize = target.maxBurstsPerCmd.U
  // this is the last command within a split command
  val lastCmd = (cmdSizeRemaining < maxSize)
  val cmdSize = Mux(lastCmd, cmdSizeRemaining, maxSize)
  val wlast = dramWriteIssue & (wdataCounter.io.out + 1.U === cmdSize)
  val cmdDone = Mux(writeCmd, wlast, dramCmdIssue)
  when(wlast) {
    writeIssued := false.B
  } .elsewhen(dramCmdIssue & writeCmd) {
    writeIssued := true.B
  }
  // keep commands queued until all split commands have been issued
  // we also need to wait until wlast for write commands
  val appCmdDeq = lastCmd & Mux(writeCmd, wlast, dramCmdIssue)

  io.dram.cmd.valid := appStream.cmd.valid & Mux(writeCmd, !writeIssued, true.B)
  io.dram.cmd.bits.size := cmdSize
  val cmdAddr = DRAMAddress(appStream.cmd.bits.addr + cmdSizeCounter.io.out)
  io.dram.cmd.bits.addr := cmdAddr.burstAddr
  io.dram.cmd.bits.rawAddr := cmdAddr.bits
  io.dram.cmd.bits.isWr := appStream.cmd.bits.isWr
  val cmdTag = Wire(new DRAMTag(appStream.cmd.bits.tag.getWidth))
  cmdTag.uid := appStream.cmd.bits.getTag.uid
  cmdTag.streamID := appId
  // tag only the last burst if we split the command, so we can send only the last wresp to the app
  cmdTag.cmdSplitLast := lastCmd
  io.dram.cmd.bits.tag := cmdTag.asUInt

  cmdSizeCounter.io.reset := appCmdDeq
  cmdSizeCounter.io.enable := cmdDone
  cmdSizeCounter.io.stride := target.maxBurstsPerCmd.U

  wdataCounter.io.reset := wlast
  wdataCounter.io.enable := dramWriteIssue
  wdataCounter.io.stride := 1.U

  val rrespTag = io.dram.rresp.bits.getTag
  val wrespTag = io.dram.wresp.bits.getTag

  io.app.zipWithIndex.foreach { case (app, i) =>
    app.cmd.ready := appCmdDeq & appDecoder(i)
    app.wdata.ready := dramWriteIssue & appDecoder(i)

    app.rresp.valid := io.dram.rresp.valid & rrespTag.streamID === i.U
    app.rresp.bits := io.dram.rresp.bits

    // only pass on the last wresp to the app if we split the write command
    app.wresp.valid := Mux(wrespTag.cmdSplitLast, io.dram.wresp.valid, false.B) &
                       (wrespTag.streamID === i.U)
    app.wresp.bits := io.dram.wresp.bits
  }

  io.dram.wdata.valid := appStream.wdata.valid & writeIssued
  io.dram.wdata.bits.wdata := appStream.wdata.bits.wdata
  io.dram.wdata.bits.wstrb := appStream.wdata.bits.wstrb
  io.dram.wdata.bits.wlast := wlast

  io.dram.rresp.ready := Vec(io.app.map { _.rresp.ready })(rrespTag.streamID)
  // only wait for app acknowledgement on the last wresp if we split the write command
  io.dram.wresp.ready := Mux(
    wrespTag.cmdSplitLast,
    Vec(io.app.map { _.wresp.ready })(wrespTag.streamID),
    true.B
  )

}
