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
    val done = Output(Bool())
  })

  val count = Reg(UInt(w.W))

  val newCount = count + io.stride
  val overflow = newCount < count

  when(io.reset) {
    count := 0.U
  } .elsewhen(io.enable) {
    count := newCount
  }

  io.out := count
  io.done := io.enable & overflow
}

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

  val cmdSizeCounter = Module(new Counter(32))
  val wdataCounter = Module(new Counter(32))

  val dramCmdIssue = io.enable & io.dram.cmd.valid & io.dram.cmd.ready
  val dramWriteIssue = io.enable & io.dram.wdata.valid & io.dram.wdata.ready

  val writeCmd = appStream.cmd.bits.isWr

  val cmdSizeRemaining = appStream.cmd.bits.size - cmdSizeCounter.io.out
  val maxSize = target.maxBurstsPerCmd.U
  val wlast = (wdataCounter.io.out === io.dram.cmd.bits.size) & dramWriteIssue
  val appCmdDone = Mux(writeCmd, wlast, true.B) & (cmdSizeRemaining < maxSize)
  io.dram.cmd.bits.size := Mux(appCmdDone, cmdSizeRemaining, maxSize)

  // tag only the last burst if we split the write command
  io.dram.cmd.bits.tag.wresp := writeCmd & appCmdDone

  cmdSizeCounter.io.reset := appCmdDone
  cmdSizeCounter.io.enable := dramCmdIssue
  cmdSizeCounter.io.stride := target.maxBurstsPerCmd.U

  wdataCounter.io.reset := wlast
  wdataCounter.io.enable := dramWriteIssue
  wdataCounter.io.stride := 1.U

  io.dram.wdata.bits.wlast := wlast
  io.app.zipWithIndex.foreach { case (app, i) =>
    app.cmd.ready := appCmdDone & appDecoder(i)
    app.wdata.ready := io.dram.wdata.ready & appDecoder(i)

    app.rresp.valid := io.dram.rresp.valid & io.dram.rresp.bits.tag.streamId === i.U
    app.rresp.bits := io.dram.rresp.bits

    app.wresp.valid := io.dram.wresp.valid & io.dram.wresp.bits.tag.streamId === i.U
    app.wresp.bits := io.dram.wresp.bits
  }

  io.dram.cmd.valid := appStream.cmd.valid
  io.dram.cmd.bits := appStream.cmd.bits
  io.dram.cmd.bits.tag.streamId := appId

  io.dram.wdata.valid := appStream.wdata.valid
  io.dram.wdata.bits := appStream.wdata.bits

  io.dram.rresp.ready := Vec(io.app.map { _.rresp.ready })(io.dram.rresp.bits.tag.streamId)
  io.dram.wresp.ready := Vec(io.app.map { _.wresp.ready })(io.dram.wresp.bits.tag.streamId)

}
