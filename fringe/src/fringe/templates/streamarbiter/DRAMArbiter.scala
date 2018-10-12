package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._
import fringe.globals._

class CommandCounter(val w: Int) extends Module {
  val io = IO(new Bundle {
    val init = Input(UInt(w.W))
    val enable = Input(Bool())
    val stride = Input(UInt(w.W))
    val out = Output(UInt(w.W))
    val done = Output(Bool())
  })

  val valid = RegInit(false.B)
  val count = Reg(UInt(w.W))

  val currentCount = Mux(valid, count, io.init)
  val newCount = currentCount - io.stride
  val underflow = newCount >= currentCount

  when(io.enable) {
    valid := !underflow
    count := newCount
  }

  io.out := count
  io.done := io.enable & underflow
}

class DRAMArbiter(dramStream: DRAMStream, streamCount: Int) extends Module {
  val io = IO(new Bundle {
    val app = Vec(streamCount, Flipped(dramStream.cloneType))
    val dram = dramStream.cloneType
  })

  val appValids = io.app.map { _.cmd.valid }
  val appId = PriorityEncoder(appValids)
  val appDecoder = UIntToOH(appId)
  val appStream = io.app(appId)

  val cmdSizeCounter = Module(new CommandCounter(32))
  val wrespCounter = Module(new CommandCounter(32))

  io.app.zipWithIndex.foreach { case (app, i) =>
    app.cmd.ready := cmdSizeCounter.io.done & appDecoder(i)
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

  val dramCmdIssue = io.dram.cmd.valid & io.dram.cmd.ready

  cmdSizeCounter.io.init := appStream.cmd.bits.size
  cmdSizeCounter.io.enable := dramCmdIssue
  cmdSizeCounter.io.stride := target.maxBurstsPerCmd.U

  io.dram.cmd.bits.size := cmdSizeCounter.io.out

}
