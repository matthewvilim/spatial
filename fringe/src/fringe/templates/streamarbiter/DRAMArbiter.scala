package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._

class DRAMArbiter(dramStream: DRAMStream, streamCount: Int) extends Module {
  val io = IO(new Bundle {
    val app = Vec(streamCount, Flipped(dramStream.cloneType))
    val dram = dramStream.cloneType
  })

  val appValids = io.app.map { _.cmd.valid }
  val appId = PriorityEncoder(appValids)
  val appDecoder = UIntToOH(appId)
  val appStream = io.app(appId)
  io.app.zipWithIndex.foreach { case (app, i) =>
    app.cmd.ready := io.dram.cmd.ready & appDecoder(i)
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

  val sizeCounter = Module(new FringeCounter(32))
  sizeCounter.io.max := appCmd.size
  val maxBurstsPerCmd = 256
  val maxBytesPerCmd = maxBurstsPerCmd * globals.target.burstSizeBytes
  sizeCounter.io.stride := maxBytesPerCmd.U

  val cmdAddr = Wire(new DRAMAddress)
  cmdAddr.addr := appStream.cmd.bits.addr + sizeCounter.io.out
}
