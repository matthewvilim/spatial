package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._

class DRAMArbiter(dramStream: DRAMStream, streamCount: Int) extends Module {
  val io = IO(new Bundle {
    val app = Vec(streamCount, Flipped(dramStream.cloneType()))
    val dram = dramStream.cloneType()
  })

  val cmdValids = io.app.map { _.cmd.valid }
  val cmdId = PriorityEncoder(cmdValids)
  val cmdDecoder = UIntToOH(cmdId)
  val stream = io.app(cmdId)
  io.app.zipWithIndex.foreach { case (app, i) =>
    app.cmd.ready := io.dram.cmd.ready & cmdDecoder(i)
    app.wdata.ready := io.dram.wdata.ready & cmdDecoder(i)

    app.rresp.valid := io.dram.rresp.valid & io.dram.rresp.bits.tag.streamId === i.U
    app.rresp.bits := io.dram.rresp.bits

    app.wresp.valid := io.dram.wresp.valid & io.dram.wresp.bits.tag.streamId === i.U
    app.wresp.bits := io.dram.wresp.bits
  }

  io.dram.cmd.valid := stream.cmd.valid
  io.dram.cmd.bits := stream.cmd.bits
  io.dram.cmd.bits.tag.streamId := cmdId

  io.dram.wdata.valid := stream.wdata.valid
  io.dram.wdata.bits := stream.wdata.bits

  io.dram.rresp.ready := Vec(io.app.map { _.rresp.ready })(io.dram.rresp.bits.tag.streamId)
  io.dram.wresp.ready := Vec(io.app.map { _.wresp.ready })(io.dram.wresp.bits.tag.streamId)

  val dramCmdIssue = io.dram.cmd.valid & io.dram.cmd.ready

  //val size = Module(new FringeCounter(32.W))
}
