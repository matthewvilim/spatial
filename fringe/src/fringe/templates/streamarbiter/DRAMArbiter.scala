package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._

class DRAMArbiter(dramStream: DRAMStream, streamCount: Int) extends Module {
  val io = IO(new Bundle {
    val app = Vec(streamCount, Flipped(dramStream.cloneType()))
    val dram = dramStream.cloneType()
  })

  val cmdValids = io.in.map { _.cmd.valid }
  val cmdIdx = PriorityEncoder(cmdValids)
  val cmdReadys = UIntToOH(cmdIdx)
  val stream = io.app(cmdIdx)
  io.app.zipWithIndex.foreach { case (app, i) =>
    app.cmd.ready := dram.cmd.ready & cmdReadys(i)
    app.wdata.ready := dram.wdata.ready & cmdReadys(i)

    app.rresp.valid := dram.rresp.valid & dram.rresp.tag.streamId === i.U
    app.rresp.bits := dram.rresp.bits

    app.wresp.valid := dram.wresp.valid & dram.wresp.tag.streamId === i.U
    app.wresp.bits := dram.wresp.bits
  }

  io.dram.cmd.valid := stream.cmd.valid
  io.dram.cmd.bits := stream.cmd.bits
  io.dram.cmd.bits.tag.streamId := cmdIdx

  io.dram.wdata.valid := stream.wdata.valid
  io.dram.wdata.bits := stream.wdata.bits

  io.dram.rresp.ready := Vec(io.app.map { _.rresp.ready })(io.dram.rresp.tag.streamId)
  io.dram.wresp.ready := Vec(io.app.map { _.wresp.ready })(io.dram.wresp.tag.streamId)
}
