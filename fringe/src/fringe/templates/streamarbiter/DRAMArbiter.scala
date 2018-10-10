package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._

class DRAMArbiter(dramStream: DRAMStream, streamCount: Int) extends Module {
  val io = IO(new Bundle {
    val app = Vec(streamCount, dramStream)
    val dram = dramStream
  })

  val cmdValids = io.in.map { _.cmd.valid }
  val cmdIdx = PriorityEncoder(cmdValids)
  val cmdReadys = UIntToOH(cmdIdx)
  val stream = io.app(cmdIdx)
  io.app.zipWithIndex.foreach { case (d, i) =>
    d.cmd.ready := dram.cmd.ready & cmdReadys(i)
    d.wdata.ready := dram.wdata.ready & cmdReadys(i)

    d.rresp.valid := dram.rresp.valid & dram.rresp.tag.streamId === i.U
    d.rresp.bits := dram.rresp.bits

    d.wresp.valid := dram.wresp.valid & dram.wresp.tag.streamId === i.U
    d.wresp.bits := dram.wresp.bits
  }

  io.dram.cmd.valid := stream.cmd.valid
  io.dram.cmd.bits := stream.cmd.bits
  io.dram.cmd.bits.tag.streamId := cmdIdx
  io.dram.wdata.valid := stream.wdata.valid
  io.dram.wdata.bits := stream.wdata.bits
}
