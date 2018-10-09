package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._

class DRAMArbiter(dramStream: DRAMStream, streamCount: Int) extends Module {
  val io = IO(new Bundle {
    val streamIn = Vec(streamCount, dramStream)
    val streamOut = dramStream
  })

  val cmdValids = io.in.map { _.cmd.valid }
  val cmdIdx = PriorityEncoder(cmdValids)
  val cmdReadys = UIntToOH(cmdIdx)
  val cmd = io.streamIn(cmdIdx)
  io.streamIn.zipWithIndex.foreach { case (d, i) =>
    d.cmd.ready := streamOut.cmd.ready & cmdReadys(i)
    d.rresp.valid := streamOut.rresp.valid & streamOut.rresp.tag.streamId === i.U
  }

  io.streamOut.cmd.valid := cmd.valid
}
