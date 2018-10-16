package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._

import fringe._
import fringe.globals._

class DRAMArbiter(dramStream: DRAMStream, streamCount: Int) extends Module {
  val io = IO(new Bundle {
    val app = Vec(streamCount, Flipped(dramStream.cloneType))
    val dram = dramStream.cloneType
  })

  val appValids = io.app.map { _.cmd.valid }
  val appId = PriorityEncoder(appValids)
  val appDecoder = UIntToOH(appId)
  val appStream = io.app(appId)

  io.dram <> appStream
  val tag = appStream.cmd.bits.getTag
  tag.streamID := appId
  io.dram.cmd.bits.setTag(tag)

  val rrespTag = io.dram.rresp.bits.getTag
  val wrespTag = io.dram.wresp.bits.getTag

  val cmdIssue = io.dram.cmd.valid & io.dram.cmd.ready
  val writeIssue = io.dram.wdata.valid & io.dram.wdata.ready

  io.app.zipWithIndex.foreach { case (app, i) =>
    app.cmd.ready := cmdIssue & appDecoder(i)
    app.wdata.ready := writeIssue & appDecoder(i)

    app.rresp.valid := io.dram.rresp.valid & (rrespTag.streamID === i.U)
    app.rresp.bits := io.dram.rresp.bits

    app.wresp.valid := io.dram.wresp.valid & (wrespTag.streamID === i.U)
    app.wresp.bits := io.dram.wresp.bits
  }

  io.dram.rresp.ready := Vec(io.app.map { _.rresp.ready })(rrespTag.streamID)
  io.dram.wresp.ready := Vec(io.app.map { _.wresp.ready })(wrespTag.streamID)
}
