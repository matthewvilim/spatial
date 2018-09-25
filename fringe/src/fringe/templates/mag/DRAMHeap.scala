package fringe.templates.mag

import chisel3._
import chisel3.util._

import fringe._
import fringe.utils._

class DRAMHeap (
  val numAlloc: Int
) extends Module {

  val io = IO(new HeapIO(numAlloc))

  val reqIdx = PriorityEncoder(io.req.map { _.valid })
  val req = io.req(reqIdx)

  val delayedReqIdx = getRetimed(reqIdx, 200)
  val delayedReq = getRetimed(req, 200)

  io.resp.zipWithIndex.foreach { case (resp, i) =>
    resp.valid := delayedReqIdx === i.U
    resp.bits.allocDealloc := req.bits.allocDealloc
    resp.bits.addr := delayedReqIdx
  }
}
