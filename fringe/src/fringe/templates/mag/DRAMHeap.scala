package fringe

import chisel3._
import chisel3.util._

class DRAMHeap (
  val allocatorCount: Int
) extends Module {

  val io = IO(new HeapIO(allocatorCount))

  val reqIdx = PriorityEncoder(io.appReq.map { _.valid })
  val req = io.appReq(reqIdx)

  val delayedReqIdx = getRetimed(reqIdx, 20)
  val delayedReq = getRetimed(req, 20)

  io.appResp.zipWithIndex.foreach { case (resp, i) =>
    resp.valid := delayedReqIdx === i.U
    resp.bits.alloc := req.bits.alloc
    resp.bits.dealloc := req.bits.dealloc
    resp.bits.addr := delayedReqIdx
  }
}
