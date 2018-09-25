package fringe

import chisel3._
import chisel3.util._

class DRAMAllocator(appReqCount: Int) extends Module {
  val io = new Bundle {
    val appReq = Vec(appReqCount, Flipped(Valid(new HeapRequest)))
    val appResp = Valid(new HeapResponse)

    val heapReq = Valid(new HeapRequest)
    val heapResp = Flipped(Valid(new HeapResponse))
  }

  var state = RegInit(new HeapResponse)
  when (io.heapResp.valid) {
    state.allocDealloc := io.heapResp.bits.allocDealloc
    state.addr := io.heapResp.bits.addr
  }
  io.appResp := state

  io.heapReq.valid := io.appReq.valid
  io.heapReq.bits.allocDealloc := io.appReq.bits.allocDealloc
  io.heapReq.bits.addr := io.appReq.bits.addr
  io.heapReq.bits.size := io.appReq.bits.size
}
