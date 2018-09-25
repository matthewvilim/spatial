package fringe.templates.memory

import chisel3._
import chisel3.util._

import fringe._

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

  val reqIdx = PriorityEncoder(io.appReq.map { _.valid })

  io.heapReq.valid := io.appReq.asUInt.orR
  io.heapReq.bits.allocDealloc := io.appReq(reqIdx).bits.allocDealloc
  io.heapReq.bits.addr := io.appReq(reqIdx).bits.addr
  io.heapReq.bits.size := io.appReq(reqIdx).bits.size
}
