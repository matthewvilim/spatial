package fringe.templates.memory

import chisel3._
import chisel3.util._

import fringe._

class DRAMAllocator(appReqCount: Int) extends Module {
  val io = IO(new Bundle {
    val appReq = Vec(appReqCount, Flipped(Valid(new HeapRequest)))
    val appResp = Output(new HeapResponse)

    val heapReq = Valid(new HeapRequest)
    val heapResp = Flipped(Valid(new HeapResponse))
  })

  val init = Wire(new HeapResponse)
  init.allocDealloc := false.B

  var state = RegInit(init)
  when (io.heapResp.valid) {
    state := io.heapResp.bits
  }
  io.appResp := state

  val reqIdx = PriorityEncoder(io.appReq.map { _.valid })
  io.heapReq.valid := io.appReq.asUInt.orR
  io.heapReq.bits := io.appReq(reqIdx).bits
}
