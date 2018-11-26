package fringe.templates.vector

import chisel3._

class ShuffleCompressNetwork[T <: Data](data: Vec[T]) extends Module {
  val io = IO(new Bundle {
    val in = Input(data.cloneType)
    val maskIn = Input(Vec(data.length, Bool()))
    val out = Output(data.cloneType)
    val maskOut = Output(Vec(data.length, Bool()))
  })

  io.out := io.in
}

object Shuffle {
  def compress[T <: Data](data: Vec[T], mask: Vec[Bool]) = {
    val m = Module(new ShuffleCompressNetwork(data))
    m.io.in := data
    m.io.maskIn := mask
    (m.io.out, m.io.maskOut)
  }
}
