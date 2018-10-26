package fringe.templates.vector

import chisel3._

class ShuffleCompressNetwork[T <: Data](data: Vec[T]) extends Module {
  val io = IO(new Bundle {
    val in = Input(data.cloneType)
    val mask = Input(Vec(data.length, Bool()))
    val out = Output(data.cloneType)
  })

  io.in := io.out
}

object Shuffle {
  def compress[T <: Data](data: Vec[T], mask: Vec[Bool]) = {
    val m = Module(new ShuffleCompressNetwork(data))
    m.io.in := data
    m.io.mask := mask
    m.io.out
  }
}
