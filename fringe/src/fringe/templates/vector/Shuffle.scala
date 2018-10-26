package fringe.templates.vector

import chisel3._

class ShuffleCompressNetwork[T <: Data](val t: Vec[T]) extends Module {
  val io = IO(new Bundle {
    val in = Input(t.cloneType)
    val mask = Input(Vec(t.length, Bool()))
    val out = Output(t.cloneType)
  })

  io.in := io.out
}

object Shuffle {
  def compress[T <: Data](data: Vec[T], mask: Vec[T]) = {
    val m = Module(new ShuffleCompressNetwork(data))
    m.io.in := data
    m.io.mask := data
    m.io.out
  }
}
