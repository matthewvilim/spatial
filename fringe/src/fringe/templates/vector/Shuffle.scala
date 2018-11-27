package fringe.templates.vector

import chisel3._

class ShuffleCompressNetwork[T <: Data](data: Vec[T]) extends Module {
  val io = IO(new Bundle {
    val dataIn = Input(data.cloneType)
    val maskIn = Input(Vec(data.length, Bool()))
    val dataOut = Output(data.cloneType)
    val maskOut = Output(Vec(data.length, Bool()))
  })

  def level0(v: Int) = {
    List.range(0, v).sliding(2, 2).map { case List(a, b) => (a, b) }
  }

  def level1(v: Int) = {
    (0, 0) ++ List.range(1, v - 1).sliding(2, 2).map { case List(a, b) => (a, b) } ++ (v - 1, v - 1)
  }

  def compressLevel(data: Vec[T], mask: Vec[Bool]) = {
  }

  val (d, m) = compressLevel(io.dataIn, io.maskIn)

  io.dataOut := d
  io.maskOut := m
}

object Shuffle {
  def compress[T <: Data](data: Vec[T], mask: Vec[Bool]) = {
    val m = Module(new ShuffleCompressNetwork(data))
    m.io.dataIn := data
    m.io.maskIn := mask
    (m.io.dataOut, m.io.maskOut)
  }
}
