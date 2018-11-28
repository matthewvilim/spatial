package fringe.templates.vector

import chisel3._
import chisel3.util._

import fringe._
import fringe.utils._

class ShuffleCompressNetwork[T <: Data](t: T, v: Int) extends Module {
  class Data extends Bundle {
    val d = t.cloneType
    val m = Bool()
  }

  val io = IO(new Bundle {
    val in = Input(Vec(v, new Data))
    val out = Output(Vec(v, new Data))
  })

  def levelEven(v: Int) = {
    List.range(0, v).sliding(2, 2).map { case List(a, b) => (a, b) }
  }

  def levelOdd(v: Int) = {
    (0, 0) +: List.range(1, v - 1).sliding(2, 2).map { case List(a, b) => (a, b) }.toList :+ (v - 1, v - 1)
  }

  
  val stages = List.fill(v) { Wire(Vec(v, new Data)) }

  val stageMap = List.tabulate(v) { i => if (i % 2 == 0) levelEven(v) else levelOdd(v) }
  stageMap.zipWithIndex.foreach { case (stage, i) =>
    val in = if (i == 0) io.in else stages(i - 1)
    val out = Vec(stage.flatMap { case (a, b) =>
      val s = in(a).m
      if (a == b) List(in(a)) else List(
        Mux(s, in(a), in(b)),
        Mux(s, in(b), in(a))
      )
    }.toList)
    stages(i) := getRetimed(out, 1)
  }

  io.out := stages.last
}

object Shuffle {
  def compress[T <: Data](data: Vec[T], mask: Vec[Bool]) = {
    val m = Module(new ShuffleCompressNetwork(data(0), data.length))
    m.io.in.zipWithIndex.foreach { case(in, i) =>
      in.d := data(i)
      in.m := mask(i)
    }
    (Vec(m.io.out.map { _.d }), Vec(m.io.out.map { _.d }))
  }
}
