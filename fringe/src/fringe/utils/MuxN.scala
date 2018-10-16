package fringe.utils

import chisel3._
import chisel3.util._

import fringe.templates.memory.FringeFF

class MuxN[T<:Data](val t: T, val numInputs: Int) extends Module {
  val numSelectBits = log2Ceil(numInputs)
  val io = IO(new Bundle {
    val ins = Input(Vec(numInputs, t.cloneType))
    val sel = Input(Bits(numSelectBits.W))
    val out = Output(t.cloneType)
  })

  io.out := io.ins(io.sel)
}

class MuxPipe[T<:Data](val t: T, val numInputs: Int) extends Module {
  val logInputs = log2Ceil(numInputs)
  val powInputs = scala.math.pow(2, logInputs).toInt

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val data = Vec(numInputs, t.cloneType)
      val sel = Bits(logInputs.W)
    }))
    val out = Decoupled(new Bundle{
      val data = t.cloneType
      val sel = Bits(logInputs.W)
    })
  })

  /*
  powInputs match {
    case 2 =>
      io.out.bits.data := Mux(io.in.bits.sel, io.in.bits(0), io.in.bits(1))
      io.out.bits.sel := io.in.bits.sel
      io.out.valid := io.in.valid
      io.in.ready := io.out.ready
    case _ =>
      val n = powInputs / 2
      val muxIn = List.fill(2) { Module(new MuxPipe(t, n)) }
      val out = MuxPipe(t, 2)
      out.

      io.out.bits.data = out.bits.data
  }
  */

}

class MuxNReg(val numInputs: Int, w: Int) extends Module {
  val numSelectBits = log2Ceil(numInputs)
  val io = IO(new Bundle {
    val ins = Input(Vec(numInputs, Bits(w.W)))
    val sel = Input(Bits(numSelectBits.W))
    val out = Output(Bits(w.W))
  })

  // Register the inputs
  val ffins = List.tabulate(numInputs) { i =>
    val ff = Module(new FringeFF(UInt(w.W)))
    ff.io.enable := true.B
    ff.io.in := io.ins(i)
    ff
  }

  val ffsel = Module(new FringeFF(UInt(numSelectBits.W)))
  ffsel.io.enable := true.B
  ffsel.io.in := io.sel
  val sel = ffsel.io.out

  val mux = Module(new MuxN(UInt(w.W), numInputs))
  mux.io.ins := Vec.tabulate(numInputs) { i => ffins(i).io.out }
  mux.io.sel := sel

  // Register the output
  val ff = Module(new FringeFF(UInt(w.W)))
  ff.io.enable := true.B
  ff.io.in := mux.io.out
  io.out := ff.io.out
}

