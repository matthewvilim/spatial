package fringe

import chisel3._
import chisel3.util.log2Ceil

import fringe.templates.memory.{FringeFF, SRFF}
import fringe.templates.retiming.{RetimeWrapperWithReset, RetimeWrapper}
import fringe.templates.math.FixedPoint

package object utils {

  /** Info that comes from the compiler:
    *
    *            |--------------|--------------|
    *            |   Buffer 0   |   Buffer 1   |
    *            |--------------|--------------|
    * bufferPort         0              1           The buffer port (None for access outside pipeline)
    *                 |x x x|        |x x x|
    *
    *                /       \      /       \
    *
    *              |x x x|x x|     |x x x|x x x|
    * muxPort         0    1          0     1       The ID for the given time multiplexed vector
    *
    *              |( ) O|O O|    |(   )|( ) O|
    * muxOfs        0   2 0 1        0    0  2      Start offset into the time multiplexed vector
    *
    */


  def mux[T<:Data](cond: Bool, op1: T, op2: T): T = Mux(cond, op1, op2)
  def mux[T<:Data](cond: UInt, op1: T, op2: T): T = Mux(cond(0), op1, op2)


  def log2Up(n: Int): Int = if (n < 0) 1 max log2Ceil(1 max (1 + scala.math.abs(n))) else 1 max log2Ceil(1 max n)
  def log2Up(n: BigInt): Int = if (n < 0) log2Ceil((n.abs + 1) max 1) max 1 else log2Ceil(n max 1) max 1
  def log2Up(n: Double): Int = log2Up(n.toInt)

  def roundUpDivide(num: Int, divisor: Int): Int = (num + divisor - 1) / divisor

  def delay(signal: Bool, length: Int): Bool = if (length == 0) signal else {
    val regs = List.tabulate(length){_ => RegInit[Bool](false.B) }
    regs.head := signal
    (length-1 until 0 by -1).foreach{i => regs(i) := regs(i - 1) }
    regs.last
  }

  def delay(signal: UInt, length: Int): UInt = if (length == 0) signal else {
    val regs = List.tabulate(length){_ => RegInit[UInt](0.U) }
    regs.head := signal
    (length-1 until 0 by -1).foreach{i => regs(i) := regs(i - 1) }
    regs.last
  }

  def delay(signal: FixedPoint, length: Int): FixedPoint = if (length == 0) signal else {
    val regs = List.tabulate(length){_ => RegInit[UInt](0.U) }
    regs.head := signal.r
    (length-1 until 0 by -1).foreach{i => regs(i) := regs(i - 1) }

    val out = Wire(new FixedPoint(signal.fmt))
    out.r := regs.last
    out
  }

  def getRetimed[T<:Data](sig: T, delay: Int, en: Bool = true.B, init: Long = 0): T = {
    if (delay == 0) {
      sig
    }
    else {
      if (globals.regression_testing == "1") { // Major hack until someone helps me include the sv file in Driver (https://groups.google.com/forum/#!topic/chisel-users/_wawG_guQgE)
        chisel3.util.ShiftRegister(sig, delay, en)
      }
      else {
        val sr = Module(new RetimeWrapper(sig.getWidth, delay, init))
        sr.io.in := sig.asUInt
        sr.io.flow := en
        sr.io.out.asTypeOf(sig)
      }
    }
  }

  def risingEdge(sig: Bool): Bool = sig & delay(!sig, length =1)

  def streamCatchDone(in_done: Bool, ready: Bool, retime: Int, rr: Bool, reset: Bool): Bool = {
    if (retime.toInt > 0) {
      val done_catch = Module(new SRFF())
      val sr = Module(new RetimeWrapperWithReset(1, retime - 1, 0))
      sr.io.in := done_catch.io.output.data & ready
      sr.io.flow := ready
      done_catch.io.input.asyn_reset := reset
      done_catch.io.input.set := in_done.toBool & ready
      val out = sr.io.out
      val out_overlap = done_catch.io.output.data
      done_catch.io.input.reset := out & out_overlap & ready
      sr.io.rst := out(0) & out_overlap & ready
      out(0) & out_overlap & ready
    } else {
      in_done & ready
    }
  }

  def getFF[T<:Data](sig: T, en: UInt): T = {
    val ff = Module(new FringeFF(sig))
    ff.io.init := 0.U(sig.getWidth.W).asTypeOf(sig)
    ff.io.in := sig
    ff.io.enable := en
    ff.io.out
  }

  def vecWidthConvert[T<:chisel3.core.Data](vec: Vec[T], newW: Int): Vec[UInt] = {
    assert(vec.getWidth % newW == 0)
    val newV = vec.getWidth / newW
    vec.asTypeOf(Vec(newV, Bits(newW.W)))
  }


  def printStackTrace(): Unit = {
    val trace = Thread.currentThread().getStackTrace
    trace.foreach{line => println(line) }
  }

  def vecSlice[T <: chisel3.core.Data](v: Vec[T], start: Int, end: Int) = {
    Vec(for (i <- start to end) yield v(i))
  }

  def vecJoin[T <: chisel3.core.Data](v1: Vec[T], v2: Vec[T]) = {
    Vec(List.tabulate(v1.length + v2.length) { i =>
      if (i < v1.length) v1(i) else v2(i - v1.length)
    })
  }

}
