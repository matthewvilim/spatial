package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._
import fringe.templates.memory._
import fringe.utils.log2Up

class FIFOVec[T <: Data](t: T, depth: Int, v: Int) extends Module {
  assert(isPow2(depth))
  assert(isPow2(v))
  assert(depth % v == 0)

  val addrWidth = log2Up(depth)

  val io = IO(new Bundle {
		val in = Flipped(Decoupled(Vec(v, t)))
		val out = Decoupled(Vec(v, t))
		val chainEnq = Input(Bool())
		val chainDeq = Input(Bool())
	})

	val enqCounter = Counter(v)
  when (io.in.valid & io.in.ready & io.chainEnq) {
    enqCounter.inc()
  }
	val deqCounter = Counter(v)
  when (io.out.valid & io.out.ready & io.chainDeq) {
    deqCounter.inc()
  }
	val enqDecoder = UIntToOH(enqCounter.value)
	val deqDecoder = UIntToOH(deqCounter.value)

	val d = depth / v
	val fifos = List.tabulate(v) { i =>
		val m = Module(new FIFO(t, d))
		m.io.in.valid := Mux(io.chainEnq, enqDecoder(i), true.B) & io.in.valid
		m.io.in.bits := Mux(io.chainEnq, io.in.bits(0), io.in.bits(i))
		m.io.out.ready := Mux(io.chainDeq, deqDecoder(i), true.B) & io.out.ready
    m
	}

	val inReady = fifos.map { _.io.in.ready }
	io.in.ready := Mux(io.chainEnq, Vec(inReady)(enqCounter.value), inReady.reduce { _&_ })
	val outValid = fifos.map { _.io.out.valid }
	io.out.valid := Mux(io.chainDeq, Vec(outValid)(deqCounter.value), outValid.reduce { _&_ })
	val outBits = fifos.map { _.io.out.bits }
	io.out.bits := Mux(io.chainDeq, Vec(outBits)(deqCounter.value), Vec(outBits))
}
