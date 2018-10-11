package fringe.templates.streamarbiter

import chisel3._
import chisel3.util._
import fringe.templates.memory._

class FIFOIO[T <: Data](t: T, depth: Int) extends Bundle {
  val in = Flipped(Decoupled(t.cloneType))
  val out = Decoupled(t.cloneType)
  val count = Output(UInt(log2Ceil(depth + 1).W))

  class Bank[T <: Data] extends Bundle {
    val wdata = Flipped(Valid(t.cloneType))
    val rdata = Valid(t.cloneType)
    override def cloneType: this.type = new Bank().asInstanceOf[this.type]
  }

  val banks = Vec(depth, new Bank)

  override def cloneType: this.type = new FIFOIO(t, depth).asInstanceOf[this.type]
}

class FIFO[T <: Data](t: T, depth: Int, banked: Boolean = false) extends Module {
  assert(isPow2(depth))

  val addrWidth = log2Ceil(depth)

  val io = IO(new FIFOIO(t, depth))

	val enqPtr = Counter(depth)
  val deqPtr = Counter(depth)
  val maybeFull = RegInit(false.B)

  val ptrMatch = enqPtr.value === deqPtr.value
  val empty = ptrMatch && !maybeFull
  val full = ptrMatch && maybeFull
  val writeEn = io.in.valid & io.in.ready
  val readEn = io.out.valid & io.out.ready

  if (t.getWidth == 1 || banked) {
    val m = Module(new FFRAM(t, depth))

		m.io.raddr := deqPtr.value
		m.io.wen := writeEn
		m.io.waddr := enqPtr.value
		m.io.wdata := io.in.bits
		io.out.bits := m.io.rdata

    m.io.banks.zip(io.banks).foreach { case (a, b) =>
      a.wdata := b.wdata
      b.rdata.bits := a.rdata
    }

		List.tabulate(depth) { i =>
			val valid = RegInit(false.B)
			io.banks(i).rdata.valid := valid
      val wen = writeEn & (enqPtr.value === i.U)
      val ren = readEn & (deqPtr.value === i.U)
			when (wen | ren) {
				valid := wen & !ren
			}
		}
  } else {
    val m = Module(new SRAM(t, depth, "VIVADO_SELECT"))

		m.io.raddr := deqPtr.value
		m.io.wen := writeEn
		m.io.waddr := enqPtr.value
		m.io.wdata := io.in.bits
		io.out.bits := m.io.rdata
    m.io.flow := true.B
  }

  when (writeEn) {
    enqPtr.inc()
  }
  when (readEn) {
    deqPtr.inc()
  }
  when (writeEn =/= readEn) {
    maybeFull := writeEn
  }

  io.out.valid := !empty
  io.in.ready := !full

  val ptrDiff = enqPtr.value - deqPtr.value
	io.count := Cat(maybeFull && ptrMatch, ptrDiff)
}

