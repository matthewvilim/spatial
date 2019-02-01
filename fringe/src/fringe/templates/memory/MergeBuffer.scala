package fringe.templates.memory

import chisel3._
import chisel3.util._
import fringe._
import fringe.utils._
import fringe.templates.dramarbiter.{FIFO => SFIFO}

class KeyValue(val w: Int) extends Bundle {
  val value = UInt((w/2).W)
  val key = UInt((w/2).W)

  def <(that: KeyValue) = this.value < that.value
}

class UpDownCounter(val w: Int) extends Module {
  val io = IO(new Bundle {
    val reset = Input(Bool())
    val init = Input(UInt(w.W))
    val incDec = Input(Bool())
    val saturate = Input(Bool())
    val stride = Input(UInt((w + 1).W))
    val en = Input(Bool())
    val out = Output(UInt(w.W))
    val done = Output(Bool())
  })

  val en = io.en | io.reset

  val counter = Module(new FringeFF(UInt(w.W)))
  counter.io <> DontCare
  counter.io.reset := false.B
  counter.io.enable := en

  val count = Mux(io.incDec, counter.io.out + io.stride, counter.io.out - io.stride)(w - 1,0)
  val overflow = Mux(io.incDec, count < counter.io.out, count > counter.io.out) | io.stride(w)
  val newCount = Mux(overflow & io.saturate, counter.io.out, count)
  counter.io.in := Mux(io.reset, io.init, newCount)
  io.out := counter.io.out

  io.done := overflow & en
}

class BarrelShifter[T<:Data](val t: T, val v: Int) extends Module {
  assert(isPow2(v))
  val shiftW = log2Ceil(v + 1)

  val io = IO(new Bundle {
    val in = Input(Vec(v, t.cloneType))
    val shiftIn = Input(Vec(v, t.cloneType))
    val shiftSel = Input(Bits(shiftW.W))
    val out = Output(Vec(v, t.cloneType))
  })

  def barrelShifter(in: Vec[T], shiftSel: Bits, shift: Int): Vec[T] = {
    val out = VecInit(List.tabulate(in.length) { i =>
      val shiftIndex = i + shift
      val a = if (shiftIndex >= in.length) in(in.length - 1) else in(shiftIndex)
      val s = shiftSel(0)
      Mux(s, a, in(i))
    })
    if (log2Ceil(shift + 1) == shiftW) out else barrelShifter(out, shiftSel(shiftSel.getWidth - 1, 1), shift * 2)
  }

  val out = barrelShifter(vecJoin(io.in, io.shiftIn), io.shiftSel, 1)
  io.out := vecSlice(out, 0, v - 1)
}

class FIFOPeek[T<:Data](val t: T) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(t.cloneType))
    val peek = Valid(t.cloneType)
    val out = Decoupled(t.cloneType)
  })
  io <> DontCare

  val fifo = Module(new SFIFO(t.cloneType, 128))
  fifo.io <> DontCare

  val ff = Module(new FringeFF(Valid(t.cloneType)))
  ff.io <> DontCare

  val deq = ~ff.io.out.valid | io.out.ready
  ff.io.enable := deq
  ff.io.in.valid := fifo.io.out.valid
  ff.io.in.bits := fifo.io.out.bits

  io.in.ready := fifo.io.in.ready

  fifo.io.in.valid := io.in.valid
  fifo.io.out.ready := deq
  fifo.io.in.bits := io.in.bits

  io.peek.valid := fifo.io.out.valid
  io.peek.bits := fifo.io.out.bits

  io.out.valid := ff.io.out.valid
  io.out.bits := ff.io.out.bits
}

class SortPipe[T<:KeyValue](val t: T, val v: Int) extends Module {
  assert(isPow2(v))

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(v, Valid(t.cloneType))))
    val out = Decoupled(Vec(v, Valid(t.cloneType)))
  })

  def sortNetworkMap(width: Int) = {
    def bitonicSort(in: List[Int]): List[List[Tuple2[Int, Int]]] = {
      if (in.length == 2) List(List((in(0), in(1)))) else {
        val (l, h) = in.splitAt(in.length / 2)
        val s = bitonicSort(l).zip(bitonicSort(h)).map { case (l, h) => l ::: h }
        l.zip(h) :: s
      }
    }

    def bitonicMerge(in: List[Int]): List[List[Tuple2[Int, Int]]] = {
      if (in.length == 2) bitonicSort(in) else {
        val (l, h) = in.splitAt(in.length / 2)
        val s = bitonicSort(l).zip(bitonicSort(h)).map { case (l, h) => l ::: h }
        l.zip(h.reverse) :: s
      }
    }

    def sort(in: List[Int]): List[List[Tuple2[Int, Int]]] = {
      if (in.length == 2) bitonicMerge(in) else {
        val (l, h) = in.splitAt(in.length / 2)
        val s = sort(l).zip(sort(h)).map { case (l, h) => l ::: h }
        s ::: bitonicMerge(in)
      }
    }

    sort(List.range(0, width))
  }

  val sortMap = sortNetworkMap(v)

  val en = io.out.ready | ~io.out.valid

  val stages = List.fill(sortMap.length) { Wire(Vec(v, Valid(t.cloneType))) }
  sortMap.zipWithIndex.foreach { case (s, i) =>
    val stageIn = if (i == 0) io.in.bits else stages(i - 1)
    val stageOut = Wire(Vec(v, Valid(t.cloneType)))
    s.foreach { case (a, b) =>
      val inA = stageIn(a)
      val inB = stageIn(b)
      val comp = (inA.bits < inB.bits) & inB.valid
      stageOut(a) := Mux(comp, inA, inB)
      stageOut(b) := Mux(comp, inB, inA)
    }
    stages(i) := getRetimed(stageOut, 1, en)
  }

  io.out.valid := getRetimed(io.in.valid, stages.length, en)
  io.in.ready := en
  io.out.bits := stages.last
}

class MergeBufferIO(val ways: Int, val w: Int, val v: Int) extends Bundle {
  class DecoupledVec extends Bundle {
    val ready = Input(Bool())
    val bits = Vec(v, Valid(new KeyValue(w)))
    def valid = bits.map { _.valid }.reduce { _|_ }
  }

  val in = Vec(ways, Flipped(new DecoupledVec))
  val initMerge = Flipped(Valid(Bool()))
  val inBound = Vec(ways, Flipped(Valid(UInt(32.W))))
  val out = new DecoupledVec
  val outBound = Valid(UInt(32.W))
}

class MergeBufferTwoWay(w: Int, v: Int) extends Module {

  val io = IO(new MergeBufferIO(2, w, v))
  io <> DontCare

  val sortPipe = Module(new SortPipe(new KeyValue(w), v))
  sortPipe.io <> DontCare
  sortPipe.io.out.ready := io.out.ready

  val countEn = sortPipe.io.in.valid & sortPipe.io.in.ready

  val counterW = log2Ceil(v)
  val headCounter = List.fill(2) { Module(new UpDownCounter(counterW)) }
  headCounter.zipWithIndex.foreach { case (hc, i) =>
    hc.io <> DontCare
    hc.io.init := 0.U
    hc.io.incDec := true.B
    hc.io.en := countEn
  }

  val initMerge = Module(new FringeFF(Bool()))
  initMerge.io <> DontCare
  initMerge.io.enable := io.initMerge.valid
  initMerge.io.in := io.initMerge.bits

  val buffers = List.fill(2) { val x = Module(new FIFOPeek(Vec(v, Valid(new KeyValue(w))))); x.io <> DontCare; x }
  buffers.zipWithIndex.foreach { case (b, i) =>
    b.io.in.valid := io.in(i).valid
    b.io.in.bits := io.in(i).bits
    io.in(i).ready := b.io.in.ready

    b.io.out.ready := headCounter(i).io.done
  }

  val streamCounter = List.fill(2) { Module(new UpDownCounter(32)) }
  streamCounter.zipWithIndex.foreach { case (sc, i) =>
    sc.io <> DontCare
    sc.io.init := io.inBound(i).bits
    sc.io.incDec := false.B
    sc.io.saturate := true.B
    sc.io.reset := io.inBound(i).valid
    sc.io.en := countEn
  }

  val shifters = List.fill(2) { Module(new BarrelShifter(Valid(new KeyValue(w)), v)) }
  shifters.zipWithIndex.foreach { case (s, i) =>
    s.io <> DontCare
    s.io.in.zipWithIndex.foreach { case (v, j) =>
      v.valid := buffers(i).io.out.valid & buffers(i).io.out.bits(j).valid
      v.bits := buffers(i).io.out.bits(j).bits
    }
    s.io.shiftIn.zipWithIndex.foreach { case (v, j) =>
      v.valid := buffers(i).io.peek.valid & buffers(i).io.peek.bits(j).valid
      v.bits := buffers(i).io.peek.bits(j).bits
    }
    s.io.shiftSel := headCounter(i).io.out
  }

  val (rBits, lBits, outBits) = List.tabulate(v) { i =>
    val r = shifters(0).io.out(i)
    val l = shifters(1).io.out(v - i - 1)
    val compare = r.bits < l.bits | initMerge.io.out
    val rBit = Mux(r.valid & l.valid, compare, r.valid)
    val lBit = Mux(r.valid & l.valid, ~compare, l.valid)
    val out = Mux(r.valid & l.valid, Mux(compare, r, l), Mux(r.valid, r, l))
    (rBit, lBit, out)
  }.unzip3

  val rValid = VecInit(rBits).asUInt
  val lValid = VecInit(lBits).asUInt
  headCounter(0).io.stride := PopCount(rValid)
  headCounter(1).io.stride := PopCount(lValid)
  streamCounter(0).io.stride := PopCount(rValid)
  streamCounter(1).io.stride := PopCount(lValid)

  val oneSided = ((rValid | lValid).andR & streamCounter.map { _.io.out < v.U }.reduce { _|_ })
  sortPipe.io.in.valid := oneSided | shifters.map { s => s.io.out.map { _.valid }.reduce { _&_ } }.reduce { _&_ }

  sortPipe.io.in.bits := VecInit(outBits)

  io.outBound.valid := io.inBound.map { _.valid }.reduce{ _|_ }
  io.outBound.bits := io.inBound.map { _.bits }.reduce{ _+_ }

  //io.out.valid := sortPipe.io.out.valid
  io.out.bits := sortPipe.io.out.bits
}

class MergeBufferNWay(ways: Int, w: Int, v: Int) extends Module {
  assert(isPow2(ways))

  val io = IO(new MergeBufferIO(ways, w, v))

  ways match {
    case 2 => {
      val m = Module(new MergeBufferTwoWay(w, v))
      m.io.in <> io.in
      m.io.initMerge := io.initMerge
      m.io.inBound := io.inBound
      io.out <> m.io.out
      io.outBound := m.io.outBound
    }
    case _ => {
      val twoWay = Module(new MergeBufferTwoWay(w, v))
      twoWay.io.initMerge := io.initMerge
      val mergeBuffers = List.tabulate(2) { i =>
        val n = ways / 2
        val nWay = Module(new MergeBufferNWay(n, w, v))
        nWay.io.in.zipWithIndex.foreach { case (in, j) =>
          in <> io.in(i * n + j)
        }
        nWay.io.initMerge := io.initMerge
        nWay.io.inBound.zipWithIndex.foreach { case (inBound, j) =>
          inBound := io.inBound(i * n + j)
        }

        twoWay.io.in(i) <> nWay.io.out
        twoWay.io.inBound(i) := nWay.io.outBound
      }
      io.out <> twoWay.io.out
      io.outBound := twoWay.io.outBound
    }
  }
}

class MergeBuffer(ways: Int, par: Int, bitWidth: Int, readers: Int) extends Module {
  val io = IO(new Bundle {
    val in_wen = Input(Vec(ways, Vec(par, Bool())))
    val in_data = Input(Vec(ways, Vec(par, UInt(bitWidth.W))))
    val initMerge_wen = Input(Bool())
    val initMerge_data = Input(Bool())
    val inBound_wen = Input(Vec(ways, UInt(bitWidth.W)))
    val inBound_data = Input(Vec(ways, UInt(bitWidth.W)))
    val out_ren = Input(Vec(readers, Vec(par, Bool())))
    val out_data = Output(Vec(par, UInt(bitWidth.W)))

    val empty = Output(Bool())
    val full = Output(Vec(ways, Bool()))
  })

  val mergeBuf = Module(new MergeBufferNWay(ways, bitWidth, par))

  mergeBuf.io.out.ready := io.out_ren.asUInt.orR

  mergeBuf.io.in.zipWithIndex.foreach { case (wayIn, i) =>
    wayIn.bits.zipWithIndex.foreach { case (v, j) =>
      v.valid := io.in_wen(i)(j)
      v.bits := io.in_data(i)(j).asTypeOf(v.bits)
    }
  }

  mergeBuf.io.inBound.zipWithIndex.foreach { case (bound, i) =>
    bound.valid := io.inBound_wen(i)
    bound.bits := io.inBound_data(i)
  }

  mergeBuf.io.initMerge.valid := io.initMerge_wen
  mergeBuf.io.initMerge.bits := io.initMerge_data

  io.out_data.zipWithIndex.foreach { case (out, i) =>
    out := mergeBuf.io.out.bits(i).bits.asTypeOf(out)
  }

  io.empty := ~mergeBuf.io.out.valid
  io.full.zipWithIndex.foreach { case (f, i) =>
    f := ~mergeBuf.io.in(i).ready
  }
}

