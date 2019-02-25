package fringe.templates.memory

import chisel3._
import chisel3.util._
import fringe._
import fringe.Ledger._
import fringe.utils._
import fringe.templates.dramarbiter.{FIFO => SFIFO}

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

class SortPipe(val w: Int, val v: Int) extends Module {
  assert(isPow2(v))

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(Vec(v, UInt(w.W))))
    val out = Decoupled(Vec(v, UInt(w.W)))
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

  val stages = List.fill(sortMap.length) { Wire(Valid(Vec(v, UInt(w.W)))) }
  sortMap.zipWithIndex.foreach { case (s, i) =>
    val first = Wire(Valid(Vec(v, UInt(w.W))))
    first.valid := io.in.valid
    first.bits := io.in.bits
    val stageIn = if (i == 0) first else stages(i - 1)
    val stageOut = Wire(Valid(Vec(v, UInt(w.W))))
    stageOut.valid := stageIn.valid
    s.foreach { case (a, b) =>
      val inA = stageIn.bits(a)
      val inB = stageIn.bits(b)
      stageOut.bits(a) := Mux(inA < inB, inA, inB)
      stageOut.bits(b) := Mux(inA < inB, inB, inA)
    }
    stages(i) := getRetimed(stageOut, 1, en)
  }

  io.in.ready := en
  io.out.valid := stages.last.valid
  io.out.bits := stages.last.bits
}

class MergeBufferIO(val ways: Int, val w: Int, val v: Int) extends Bundle {
  val in = Vec(ways, Flipped(Decoupled(Vec(v, UInt(w.W)))))
  val initMerge = Flipped(Valid(Bool()))
  val inBound = Vec(ways, Flipped(Valid(UInt(w.W))))
  val out = Decoupled(Vec(v, UInt(w.W)))
  val outBound = Valid(UInt(w.W))

  override def cloneType = (new MergeBufferIO(ways, w, v)).asInstanceOf[this.type] // See chisel3 bug 358
}

class MergeBufferTwoWay(w: Int, v: Int) extends Module {

  val io = IO(new MergeBufferIO(2, w, v))
  io <> DontCare

  val sortPipe = Module(new SortPipe(w, v))
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

  val buffers = List.fill(2) { val x = Module(new FIFOPeek(Vec(v, UInt(w.W)))); x.io <> DontCare; x }
  buffers.zipWithIndex.foreach { case (b, i) =>
    b.io.in.valid := io.in(i).valid
    b.io.in.bits := io.in(i).bits

    io.in(i).ready := b.io.in.ready
    b.io.in.valid := io.in(i).valid
    b.io.out.ready := headCounter(i).io.done
  }

  val streamCounter = List.fill(2) { Module(new UpDownCounter(w)) }
  streamCounter.zipWithIndex.foreach { case (sc, i) =>
  sc.io <> DontCare
    sc.io.init := io.inBound(i).bits
    sc.io.incDec := false.B
    sc.io.saturate := true.B
    sc.io.reset := io.inBound(i).valid
    sc.io.en := countEn
  }

  val shifters = List.fill(2) { Module(new BarrelShifter(Valid(UInt(w.W)), v)) }
  shifters.zipWithIndex.foreach { case (s, i) =>
    s.io <> DontCare
    s.io.in.zipWithIndex.foreach { case (v, j) =>
      v.valid := buffers(i).io.out.valid
      v.bits := buffers(i).io.out.bits(j)
    }
    s.io.shiftIn.zipWithIndex.foreach { case (v, j) =>
      v.valid := buffers(i).io.peek.valid
      v.bits := buffers(i).io.peek.bits(j)
    }
    s.io.shiftSel := headCounter(i).io.out
  }

  val (rBits, lBits, outBits) = List.tabulate(v) { i =>
    val r = shifters(0).io.out(i)
    val l = shifters(1).io.out(v - i - 1)
    val compare = r.bits < l.bits | initMerge.io.out
    val rBit = Mux(r.valid & l.valid, compare, r.valid)
    val lBit = Mux(r.valid & l.valid, ~compare, l.valid)
    val out = Mux(r.valid & l.valid, Mux(compare, r.bits, l.bits), Mux(r.valid, r.bits, l.bits))
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

  io.out.valid := sortPipe.io.out.valid
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

class MergeBufferFullIO(val ways: Int, val par: Int, val bitWidth: Int, val readers: Int) extends Bundle {
  def this(tup: (Int, Int, Int, Int)) = this(tup._1, tup._2, tup._3, tup._4)

  val input = new Bundle {
    val in_wen = Input(Vec(ways, Vec(par, Bool())))
    val in_data = Input(Vec(ways, Vec(par, UInt(bitWidth.W))))
    val initMerge_wen = Input(Bool())
    val initMerge_data = Input(Bool())
    val inBound_wen = Input(Vec(ways, Bool()))
    val inBound_data = Input(Vec(ways, UInt(bitWidth.W)))
    val out_ren = Input(Vec(readers, Vec(par, Bool())))
  }
  val output = new Bundle {
    val out_data = Output(Vec(par, UInt(bitWidth.W)))
    val empty = Output(Bool())
    val full = Output(Vec(ways, Bool()))
  }
  
  def connectLedger(op: MergeBufferFullIO)(implicit stack: List[KernelHash]): Unit = {
    if (stack.isEmpty) this <> op
    else {
      val cxn = Ledger.lookup(op.hashCode)
      cxn.mergeEnq.foreach{p => input.in_wen(p) := op.input.in_wen(p); input.in_data(p) := op.input.in_data(p)}
      cxn.mergeDeq.foreach{p => input.out_ren(p) := op.input.out_ren(p)}
      cxn.mergeBound.foreach{p => input.inBound_data(p) := op.input.inBound_data(p); input.inBound_wen(p) := op.input.inBound_wen(p)}
      cxn.mergeInit.foreach{p => input.initMerge_data := op.input.initMerge_data; input.initMerge_wen := op.input.initMerge_wen }
      Ledger.substitute(op.hashCode, this.hashCode)
    }
  }

  def connectMergeEnq(lane: Int, data: List[UInt], en: List[Bool])(implicit stack: List[KernelHash]): Unit = {
    input.in_data(lane).zip(data).foreach{case (l,r) => l := r}
    input.in_wen(lane).zip(en).foreach{case (l,r) => l := r}
    Ledger.connectMergeEnq(this.hashCode, lane)
  }
  def connectMergeDeq(lane: Int, en: List[Bool])(implicit stack: List[KernelHash]): Seq[UInt] = {
    input.out_ren(lane).zip(en).foreach{case (l, r) => l := r}
    Ledger.connectMergeDeq(this.hashCode, lane)
    output.out_data.toSeq
  }
  def connectMergeBound(lane: Int, data: UInt, en: Bool)(implicit stack: List[KernelHash]): Unit = {
    input.inBound_data(lane) := data
    input.inBound_wen(lane) := en
    Ledger.connectMergeBound(this.hashCode, lane)
  }
  def connectMergeInit(data: Bool, en: Bool)(implicit stack: List[KernelHash]): Unit = {
    input.initMerge_data := data
    input.initMerge_wen := en
    Ledger.connectMergeInit(this.hashCode, 0)
  }

  override def cloneType = (new MergeBufferFullIO(ways, par, bitWidth, readers)).asInstanceOf[this.type] // See chisel3 bug 358
}

class MergeBuffer(ways: Int, par: Int, bitWidth: Int, readers: Int) extends Module {
  val io = IO(new MergeBufferFullIO(ways, par, bitWidth, readers))

  val mergeBuf = Module(new MergeBufferNWay(ways, bitWidth, par))

  mergeBuf.io.out.ready := io.input.out_ren.asUInt.orR

  mergeBuf.io.in.zipWithIndex.foreach { case (in, i) =>
    in.valid := io.input.in_wen(i).reduce{_|_}
    in.bits := io.input.in_data(i)
  }

  mergeBuf.io.inBound.zipWithIndex.foreach { case (bound, i) =>
    bound.valid := io.input.inBound_wen(i)
    bound.bits := io.input.inBound_data(i)
  }

  mergeBuf.io.initMerge.valid := io.input.initMerge_wen
  mergeBuf.io.initMerge.bits := io.input.initMerge_data

  io.output.out_data := mergeBuf.io.out.bits

  io.output.empty := ~mergeBuf.io.out.valid
  io.output.full.zipWithIndex.foreach { case (f, i) =>
    f := ~mergeBuf.io.in(i).ready
  }
}

