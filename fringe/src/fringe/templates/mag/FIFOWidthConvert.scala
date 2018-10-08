package fringe.templates.mag

import chisel3._
import chisel3.util.Cat

import chisel3._
import chisel3.util._
import fringe.templates.counters.{UpDownCtr, CounterChainOpcode, CounterChainCore}
import fringe.templates.memory.{SRAM,FFRAM,FringeFF}
import fringe.utils.MuxN
import fringe.utils.log2Up

/** FIFO config register format */
class FIFOOpcode extends Bundle {
  var chainWrite = Bool()
  var chainRead = Bool()

  override def cloneType(): this.type = new FIFOOpcode().asInstanceOf[this.type]
}

class FIFOBaseIO[T <: Data](t: T, depth: Int, v: Int) extends Bundle {
  class BankInterface extends Bundle {
    val wen = Input(Bool())
    val rdata = Output(t)
    val wdata = Input(t)
    val valid = Output(Bool())

    override def cloneType(): this.type = new BankInterface().asInstanceOf[this.type]
  }

  val enq = Input(Vec(v, t))
  val enqVld = Input(Bool())
  val deq = Output(Vec(v, t))
  val deqVld = Input(Bool())
  val full = Output(Bool())
  val empty = Output(Bool())
  val almostFull = Output(Bool())
  val almostEmpty = Output(Bool())
  val config = Input(new FIFOOpcode)
  val fifoSize = Output(UInt(32.W))
  val banks = Vec(depth, Vec(v, new BankInterface))

  override def cloneType(): this.type = new FIFOBaseIO(t, depth, v).asInstanceOf[this.type]
}

// TODO: move to new FIFO class; this one is only used in this file
abstract class FIFOBase[T <: Data](val t: T, val d: Int, val v: Int) extends Module {
  val w = t.getWidth
  val addrWidth = log2Up(d/v)
  val bankSize = d/v

  val io = IO(new FIFOBaseIO(t, bankSize, v))

  // Check for sizes and v
  Predef.assert(d%v == 0, s"Unsupported FIFO size ($d)/banking($v) combination; $d must be a multiple of $v")
  Predef.assert(isPow2(v), s"Unsupported banking number $v; must be a power-of-2")
  Predef.assert(isPow2(d), s"Unsupported FIFO size $d; must be a power-of-2")

  // Create size register
  val sizeUDC = Module(new UpDownCtr(log2Up(d+1)))
  val size = sizeUDC.io.out
  io.fifoSize := size
  val remainingSlots = d.U - size
  val nextRemainingSlots = d.U - sizeUDC.io.nextInc

  val strideInc = Mux(io.config.chainWrite, 1.U, v.U)
  val strideDec = Mux(io.config.chainRead, 1.U, v.U)

  // FIFO is full if #rem elements (d - size) < strideInc
  // FIFO is emty if elements (size) < strideDec
  // FIFO is almostFull if the next enqVld will make it full
  val empty = size < strideDec
  val almostEmpty = sizeUDC.io.nextDec < strideDec
  val full = remainingSlots < strideInc
  val almostFull = nextRemainingSlots < strideInc

  sizeUDC.io.initval := 0.U
  sizeUDC.io.max := d.U
  sizeUDC.io.init := 0.U
  sizeUDC.io.strideInc := strideInc
  sizeUDC.io.strideDec := strideDec
  sizeUDC.io.init := 0.U

  val writeEn = io.enqVld & !full
  val readEn = io.deqVld & !empty
  sizeUDC.io.inc := writeEn
  sizeUDC.io.dec := readEn

  io.empty := empty
  io.almostEmpty := almostEmpty
  io.full := full
  io.almostFull := almostFull
}

class FIFOCounter(override val d: Int, override val v: Int) extends FIFOBase(UInt(1.W), d, v) {
  io.deq.foreach{d => d := ~empty }
}

class FIFOCore[T<:Data](
    override val t: T,
    override val d: Int,
    override val v: Int)
  extends FIFOBase(t, d, v) {

  // Create wptr (tail) counter chain
  val wptrConfig = Wire(CounterChainOpcode(log2Up(scala.math.max(bankSize,v)+1), 2, 0, 0))
  wptrConfig.chain(0) := io.config.chainWrite

  // TODO: Why is this written as a loop?
  (0 until 2).foreach{
    case 1 => // Localaddr: max = bankSize, stride = 1
      val cfg = wptrConfig.counterOpcode(1)
      cfg.max := bankSize.U
      cfg.stride := 1.U
      cfg.maxConst := true.B
      cfg.strideConst := true.B
    case 0 => // Bankaddr: max = v, stride = 1
      val cfg = wptrConfig.counterOpcode(0)
      cfg.max := v.U
      cfg.stride := 1.U
      cfg.maxConst := true.B
      cfg.strideConst := true.B
  }
  val wptr = Module(new CounterChainCore(log2Up(scala.math.max(bankSize,v)+1), 2, 0, 0))
  wptr.io.enable(0) := writeEn & io.config.chainWrite
  wptr.io.enable(1) := writeEn
  wptr.io.config := wptrConfig
  val tailLocalAddr = wptr.io.out(1)
  val tailBankAddr = wptr.io.out(0)


  // Create rptr (head) counter chain
  val rptrConfig = Wire(CounterChainOpcode(log2Up(scala.math.max(bankSize,v)+1), 2, 0, 0))
  rptrConfig.chain(0) := io.config.chainRead
  // TODO: Why is this written as a loop?
  (0 until 2).foreach{
    case 1 => // Localaddr: max = bankSize, stride = 1
      val cfg = rptrConfig.counterOpcode(1)
      cfg.max := bankSize.U
      cfg.stride := 1.U
      cfg.maxConst := true.B
      cfg.strideConst := true.B
    case 0 => // Bankaddr: max = v, stride = 1
      val cfg = rptrConfig.counterOpcode(0)
      cfg.max := v.U
      cfg.stride := 1.U
      cfg.maxConst := true.B
      cfg.strideConst := true.B
  }
  val rptr = Module(new CounterChainCore(log2Up(scala.math.max(bankSize,v)+1), 2, 0, 0))
  rptr.io.enable(0) := readEn & io.config.chainRead
  rptr.io.enable(1) := readEn
  rptr.io.config := rptrConfig
  val headLocalAddr = rptr.io.out(1)
  val nextHeadLocalAddr = Mux(io.config.chainRead, Mux(rptr.io.done(0), rptr.io.next(1), rptr.io.out(1)), rptr.io.next(1))
  val headBankAddr = rptr.io.out(0)
  val nextHeadBankAddr = rptr.io.next(0)

  // Backing SRAM
  val mems = List.fill(v) {
    val sram = Module(new SRAM(t, bankSize, "VIVADO_SELECT"))
    sram.io.flow := true.B
    sram
  }

  val rdata = mems.map(_.io.rdata)

  val wdata = Vec(List.tabulate(v) { i => if (i == 0) io.enq(i) else Mux(io.config.chainWrite, io.enq(0), io.enq(i)) })

  mems.zipWithIndex.foreach { case (m, i) =>
    m.io.raddr := Mux(readEn, nextHeadLocalAddr, headLocalAddr)
    m.io.waddr := tailLocalAddr

    m.io.wdata := wdata(i)
    m.io.wen := Mux(io.config.chainWrite, writeEn & tailBankAddr === i.U, writeEn)

    // Read data output
    val deqData = i match {
      case 0 =>
        val rdata0Mux = Module(new MuxN(t, v))
        val addrFF = Module(new FringeFF(UInt(log2Ceil(v).W)))
        addrFF.io.in := Mux(readEn, nextHeadBankAddr, headBankAddr)
        addrFF.io.enable := true.B

        rdata0Mux.io.ins := rdata
        rdata0Mux.io.sel := Mux(io.config.chainRead, addrFF.io.out, 0.U)
        rdata0Mux.io.out
      case _ =>
        rdata(i)
    }
    io.deq(i) := deqData
  }
}

class FIFOWidthConvertIO(val win: Int, val vin: Int, val wout: Int, val vout: Int) extends Bundle {
  val enq = Input(Vec(vin, Bits(win.W)))
  val enqStrb = Input(UInt(vin.W))
  val enqVld = Input(Bool())
  val deq = Output(Vec(vout, Bits(wout.W)))
  val deqStrb = Output(UInt((wout*vout/8).W))
  val deqVld = Input(Bool())
  val full = Output(Bool())
  val empty = Output(Bool())
  val almostEmpty = Output(Bool())
  val almostFull = Output(Bool())
  val fifoSize = Output(UInt(32.W))

  override def cloneType(): this.type = {
    new FIFOWidthConvertIO(win, vin, wout, vout).asInstanceOf[this.type]
  }
}

/**
 * WidthConverterFIFO: Convert a stream of width w1 bits to
 * a stream of width w2 bits
 * @param win: Input word width (bits)
 * @param vin: Input vector length
 * @param wout: Output word width
 * @param vout: Out vector length
 * @param d: FIFO depth
 */
class FIFOWidthConvert(val win: Int, val vin: Int, val wout: Int, val vout: Int, val d: Int) extends Module {
  val io = IO(new FIFOWidthConvertIO(win, vin, wout, vout))

  def convertVec(inVec: Vec[UInt], outw: Int, outv: Int): Vec[UInt] = {
    // 1. Cat everything
    val unifiedUInt = inVec.reverse.reduce{ Cat(_,_) }

    // 2. Split apart
    val out = Vec(List.tabulate(outv){ i =>
      unifiedUInt(i*outw+outw-1, i*outw)
    })

    out
  }

  def bytify(inVec: UInt, outbytes: Int, win: Int): UInt = {
    assert(win / 8 > 0)
    val boolVec = Vec(List.tabulate(outbytes){i => 
      val id = (i / (win/8)).toInt
      inVec(id)
    }.reverse)

    boolVec.reduce[UInt](Cat(_,_))
  }

  /**
   * Queue is full if no new element of 'inWidth' can be enqueued
   * Queue is empty if no element of 'outWidth' can be dequeued
   */
  val inWidth = win * vin
  val outWidth = wout * vout

  if ((inWidth < outWidth) || (inWidth == outWidth && wout < win)) {
    Predef.assert(outWidth % inWidth == 0, s"ERROR: Width conversion attempted between widths that are not multiples (in: $inWidth, out: $outWidth)")
    val v = outWidth / inWidth
    val fifo = Module(new FIFOCore(UInt(inWidth.W), d, v))
    val fifoStrb = Module(new FIFOCore(UInt(vin.W), d, v))
    val fifoConfig = Wire(new FIFOOpcode)
    fifoConfig.chainWrite := 1.U
    fifoConfig.chainRead := 0.U
    fifo.io.config := fifoConfig
    fifo.io.enq(0) := io.enq.reverse.reduce { Cat(_,_) }
    fifo.io.enqVld := io.enqVld
    fifoStrb.io.config := fifoConfig
    fifoStrb.io.enq(0) := io.enqStrb
    fifoStrb.io.enqVld := io.enqVld
    io.full := fifo.io.full
    io.empty := fifo.io.empty
    io.almostEmpty := fifo.io.almostEmpty
    io.almostFull := fifo.io.almostFull
    io.deq := convertVec(fifo.io.deq, wout, vout)
    io.deqStrb := bytify(fifoStrb.io.deq.reduce(Cat(_,_)), wout*vout/8, win)
    io.fifoSize := fifo.io.fifoSize
    fifo.io.deqVld := io.deqVld
    fifoStrb.io.deqVld := io.deqVld
  }
  else if ((inWidth > outWidth) || (inWidth == outWidth && wout > win)) {
    Predef.assert(inWidth % outWidth == 0, s"ERROR: Width conversion attempted between widths that are not multiples (in: $inWidth, out: $outWidth)")
    val v = inWidth / outWidth
    val fifo = Module(new FIFOCore(UInt(outWidth.W), d, v))
    val fifoStrb = Module(new FIFOCore(UInt(vin.W), d, 1))
    val fifoConfig = Wire(new FIFOOpcode)
    fifoConfig.chainWrite := 0.U
    fifoConfig.chainRead := 1.U
    fifo.io.config := fifoConfig
    fifoStrb.io.config := fifoConfig
    io.full := fifo.io.full
    io.empty := fifo.io.empty
    io.almostEmpty := fifo.io.almostEmpty
    io.almostFull := fifo.io.almostFull
    io.fifoSize := fifo.io.fifoSize

    fifo.io.enq := convertVec(io.enq, outWidth, v)
    fifo.io.enqVld := io.enqVld
    fifoStrb.io.enq(0) := io.enqStrb
    fifoStrb.io.enqVld := io.enqVld


    io.deq := convertVec(Vec(fifo.io.deq(0)), wout, vout)
    io.deqStrb := bytify(fifoStrb.io.deq(0), wout*vout/8, win)
    fifo.io.deqVld := io.deqVld
  } else {
    val fifo = Module(new FIFOCore(UInt(win.W), d, vin))
    val fifoStrb = Module(new FIFOCore(UInt(vin.W), d, 1))
    val fifoConfig = Wire(new FIFOOpcode)
    fifoConfig.chainWrite := 0.U
    fifoConfig.chainRead := 0.U
    fifo.io.config := fifoConfig
    fifoStrb.io.config := fifoConfig
    io.full := fifo.io.full
    io.empty := fifo.io.empty
    io.almostEmpty := fifo.io.almostEmpty
    io.almostFull := fifo.io.almostFull
    io.fifoSize := fifo.io.fifoSize

    fifo.io.enq := io.enq
    fifo.io.enqVld := io.enqVld
    fifoStrb.io.enq(0) := io.enqStrb
    fifoStrb.io.enqVld := io.enqVld

    io.deq := fifo.io.deq
    fifo.io.deqVld := io.deqVld
    io.deqStrb := bytify(fifoStrb.io.deq.reduce[UInt](Cat(_,_)), wout*vout/8, win)
    fifoStrb.io.deqVld := io.deqVld
  }
}