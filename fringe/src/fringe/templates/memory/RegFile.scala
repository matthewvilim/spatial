package fringe.templates.memory

import chisel3._
import chisel3.util._
import fringe.utils.{log2Up, MuxN}
import fringe.globals

class RegFileIO(val w: Int, val numArgIns: Int = 0, val numArgOuts: Int = 0,
                val numArgIOs: Int = 0, val numDebugReg: Int = 0) extends Module {

  object RegType extends Enumeration {
    type RegType = Value
    val status, command, heapStatus, heapCommand, in, io, out, debug = Value
  }

  val reservedReg = List((RegType.status, 0), (RegType.command, 1),
                         (RegType.heapStatus, 2), (RegType.heapCommand, 3))
  val regLayout = reservedReg ++
                  List.tabulate(numArgIns) { i => (RegType.in, i) } ++
                  List.tabulate(numArgIOs) { i => (RegType.io, i) } ++
                  List.tabulate(numArgOuts) { i => (RegType.out, i) } ++
                  List.tabulate(numDebugReg) { i => (RegType.debug, i) }
  val regCount = regLayout.size

  val addrWidth = globals.target.regFileAddrWidth(regCount)

  val io = IO(new Bundle {
    val raddr = Input(UInt(addrWidth.W))
    val wen  = Input(Bool())
    val waddr = Input(UInt(addrWidth.W))
    val wdata = Input(Bits(w.W))
    val rdata = Output(Bits(w.W))
    val reset = Input(Bool())
    val argIns = Output(Vec(numArgIns, Bits(w.W)))
    val argOuts = Vec(numArgOuts, Flipped(Valid(Bits(w.W))))
    val argOutLoopbacks = Output(Vec(numArgOuts, Bits(w.W)))

    val debug = Vec(numDebugs, Flipped(Valid(Bits(w.W))))

    val outStatus = Flipped(Valid(Bits(w.W)))
    val inStatus = Output(Bits(w.W))

    val outCommand = Flipped(Valid(Bits(w.W)))
    val inCommand = Output(Bits(w.W))

    val outHeapCmdStatus = Flipped(Valid(Bits(w.W)))
    val inHeapCmdStatus = Output(Bits(w.W))

    val outHeapArgResp = Flipped(Valid(Bits(w.W)))
    val inHeapArgResp = Output(Bits(w.W))
  })

  val regs = List.tabulate(regCount) { i =>
    val ff = Module(new FringeFF(Bits(w.W)))
    val addrId = if (globals.target.isInstanceOf[fringe.targets.zcu.ZCU]) i*2 else i

    val (regType, regId) = regLayout(i)

    val addrWr = io.wen & (io.waddr === addrId.U(addrWidth.W))
    ff.io.init := 0.U

    regType match {
      case RegType.status | RegType.command | RegType.heapStatus | RegType.heapCommand =>
        val (in, out) = regType match {
          case RegType.status =>
            (io.inStatus, io.outStatus)
          case RegType.command =>
            (io.inCommand, io.outCommand)
          case RegType.heapStatus =>
            (io.inHeapCmdStatus, io.outHeapCmdStatus)
          case RegType.heapCommand =>
            (io.inHeapArgResp, io.outHeapArgResp)
          case => _
        }

        ff.io.enable := addrWr | out.valid
        ff.io.in := Mux(addrWr, io.wdata, out.bits)
        ff.reset := reset.toBool
        ff.io.reset := reset.toBool
        in := ff.io.out
      case RegType.in =>
        ff.io.enable := addrWr
        ff.io.in := io.wdata
        ff.reset := reset.toBool
        ff.io.reset := reset.toBool
        io.argIns(regId) := ff.io.out
      case RegType.out =>
        ff.io.enable := addrWr | io.argOuts(regId).valid
        ff.io.in := Mux(addrWr, io.wdata, io.argOuts(regId).bits)
        ff.reset := reset.toBool
        ff.io.reset := reset.toBool
        io.argOutLoopbacks(regId) := ff.io.out
      case RegType.debug =>
        ff.io.enable := addrWr | io.debug(regId).valid
        ff.io.in := io.debug(regId).bits
        ff.reset := reset.toBool
        ff.io.reset := reset.toBool
    }

    ff
  }

  val readMux = Module(new MuxN(Bits(w.W), regCount))
  readMux.io.ins := Vec(regs.map { _.io.out })
  if (globals.target.isInstanceOf[fringe.targets.zcu.ZCU]) {
    readMux.io.sel := io.raddr / 2.U(addrWidth.W)
    io.rdata := readMux.io.out
  } else if (globals.target.isInstanceOf[fringe.targets.zcu.ZCU]) {
    // Use MSB of addr to read either lower or upper 32 bits.  Bridge gives true addr bits 18:2
    readMux.io.sel := io.raddr & Cat(Fill(15, true.B), false.B, Fill(16, true.B))
    io.rdata := Mux(io.raddr(16), readMux.io.out(63,32), readMux.io.out(31,0))
  } else {
    readMux.io.sel := io.raddr
    io.rdata := readMux.io.out
  }
}

class RegFilePure[T <: Data](val t: T, val d: Int) extends Module {
  val addrWidth = log2Up(d)

  val io = IO(new Bundle {
    val raddr = Input(UInt(addrWidth.W))
    val wen  = Input(Bool())
    val waddr = Input(UInt(addrWidth.W))
    val wdata = Input(t)
    val rdata = Output(t)
  })

  val regs = List.tabulate(d) { i =>
    val ff = Module(new FringeFF(t))
    ff.io.in := io.wdata
    ff.io.enable := io.wen & (io.waddr === i.U)
    ff.io.init := (0.U).asTypeOf(t)
    ff
  }

  val rport = Module(new MuxN(t, d))
  val regOuts = Vec(regs.map{_.io.out})
  rport.io.ins := regOuts
  rport.io.sel := io.raddr
  io.rdata := rport.io.out
}
