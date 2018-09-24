package fringe.templates.memory

import chisel3._
import chisel3.util._
import fringe.utils.{log2Up, MuxN}
import fringe.globals

/** Register file parameterized by width and height similar to SRAM
  * @param w: Word width
  * @param d: Number of registers
  * @param numArgIns: Number of 'argin' registers that can be read in parallel
  * @param numArgOuts: Number of 'argOut' registers that can be written to in parallel
  */
class RegFile(val w: Int, val d: Int, val numArgIns: Int = 0, val numArgOuts: Int = 0, val numArgIOs: Int = 0, val argOutLoopbacksMapRaw: scala.collection.immutable.Map[Int,Int]) extends Module {
  val addrWidth = globals.target.regFileAddrWidth(d)
  val pureArgIns = numArgIns-numArgIOs
  val pureArgOuts = numArgOuts-numArgIOs
  val argInRange = List(0, 1) ++ (2 until numArgIns).toList
  val argOutRange = List(1) ++ (pureArgIns until (numArgIns + pureArgOuts - 1)).toList
  val argOutLoopbacksMap = argOutLoopbacksMapRaw.map{case (k,v) => (k + 1 -> v)}
  // Console.println("argin: " + argInRange + ", argout: " + argOutRange)

  /*
      _____________________________
     |  |  |  |  |  |  |  |  |  |  |
     ```````````````````````````````
       ^ ^   ^ ^   ^ ^   ^ ^   ^ ^
  StatusRegs  |   ArgIOs  |  ArgInstrs
          ArgIns       ArgOuts
                   
  */
  // Helper function to convert an argOut index into
  // register index. Used in the unit test
  def argOut2RegIdx(argOut: Int) = {
    argOutRange(argOut)
  }

  // Helper function to convert reg index to argOut index.
  def regIdx2ArgOut(regIdx: Int) = {
    argOutRange.indexOf(regIdx)
  }

  val io = IO(new Bundle {
    val raddr = Input(UInt(addrWidth.W))
    val wen  = Input(Bool())
    val waddr = Input(UInt(addrWidth.W))
    val wdata = Input(Bits(w.W))
    val rdata = Output(Bits(w.W))
    val reset = Input(Bool())
    val argIns = Output(Vec(numArgIns, UInt(w.W)))
    val argOuts = Vec(numArgOuts, Flipped(Decoupled(UInt(w.W))))
    val argOutLoopbacks = Output(Vec(1 max argOutLoopbacksMap.toList.length, UInt(w.W)))
  })

  // Sanity-check module parameters
  Predef.assert(numArgIns >= 0, s"Invalid numArgIns ($numArgIns): must be >= 0.")
  Predef.assert(numArgOuts >= 0, s"Invalid numArgOuts ($numArgOuts): must be >= 0.")
  Predef.assert(numArgIns <= d, s"numArgIns ($numArgIns) must be less than number of registers ($d)!")
  Predef.assert(numArgOuts <= d, s"numArgOuts ($numArgOuts) must be less than number of registers ($d)!")
  
  val regs = List.tabulate(d) { i =>
    val id = if (globals.target.isInstanceOf[fringe.targets.zcu.ZCU]) i*2 else i
    val ff = Module(new FringeFF(UInt(w.W)))
    if ((argOutRange contains i) & (argInRange contains i)) {
      ff.io.enable := Mux(io.wen & (io.waddr === id.U(addrWidth.W)), io.wen & (io.waddr === id.U(addrWidth.W)), io.argOuts(argOutRange.indexOf(i)).valid)
      ff.io.in := Mux(io.wen & (io.waddr === id.U(addrWidth.W)), io.wdata, io.argOuts(regIdx2ArgOut(i)).bits)
      ff.reset := reset.toBool
      ff.io.reset := reset.toBool // Board level
    } else if (argOutRange contains i) {
      ff.io.enable := io.argOuts(argOutRange.indexOf(i)).valid | (io.wen & (io.waddr === id.U(addrWidth.W)))
      ff.io.in := Mux(io.argOuts(regIdx2ArgOut(i)).valid, io.argOuts(regIdx2ArgOut(i)).bits, io.wdata)
      if (argOutLoopbacksMap.contains(regIdx2ArgOut(i))) {io.argOutLoopbacks(argOutLoopbacksMap(regIdx2ArgOut(i))) := ff.io.out}
      ff.reset := io.reset
      ff.io.reset := reset.toBool //io.reset // reset.toBool 
    } else {
      ff.io.enable := io.wen & (io.waddr === id.U(addrWidth.W))
      ff.io.in := io.wdata
      ff.reset := reset.toBool
      ff.io.reset := reset.toBool // Board level
    }

    ff.io.init := 0.U
    ff
  }

  val rport = Module(new MuxN(UInt(w.W), d))
  val regOuts = Vec(regs.map{_.io.out})
  rport.io.ins := regOuts
  if (globals.target.isInstanceOf[fringe.targets.zcu.ZCU]) {
    rport.io.sel := io.raddr / 2.U(addrWidth.W)
    io.rdata := rport.io.out
  } else if (globals.target.isInstanceOf[fringe.targets.zcu.ZCU]) {
    // Use MSB of addr to read either lower or upper 32 bits.  Bridge gives true addr bits 18:2
    rport.io.sel := io.raddr & Cat(Fill(15, true.B), false.B, Fill(16, true.B))
    io.rdata := Mux(io.raddr(16), rport.io.out(63,32), rport.io.out(31,0))
  } else {
    rport.io.sel := io.raddr
    io.rdata := rport.io.out
  }
  

  io.argIns := Vec(regOuts.zipWithIndex.filter { case (arg, idx) => argInRange.contains(idx) }.map {_._1})
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
