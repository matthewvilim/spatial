package fringe.templates.mag

import chisel3._
import chisel3.util._
import fringe._
import fringe.utils.{SwitchParams, CrossbarConfig, CrossbarCore}
import fringe.utils.vecWidthConvert

class GatherBuffer(
  val streamW: Int,
  val streamV: Int,
  val depth: Int,
  val burstSize: Int,
  val addrWidth: Int,
  val loadCmd: AppCommandDense,
  val readResp: DRAMReadResponse
) extends Module {

  class GatherData extends Bundle {
    val data = UInt(streamW.W)
    val meta = new Bundle {
      val valid = Bool()
      val addr = new BurstAddr(addrWidth, streamW, burstSize)
    }

    override def cloneType(): this.type = new GatherData().asInstanceOf[this.type]
  }

  val io = IO(new Bundle {
    val in = Vec(streamV, Flipped(Decoupled(UInt(streamW.W))))
    val out = Decoupled(Vec(streamV, UInt(streamW.W)))
    val rresp = Input(Valid(readResp))
    val cmd = Input(Valid(loadCmd))
    val hit = Output(Bool())
  })
  
  val fifos = List.fill(streamV) { Module(new FIFO(new GatherData, depth, true)) }

  val banks = fifos.map { _.io.banks }

  val cmdAddr = Wire(new BurstAddr(addrWidth, streamW, burstSize))
  cmdAddr.bits := io.cmd.bits.addr

  io.hit := banks.map{ _.map{ i =>
    val meta = i.rdata.bits.meta
    i.rdata.valid & (cmdAddr.burstTag === meta.addr.burstTag) & !meta.valid
  }.reduce{_|_}}.reduce{_|_} & io.cmd.valid

  val data = Wire(new GatherData)
  data.meta.valid := false.B
  data.meta.addr.bits := io.cmd.bits.addr
  fifos(0).io.in.bits := data
  fifos(0).io.in.valid := io.cmd.valid
  
  val crossbars = List.tabulate(depth) { i => 
    val rrespVec = vecWidthConvert(io.rresp.bits.rdata, streamW)
    val switch = SwitchParams(rrespVec.length, streamV)
    val config = Wire(CrossbarConfig(switch))

    val valid = banks.map { _(i).rdata.valid }
    val rdata = banks.map { _(i).rdata.bits }
    val wen = banks.map { _(i).wdata.valid }
    val wdata = banks.map{ _(i).wdata.bits }
    val respHits = rdata.map { _.meta }.zip(valid).map { case (m, v) =>
      v & io.rresp.valid & (m.addr.burstTag === io.rresp.bits.tag.uid) & !m.valid
    }

    config.outSelect.zip(rdata).foreach { case (s, d) =>
      s := d.meta.addr.wordOffset
    }
    wen.zip(respHits).foreach{case (w, h) => w := h }

    val core = Module(new CrossbarCore(UInt(streamW.W), switch))
    core.io.ins := rrespVec
    wdata.zip(core.io.outs).foreach { case (w, o) => w.data := o }
    wdata.zip(rdata).foreach{ case (w, r) =>
      w.meta.valid := true.B
      w.meta.addr := r.meta.addr
    }
    core.io.config := config
    core
  }

  io.in.zip(fifos).foreach { case (a, b) => a.ready := b.io.in.ready }
  io.out.valid := fifos.map { case a => a.io.out.valid & a.io.out.bits.meta.valid }.reduce { _&_ }
  io.out.bits := Vec(fifos.map { _.io.out.bits.data })
  
  fifos.foreach { _.io.out.ready := io.out.valid & io.out.ready }
}
