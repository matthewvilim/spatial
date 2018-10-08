package fringe.templates.mag

import chisel3._
import chisel3.util.{Valid,UIntToOH}
import fringe._
import fringe.utils.vecWidthConvert

class ScatterBuffer(
  val streamW: Int,
  val d: Int,
  val streamV: Int,
  val burstSize: Int,
  val addrWidth: Int,
  val sizeWidth: Int,
  val readResp: DRAMReadResponse
) extends Module {
  val v = readResp.rdata.getWidth / streamW

  class ScatterData extends Bundle {
    val data = UInt(streamW.W)
    val meta = new Bundle {
      val valid = Bool()
    }

    override def cloneType(): this.type = new ScatterData().asInstanceOf[this.type]
  }

  val countWidth = 16

  val dataFIFO = List.fill(v) { Module(new FIFO(new ScatterData, d, true)) }
  val countFIFO = Module(new FIFO(UInt(countWidth.W), d, true))
  val cmdFIFO = Module(new FIFO(new Command(addrWidth, sizeWidth, 0), d, true))

  class ScatterBufferIO extends Bundle {
    class WData extends Bundle {
      val data = Vec(v, UInt(streamW.W))
      val count = UInt(countWidth.W)
      val cmd = new Command(addrWidth, sizeWidth, 0)

      override def cloneType(): this.type = new WData().asInstanceOf[this.type]
    }

    val fifo = new FIFOIO(new WData, d)
    val rresp = Input(Valid(readResp))
    val hit = Output(Bool())
    val complete = Output(Bool())
  }
  
  val io = IO(new ScatterBufferIO)
  
  /*
  val cmdAddr = Wire(new BurstAddr(addrWidth, streamW, burstSize))
  cmdAddr.bits := io.fifo.enq(0).cmd.addr

  dataFIFO.io.enq.zipWithIndex.foreach { case (e, i) =>
    e.meta.valid := (cmdAddr.wordOffset === i.U)
    e.data := io.fifo.enq(0).data(i)
  }
  cmdFIFO.io.enq(0) := io.fifo.enq(0).cmd
  countFIFO.io.enq(0) := 1.U

  val enqVld = io.fifo.enqVld & ~io.hit
  dataFIFO.io.enqVld := enqVld
  cmdFIFO.io.enqVld := enqVld
  countFIFO.io.enqVld := enqVld
  val (issueHits, respHits) = cmdFIFO.io.banks.zipWithIndex.map { case (bank, i) => 
    val addr = Wire(new BurstAddr(addrWidth, streamW, burstSize))
    addr.bits := bank(0).rdata.addr
    val valid = bank(0).valid
    val issueHit = valid & (addr.burstTag === cmdAddr.burstTag)
    val respHit = valid & (addr.burstTag === io.rresp.bits.tag.uid) & io.rresp.valid
    (issueHit, respHit)
  }.unzip
  io.hit := issueHits.reduce { _|_ }

  dataFIFO.io.banks.zipWithIndex.foreach { case (bank, i) => 
    val count = countFIFO.io.banks.apply(i).apply(0)
    val wen = issueHits(i) & io.fifo.enqVld
    count.wen := wen
    count.wdata := count.rdata + 1.U
    bank.zipWithIndex.foreach { case (d, j) =>
      val writeWord = UIntToOH(cmdAddr.wordOffset)(j) & wen
      val writeResp = respHits(i) & !d.rdata.meta.valid
      d.wen := writeWord | writeResp
      val rdata = vecWidthConvert(io.rresp.bits.rdata, streamW)
      d.wdata.data := Mux(writeWord, io.fifo.enq(0).data(j), rdata(j))
      d.wdata.meta.valid := true.B
    }
  }

  io.fifo.deq(0).data := dataFIFO.io.deq.map { _.data }
  io.fifo.deq(0).cmd := cmdFIFO.io.deq(0)
  io.fifo.deq(0).count := countFIFO.io.deq(0)
  io.fifo.full := dataFIFO.io.full
  io.complete := dataFIFO.io.deq.map { _.meta.valid }.reduce { _&_ } & !dataFIFO.io.empty
  io.fifo.empty := dataFIFO.io.empty
  

  val deqVld = io.fifo.deqVld & io.complete
  dataFIFO.io.deqVld := deqVld
  cmdFIFO.io.deqVld := deqVld
  countFIFO.io.deqVld := deqVld
  */
}
