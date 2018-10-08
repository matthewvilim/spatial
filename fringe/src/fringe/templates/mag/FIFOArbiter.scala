package fringe.templates.mag

import chisel3._
import chisel3.util._
import fringe.globals
import fringe.utils.{MuxN, MuxNPipe}
import fringe.templates.memory.{FringeFF}
import fringe.utils.getRetimed

import scala.language.reflectiveCalls

class FIFOArbiter[T<:Data] (val t: T, val d: Int, val numStreams: Int) extends Module {
  val tagWidth = log2Ceil(numStreams)

  val io = IO(new Bundle {
    val fifo = Vec(numStreams, Flipped(new FIFOIO(t, d)))
    val enq = Input(Vec(numStreams, t.cloneType))
    val enqVld = Input(Vec(numStreams, Bool()))
    val full = Output(Vec(numStreams, Bool()))
    val deq = Output(t.cloneType)
    val deqVld = Input(Bool())
    // ready/valid interface is used here to retime the critical path through the deque mux
    // this logic is a bit brittle but hides all the retime details from outside modules
    val deqReady = Output(Bool())
    val forceTag = Input(Valid(UInt(tagWidth.W)))
    val empty = Output(Bool())
    val tag = Output(UInt(tagWidth.W))
    val fifoSize = Output(UInt(32.W))
  })

  // max(0) to account for the unlikely single stream case
  val delay = if (globals.retime) (tagWidth - 1).max(0) else 0
  val tagFF = Module(new FringeFF(UInt(tagWidth.W)))
  tagFF.io.init := 0.U
  val tag = Mux(io.forceTag.valid, io.forceTag.bits, tagFF.io.out)

  // FIFOs
  if (numStreams > 0) {
    val deq = if (delay > 0) io.deqVld | !io.deqReady else io.deqVld
    io.fifo.zipWithIndex.foreach { case (f, i) =>
      f.in.bits := io.enq(i)
      f.in.valid := io.enqVld(i)
      f.out.ready := deq & (tag === i.U) & f.out.valid
      io.full(i) := !f.in.ready
    }

    val enqSomething = io.enqVld.reduce{_|_}
    val allFifoEmpty = io.fifo.map { !_.out.valid }.reduce{_&_}
    tagFF.io.enable := io.deqVld | (allFifoEmpty & enqSomething)

    val fifoValids = Mux(allFifoEmpty,
      io.enqVld,
      Vec(List.tabulate(numStreams) { i =>
        !((!io.enqVld(i) & !io.fifo(i).out.valid) | ((tag === i.U) & io.deqVld & !io.enqVld(i)))
      })
    )

    // Priority encoder and output interfaces
    val activeFifo = PriorityEncoder(fifoValids)
    tagFF.io.in := activeFifo

    val empties = Array.tabulate(numStreams) { i => i.U -> !io.fifo(i).out.valid }
    val empty = MuxLookup(tag, false.B, empties)
    io.empty := empty

    val outMux = Module(new MuxNPipe(t, numStreams, delay))
    outMux.io.ins := Vec(io.fifo.map {e => e.out.bits})
    outMux.io.sel := tag
    outMux.io.en := deq
    io.deqReady := getRetimed(if (delay > 0) !empty & deq else !empty, delay, deq)

    val sizeMux = Module(new MuxN(UInt(32.W), numStreams))
    sizeMux.io.ins := Vec(io.fifo.map {e => e.count})
    sizeMux.io.sel := tag

    io.tag := getRetimed(tag, delay, deq)
    io.deq := outMux.io.out
    io.fifoSize := getRetimed(sizeMux.io.out, delay, deq)
  } else { // Arbiter does nothing if there are no memstreams
    io.tag := 0.U(tagWidth.W)
    io.deq := 0.U(t.getWidth)
    io.empty := true.B
  }

}

