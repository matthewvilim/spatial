package spatial.codegen.chiselgen

import argon._
import spatial.lang._
import spatial.node._
import spatial.metadata.access._
import spatial.metadata.control._
import spatial.metadata.memory._
import spatial.metadata.retiming._
import spatial.metadata.types._
import spatial.util.spatialConfig

trait ChiselGenDRAM extends ChiselGenCommon {
  var requesters = scala.collection.mutable.HashMap[Sym[_], Int]()

  override protected def gen(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case DRAMNew() =>
      if (inHw) {
        val reqCount = lhs.consumers.collect {
          case w@Op(_: DRAMAlloc[_,_] | _: DRAMDealloc[_,_]) => w
        }.size
        emitGlobalModule(src"""val $lhs = Module(new DRAMAllocator($reqCount))""")
        val id = drams.size
        emitt(src"io.heap.req($id) := $lhs.io.heapReq")
        emitt(src"$lhs.io.heapResp := io.heap.resp($id)")
        drams += (lhs -> id)
      }

    case DRAMAlloc(dram, dims) =>
      if (inHw) {
        val id = requesters.size
        val parent = lhs.parent
        val invEnable = src"""${DL(src"${swap(parent, DatapathEn)} & ${swap(parent, IIDone)}", lhs.fullDelay, true)}"""
        emitt(src"${dram}.io.appReq($id).valid := $invEnable")
        emitt(src"${dram}.io.appReq($id).bits.allocDealloc := true.B")
        emitt(src"${dram}.io.appReq($id).bits.size := ${dims}.r")
        requesters += (lhs -> id)
      }

    case DRAMDealloc(dram) =>
      if (inHw) {
        val id = requesters.size
        val parent = lhs.parent
        val invEnable = src"""${DL(src"${swap(parent, DatapathEn)} & ${swap(parent, IIDone)}", lhs.fullDelay, true)}"""
        emitt(src"${dram}.io.appReq($id).valid := $invEnable")
        emitt(src"${dram}.io.appReq($id).bits.allocDealloc := false.B")
        requesters += (lhs -> id)
      }

    case GetDRAMAddress(dram) =>
      emit(src"val $lhs = ${dram}.io.appResp.addr")

    case _ => super.gen(lhs, rhs)
  }

  override def emitFooter(): Unit = {
  	inAccel{
      inGenn(out, "IOModule", ext) {
        emit("// Heap")
        emit(src"val io_numAllocators = ${drams.size}")
      }

      inGen(out, "Instantiator.scala") {
        emit(src"// Heap")
        emit(src"val numAllocators = ${drams.size}")
      }
    }
    super.emitFooter()
  }
}
