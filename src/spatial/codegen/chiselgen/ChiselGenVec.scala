package spatial.codegen.chiselgen

import argon._
import argon.node._
import spatial.node._

trait ChiselGenVec extends ChiselGenCommon {

  override protected def gen(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case VecAlloc(elems) => emit(src"val $lhs = Vec($elems)")

    case VecSlice(vec, start, stop) =>
      emit(src"val $lhs = Wire(${lhs.tp})")
      emit(src"$lhs.zipWithIndex.foreach{case(w, i) => w := $vec(i+$stop)}")

    case VecConcat(list) => 
      emit(src"val $lhs = Wire(${lhs.tp})")
      emit(s"var ${lhs}_i = 0")
      list.zipWithIndex.foreach{case (a, i) => emit(s"${quote(a)}.zipWithIndex.foreach{case (a,i) => ${quote(lhs)}(${lhs}_i + i) := a}; ${lhs}_i = ${lhs}_i + ${quote(a)}.length") }

    case VecApply(vec, id) =>
      emit(src"val $lhs = Wire(${lhs.tp})")
      emit(src"$lhs.r := $vec($id).r")

    case ShuffleCompressVec(in) =>
      emit(src"val $lhs = Wire(${lhs.tp})")
      val (datamsb, datalsb) = getField(in.head.tp, "_1")
      val (maskmsb, masklsb) = getField(in.head.tp, "_2")
      val data = in.map{ quote(_) + s".r($datamsb, $datalsb)" }.mkString(src"List[UInt](", ",", ")")
      val mask = in.map{ quote(_) + s".r($maskmsb)" }.mkString(src"List[Bool](", ",", ")")
      emit(src"$lhs := Shuffle.compress(Vec($data), Vec($mask))")

    case _ => super.gen(lhs, rhs)
  }

}
