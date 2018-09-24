package spatial.codegen.chiselgen

import argon._
import argon.node._
import argon.codegen.Codegen
import spatial.lang._
import spatial.node._
import spatial.metadata.control._
import spatial.metadata.memory._
import spatial.util.spatialConfig

trait ChiselGenMath extends ChiselGenCommon {

  // TODO: Clean this and make it nice
  private def MathDL(lhs: Sym[_], rhs: Op[_], lat: String): Unit = {
    emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")

    val backpressure = if (controllerStack.nonEmpty) getBackPressure(controllerStack.head.toCtrl) else "true.B"
    rhs match {
      case FixMul(a,b) => emitt(src"$lhs.r := ($a.mul($b, $lat, $backpressure)).r")
      case UnbMul(a,b) => emitt(src"$lhs.r := ($a.mul($b, $lat, $backpressure, rounding = Unbiased)).r")
      case SatMul(a,b) => emitt(src"$lhs.r := ($a.mul($b, $lat, $backpressure, saturating = Saturating)).r")
      case UnbSatMul(a,b) => emitt(src"$lhs.r := ($a.mul($b, $lat, $backpressure, saturating = Saturating, rounding = Unbiased)).r")
      case FixDiv(a,b) => emitt(src"$lhs.r := ($a.div($b, $lat, $backpressure)).r")
      case UnbDiv(a,b) => emitt(src"$lhs.r := ($a.div($b, $lat, $backpressure, rounding = Unbiased)).r")
      case SatDiv(a,b) => emitt(src"$lhs.r := ($a.div($b, $lat, $backpressure, saturating = Saturating)).r")
      case UnbSatDiv(a,b) => emitt(src"$lhs.r := ($a.div($b, $lat, $backpressure, saturating = Saturating, rounding = Unbiased)).r")
      case FixMod(a,b) => emitt(src"$lhs.r := ($a.mod($b, $lat, $backpressure)).r")
      case FixRecip(a) => emitt(src"$lhs.r := (${lhs}_one.div($a, $lat, $backpressure)).r")
      case FixFMA(x,y,z) => emitt(src"${lhs}.r := Math.fma($x,$y,$z,${latencyOptionString("FixFMA", Some(bitWidth(lhs.tp)))}.getOrElse(0.0).toInt, $backpressure).toFixed($lhs).r")
    }
  }

  override protected def gen(lhs: Sym[_], rhs: Op[_]): Unit = rhs match {
    case _ if lhs.isBroadcastAddr => // Do nothing
    case FixInv(x)   => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := (~$x).r")
    case FixNeg(x)   => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := (-$x).r")
    case FixAdd(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := ($x + $y).r")
    case FixSub(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := ($x - $y).r")
    case FixAnd(x,y)  => emitGlobalWire(src"val $lhs = Wire(${lhs.tp})");emitt(src"$lhs := ($x & $y).r")
    case FixOr(x,y)   => emitGlobalWire(src"val $lhs = Wire(${lhs.tp})");emitt(src"$lhs := ($x | $y).r")
    case FixXor(x,y)  => emitGlobalWire(src"val $lhs = Wire(${lhs.tp})");emitt(src"$lhs := ($x ^ $y).r")
    case FixPow(x,y)  => throw new Exception(s"FixPow($x, $y) should have transformed to either a multiply tree (constant exp) or reduce structure (variable exp)")
    case VecApply(vector, i) => emitGlobalWireMap(src"""$lhs""", src"""Wire(${lhs.tp})"""); emitt(src"$lhs := $vector.apply($i)")

    case FixLst(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})"); emitt(src"$lhs := $x < $y")
    case FixLeq(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})"); emitt(src"$lhs := $x <= $y")
    case FixNeq(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})"); emitt(src"$lhs := $x =/= $y")
    case FixEql(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})"); emitt(src"$lhs := $x === $y")
    case FltLst(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})"); emitt(src"$lhs := $x < $y")
    case FltLeq(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})"); emitt(src"$lhs := $x <= $y")
    case FltNeq(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})"); emitt(src"$lhs := $x =/= $y")
    case FltEql(x,y) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})"); emitt(src"$lhs := $x === $y")
    case UnbMul(x,y) => MathDL(lhs, rhs, latencyOptionString("UnbMul", Some(bitWidth(lhs.tp)))) 
    case UnbDiv(x,y) => MathDL(lhs, rhs, latencyOptionString("UnbDiv", Some(bitWidth(lhs.tp)))) 
    case SatMul(x,y) => MathDL(lhs, rhs, latencyOptionString("SatMul", Some(bitWidth(lhs.tp)))) 
    case SatDiv(x,y) => MathDL(lhs, rhs, latencyOptionString("SatDiv", Some(bitWidth(lhs.tp)))) 
    case UnbSatMul(x,y) => MathDL(lhs, rhs, latencyOptionString("SatMul", Some(bitWidth(lhs.tp)))) 
    case UnbSatDiv(x,y) => MathDL(lhs, rhs, latencyOptionString("SatDiv", Some(bitWidth(lhs.tp)))) 
    case FixMul(x,y) => MathDL(lhs, rhs, latencyOptionString("FixMul", Some(bitWidth(lhs.tp))))
    case FixDiv(x,y) => MathDL(lhs, rhs, latencyOptionString("FixDiv", Some(bitWidth(lhs.tp))))
    case FixRecip(y) => 
      emitGlobalWireMap(src"${lhs}_one", src"Wire(${lhs.tp})")
      emit(src"${lhs}_one.r := 1.FP(${lhs}_one.s, ${lhs}_one.d, ${lhs}_one.f).r")
      MathDL(lhs, rhs, latencyOptionString("FixDiv", Some(bitWidth(lhs.tp)))) 
    case FixMod(x,y) => MathDL(lhs, rhs, latencyOptionString("FixMod", Some(bitWidth(lhs.tp)))) 
    case FixFMA(x,y,z) => MathDL(lhs, rhs, latencyOptionString("FixFMA", Some(bitWidth(lhs.tp)))) 
      

    case SatAdd(x,y) => emitt(src"val $lhs = $x <+> $y")
    case SatSub(x,y) => emitt(src"val $lhs = $x <-> $y")
    case FixSLA(x,y) => 
      val shift = DLTrace(y).getOrElse(throw new Exception("Cannot shift by non-constant amount in accel")).replaceAll("\\.FP.*|\\.U.*|\\.S.*|L","")
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := ($x.r << $shift).r // TODO: cast to proper type (chisel expands bits)")
    case FixSRA(x,y) => 
      val shift = DLTrace(y).getOrElse(throw new Exception("Cannot shift by non-constant amount in accel")).replaceAll("\\.FP.*|\\.U.*|\\.S.*|L","")
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := ($x >> $shift).r")
    case FixSRU(x,y) => 
      val shift = DLTrace(y).getOrElse(throw new Exception("Cannot shift by non-constant amount in accel")).replaceAll("\\.FP.*|\\.U.*|\\.S.*|L","")
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := ($x >>> $shift).r")
    case BitRandom(None) if lhs.parent.s.isDefined => emitt(src"val $lhs = Math.fixrand(${scala.math.random*scala.math.pow(2, bitWidth(lhs.tp))}.toInt, ${bitWidth(lhs.tp)}, ${swap(lhs.parent.s.get, DatapathEn)}) === 1.U")
    case FixRandom(None) if lhs.parent.s.isDefined => emitGlobalWire(src"val $lhs = Wire(${lhs.tp})");emitt(src"$lhs.r := Math.fixrand(${scala.math.random*scala.math.pow(2, bitWidth(lhs.tp))}.toInt, ${bitWidth(lhs.tp)}, ${swap(lhs.parent.s.get, DatapathEn)}).r")
    case FixRandom(x) =>
      emitGlobalWire(src"val $lhs = Wire(${lhs.tp})")
      val seed = (scala.math.random*1000).toInt
      val size = x match{
        case Some(Const(xx)) => s"$xx"
        case Some(_) => s"$x"
        case None => "4096"
      }
      emitt(s"val ${quote(lhs)}_bitsize = fringe.utils.log2Up($size) max 1")
      emitGlobalModule(src"val ${lhs}_rng = Module(new PRNG($seed))")
      val en = if (lhs.parent.s.isDefined) src"${swap(lhs.parent.s.get, DatapathEn)}" else "true.B"
      emitGlobalModule(src"${lhs}_rng.io.en := $en")
      emitt(src"${lhs}.r := ${lhs}_rng.io.output(${lhs}_bitsize,0)")
    // case FixUnif() =>
    //   val bits = lhs.tp match {
    //     case FixPtType(s,d,f) => f
    //   }
    //   val seed = (random*1000).toInt
    //   emitGlobalModule(src"val ${lhs}_rng = Module(new PRNG($seed))")
    //   emitGlobalModule(src"${lhs}_rng.io.en := true.B")
    //   emitt(src"val ${lhs} = Wire(new FixedPoint(false, 0, $bits))")
    //   emitt(src"${lhs}.r := ${lhs}_rng.io.output(${bits},0)")
    // case FixConvert(x) => lhs.tp match {
    //   case IntType()  =>
    //     emitGlobalWireMap(src"$lhs", "Wire(new FixedPoint(true, 32, 0))")
    //     emitt(src"${x}.cast($lhs)")
    //   case LongType() =>
    //     // val pad = bitWidth(lhs.tp) - bitWidth(x.tp)
    //     emitGlobalWireMap(src"$lhs","Wire(new FixedPoint(true, 64, 0))")
    //     emitt(src"${x}.cast($lhs)")
    //     // if (pad > 0) {
    //     //   emitt(src"${lhs}.r := chisel3.util.Cat(0.U(${pad}.W), ${x}.r)")
    //     // } else {
    //     //   emitt(src"${lhs}.r := ${x}.r.apply(${bitWidth(lhs.tp)-1}, 0)")
    //     // }
    //   case FixPtType(s,d,f) =>
    //     emitt(src"val $lhs = Wire(new FixedPoint($s, $d, $f))")
    //     emitt(src"${x}.cast($lhs)")
    // }
    // case FixPtToFltPt(x) => 
    //    val FltPtType(m,e) = lhs.tp
    //    emitt(src"val $lhs = $x.toFloat($m,$e)")

    // case StringToFixPt(x) => lhs.tp match {
    //   case IntType()  => emitt(src"val $lhs = $x.toInt")
    //   case LongType() => emitt(src"val $lhs = $x.toLong")
    //   case _ => emitt(src"val $lhs = $x // No rule for this")
    // }

    case FixAbs(x) =>
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Mux($x < 0.U, -$x, $x).r")

    case FixSqrt(x) =>
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Math.sqrt($x, ${latencyOptionString("FixSqrt", Some(bitWidth(lhs.tp)))}.getOrElse(0.0).toInt).r")

    case FixSin(x) =>
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Math.sin($x, ${latencyOptionString("FixSin", Some(bitWidth(lhs.tp)))}.getOrElse(0.0).toInt).r")

    case FixCos(x) =>
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Math.cos($x, ${latencyOptionString("FixCos", Some(bitWidth(lhs.tp)))}.getOrElse(0.0).toInt).r")

    case FixAtan(x) =>
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Math.tan($x, ${latencyOptionString("FixAtan", Some(bitWidth(lhs.tp)))}.getOrElse(0.0).toInt).r")

    case FixSinh(x) =>
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Math.sin($x, ${latencyOptionString("FixSinh", Some(bitWidth(lhs.tp)))}.getOrElse(0.0).toInt).r")

    case FixCosh(x) =>
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Math.cos($x, ${latencyOptionString("FixCosh", Some(bitWidth(lhs.tp)))}.getOrElse(0.0).toInt).r")

    case FltFMA(x,y,z) => 
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := ($x.FltFMA($y, $z).r)")

    case FltNeg(x) =>
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := (-$x).r")

    case FltAdd(x,y) => 
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := ($x + $y).r")

    case FltSub(x,y) => 
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := ($x - $y).r")

    case FltMul(x,y) => 
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := ($x * $y).r")

    case FltDiv(x,y) => 
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := ($x / $y).r")

    case FltMax(x,y) => 
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Mux($x > $y, ${x}.r, ${y}.r)")

    case FltMin(x,y) => 
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Mux($x < $y, ${x}.r, ${y}.r)")

    case FltPow(x,exp) => throw new Exception(s"FltPow($x, $exp) should have transformed to either a multiply tree (constant exp) or reduce structure (variable exp)")

    case FltSqrt(x) => emitt(src"val $lhs = Math.sqrt($x)")

    // case FltPow(x,y) => if (emitEn) throw new Exception("Pow not implemented in hardware yet!")
    // case FixFloor(x) => emitt(src"val $lhs = floor($x)")
    // case FixCeil(x) => emitt(src"val $lhs = ceil($x)")

    // case FltSin(x)  => throw new spatial.TrigInAccelException(lhs)
    // case FltCos(x)  => throw new spatial.TrigInAccelException(lhs)
    // case FltTan(x)  => throw new spatial.TrigInAccelException(lhs)
    // case FltSinh(x) => throw new spatial.TrigInAccelException(lhs)
    // case FltCosh(x) => throw new spatial.TrigInAccelException(lhs)
    // case FltTanh(x) => emitt(src"val $lhs = tanh($x)")
    // case FltSigmoid(x) => emitt(src"val $lhs = sigmoid($x)")
    // case FltAsin(x) => throw new spatial.TrigInAccelException(lhs)
    // case FltAcos(x) => throw new spatial.TrigInAccelException(lhs)
    // case FltAtan(x) => throw new spatial.TrigInAccelException(lhs)

    case OneHotMux(sels, opts) => 
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Mux1H(List($sels), List(${opts.map{x => src"$x.r"}}))")

    case Mux(sel, a, b) => 
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})")
      emitt(src"$lhs.r := Mux(($sel), $a.r, $b.r)")

    case FixMin(a, b) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := Mux(($a < $b), $a, $b).r")
    case FixMax(a, b) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := Mux(($a > $b), $a, $b).r")
    case FixToFix(a, fmt) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"${lhs}.r := Math.fix2fix(${a}, ${fmt.sign}, ${fmt.ibits}, ${fmt.fbits}, Truncate, Wrapping).r")
    case FixToFixSat(a, fmt) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"${lhs}.r := Math.fix2fix(${a}, ${fmt.sign}, ${fmt.ibits}, ${fmt.fbits}, Truncate, Saturating).r")
    case FixToFixUnb(a, fmt) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"${lhs}.r := Math.fix2fix(${a}, ${fmt.sign}, ${fmt.ibits}, ${fmt.fbits}, Unbiased, Wrapping).r")
    case FixToFixUnbSat(a, fmt) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"${lhs}.r := Math.fix2fix(${a}, ${fmt.sign}, ${fmt.ibits}, ${fmt.fbits}, Unbiased, Saturating).r")
    case FixToFlt(a, fmt) => 
      val FixPtType(s,d,f) = a.tp
      val FltPtType(m,e) = lhs.tp
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"fix2flt($a.r, $s,$d,$f, $m,$e)")
    case FltToFix(a, fmt) => 
      val FixPtType(s,d,f) = lhs.tp
      val FltPtType(m,e) = a.tp
      emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"flt2fix($a.r, $s,$d,$f, $m,$e, Truncate, Wrapping)")
    case FltRecip(x) => x.tp match {
      case DoubleType() => throw new Exception("DoubleType not supported for FltRecip") 
      case HalfType()   => emitt(src"val $lhs = frec($x)")
      case FloatType()  => emitt(src"val $lhs = frec($x)")
    }
    
    case And(a, b) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs := $a & $b")
    case Not(a) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs := ~$a")
    case Or(a, b) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs := $a | $b")
    case Xor(a, b) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs := $a ^ $b")
    case Xnor(a, b) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs := ~($a ^ $b)")

    case FixFloor(a) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := Cat($a.raw_dec, 0.U(${fracBits(a)}.W))")
    case FixCeil(a) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := Mux($a.raw_frac === 0.U, $a.r, Cat($a.raw_dec + 1.U, 0.U(${fracBits(a)}.W)))")
    case DataAsBits(data) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.zipWithIndex.foreach{case (dab, i) => dab := $data(i)}")
    case BitsAsData(data, fmt) => emitGlobalWireMap(src"$lhs", src"Wire(${lhs.tp})");emitt(src"$lhs.r := chisel3.util.Cat($data.reverse)")
    // case FltInvSqrt(x) => x.tp match {
    //   case DoubleType() => throw new Exception("DoubleType not supported for FltInvSqrt") 
    //   case HalfType() =>  emitt(src"val $lhs = frsqrt($x)")
    //   case FloatType()  => emitt(src"val $lhs = frsqrt($x)")
    // }

	  case _ => super.gen(lhs, rhs)
  }


}