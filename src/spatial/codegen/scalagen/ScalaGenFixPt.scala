package spatial.codegen.scalagen

import argon._
import argon.node._
import spatial.lang._
import spatial.node._
import emul.FixedPoint
import spatial.util.spatialConfig

trait ScalaGenFixPt extends ScalaGenBits {

  override protected def remap(tp: Type[_]): String = tp match {
    case _: Fix[_, _, _] => "FixedPoint"
    case _ => super.remap(tp)
  }

  override protected def quoteConst(tp: Type[_], c: Any): String = (tp, c) match {
    case (FixPtType(sign, int, frac), c: FixedPoint) =>
      if (int > 32 | (!sign & int == 32))
        s"""FixedPoint(BigDecimal("$c"),FixFormat($sign,$int,$frac))"""
      else
        s"""FixedPoint(BigDecimal("$c"),FixFormat($sign,$int,$frac))"""
    case _ => super.quoteConst(tp, c)
  }

  override def invalid(tp: Type[_]): String = tp match {
    case FixPtType(s, i, f) => src"FixedPoint.invalid(FixFormat($s,$i,$f))"
    case _ => super.invalid(tp)
  }

  override protected def gen(lhs: Sym[_], rhs: Op[_]): Unit = {

    rhs match {
      case FixInv(_) |
           FixAdd(_, _) |
           FixSub(_, _) |
           FixMul(_, _) |
           FixDiv(_, _) |
           FixRecip(_) |
           FixMod(_, _) |
           FixAnd(_, _) |
           FixOr(_, _) |
           FixLst(_, _) |
           FixLeq(_, _) |
           FixXor(_, _) |
           FixSLA(_, _) |
           FixSRA(_, _) |
           FixSRU(_, _) |
           SatAdd(_, _) |
           SatSub(_, _) |
           SatMul(_, _) |
           SatDiv(_, _) |
           UnbMul(_, _) |
           UnbDiv(_, _) |
           UnbSatMul(_, _) |
           UnbSatDiv(_, _) |
           FixNeq(_, _) |
           FixEql(_, _) |
           FixMax(_, _) |
           FixMin(_, _) |
           FixFMA(_, _, _)
      if (spatialConfig.enableResourceReporter) => emit(
        src"""StatTracker.change(("FixPt", ${
          lhs.tp match {
            case Bits(bT) => bT.nbits
            case _ => -1
          }
        }), 1)""")
      case _ => Unit
    }

    rhs match {
      case FixInv(x) => emit(src"val $lhs = ~$x")
      case FixNeg(x) => emit(src"val $lhs = -$x")
      case FixAdd(x, y) => emit(src"val $lhs = $x + $y")
      case FixSub(x, y) => emit(src"val $lhs = $x - $y")
      case FixMul(x, y) => emit(src"val $lhs = $x * $y")
      case FixDiv(x, y) => emit(src"val $lhs = $x / $y")
      case FixRecip(x) => emit(src"val $lhs = Number.recip($x)")
      case FixMod(x, y) => emit(src"val $lhs = $x % $y")
      case FixAnd(x, y) => emit(src"val $lhs = $x & $y")
      case FixOr(x, y) => emit(src"val $lhs = $x | $y")
      case FixLst(x, y) => emit(src"val $lhs = $x < $y")
      case FixLeq(x, y) => emit(src"val $lhs = $x <= $y")
      case FixXor(x, y) => emit(src"val $lhs = $x ^ $y")

      case FixSLA(x, y) => emit(src"val $lhs = $x << $y")
      case FixSRA(x, y) => emit(src"val $lhs = $x >> $y")
      case FixSRU(x, y) => emit(src"val $lhs = $x >>> $y")

      case SatAdd(x, y) => emit(src"val $lhs = $x +! $y")
      case SatSub(x, y) => emit(src"val $lhs = $x -! $y")
      case SatMul(x, y) => emit(src"val $lhs = $x *! $y")
      case SatDiv(x, y) => emit(src"val $lhs = $x /! $y")
      case UnbMul(x, y) => emit(src"val $lhs = $x *& $y")
      case UnbDiv(x, y) => emit(src"val $lhs = $x /& $y")
      case UnbSatMul(x, y) => emit(src"val $lhs = $x *&! $y")
      case UnbSatDiv(x, y) => emit(src"val $lhs = $x /&! $y")

      case FixNeq(x, y) => emit(src"val $lhs = $x !== $y")
      case FixEql(x, y) => emit(src"val $lhs = $x === $y")

      case FixMax(x, y) => emit(src"val $lhs = Number.max($x,$y)")
      case FixMin(x, y) => emit(src"val $lhs = Number.min($x,$y)")

      case FixToFix(x, fmt) =>
        emit(src"val $lhs = $x.toFixedPoint(FixFormat(${fmt.sign},${fmt.ibits},${fmt.fbits}))")
      case FixToFixSat(x, fmt) => 
        emit(src"val $lhs = FixedPoint.saturating($x.value, $x.valid, FixFormat(${fmt.sign},${fmt.ibits},${fmt.fbits}))")
      case FixToFixUnb(x, fmt) => 
        emit(src"val $lhs = FixedPoint.unbiased($x.value, $x.valid, FixFormat(${fmt.sign},${fmt.ibits},${fmt.fbits}))")
      case FixToFixUnbSat(x, fmt) => 
        emit(src"val $lhs = FixedPoint.unbiased($x.value, $x.valid, FixFormat(${fmt.sign},${fmt.ibits},${fmt.fbits}), saturate = true)")

      case FixToFlt(x, fmt) =>
        emit(src"val $lhs = $x.toFloatPoint(FltFormat(${fmt.mbits - 1},${fmt.ebits}))")

      case FixToText(x) => emit(src"val $lhs = $x.toString")

      case TextToFix(x, _) =>
        val FixPtType(s, i, f) = lhs.tp
        emit(src"val $lhs = FixedPoint($x, FixFormat($s,$i,$f))")

      case FixRandom(Some(max)) =>
        val FixPtType(s, i, f) = lhs.tp
        emit(src"val $lhs = FixedPoint.random($max, FixFormat($s,$i,$f))")

      case FixRandom(None) =>
        val FixPtType(s, i, f) = lhs.tp
        emit(src"val $lhs = FixedPoint.random(FixFormat($s,$i,$f))")

      case FixAbs(x) => emit(src"val $lhs = Number.abs($x)")
      case FixFloor(x) => emit(src"val $lhs = Number.floor($x)")
      case FixCeil(x) => emit(src"val $lhs = Number.ceil($x)")
      case FixLn(x) => emit(src"val $lhs = Number.ln($x)")
      case FixExp(x) => emit(src"val $lhs = Number.exp($x)")
      case FixSqrt(x) => emit(src"val $lhs = Number.sqrt($x)")
      case FixSin(x) => emit(src"val $lhs = Number.sin($x)")
      case FixCos(x) => emit(src"val $lhs = Number.cos($x)")
      case FixTan(x) => emit(src"val $lhs = Number.tan($x)")
      case FixSinh(x) => emit(src"val $lhs = Number.sinh($x)")
      case FixCosh(x) => emit(src"val $lhs = Number.cosh($x)")
      case FixTanh(x) => emit(src"val $lhs = Number.tanh($x)")
      case FixAsin(x) => emit(src"val $lhs = Number.asin($x)")
      case FixAcos(x) => emit(src"val $lhs = Number.acos($x)")
      case FixAtan(x) => emit(src"val $lhs = Number.atan($x)")
      case FixPow(x, exp) => emit(src"val $lhs = Number.pow($x, $exp);")
      case FixFMA(m1, m2, add) => emit(src"val $lhs = ($m1 * $m2) + $add")
      case FixRecipSqrt(x) => emit(src"val $lhs = ${one(x.tp)} / Number.sqrt($x)")
      case FixSigmoid(x) => emit(src"val $lhs = ${one(x.tp)} / (Number.exp(-$x) + ${one(x.tp)})")

      case _ => super.gen(lhs, rhs)
    }
  }
}
