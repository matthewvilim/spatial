package spatial.lang
package static

import core._
import forge.tags._
import utils.math.ReduceTree

trait StaticMath {

  /** Creates a sum reduction tree of a fixed number of numeric elements. */
  @api def sum[T:Num](xs: T*): T = if (xs.isEmpty) Num[T].zero else reduce(xs:_*){_+_}

  /** Creates a product reduction tree for a fixed number of numeric elements. */
  @api def product[T:Num](xs: T*): T = if (xs.isEmpty) Num[T].one else reduce(xs:_*){_*_}

  /** Creates a reduction tree for a fixed number of elements. */
  @api def reduce[T](xs: T*)(reduce: (T,T) => T): T = ReduceTree(xs:_*)(reduce)

  @api def min[A:Num](a: A, b: A): A = Num[A].min(a,b)
  @api def max[A:Num](a: A, b: A): A = Num[A].max(a,b)
  @api def abs[A:Num](a: A): A = Num[A].abs(a)
  @api def ceil[A:Num](a: A): A = Num[A].ceil(a)
  @api def floor[A:Num](a: A): A = Num[A].floor(a)
  @api def pow[A:Num](b: A, e: A): A = Num[A].pow(b,e)
  @api def exp[A:Num](a: A): A = Num[A].exp(a)
  @api def ln[A:Num](a: A): A = Num[A].ln(a)
  @api def sqrt[A:Num](a: A): A = Num[A].sqrt(a)
  @api def sin[A:Num](a: A): A = Num[A].sin(a)
  @api def cos[A:Num](a: A): A = Num[A].cos(a)
  @api def tan[A:Num](a: A): A = Num[A].tan(a)
  @api def sinh[A:Num](a: A): A = Num[A].sinh(a)
  @api def cosh[A:Num](a: A): A = Num[A].cosh(a)
  @api def tanh[A:Num](a: A): A = Num[A].tanh(a)
  @api def asin[A:Num](a: A): A = Num[A].asin(a)
  @api def acos[A:Num](a: A): A = Num[A].acos(a)
  @api def atan[A:Num](a: A): A = Num[A].atan(a)
  @api def sigmoid[A:Num](a: A): A = Num[A].sigmoid(a)

}