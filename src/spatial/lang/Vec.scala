package spatial.lang

import core._
import emul.{FixedPoint, Number}
import forge.tags._
import utils.math.ReduceTree
import utils.plural

import spatial.node._

@ref class Vec[A:Bits](val length: Int) extends Top[Vec[A]] with Ref[Array[Any],Vec[A]] with Arith[Vec[A]] with Bits[Vec[A]] {
  val tA: Bits[A] = Bits[A]
  val aA: Option[Arith[A]] = tA.getView[Arith]
  override val box: Vec[A] <:< (Arith[Vec[A]] with Bits[Vec[A]]) = implicitly[Vec[A] <:< (Arith[Vec[A]] with Bits[Vec[A]])]
  private implicit val evv: A <:< Bits[A] = tA.box

  // TODO[4]: These are all quite expensive for large vectors
  @api def elems: List[A] = List.tabulate(length){i => this.apply(i) }
  @api def map[B:Bits](func: A => B): Vec[B] = Vec.LeastFirst(elems.map(func):_*)
  @api def zip[B:Bits,R:Bits](that: Vec[B])(func: (A,B) => R): Vec[R] = {
    if (that.length != this.length) {
      implicit val tV: Vec[R] = Vec.bits[R](length)
      error(ctx,s"Mismatched vector lengths. Expected length $length, got ${that.length}.")
      error(ctx)
      err[Vec[R]]
    }
    else {
      Vec.LeastFirst(this.elems.zip(that.elems).map{case (a,b) => func(a,b) }:_*)
    }
  }
  @api def reduce(func: (A,A) => A): A = ReduceTree(elems:_*)(func)

  @rig def arith(name: String)(func: Arith[A] => Vec[A]): Vec[A] = aA.map(func).getOrElse{
    implicit val tV: Vec[A] = Vec.bits[A](length)
    error(ctx, s"Arithmetic $name is not defined for ${this.tp}")
    error(ctx)
    err[Vec[A]]
  }

  // TODO[5]: This is a bit hacky - is there a better way to define these?
  @api def unary_-(): Vec[A] = arith("negation"){a => this.map(a.neg) }
  @api def +(b: Vec[A]): Vec[A] = arith("addition"){a => this.zip(b)(a.add) }
  @api def -(b: Vec[A]): Vec[A] = arith("subtraction"){a => this.zip(b)(a.sub) }
  @api def *(b: Vec[A]): Vec[A] = arith("multiplication"){a => this.zip(b)(a.mul) }
  @api def /(b: Vec[A]): Vec[A] = arith("division"){a => this.zip(b)(a.div) }
  @api def %(b: Vec[A]): Vec[A] = arith("modulus"){a => this.zip(b)(a.mod) }


  /**
    * Returns the word at index i in this vector.
    * Index 0 is always the least significant word.
    */
  @api def apply(i: Int): A = stage(VecApply(this,i))

  /**
    * Returns a new vector by slicing this vector in the given range.
    * The range must be statically known, and must have a stride of 1.
    */
  @api def apply(s: Series[I32]): Vec[A] = (s.start, s.end, s.step) match {
    case (Const(x1),Const(x2),Const(c)) =>
      if (c !== 1) {
        error(this.ctx, "Strides for vector slice are currently unsupported.")
        error(this.ctx)
        Vec.empty[A]
      }
      else {
        val msb = Number.max(x1, x2).toInt
        val lsb = Number.min(x1, x2).toInt
        if (msb - lsb == 0) {
          warn(this.ctx, "Empty vector slice.")
          warn(this.ctx)
        }
        Vec.slice(this, msb, lsb)
      }
    case _ =>
      error(this.ctx, "Apply range for bit slicing must be statically known.")
      error(this.ctx)
      Vec.empty[A]
  }

  /**
    * Returns a new vector formed by the concatenation of this and that.
    */
  @api def ++(that: Vec[A]): Vec[A] = Vec.concat(Seq(this,that))

  /**
    * Returns a new vector with this vector's elements in reverse order.
    */
  @api def reverse: Vec[A] = {
    implicit val tV: Vec[A] = this.selfType
    stage(VecReverse(this))
  }

  @api override def neql(that: Vec[A]): Bit = this.zip(that){(a,b) => a.neql(b) }.reduce{_||_}
  @api override def eql(that: Vec[A]): Bit = this.zip(that){(a,b) => a.eql(b) }.reduce{_&&_}

  // --- Typeclass Methods
  override protected val __isPrimitive: Boolean = false
  override def nbits: Int = tA.nbits * length

  @rig def zero: Vec[A] = Vec.LeastLast(Seq.fill(length){ tA.zero }:_*)
  @rig def one: Vec[A] = Vec.LeastLast(Seq.fill(length-1){ tA.zero} :+ tA.one :_*)
  @rig def random(max: Option[Vec[A]]): Vec[A] = {
    if (max.isDefined && max.get.length != length) {
      error(ctx, s"Vector length mismatch. Expected $length ${plural(length,"word")}, got ${max.get.length}")
      error(ctx)
    }
    val elems = Seq.tabulate(length){i => tA.random(max.map{vec => vec(i)}) }
    Vec.LeastLast(elems:_*)
  }

  @rig def abs(a: Vec[A]): Vec[A] = arith("abs"){a => this.map(a.abs) }
  @rig def ceil(a: Vec[A]): Vec[A] = arith("ceil"){a => this.map(a.ceil) }
  @rig def floor(a: Vec[A]): Vec[A] = arith("floor"){a => this.map(a.floor) }
}



object Vec {
  def bits[A:Bits](length: Int): Vec[A] = new Vec[A](length).asType
  def arith[A](length: Int)(implicit ev: (Arith[A] with Bits[A])): Vec[A] = new Vec[A](length).asType

  /**
    * Creates a little-endian vector from the given N elements
    * The first element is the most significant word (vector index N-1).
    * The last element is the least significant word (vector index of 0).
    */
  @api def LittleEndian[A:Bits](elems: A*): Vec[A] = fromSeq(elems.reverse)

  /**
    * (Alias for LittleEndian)
    * Creates a little-endian vector from the given N elements
    * The first element is the most significant word (vector index N-1).
    * The last element is the least significant word (vector index of 0).
    */
  @api def LeastLast[A:Bits](elems: A*): Vec[A] = fromSeq(elems.reverse)

  /**
    * Creates a big-endian vector from the given N elements.
    * The first element is the least significant word (vector index 0).
    * The last element is the most significant word (vector index of N-1).
    */
  @api def BigEndian[A:Bits](elems: A*): Vec[A] = fromSeq(elems)

  /**
    * (Alias for BigEndian)
    * Creates a big-endian vector from the given N elements.
    * The first element is the least significant word (vector index 0).
    * The last element is the most significant word (vector index of N-1).
    */
  @api def LeastFirst[A:Bits](elems: A*): Vec[A] = fromSeq(elems)

  /**
    * Creates an empty vector (of the given type).
    */
  @rig def empty[A:Bits]: Vec[A] = Vec.fromSeq[A](Nil)

  /**
    * Creates a vector from the concatenation of the given elements.
    */
  @rig def fromSeq[A:Bits](elems: Seq[A]): Vec[A] = {
    implicit val tV: Vec[A] = Vec.bits[A](elems.length)
    stage(VecAlloc(elems))
  }

  /**
    * Creates an element slice of the vector from [lsb,msb]
    */
  @rig def slice[A:Bits](vec: Vec[A], msw: Int, lsw: Int): Vec[A] = {
    implicit val tV: Vec[A] = Vec.bits[A](Math.max(msw - lsw + 1, 0))
    stage(VecSlice(vec,msw,lsw))
  }

  /**
    * Creates a new vector which is the concatenation of all given vectors.
    */
  @rig def concat[A:Bits](vecs: Seq[Vec[A]]): Vec[A] = {
    implicit val tV: Vec[A] = Vec.bits[A](vecs.map(_.length).sum)
    stage(VecConcat(vecs))
  }
}