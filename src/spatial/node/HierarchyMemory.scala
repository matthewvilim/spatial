package spatial.node

import argon._
import argon.node._
import forge.tags._
import spatial.lang._

/** Memory allocation */
abstract class MemAlloc[A:Bits,C[T]](
    mutable: Boolean = true
    )(implicit C: Type[C[A]])
  extends Alloc[C[A]] {

  val A: Bits[A] = Bits[A]

  def dims: Seq[I32]
  def rank: Seq[Int] = Seq.tabulate(dims.length){i => i}
  override def effects: Effects = if (mutable) Effects.Mutable else super.effects
}
object MemAlloc {
  def unapply(x: Sym[_]): Option[Sym[_]] = x match {
    case Op(_: MemAlloc[_,_]) => Some(x)
    case _ => None
  }
}

abstract class MemAlias[A, Src[T], Alias[T]](implicit Alias: Type[Alias[A]]) extends Alloc[Alias[A]] {
  def mem: Seq[Src[A]]
  /** Returns a list of dimensions which are not unit (not dropped during loads, for example.) */
  def sparseRank: Seq[Int]
  /** Returns a list of all dimensions. */
  def rawRank: Seq[Int]
  def mutable: Boolean
  def A: Type[A]
  def Src: Type[Src[A]]
  override def effects: Effects = if (mutable) Effects.Mutable else super.effects
}

/** A dense alias of an allocated memory.
  *
  * @param mem The memory being aliased.
  * @param ranges View ranges for this alias.
  * @param A The type of the element in the memory.
  * @param Src The type of the memory being aliased.
  * @param Alias The type of the alias (can be a different rank than the target).
  */
@op case class MemDenseAlias[A,Src[T],Alias[T]](
    cond:    Seq[Bit],
    mem:     Seq[Src[A]],
    ranges:  Seq[Seq[Series[Idx]]]
  )(implicit
    val A:     Type[A],
    val Src:   Type[Src[A]],
    val Alias: Type[Alias[A]])
  extends MemAlias[A,Src,Alias] {

  def sparseRank: Seq[Int] = ranges.head.zipWithIndex.collect{case(r,i) if !r.isUnit => i}
  def rawRank: Seq[Int] = Seq.tabulate(ranges.head.length){i => i}
  val mutable = true

  override def aliases: Set[Sym[_]] = syms(mem)
}
object MemDenseAlias {
  @rig def apply[A,Src[T],Alias[T]](mem: Src[A], ranges: Seq[Series[Idx]])(implicit
    A: Type[A],
    Src: Type[Src[A]],
    Alias: Type[Alias[A]]
  ): MemDenseAlias[A,Src,Alias] = MemDenseAlias[A,Src,Alias](Seq(Bit(true)),Seq(mem),Seq(ranges))
}

/** A sparse alias of an allocated memory
  *
  * @param mem The memory being aliased.
  * @param addr The sparse addresses for this alias.
  * @param A The type of element in this memory.
  * @param Addr The type of the memory holding the addresses.
  * @param Src The type of the memory being aliased.
  * @param Alias The type of the alias (can be different rank than the target, should be rank 1).
  */
@op case class MemSparseAlias[A,Addr[B],W,Src[T],Alias[T]](
    cond: Seq[Bit],
    mem:  Seq[Src[A]],
    addr: Seq[Addr[Ind[W]]],
    size: Seq[I32],
    origin: Seq[Ind[W]]
  )(implicit
    val A:     Type[A],
    val Addr:  Type[Addr[Ind[W]]],
    val Src:   Type[Src[A]],
    val Alias: Type[Alias[A]])
  extends MemAlias[A,Src,Alias] {
  def sparseRank: Seq[Int] = Seq(0)
  def rawRank: Seq[Int] = Seq(0)
  val mutable = true

  override def aliases: Set[Sym[_]] = syms(mem)
}
object MemSparseAlias {
  @rig def apply[A,Addr[B],W,Src[T],Alias[T],W2](mem: Src[A], addr: Addr[Ind[W]], size: Ind[W2], origin: Ind[W])(implicit
    A:     Type[A],
    Addr:  Type[Addr[Ind[W]]],
    Src:   Type[Src[A]],
    Alias: Type[Alias[A]],
    cast1: Cast[Ind[W],I64],
    cast2: Cast[Ind[W2],I32]
  ): MemSparseAlias[A,Addr,W,Src,Alias] = {
    MemSparseAlias[A,Addr,W,Src,Alias](Seq(Bit(true)),Seq(mem),Seq(addr),Seq(size.to[I32]), Seq(origin))
  }
}

@op case class MemStart(mem: Sym[_], d: Int)extends Transient[I32]
@op case class MemStep(mem: Sym[_], d: Int) extends Transient[I32]
@op case class MemEnd(mem: Sym[_], d: Int) extends Transient[I32]
@op case class MemPar(mem: Sym[_], d: Int) extends Transient[I32]
@op case class MemLen(mem: Sym[_], d: Int) extends Transient[I32]
@op case class MemOrigin[W:INT](mem: Sym[_], d: Int) extends Transient[Ind[W]]

@op case class MemDim(mem: Sym[_], d: Int) extends Transient[I32]
@op case class MemRank(mem: Sym[_]) extends Transient[I32]
