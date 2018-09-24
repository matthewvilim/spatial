package spatial.lang

import argon._
import forge.tags._
import spatial.node._
import spatial.lang.types._

import utils.implicits.collections._

abstract class DRAM[A:Bits,C[T]](implicit val evMem: C[A] <:< DRAM[A,C]) extends Top[C[A]] with RemoteMem[A,C] {
  val A: Bits[A] = Bits[A]

  protected def M1: Type[DRAM1[A]] = implicitly[Type[DRAM1[A]]]
  protected def M2: Type[DRAM2[A]] = implicitly[Type[DRAM2[A]]]
  protected def M3: Type[DRAM3[A]] = implicitly[Type[DRAM3[A]]]
  protected def M4: Type[DRAM4[A]] = implicitly[Type[DRAM4[A]]]
  protected def M5: Type[DRAM5[A]] = implicitly[Type[DRAM5[A]]]
  def rank: Seq[Int]
  @api def size: I32 = product(dims:_*)
  @api def dims: Seq[I32] = Seq.tabulate(rank.length){d => stage(MemDim(this,rank(d))) }
  @api def dim0: I32 = dims.head
  @api def dim1: I32 = dims.indexOrElse(1, I32(1))
  @api def dim2: I32 = dims.indexOrElse(2, I32(1))
  @api def dim3: I32 = dims.indexOrElse(3, I32(1))
  @api def dim4: I32 = dims.indexOrElse(4, I32(1))

  /** Returns the 64-bit address of this DRAM */
  @api def address: I64 = stage(GetDRAMAddress(me))
}

/** A 1-dimensional [[DRAM]] with elements of type A. */
@ref class DRAM1[A:Bits] extends DRAM[A,DRAM1] with Ref[Array[Any],DRAM1[A]] with Mem1[A,DRAM1] {
  def rank: Seq[Int] = Seq(0)
  @api def length: I32 = dims.head
  @api override def size: I32 = dims.head

  @api def alloc(len: I32): Void = {
    stage(DRAMAlloc[A,DRAM1](this, Seq(len)))
  }

  @api def dealloc(): Void = {
    stage(DRAMDealloc[A,DRAM1](this))
  }

  /** Creates a view of a sparse region of this DRAM1 for use in scatter and gather transfers. */
  @api def apply(addrs: SRAM1[I32]): DRAMSparseTile[A] = {
    stage(MemSparseAlias[A,SRAM1,DRAM1,DRAMSparseTile](this,addrs,addrs.length))
  }
  /** Creates a view of a sparse region of this DRAM1 for use in scatter and gather transfers, with number of addresses to operate on. */
  @api def apply(addrs: SRAM1[I32], size: I32): DRAMSparseTile[A] = {
    stage(MemSparseAlias[A,SRAM1,DRAM1,DRAMSparseTile](this,addrs,size))
  }
  /** Creates a view of a sparse region of this DRAM1 for use in scatter and gather transfers. */
  @api def apply(addrs: FIFO[I32]): DRAMSparseTile[A] = {
    stage(MemSparseAlias[A,FIFO,DRAM1,DRAMSparseTile](this,addrs,addrs.numel))
  }
  /** Creates a view of a sparse region of this DRAM1 for use in scatter and gather transfers, with number of addresses to operate on. */
  @api def apply(addrs: FIFO[I32], size: I32): DRAMSparseTile[A] = {
    stage(MemSparseAlias[A,FIFO,DRAM1,DRAMSparseTile](this,addrs,size))
  }
  /** Creates a view of a sparse region of this DRAM1 for use in scatter and gather transfers. */
  @api def apply(addrs: LIFO[I32]): DRAMSparseTile[A] = {
    stage(MemSparseAlias[A,LIFO,DRAM1,DRAMSparseTile](this,addrs,addrs.numel))
  }
  /** Creates a view of a sparse region of this DRAM1 for use in scatter and gather transfers, with number of addresses to operate on. */
  @api def apply(addrs: LIFO[I32], size: I32): DRAMSparseTile[A] = {
    stage(MemSparseAlias[A,LIFO,DRAM1,DRAMSparseTile](this,addrs,size))
  }

  /** Creates a dense, burst transfer from the on-chip `local` to this DRAM's region of main memory. */
  @api def store[Local[T]<:LocalMem1[T,Local]](local: Local[A])(implicit tp: Type[Local[A]]): Void = {
    stage(DenseTransfer(this,local,isLoad = false))
  }

  /** Creates a dense, burst transfer from the on-chip `local` to this DRAM's region of main memory.
    * Restricted to the first `len` elements in local.
    **/
  @api def store(local: SRAM1[A], len: I32): Void = stage(DenseTransfer(this,local.apply(0::len),isLoad = false))
}

object DRAM1 {
  @api def apply[A:Bits](): DRAM1[A] = stage(DRAMNew[A,DRAM1]())
}

/** A 2-dimensional [[DRAM]] with elements of type A. */
@ref class DRAM2[A:Bits] extends DRAM[A,DRAM2] with Ref[Array[Any],DRAM2[A]] with Mem2[A,DRAM1,DRAM2] {
  def rank: Seq[Int] = Seq(0,1)
  @api def rows: I32 = dims.head
  @api def cols: I32 = dim1

  /** Creates a dense, burst transfer from the SRAM2 `data` to this region of main memory. */
  @api def store(data: SRAM2[A]): Void = stage(DenseTransfer(this, data, isLoad = false))

  /** Creates a dense, burst transfer from the RegFile2 `data` to this region of main memory. */
  @api def store(data: RegFile2[A]): Void = stage(DenseTransfer(this, data, isLoad = false))
}

object DRAM2 {
  @api def apply[A:Bits](): DRAM2[A] = stage(DRAMNew[A,DRAM2]())
}

/** A 3-dimensional [[DRAM]] with elements of type A. */
@ref class DRAM3[A:Bits] extends DRAM[A,DRAM3] with Ref[Array[Any],DRAM3[A]] with Mem3[A,DRAM1,DRAM2,DRAM3] {
  def rank: Seq[Int] = Seq(0,1,2)

  /** Creates a dense, burst transfer from the SRAM3 `data` to this region of main memory. */
  @api def store(data: SRAM3[A]): Void = stage(DenseTransfer(this, data, isLoad = false))

  /** Creates a dense, burst transfer from the RegFile3 `data` to this region of main memory. */
  @api def store(data: RegFile3[A]): Void = stage(DenseTransfer(this, data, isLoad = false))
}

object DRAM3 {
  @api def apply[A:Bits](): DRAM3[A] = stage(DRAMNew[A,DRAM3]())
}

/** A 4-dimensional [[DRAM]] with elements of type A. */
@ref class DRAM4[A:Bits] extends DRAM[A,DRAM4] with Ref[Array[Any],DRAM4[A]] with Mem4[A,DRAM1,DRAM2,DRAM3,DRAM4] {
  def rank: Seq[Int] = Seq(0,1,2,3)

  /** Creates a dense, burst transfer from the SRAM4 `data` to this region of main memory. */
  @api def store(data: SRAM4[A]): Void = stage(DenseTransfer(this, data, isLoad = false))
}

object DRAM4 {
  @api def apply[A:Bits](): DRAM4[A] = stage(DRAMNew[A,DRAM4]())
}

/** A 5-dimensional [[DRAM]] with elements of type A. */
@ref class DRAM5[A:Bits] extends DRAM[A,DRAM5] with Ref[Array[Any],DRAM5[A]] with Mem5[A,DRAM1,DRAM2,DRAM3,DRAM4,DRAM5] {
  def rank: Seq[Int] = Seq(0,1,2,3,4)

  /** Creates a dense, burst transfer from the SRAM5 `data` to this region of main memory. */
  @api def store(data: SRAM5[A]): Void = stage(DenseTransfer(this, data, isLoad = false))
}

object DRAM5 {
  @api def apply[A:Bits](): DRAM5[A] = stage(DRAMNew[A,DRAM5]())
}

/** A sparse, 1-dimensional region of DRAM with elements of type A. */
@ref class DRAMSparseTile[A:Bits] extends DRAM[A,DRAMSparseTile] with Ref[Array[Any],DRAMSparseTile[A]] {
  def rank: Seq[Int] = Seq(0)

  /** Creates a sparse transfer from the on-chip `data` to this sparse region of main memory. */
  @api def scatter[Local[T]<:LocalMem1[T,Local]](local: Local[A])(implicit L: Type[Local[A]]): Void = {
    stage(SparseTransfer(this,local,isGather = false))
  }
}


