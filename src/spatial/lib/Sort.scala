package spatial.lib

import forge.tags._
import spatial.dsl._

object Sort {

  def log(x: scala.Int, base: scala.Int) = Math.round(scala.math.log(x.toDouble) / scala.math.log(base.toDouble)).toInt

  @virtualize
  @api def mergeSort[T:Num](
    mem: SRAM1[Tup2[I32, T]],
    mergePar: scala.Int,
    ways: scala.Int
  ): Unit = {
    val sramBlockSize = 256
    val blockSizeInit = mergePar
    val mergeSizeInit = blockSizeInit * ways
    val mergeCountInit = sramBlockSize / mergeSizeInit
    // extra level for base case
    val levelCount = log(sramBlockSize / blockSizeInit, ways) + 1

    val sramMergeBuf = MergeBuffer[Tup2[I32, T]](ways, mergePar)

    val blockSize = Reg[I32](blockSizeInit)
    val mergeSize = Reg[I32](mergeSizeInit)
    val mergeCount = Reg[I32](mergeCountInit)

    Sequential.Foreach(0 until levelCount) { level =>
      val initMerge = (level == 0)
      Pipe { sramMergeBuf.init(initMerge) }
      Sequential.Foreach(0 until mergeCount) { block =>
        List.tabulate(ways) { i => i }.foreach { case i =>
          Pipe { sramMergeBuf.bound(i, blockSize) }
        }
        val addr = List.tabulate(ways) { i => (block * mergeSize) + (i * blockSize) }
        addr.zipWithIndex.foreach { case (a, i) =>
          Foreach(0 until blockSize par mergePar) { j =>
            sramMergeBuf.enq(i, mem(a + j))
          }
        }
        Stream {
          Foreach(0 until mergeSize par mergePar) { j =>
            mem(addr(0) + j) = sramMergeBuf.deq()
          }
        }
      }
      Pipe {
        if (!initMerge) {
          blockSize := blockSize * ways
          mergeSize := mergeSize * ways
          mergeCount := mergeCount / ways
        }
      }
    }

    blockSize := blockSizeInit
    mergeSize := mergeSizeInit
    mergeCount := mergeCountInit
  }

  @virtualize
  @api def mergeSort[T:Num](
    srcDoubleBuf: DRAM1[Tup2[I32, T]],
    mergePar: scala.Int = 2,
    ways: scala.Int = 2,
    numel: scala.Int
  ): Unit = {

    val sramBlockSize = 256

    // SRAM block sort
    Pipe {
      val sram = SRAM[Tup2[I32, T]](sramBlockSize)
      Sequential.Foreach(0 until numel by sramBlockSize) { blockAddr =>
        val blockRange = blockAddr::blockAddr + sramBlockSize par mergePar
        sram load srcDoubleBuf(blockRange)
        mergeSort(sram, mergePar, ways)
        srcDoubleBuf(blockRange) store sram
      }
    }

    // off-chip DRAM sort
    Pipe {
      val blockSizeInit = sramBlockSize
      val mergeSizeInit = blockSizeInit * ways
      val mergeCountInit = numel / mergeSizeInit
      // extra level for base case
      val levelCount = log(numel / blockSizeInit, ways)

      val doubleBuf = Reg[Boolean]
      doubleBuf := true
      val fifos = List.fill(ways) { FIFO[Tup2[I32, T]](128) }
      val mergeBuf = MergeBuffer[Tup2[I32, T]](ways, mergePar)

      val blockSize = Reg[I32](blockSizeInit)
      val mergeSize = Reg[I32](mergeSizeInit)
      val mergeCount = Reg[I32](mergeCountInit)

      Sequential.Foreach(0 until levelCount) { level =>
        Sequential.Foreach(0 until mergeCount) { block =>
          fifos.zipWithIndex.foreach { case (f, i) =>
            Pipe { mergeBuf.bound(i, blockSize) }
          }
          val addr = List.tabulate(ways) { i => (block * mergeSize) + (i * blockSize) }
          Stream {
            fifos.zipWithIndex.foreach { case (f, i) =>
              val lOffset = mux(doubleBuf, 0, numel) 
              val lAddr = addr(i) + lOffset::addr(i) + lOffset + blockSize par mergePar
              f load srcDoubleBuf(lAddr)
              Foreach(0 until blockSize par mergePar) { j =>
                mergeBuf.enq(i, f.deq())
              }
            }
            val sOffset = mux(doubleBuf, numel, 0) 
            val sAddr = addr(0) + sOffset::addr(0) + sOffset + mergeSize par mergePar
            srcDoubleBuf(sAddr) store mergeBuf
          }
        }
        Pipe {
          blockSize := blockSize * ways
          mergeSize := mergeSize * ways
          mergeCount := mergeCount / ways
          doubleBuf := !doubleBuf
        }
      }
    }
  }

}
