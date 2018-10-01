package spatial.tests.feature.banking

import spatial.dsl._
import argon.Block

@spatial class ModuloIndexing extends SpatialTest {
  
  val P1 = 4
  val P2 = 4
  val P3 = 4
  val P4 = 4

  def main(args: Array[String]): Unit = {
    val dram1 = DRAM[Int](16)
    val dram2 = DRAM[Int](15)
    val dram3 = DRAM[Int](16)
    val dram4 = DRAM[Int](8)
    Accel {
      val x1 = SRAM[Int](16)
      Foreach(16 by 1 par P1){i => x1((i+1) % 16) = i}
      dram1 store x1

      val x2 = SRAM[Int](16)
      Foreach(16 by 1 par P2){i => x2((i+1) % 15) = i}
      dram2 store x2(0::15)

      val x3 = SRAM[Int](16)
      Foreach(32 by 2 par P3){i => x3(i/2) = i}
      dram3 store x3

      val x4 = SRAM[Int](32)
      Foreach(16 by 2 par P4){i => x4(i*2) = i}
      dram4 store x4(0::32 by 4)

    }

    val gold1 = Array.tabulate(16){i => (i - 1) % 16}
    val result1 = getMem(dram1)
    val gold2 = Array.tabulate(15){i => if (i < 2) 14+i else i-1}
    val result2 = getMem(dram2)
    val gold3 = Array.tabulate(16){i => i*2}
    val result3 = getMem(dram3)
    val gold4 = Array.tabulate(8){i => i*2}
    val result4 = getMem(dram4)

    printArray(gold1, "Gold1: ")
    printArray(result1, "Result1: ")
    printArray(gold2, "Gold2: ")
    printArray(result2, "Result2: ")
    printArray(gold3, "Gold3: ")
    printArray(result3, "Result3: ")
    printArray(gold4, "Gold4: ")
    printArray(result4, "Result4: ")

    val cksum = gold1 == result1 && gold2 == result2 && gold3 == result3 && gold4 == result4
    println(r"PASS: ${cksum}")
    assert(cksum)
  }

  override def checkIR(block: Block[_]): Result = {
    val srams = block.nestedStms.collect{case p:spatial.node.SRAMNew[_,_] => p }

    require(srams.length == 4, s"There should be 4 srams in this app if banking is all correct, found ${srams.length}")

    super.checkIR(block)
  }

}

