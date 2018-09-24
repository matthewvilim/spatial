package spatial.tests.feature.memories.dram

import spatial.dsl._

@spatial class DRAMDynTest extends SpatialTest {

  def main(args: Array[String]): Unit = {
    val size = random[Int](100)
    val dram = DRAM1[Int]()

    Accel {
      val sram = SRAM[Int](50)
      dram.alloc(size)
      if (size < 50) {
        dram(0::50) store sram
      } else {
        dram.dealloc()
      }
    }
  }
}
