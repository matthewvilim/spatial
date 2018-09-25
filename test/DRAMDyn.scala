package spatial.tests.feature.memories.dram

import spatial.dsl._

@spatial class DRAMDynTest extends SpatialTest {

  def main(args: Array[String]): Unit = {
    val size = random[Int](100)

    Accel {
      val sram = SRAM[Int](50)
      val dram = DRAM[Int](size)
      if (size < 50) {
        dram(0::50) store sram
      } else {
        dram.dealloc()
      }
    }
  }
}
