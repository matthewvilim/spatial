package spatial.tests.feature.vectors

import spatial.dsl._

@spatial class ShuffleTest extends SpatialTest {

  def main(args: Array[String]): Unit = {
    val out = ArgOut[Int]

    Accel {
      val data = SRAM[Int](4)
      val mask = SRAM[Bit](4)
      val reg = Reg[Int]
      Foreach(4 by 1 par 4) { i =>
        reg := shuffleCompress(data(i), mask(i))
      }
      out := reg
    }
  }
}
