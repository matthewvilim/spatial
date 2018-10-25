package spatial.tests.feature.vectors

import spatial.dsl._

@spatial class ShuffleTest extends SpatialTest {

  def main(args: Array[String]): Unit = {
    val out = ArgOut[Int]

    Accel {
      val data = SRAM[Int](4)
      val mask = SRAM[Bit](4)
      val fifo = FIFO[Int](4)
      val p = 4
      Foreach(4 by 1 par p) { i =>
        val test = Vec.fromSeq(List.tabulate(p) { i => data(i) })
        val test2 = Vec.fromSeq(List.tabulate(p) { i => mask(i) })
        val out = compress(test, test2)
        val test3 = out(i::i)
        fifo.enq(test3)
      }
      out := fifo.deq
    }

    println(getArg(out))
  }
}
