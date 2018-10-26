package spatial.tests.feature.vectors

import spatial.dsl._

@spatial class ShuffleTest extends SpatialTest {

  def main(args: Array[String]): Unit = {
    val out = ArgOut[Int]

    val src = Array.tabulate(4){ i => i }
    val src2 = Array.tabulate(4){ i => 1.to[Bit] }
    val dram = DRAM[Int](4)
    val dram2 = DRAM[Bit](4)
    setMem(dram, src)
    setMem(dram2, src2)

    Accel {
      val data = SRAM[Int](4)
      val mask = SRAM[Bit](4)
      val fifo = FIFO[Int](4)
      val p = 4

      data load dram(0::4)
      mask load dram2(0::4)
      Foreach(4 by 1 par p) { i =>
        val test = Vec.fromSeq(List.tabulate(p) { i => data(i) })
        val test2 = Vec.fromSeq(List.tabulate(p) { i => mask(i) })
        val comp = compress(test, test2)
        fifo.enq(comp(i))
      }
      out := fifo.deq
    }

    println(getArg(out))
  }
}
