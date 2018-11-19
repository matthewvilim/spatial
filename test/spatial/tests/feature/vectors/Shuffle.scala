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
      val data = FIFO[Int](4)
      val mask = SRAM[Bit](4)
      val fifo = FIFO[Int](4)
      val p = 2

      data load dram(0::4 par p)
      mask load dram2(0::4 par p)
      Foreach(4 by 1 par p) { i =>
        val c = compress(pack(data.deq(), mask(i)))
        fifo.enq(c._1)
      }
      out := fifo.deq
    }

    println(getArg(out))
  }
}
