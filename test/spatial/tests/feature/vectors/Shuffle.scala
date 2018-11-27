package spatial.tests.feature.vectors

import spatial.dsl._

@spatial class ShuffleTest extends SpatialTest {

  def main(args: Array[String]): Unit = {
    val out = ArgOut[Int]

    val src = Array.tabulate(128){ i => i.to[Int] }
    val src2 = Array.tabulate(128){ i => 1.to[Bit] }
    val dataDRAM = DRAM[Int](128)
    val maskDRAM = DRAM[Bit](128)
    setMem(dataDRAM, src)
    setMem(maskDRAM, src2)

    Accel {
      val data = FIFO[Int](128)
      val mask = FIFO[Bit](128)
      val fifo = FIFO[Int](128)
      val p = 8

      data load dataDRAM(0::128 par p)
      mask load maskDRAM(0::128 par p)
      Foreach(128 par p) { i =>
        val c = compress(pack(data.deq(), mask.deq()))
        fifo.enq(c._1)
      }
      out := fifo.deq
    }

    println(getArg(out))
  }
}
