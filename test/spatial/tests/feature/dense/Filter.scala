package spatial.tests.feature.vectors

import spatial.dsl._

@spatial class FilterTest extends SpatialTest {

  def main(args: Array[String]): Unit = {
    val n = 128

    val data = Array.tabulate(n){ i => i.to[Int] }
    val mask = Array.tabulate(n){ i => (i % 2 == 0).to[Int] }
    val dataDRAM = DRAM[Int](n)
    val maskDRAM = DRAM[Int](n)
    val outDRAM = DRAM[Int](n)
    setMem(dataDRAM, data)
    setMem(maskDRAM, mask)

    Accel {
      val p = 8

      val dataF = FIFO[Int](p)
      val maskF = FIFO[Int](p)
      val outF = FIFO[Int](p)

      Stream {
        dataF load dataDRAM(0::n par p)
        maskF load maskDRAM(0::n par p)
        Foreach(n par p) { i =>
          val m = maskF.deq().bit(0)
          val c = compress(pack(dataF.deq(), m))
          outF.enq(c._1)
        }
        outDRAM(0::n par p) store outF
      }
    }

    //println(mask)

  }
}
