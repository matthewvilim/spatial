package spatial.tests.feature.control

import spatial.dsl._

@spatial class Breakpoint extends SpatialTest {

  def main(args: Array[String]): Void = {
    val y = ArgOut[Int]
    val z = HostIO[Int]

    Accel {
      Sequential.Foreach(16 by 1) {i =>
        sleep(100)
        Pipe{y := i}
        if (i == 8) { Sequential{
          Pipe{ exit() }
          sleep(100)
        }} // breakpoint() also works
        Pipe{z := i}
      }
    }

    val Y = getArg(y)
    val Z = getArg(z)

    println("Y = " + Y + ", Z = " + Z)

    val cksum = Y == 8 && Z == 7
    println("PASS: " + cksum + " (Breakpoint)")
    assert(cksum)
  }
}


@spatial class Break extends SpatialTest {
  override def runtimeArgs: Args = "9 4"

  val tileSize = 16
  def main(args: Array[String]): Void = {
    val iBreakAt = ArgIn[Int]
    val breakInner = args(0).to[Int]
    setArg(iBreakAt, breakInner)
    val oBreakAt = ArgIn[Int]
    val breakOuter = args(1).to[Int]
    setArg(oBreakAt, breakOuter)

    val dsrc = DRAM[Int](6,tileSize)
    setMem(dsrc, (0::6,0::tileSize){(i,j) => i+j})

    val d2 = DRAM[Int](tileSize)
    // val d3 = DRAM[Int](tileSize)
    val d4 = DRAM[Int](tileSize)
    val d5 = DRAM[Int](tileSize)
    // val d6 = DRAM[Int](tileSize)
    // val d7 = DRAM[Int](tileSize)

    Accel {
      val src = SRAM[Int](tileSize)
      val s2 = SRAM[Int](tileSize)
      // val s3 = SRAM[Int](tileSize)
      val s4 = SRAM[Int](tileSize)
      val s5 = SRAM[Int](tileSize)
      // val s6 = SRAM[Int](tileSize)
      // val s7 = SRAM[Int](tileSize)
      Sequential.Foreach(3 by 1){_ => 
        Foreach(tileSize by 1){i => src(i) = i; s2(i) = 0; /*s3(i) = 0; */s4(i) = 0; /*s5(i) = 0; s6(i) = 0; s7(i) = 0*/}

        val stop2 = Reg[Bit](false)
        Sequential(breakWhen = stop2).Foreach(tileSize by 1) {i =>

          stop2 := src(i) == iBreakAt.value
          s2(i) = src(i)
        }
        d2 store s2

        val stop4 = Reg[Bit](false)
        Sequential(breakWhen = stop4).Foreach(6 by 1){i => 
          src load dsrc(i,0::tileSize)
          stop4 := (src(0) == oBreakAt.value)
          Foreach(tileSize by 1){ j => 
            s4(j) = src(j)  
          }
          d4 store s4
        }

        val stop5 = Reg[Bit](false)
        Stream(breakWhen = stop5).Foreach(*){i => 
          val fifo = FIFO[Int](4)
          Pipe{fifo.enq(i)}
          Pipe{
            val x = fifo.deq
            Foreach(tileSize by 1 par tileSize){j => s5(j) = i}
            d5 store s5
            stop5 := x == oBreakAt.value
          }
        }
      }
    }

    val gold2 = Array.tabulate(tileSize){i => if (i < args(0).to[Int]) i else 0}
    // val gold3 = Array.tabulate(tileSize){i => if (i < iBreakAt) i else 0}
    val gold4 = Array.tabulate(tileSize){i => i + (args(1).to[Int] - 1)}
    val gold5 = Array.tabulate(tileSize){i => args(1).to[Int]}
    // val gold6 = Array.tabulate(tileSize){i => if (i < iBreakAt) i else 0}
    // val gold7 = Array.tabulate(tileSize){i => i + oBreakAt}

    printArray(gold2, "Gold2:")
    printArray(getMem(d2), "Got2")
    // printArray(gold3, "Gold3:")
    // printArray(getMem(d3), "Got3")
    printArray(gold4, "Gold4:")
    printArray(getMem(d4), "Got4")
    printArray(gold5, "Gold5:")
    printArray(getMem(d5), "Got5")
    // printArray(gold6, "Gold6:")
    // printArray(getMem(d6), "Got6")
    // printArray(gold7, "Gold7:")
    // printArray(getMem(d7), "Got7")

    val cksum2 = gold2 == getMem(d2)
    // val cksum3 = gold3 == getMem(d3)
    val cksum4 = gold4 == getMem(d4)
    val cksum5 = gold5 == getMem(d5)
    // val cksum6 = gold6 == getMem(d6)
    // val cksum7 = gold7 == getMem(d7)

    // val cksum = cksum2 && cksum3 && cksum4 && cksum5 && cksum6 && cksum7
    val cksum = cksum2 && cksum4 && cksum5
    // println(r"cksums: $cksum2 $cksum3 $cksum4 $cksum5 $cksum6 $cksum7")
    println(r"cksums: $cksum2 $cksum4 $cksum5")
    println("PASS: " + cksum + " (Break)")
  }
}
