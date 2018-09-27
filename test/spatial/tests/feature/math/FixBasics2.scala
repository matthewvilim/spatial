package spatial.tests.feature.math

import spatial.dsl._

@spatial class FixBasics2 extends SpatialTest {
  type T = FixPt[TRUE,_16,_16]

  def main(args: Array[String]): Unit = {

    val length = 8

    val data1 = Array.tabulate(length){i => if (i % 3 == 1) random[T](128) else -random[T](128)}
    val data2 = Array.tabulate(length){i => if (i % 3 == 1) random[T](128) else -random[T](128)}
    val data3 = Array.tabulate(length){i => if (i % 3 == 1) random[T](128) else -random[T](128)}

    val dram1 = DRAM[T](length)
    val dram2 = DRAM[T](length)
    val dram3 = DRAM[T](length)

    setMem(dram1, data1)
    setMem(dram2, data2)
    setMem(dram3, data3)

    val ff_out = DRAM[T](16,length)

    Accel{
      val sram1    = SRAM[T](length)
      val sram2    = SRAM[T](length)
      val sram3    = SRAM[T](length)

      sram1 load dram1
      sram2 load dram2
      sram3 load dram3

      val ff_out_sram = SRAM[T](16,length)

      Sequential.Foreach (0 until length) { i=>

        Pipe { ff_out_sram(0, i) = sram1(i) + sram2(i) }
        Pipe { ff_out_sram(1, i) = sram1(i) * sram2(i) }
        Pipe { ff_out_sram(2, i) = sram1(i) / sram2(i) }
        Pipe { ff_out_sram(3, i) = sqrt_approx(abs(sram1(i).to[Int])).to[T] }
        Pipe { ff_out_sram(4, i) = sram1(i) - sram2(i) }
        Pipe { ff_out_sram(5, i) = mux((sram1(i) < sram2(i)),1.to[T],0.to[T])  }
        Pipe { ff_out_sram(6, i) = mux((sram1(i) > sram2(i)),1.to[T],0.to[T]) }
        Pipe { ff_out_sram(7, i) = mux((sram1(i) == sram2(i)),1.to[T],0.to[T]) }

        Pipe { ff_out_sram(8, i) = abs(sram1(i)) }
        Pipe { ff_out_sram(9, i) = 0 /*exp(sram1(i))*/ }
        Pipe { ff_out_sram(10, i) = 0 /*ln(sram1(i))*/ }
        Pipe { ff_out_sram(11, i) = 0 /*(1.to[Float]/sram1(i).to[Float]).to[T]*/ }
        Pipe { ff_out_sram(12, i) = 0 /*1.to[T]/sqrt(sram1(i))*/ }
        Pipe { ff_out_sram(13, i) = 0 /*sigmoid(sram1(i))*/ }
        Pipe { ff_out_sram(14, i) = 0 /*tanh(sram1(i))*/ }
        Pipe { ff_out_sram(15, i) = sram1(i) * sram2(i) + sram3(i) }
      }

      ff_out store ff_out_sram
    }

    printArray((getMem(dram1)), "A : ")
    printArray((getMem(dram2)), "B : ")
    printArray((getMem(dram3)), "C : ")
    println("Result ADD, MUL, DIV, SRT, SUB, FLT, FGT, FEQ, ABS, EXP, LOG, REC, RST, SIG, TAN, FMA ")

    val out_ram = getMatrix(ff_out)
    val margin = 0.0001.to[T]
    val ops = Array[String]("ADD", "MUL", "DIV", "SRT", "SUB", "FLT", "FGT", "FEQ", "ABS", "EXP", "LOG", "REC", "RST", "SIG", "TAN", "FMA")
    var allgood = true


    (0::16,0::length).foreach{(i,j) =>
      val a = if (i == 0 ) {data1(j) + data2(j) }
      else if (i == 1 ) { data1(j) * data2(j) }
      else if (i == 2 ) { data1(j) / data2(j) }
      else if (i == 3 ) { sqrt_approx(abs(data1(j).to[Int])).to[T] }
      else if (i == 4 ) { data1(j) - data2(j) }
      else if (i == 5 ) { if (data1(j) < data2(j)) 1.to[T] else 0.to[T] }
      else if (i == 6 ) { if (data1(j) > data2(j)) 1.to[T] else 0.to[T] }
      else if (i == 7 ) { if (data1(j) == data2(j)) 1.to[T] else 0.to[T] }
      else if (i == 8 ) { abs(data1(j)) }
      // else if (i == 9 ) { exp(data1(j)) }
      // else if (i == 10) { ln(data1(j)) }
      // else if (i == 11) { (1.to[Float]/data1(j).to[Float]).to[T] }
      // else if (i == 12) { 1.to[T]/sqrt(data1(j)) }
      // else if (i == 13) { sigmoid(data1(j)) }
      // else if (i == 14) { tanh(data1(j)) }
      else if (i == 15) { data1(j) * data2(j) + data3(j) }
      else 0.to[T]
      val b = out_ram(i,j)
      val good = if (i == 3 || i == 12)  if (a >= (b - 8.to[T]) && a <= (b + 8.to[T])) true else false
                 else                    if (a >= (b - margin) && a <= (b + margin)) true else false
      println(r"$i,$j (${ops(i)}: $good) Expected: $a, Actual: $b")
      if (good == false) allgood = false
    }
    assert(allgood)
  }
}
