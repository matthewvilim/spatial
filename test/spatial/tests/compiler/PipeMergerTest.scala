package spatial.tests.compiler

import spatial.dsl._
import argon.Block

@spatial class PipeMergerTest extends SpatialTest {
  override def runtimeArgs: Args = "16"

  def main(args: Array[String]): Unit = {
    val N = ArgIn[Int]
    setArg(N, args(0).to[Int])
    val mat = (0::16,0::16){(i,j) => i }

    val img = DRAM[Int](16,16)
    setMem(img, mat)

    val res1 = ArgOut[Int]
    val res2 = ArgOut[Int]
    val res3 = ArgOut[Int]
    val res4 = ArgOut[Int]
    val res5 = ArgOut[Int]
    val res6 = ArgOut[Int]

    Accel {
      val sram = SRAM[Int](16,16)
      sram load img
      Pipe{
        Foreach(N by 1){i =>
          res1 := Reduce(Reg[Int])(5 by 1 par 5){i =>
            Reduce(Reg[Int])(5 by 1 par 5){j =>
              sram(i,j) * 3
            }{_+_}
          }{_+_}
        }
      }

      Pipe{
        Foreach(N by 1){i =>
          res2 := Reduce(Reg[Int])(5 by 1, 5 by 1 par 5){(i,j) =>
            sram(i,j) * 3
          }{_+_}
        }
      }

      Pipe{
        Foreach(N by 1){i =>
          res3 := List.tabulate(5){i => List.tabulate(5){j => sram(i,j) * 3}}.flatten.reduce{_+_}
        }
      }

      Pipe{Pipe{Pipe{Pipe{res4 := 5}}}}
      Pipe{Pipe{Pipe{Foreach(5 by 1){i => res5 := 5}}}}

      val s = SRAM[Int](16)
      Foreach(16 by 1 par 8){i => s(i) = 0}
      'REDUCEMERGE.Foreach(16 by 1){i => 
        s(i) = Reduce(Reg[Int])(5 by 1){i => N}{_+_}
      }

      Foreach(10 by 1){i => Pipe{res6 := s(i)}}
    }

    println("y1 = " + getArg(res1))
    println("y2 = " + getArg(res2))
    println("y3 = " + getArg(res3))
    println("y4 = " + getArg(res4))
    println("y5 = " + getArg(res5))
    println("y6 = " + getArg(res6))
    assert(getArg(res1) == 150)
    assert(getArg(res2) == 150)
    assert(getArg(res3) == 150)
    assert(getArg(res4) == 5)
    assert(getArg(res5) == 5)
    assert(getArg(res6) == args(0).to[Int]*5)
  }

  override def checkIR(block: Block[_]): Result = {
    // val pipes = block.nestedStms.collect{case p:spatial.node.UnitPipe => p }

    // require(pipes.length == 5,s"There should (probably) only be 5 Unit Pipes in this app, found ${pipes.length}")

    super.checkIR(block)
  }


}



@spatial class Perfection extends SpatialTest {

  def main(args: Array[String]): Unit = {
    val img = DRAM[Int](16)

    Accel {
      val sram = SRAM[Int](16)
      Foreach(16 by 1){ i => 
        sram(i) = Reduce(Reg[Int])(i + 2 by 1){j => j*j}{_+_}
      }
      img store sram
    }

    val gold = Array.tabulate(16){i => Array.tabulate(i+2){j => j*j}.reduce{_+_}}
    printArray(gold, "Gold")
    printArray(getMem(img), "Got")
    assert(gold == getMem(img))
  }

  override def checkIR(block: Block[_]): Result = {
    super.checkIR(block)
  }


}
