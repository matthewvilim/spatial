package spatial.tests.feature.banking

import argon._
import spatial.dsl._
import spatial.node._

@spatial class RegCoalescing extends SpatialTest {

  def main(args: Array[String]): Unit = {
    val out1 = ArgOut[Int]
    val out2 = ArgOut[Int]
    Accel {
      Foreach(16 by 1) {i =>
        val reg = Reg[Int]
        Pipe { reg := i }
        Pipe { out1 := reg.value }
        Pipe { out2 := reg.value }
      }
    }
    assert(getArg(out1) == 15)
    assert(getArg(out2) == 15)
  }

  override def checkIR(block: Block[_]): Result = {
    import spatial.metadata.memory._

    val regs = block.nestedStms.collect{case s @ Op(_:RegNew[_]) => s}

    regs.foreach{s => Console.out.println(stm(s) + s" (${s.name})")}

    val regDuplicates = regs.filter{r => r.name.isDefined && r.name.get.startsWith("reg") }

    regDuplicates.length shouldBe 1
    regDuplicates.headOption.foreach{head =>
      (head.instance.depth > 1) shouldBe true
    }
    super.checkIR(block)
  }
}
