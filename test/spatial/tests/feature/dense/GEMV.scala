package spatial.tests.feature.dense

import spatial.dsl._

@spatial class GEMV extends SpatialTest {
    override def runtimeArgs: Args = "512 512"
   def main(args: Array[String]): Unit = {
    val M = ArgIn[Int] //8192
    setArg(M, args(0).to[Int])
    val N = ArgIn[Int] //8192
    setArg(N, args(1).to[Int])
    val B = 64 //(1 -> 1 -> 1024)    // Parameter [1, 1024] with step of 64
    val T = 64 //(1 -> 1 -> 1024)
    val Pblk = 1 //(1 -> 1 -> 32)
    val Pelm = 1 //(1 -> 1 -> 16)

    val hostMatrix: Matrix[Int] = (0::M,0::N){(i,j) => random[Int] }
    val hostVector: Array[Int] = Array.tabulate(N){i => random[Int] }

    val matrix = DRAM[Int](M,N)
    val vector = DRAM[Int](N)
    val output = DRAM[Int](M)

    setMem(matrix, hostMatrix)
    setMem(vector, hostVector)

    val M_reg = ArgIn[Int]
    val N_reg = ArgIn[Int]

    setArg(M_reg, M)
    setArg(N_reg, N)

    Accel {
      // over rows
      Foreach(M_reg by T ) { i =>
        val outBlk = SRAM[Int](B)

        Foreach(T by 1) { ii =>
          val element = Reduce(0)(N_reg by B par Pblk) { j =>

            val rowBlk = SRAM[Int](B)
            val vecBlk = SRAM[Int](B)

            rowBlk load matrix(i+ii, j :: j + B)
            vecBlk load vector(j :: j + B)

            val res = Reg[Int]
            //Foreach(B by 1 par Pelm){jj => res :+= rowBlk(jj)*vecBlk(jj) }
            Reduce(0)(B by 1 par Pelm){jj => rowBlk(jj) * vecBlk(jj) }{(a,b) => a + b}
          }{(a,b) => a + b}

          outBlk(ii) = element
        }

        output(i::i+B) store outBlk
      }
    }

    val gold = Array.tabulate(M){i =>
      Array.tabulate(N){j => hostMatrix(i,j)*hostVector(j) }.reduce(_+_)
    }

    val outHost: Array[Int] = getMem(output)

    assert(gold == outHost, "Gold and out did not match!")

    printArray(outHost)
    println(r"Pass: ${gold == outHost}")
  }
}
