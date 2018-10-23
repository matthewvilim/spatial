import spatial.dsl._

@spatial object GEMV extends SpatialApp {

  type T = FixPt[TRUE,_10,_22]

  val N = 1024  // A is NxN, and x is N wide.

  def main(args: Array[String]): Unit = {

    // These are on the HOST
    val x_host = loadCSV1D[T]("vector.csv")
    val A_host = loadCSV2D[T]("matrix.csv")

    val vec = DRAM[T](N)
    val matrix = DRAM[T](N,N)
    val out = DRAM[T](N)
    val p = 16

    setMem(vec, x_host)
    setMem(matrix, A_host)

    Accel {
      // TODO: Add in your accelerator code here
      val sramVec = SRAM[T](N)
      val sramRow = SRAM[T](N)
      val sramOut = SRAM[T](N)
      sramVec load vec(0::N par p)
      Foreach(N by 1 par 2) { i =>
        sramRow load matrix(i, 0::N par p)
        val a = Reduce(0)(N by 1 par p) { j =>
          sramRow(j) * sramVec(j)
        }{(a,b) => a + b }
        sramOut(i) = a
      }
    }

    // TODO: Write output using writeCSV1D to output.csv
    writeCSV1D(getMem(out), "output.csv")
  }
}
