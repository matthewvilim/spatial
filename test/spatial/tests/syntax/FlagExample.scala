package spatial.tests.syntax

import spatial.dsl._

@spatial class FlagExample extends SpatialTest {
  override def compileArgs: Args = "-Dfoo=16 -Dbar=16 -Dzee=32" and
                                   "-Dfoo=4  -Dbar=12 -Dzee=16"

  def main(args: Array[String]): Unit = {
    val x = define("foo", 32)
    val y = define("bar", 64)
    val z = define("zee", 96)

    Console.out.println(s"foo: $x, bar: $y, z: $z")

    println(z)
    assert(x.to[Int] + y.to[Int] == z.to[Int])

    Accel { }
  }

}
