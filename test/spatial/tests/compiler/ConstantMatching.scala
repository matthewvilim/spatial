package spatial.tests.compiler

import argon._
import spatial.SpatialTestbench
import spatial.lang._

class ConstantMatching extends SpatialTestbench {
  "MatchI32" should "match constants with Literal" in {
    val x = I32(32)
    val y = I32(32)

    if (!(x.tp =:= y.tp)) throw new Exception("Types differ!")
    if (!(x.c.get == y.c.get)) throw new Exception("Values differ!")

    x match {
      case Literal(32) => println("Aww yeah")
      case _ => throw new Exception("Nooo")
    }
  }

}