package spatial.lang.api

import argon._
import argon.tags.struct
import forge.tags._
import spatial.node._

trait ShuffleAPI { this: Implicits =>
  @api def compress[A:Bits](in: Tup2[Bit, A]): Tup2[Bit, A] = {
    stage(ShuffleCompress(in))
  }
}
