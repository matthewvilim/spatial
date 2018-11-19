package spatial.lang.api

import argon._
import forge.tags._
import spatial.node._

trait ShuffleAPI { this: Implicits =>

  @rig def compress[A:Bits](data: Bits[A], mask: Bit): A = {
    stage(ShuffleCompress(data, mask))
  }

}
