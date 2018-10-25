package spatial.lang.api

import argon._
import forge.tags._
import spatial.node._

trait ShuffleAPI { this: Implicits =>

  @rig def compress[A:Bits](data: Vec[A], mask: Vec[Bit]): Vec[A] = {
    implicit val tV: Vec[A] = data.selfType
    stage(ShuffleCompress(data, mask))
  }

}
