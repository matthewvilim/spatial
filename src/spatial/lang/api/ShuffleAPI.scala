package spatial.lang.api

import argon._
import forge.tags._
import spatial.node._

trait ShuffleAPI { this: Implicits =>

  @rig def compress[T](data: Vec[T], mask: Vec[Bit]): Vec[T] = {
    implicit val tV: Vec[T] = data
    stage(ShuffleCompress(data, mask))
  }

}
