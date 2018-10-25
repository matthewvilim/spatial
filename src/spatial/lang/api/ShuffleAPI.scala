package spatial.lang.api

import argon._
import forge.tags._
import spatial.node._

trait ShuffleAPI { this: Implicits =>

  @api def shuffleCompress[A:Bits](data: Bits[A], mask: Bit): A = {
    //implicit val tA: Bits[A] = a.selfType
    stage(ShuffleCompress(data, mask))
  }

}
