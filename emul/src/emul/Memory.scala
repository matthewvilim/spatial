package emul

import scala.reflect.ClassTag

class Memory[T:ClassTag](name: String) {
  var data: Array[T] = _
  private var needsInit: Boolean = true
  def initMem(size: Int): Unit = if (needsInit) {
    data = Array.ofDim[T](size)
    needsInit = false
  }

  def apply(i: Int): T = data.apply(i)
  def update(i: Int, x: T): Unit = data.update(i, x)
}
