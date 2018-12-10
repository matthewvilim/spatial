package spatial.lang
package control

import argon._
import forge.tags._
import spatial.metadata.control._
import spatial.node._

abstract class Directives(options: CtrlOpt) {
  lazy val Foreach   = new ForeachClass(options)
  lazy val Reduce    = new ReduceClass(options)
  lazy val Fold      = new FoldClass(options)
  lazy val MemReduce = new MemReduceClass(options)
  lazy val MemFold   = new MemFoldClass(options)

  @rig protected def unit_pipe(func: => Any, ens: Set[Bit] = Set.empty): Void = {
    val block = stageBlock{ func; void }
    stageWithFlow(UnitPipe(Set.empty, block)){pipe => options.set(pipe) }
  }
}

class Pipe(name: Option[String], ii: Option[Int]) extends Directives(CtrlOpt(name,Some(Pipelined),ii)) {
  /** "Pipelined" unit controller */
  @api def apply(func: => Any): Void = unit_pipe(func)
  @rig def apply(ens: Set[Bit], func: => Any): Void = unit_pipe(func, ens)

  def II(ii: Int) = new Pipe(name, Some(ii))
}
class Stream(name: Option[String], stopWhen: Option[Reg[Bit]]) extends Directives(CtrlOpt(name,Some(Streaming),None,stopWhen)) {
  /** "Streaming" unit controller */
  @api def apply(func: => Any): Void = unit_pipe(func)

  @api def apply(wild: Wildcard)(func: => Any): Void = Stream.Foreach(*){_ => func }

  /** "Stream" controller that will break immediately when `breakWhen` is true.
      It will reset the value in `breakWhen` each time the controller finishes.
      Note that this behavior is discouraged because it can lead to tricky 
      behavior if you are not careful about how you use it.
    */
  @api def apply(breakWhen: Reg[Bit]): Stream = new Stream(name, Some(breakWhen))
}
class Sequential(name: Option[String], stopWhen: Option[Reg[Bit]]) extends Directives(CtrlOpt(name,Some(Sequenced),None,stopWhen)) {
  /** "Sequential" unit controller */
  @api def apply(func: => Any): Void = unit_pipe(func)

  /** "Sequential" controller that will break immediately when `breakWhen` is true.
      It will reset the value in `breakWhen` each time the controller finishes
    */
  @api def apply(breakWhen: Reg[Bit]): Sequential = new Sequential(name, Some(breakWhen))
}

object Named {
  def apply(name: String) = new NamedClass(name)
}

object Pipe extends Pipe(ii = None, name = None)
object Sequential extends Sequential(name = None, stopWhen = None)
object Stream extends Stream(name = None, stopWhen = None)

object Accel extends AccelClass(None)
object Foreach extends ForeachClass(CtrlOpt(None,None,None,None))
object Reduce extends ReduceClass(CtrlOpt(None,None,None,None))
object Fold extends FoldClass(CtrlOpt(None,None,None,None))
object MemReduce extends MemReduceClass(CtrlOpt(None,None,None,None))
object MemFold extends MemFoldClass(CtrlOpt(None,None,None,None))
