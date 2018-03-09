package core

import scala.annotation.unchecked.{uncheckedVariance => uV}

sealed abstract class Def[+A,+B] {
  def isValue: Boolean = isConst || isParam
  def isConst: Boolean = false
  def isParam: Boolean = false
  def isBound: Boolean = false
  def isNode: Boolean = false
  def isError: Boolean = false
  def isType: Boolean = false

  def getValue: Option[A] = None
  def getOp: Option[Op[B@uV]] = None
  def getID: Option[Int] = None
}
object Def {
  case object TypeRef extends Def[Nothing,Nothing] {
    override def isType: Boolean = true
  }
  case class Error[B](id: Int) extends Def[Nothing,B] {
    override def isError: Boolean = true
  }
  case class Bound[B](id: Int) extends Def[Nothing,B] {
    override def isBound: Boolean = true
    override def getID: Option[Int] = Some(id)
  }
  case class Const[A](c: A) extends Def[A,Nothing] {
    override def isConst: Boolean = true
    override def getValue: Option[A] = Some(c)
  }
  case class Param[A](id: Int, c: A) extends Def[A,Nothing] {
    override def isParam: Boolean = true
    override def getValue: Option[A] = Some(c)
    override def getID: Option[Int] = Some(id)
  }
  case class Node[B](id: Int, op: Op[B]) extends Def[Nothing,B] {
    override def isNode: Boolean = true
    override def getOp: Option[Op[B]] = Some(op)
    override def getID: Option[Int] = Some(id)
  }
}

/** Helper function for testing equality with literal constants
  * The extracted value may not be the same type as the normal constant type for the given symbol
  * (This is a hack to get around the fact that new types cannot have value equality with Int, Long, etc.)
  */
object Literal {
  def unapply(x: Any): Option[Any] = x match {
    case s: Sym[_] if s.isConst => s.__extract
    case _ => None
  }
}

/** Used to match on or extract the constant value of this symbol if it is constant (not a parameter) */
object Const {
  def unapply[C,A](x: Exp[C,A]): Option[C] = if (x.isConst) x.c else None
}

/** Used to match on or extract the constant value of this symbol if it is a parameter (not a constant) */
object Param {
  def unapply[C,A](x: Exp[C,A]): Option[C] = if (x.isParam) x.c else None
}

/** Used to match on or extract the constant value of this symbol if it is a parameter or constant */
object Value {
  def unapply[C,A](x: Exp[C,A]): Option[C] = x.c
}