package argon

import forge.tags._

import scala.annotation.implicitNotFound
import scala.reflect.{ClassTag, classTag}

/** Describes conversion from type A to staged type B.
  *
  * val a: A
  * val b: B
  * val cast: Cast[A,B]
  *
  * val b' = cast.apply(a)
  * val a' = cast.getLeft(b) // May be None
  */
abstract class CastFunc[A,B:Type] {
  def tB: Type[B] = Type[B]
  @rig def apply(a: A): B
  @rig def get(a: A): Option[B] = Some(apply(a))
  @rig def getLeft(b: B): Option[A] = None

  @rig def saturating(a: A): B = apply(a)
  @rig def unbiased(a: A): B = apply(a)
  @rig def unbsat(a: A): B = apply(a)
  @rig def saturatingGetLeft(b: B): Option[A] = getLeft(b)
  @rig def unbiasedGetLeft(b: B): Option[A] = getLeft(b)
  @rig def unbsatGetLeft(b: B): Option[A] = getLeft(b)

  @rig def unchecked(a: A): B = apply(a)
  @rig def uncheckedGetLeft(b: B): Option[A] = getLeft(b)
}

/** Describes conversion from type A to staged type B and vice versa.
  *
  * val a: A
  * val b: B
  * val cast: Cast2Way[A,B]
  *
  * val b' = cast.apply(a)
  * val a' = cast.applyLeft(b)
  */
abstract class Cast2Way[A:Type,B:Type] extends CastFunc[A,B] {
  def tA: Type[A] = Type[A]
  @rig def apply(a: A): B
  @rig def applyLeft(b: B): A
  @rig override def getLeft(b: B): Option[A] = Some(applyLeft(b))

  @rig override def saturating(a: A): B = apply(a)
  @rig override def unbiased(a: A): B = apply(a)
  @rig override def unbsat(a: A): B = apply(a)
  @rig def saturatingLeft(b: B): A = applyLeft(b)
  @rig def unbiasedLeft(b: B): A = applyLeft(b)
  @rig def unbsatLeft(b: B): A = applyLeft(b)

  @rig def uncheckedLeft(b: B): A = applyLeft(b)
}


/**
  * Describes conversion from unstaged type A to staged type B.
  */
class Lifter[A,B:Type] extends CastFunc[A,B] {
  @rig def apply(a: A): B = tB.from(a, warnOnLoss = true)
  @rig override def saturating(a: A): B = tB.from(a, saturating = true)
  @rig override def unbiased(a: A): B = tB.from(a, unbiased = true)
  @rig override def unbsat(a: A): B = tB.from(a, unbiased = true, saturating = true)
  @rig override def unchecked(a: A): B = tB.from(a)
}



/**
  * Used when no other evidence exists for how to lift an unstaged type.
  * Unstaged types should have at most one implicit Lift typeclass instance.
  *
  * E.g. staged statement:
  *   if (c) 1 else 0
  *
  *   Needs implicit evidence for what type scala.Int should be lifted to.
  */
class Lift[B:Type](orig: Any, b: B) {
  def literal: Any = orig
  def unbox: B = b
  def B: Type[B] = Type[B]
}

