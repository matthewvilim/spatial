package argon.lang

import forge.tags._
import argon._
import argon.node._

trait Struct[A] extends Top[A] with Ref[Nothing,A] {
  override val __neverMutable = false
  val box: A <:< Struct[A]
  private implicit lazy val A: Struct[A] = this.selfType

  @rig def field[F:Type](name: String): F = Struct.field[A,F](me, name)

  @rig private def __field[F](name: String, tp: Type[_]): Sym[F] = {
    implicit val F: Type[F] = tp.asInstanceOf[Type[F]]
    F.boxed(Struct.field[A,F](me, name))
  }

  def fields: Seq[(String,ExpType[_,_])]
  @rig def fieldMap: Seq[(String,Exp[_,_])] = fields.map{case (name,tp) => (name, __field(name, tp)) }

  @api override def neql(that: A): Bit = fieldMap.zip(box(that).fieldMap).map{case ((_, a: Bits[_]), (_,b: Bits[_])) => a !== b}.reduce{_|_}
  @api override def eql(that: A): Bit = fieldMap.zip(box(that).fieldMap).map{case ((_, a: Bits[_]), (_,b: Bits[_])) => a === b}.reduce{_&_}
}

object Struct {
  def tp[S:Struct]: Struct[S] = implicitly[Struct[S]]

  @rig def apply[S:Struct](elems: (String,Sym[_])*): S = stage(SimpleStruct[S](elems))
  @rig def field[S:Struct,A:Type](struct: S, name: String): A = stage(FieldApply[S,A](Struct.tp[S].box(struct),name))
  @rig def field_update[S:Struct,A:Type](struct: S, name: String, data: A): Void = stage(FieldUpdate[S,A](Struct.tp[S].box(struct),name,data))
}
