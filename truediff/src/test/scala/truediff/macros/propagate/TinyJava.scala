package truediff.macros.propagate

import truediff.Diffable
import truediff.macros.diffable

@diffable
case class ClassDeclaration(name: String, isFinal: Boolean, members: List[ClassMember]) extends Diffable

@diffable
trait ClassMember extends Diffable

@diffable
case class FieldDeclaration(name: String, visibility: Visibility) extends ClassMember {}

@diffable
trait Visibility extends Diffable

@diffable
case class PublicVisibility() extends Visibility

@diffable
case class ProtectedVisibility() extends Visibility

@diffable case class PrivateVisibility() extends Visibility


//trait Primitive {
//  val value: Any
//}
//
//@diffable
//case class BooleanConstant(value: Boolean) extends Primitive
//
//@diffable
//case class IntegerConstant(value: Int) extends Primitive
//
//@diffable
//case class LongConstant(value: Long) extends Primitive
//
