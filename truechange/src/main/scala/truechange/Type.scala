package truechange

import truechange.Type.Subsorts

sealed trait Type {
  def subtypeOf(other: Type)(implicit subsorts: Subsorts): Boolean
}
object Type {
  type Subsorts = Map[SortType, Set[SortType]]
}
case object NothingType extends Type {
  override def toString: String = "Nothing"
  override def subtypeOf(other: Type)(implicit subsorts: Subsorts): Boolean =
    other == NothingType
}
case object AnyType extends Type {
  override def toString: String = "Any"
  override def subtypeOf(other: Type)(implicit subsorts: Subsorts): Boolean =
    true
}
case class SortType(name: String) extends Type {
  override def toString: String = name

  override def subtypeOf(other: Type)(implicit subsorts: Subsorts): Boolean = other match {
    case AnyType => true
    case ty: SortType => subsorts.getOrElse(this, Set()).contains(ty)
    case _ => false
  }
}
case class ListType(ty: Type) extends Type {
  override def toString: String = s"List[$ty]"
  override def subtypeOf(other: Type)(implicit subsorts: Subsorts): Boolean = other match {
    case ListType(otherTy) => ty.subtypeOf(otherTy)
    case AnyRef => true
    case _ => false
  }
}
case class OptionType(ty: Type) extends Type {
  override def toString: String = s"Option[$ty]"
  override def subtypeOf(other: Type)(implicit subsorts: Subsorts): Boolean =
    ty.subtypeOf(other)
}

trait LitType {
  def accepts(value: Any): Boolean
}
case class JavaLitType(cl: Class[_]) extends LitType {
  override def accepts(value: Any): Boolean = cl.isInstance(value)
}

case class Signature(sort: Type, constr: Tag, kids: Map[String, Type], lits: Map[String, LitType])
object RootSig extends Signature(AnyType, RootTag, Map(RootLink.name -> AnyType), Map())
