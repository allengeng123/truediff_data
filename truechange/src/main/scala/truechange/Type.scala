package truechange

import truechange.Type.Subsorts

sealed trait Type {
  def isAssignableFrom(other: Type)(implicit subsorts: Subsorts): Boolean
}
object Type {
  type Subsorts = Map[SortType, Set[SortType]]
}
case object NothingType extends Type {
  override def toString: String = "Nothing"
  override def isAssignableFrom(other: Type)(implicit subsorts: Subsorts): Boolean =
    other == NothingType
}
case object AnyType extends Type {
  override def toString: String = "Any"
  override def isAssignableFrom(other: Type)(implicit subsorts: Subsorts): Boolean =
    true
}
case class SortType(name: String) extends Type {
  override def toString: String = name

  override def isAssignableFrom(other: Type)(implicit subsorts: Subsorts): Boolean = other match {
    case NothingType => true
    case other: SortType => name==other.name || subsorts.getOrElse(other, Set()).contains(this)
    case _ => false
  }
}
case class ListType(ty: Type) extends Type {
  override def toString: String = s"List[$ty]"
  override def isAssignableFrom(other: Type)(implicit subsorts: Subsorts): Boolean = other match {
    case NothingType => true
    case ListType(otherTy) => ty.isAssignableFrom(otherTy)
    case _ => false
  }
}
case class OptionType(ty: Type) extends Type {
  override def toString: String = s"Option[$ty]"
  override def isAssignableFrom(other: Type)(implicit subsorts: Subsorts): Boolean = other match {
    case NothingType => true
    case _ => ty.isAssignableFrom(other)
  }
}

case class RefType(ty: Type) extends Type {
  override def toString: String = s"Ref[$ty]"
  override def isAssignableFrom(other: Type)(implicit subsorts: Subsorts): Boolean = other match {
    case NothingType => true
    case RefType(otherTy) => ty.isAssignableFrom(otherTy)
    case _ => false
  }
}

trait LitType {
  def accepts(value: Any): Boolean
}
case class JavaLitType(cl: Class[_]) extends LitType {
  override def accepts(value: Any): Boolean = cl.isInstance(value)
}

case class Signature(sort: Type, constr: Tag, kids: Map[String, Type], lits: Map[String, LitType])
object RootSig extends Signature(AnyType, RootTag, Map(RootLink.name -> AnyType), Map())

trait NodeMetaInfo {
  def sort: SortType
  def superSorts: Set[SortType]
  def links: Map[NamedLink, Type]
  def litLinks: Map[NamedLink, LitType]
}
