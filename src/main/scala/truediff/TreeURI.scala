package truediff

import truediff.TreeURI.{LitTag, NodeTag}

import scala.reflect.ClassTag

object TreeURI {
  type NodeTag = Type
  type LitTag = Class[_]
}

sealed trait Node
//case object RootNode extends Node
class NodeURI extends Node {
  override def toString: String = {
    val s = super.toString
    s.substring(s.lastIndexOf('@')+1)
  }
}
case class Literal[T: ClassTag](value: T) extends Node {
  def tag: LitTag = scala.reflect.classTag[T].runtimeClass
  override def toString: String = value.toString
}




sealed trait Link
case object RootLink extends Link {
  override def toString: String = "#root"
}
case class NamedLink(tag: NodeTag, name: String) extends Link {
  override def toString: String = s"$tag.$name"
}
case class ListFirstLink(ty: Type) extends Link {
  override def toString: String = "#first"
}
case class ListNextLink(ty: Type) extends Link {
  override def toString: String = "#next"
}


sealed trait Type {
  def isAssignableFrom(ty: Type): Boolean
}
case object NothingType extends Type {
  override def toString: String = "Nothing"
  override def isAssignableFrom(ty: Type): Boolean = ty == NothingType
}
case object AnyType extends Type {
  override def toString: String = "Any"
  override def isAssignableFrom(ty: Type): Boolean = true
}
case class SortType(tag: Class[_]) extends Type {
  override def toString: String = tag.getSimpleName

  override def isAssignableFrom(ty: Type): Boolean = ty match {
    case SortType(tag2) => tag.isAssignableFrom(tag2)
    case NothingType => true
    case _ => false
  }
}
case class ListType(ty: Type) extends Type {
  override def toString: String = s"List[$ty]"
  override def isAssignableFrom(other: Type): Boolean = other match {
    case ListType(otherTy) => ty.isAssignableFrom(otherTy)
    case NothingType => true
    case _ => false
  }
}
case class UnwrappedListType(ty: Type) extends Type {
  override def toString: String = s"UnwrappedList[$ty]"
  override def isAssignableFrom(other: Type): Boolean = other match {
    case UnwrappedListType(otherTy) => ty.isAssignableFrom(otherTy)
    case NothingType => true
    case _ => false
  }
}
case class OptionType(ty: Type) extends Type {
  override def toString: String = s"Option[$ty]"
  override def isAssignableFrom(other: Type): Boolean = ty.isAssignableFrom(other)
}

case class Signature(sort: Type, constr: NodeTag, kids: Map[String, Type], lits: Map[String, LitTag])