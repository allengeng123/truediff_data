package truechange

import scala.reflect.ClassTag

//case object RootNode extends Node
class NodeURI {
  override def toString: String = {
    val s = super.toString
    s.substring(s.lastIndexOf('@')+1)
  }
}
case class Literal[T: ClassTag](value: T) {
  def tag: LitTag = scala.reflect.classTag[T].runtimeClass
  override def toString: String = value.toString
}




sealed trait Link {
  def isOptional: Boolean
}
case object RootLink extends Link {
  override def toString: String = "#root"
  override def isOptional: Boolean = false
}
case class NamedLink(name: String) extends Link {
  override def toString: String = name
  override def isOptional: Boolean = false
}
case class OptionalLink(link: Link) extends Link {
  override def toString: String = super.toString + "?"
  override def isOptional: Boolean = true
}
case class ListFirstLink(ty: Type) extends Link {
  override def toString: String = "#first"
  override def isOptional: Boolean = true
}
case class ListNextLink(ty: Type) extends Link {
  override def toString: String = "#next"
  override def isOptional: Boolean = true
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

case class OptionType(ty: Type) extends Type {
  override def toString: String = s"Option[$ty]"
  override def isAssignableFrom(other: Type): Boolean = ty.isAssignableFrom(other)
}

case class Signature(sort: Type, constr: NodeTag, kids: Map[String, Type], lits: Map[String, LitTag])