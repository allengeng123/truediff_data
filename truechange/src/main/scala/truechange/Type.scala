package truechange

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

trait LitType {
  def accepts(value: Any): Boolean
}
case class JavaLitType(cl: Class[_]) extends LitType {
  override def accepts(value: Any): Boolean = cl.isInstance(value)
}

case class Signature(sort: Type, constr: NodeTag, kids: Map[String, Type], lits: Map[String, LitType])
object RootSig extends Signature(AnyType, RootTag, Map(RootLink.name -> AnyType), Map())
