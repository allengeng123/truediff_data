package truechange

sealed trait NodeTag
case class NamedTag(c: String) extends NodeTag {
  override def toString: String = c
}
object RootTag extends NamedTag("#Root")

case class OptionTag(ty: Type) extends NodeTag {
  override def toString: String = s"Option[$ty]"
}
case class ListTag(ty: Type) extends NodeTag {
  override def toString: String = s"List[$ty]"
}