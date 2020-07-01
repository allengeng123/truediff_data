package truechange

sealed trait Tag
case class NamedTag(c: String) extends Tag {
  override def toString: String = c
}
object RootTag extends NamedTag("#Root")

case class OptionTag(ty: Type) extends Tag {
  override def toString: String = s"Option[$ty]"
}
case class ListTag(ty: Type) extends Tag {
  override def toString: String = s"List[$ty]"
}