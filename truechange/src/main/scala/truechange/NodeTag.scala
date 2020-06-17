package truechange

sealed trait NodeTag
case object RootTag extends NodeTag {
  override def toString: String = "#Root"
}
case class ConstrTag(c: String) extends NodeTag {
  override def toString: String = c
}
case class OptionTag(ty: Type) extends NodeTag {
  override def toString: String = s"Option[$ty]"
}
case class ListTag(ty: Type) extends NodeTag {
  override def toString: String = s"List[$ty]"
}