package truechange

sealed trait NodeTag
case object RootTag extends NodeTag
case class ConstrTag(c: String) extends NodeTag
case class OptionTag(ty: Type) extends NodeTag
case class ListTag(ty: Type) extends NodeTag