package truechange

sealed trait Link {
  def isOptional: Boolean
}
case class NamedLink(name: String) extends Link {
  override def toString: String = name
  override def isOptional: Boolean = false
}
object RootLink extends NamedLink("#root")

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
