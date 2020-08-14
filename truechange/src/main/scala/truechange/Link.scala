package truechange

sealed trait Link {
  def isOptional: Boolean
  def getRawLink: RawLink
}

sealed trait RawLink extends Link

case class NamedLink(name: String) extends RawLink {
  override def toString: String = name
  override def isOptional: Boolean = false
  override def getRawLink: RawLink = this
}
object RootLink extends NamedLink("#root")

case class ListFirstLink(ty: Type) extends RawLink {
  override def toString: String = "#first"
  override def isOptional: Boolean = true
  override def getRawLink: RawLink = this
}

case class ListNextLink(ty: Type) extends RawLink {
  override def toString: String = "#next"
  override def isOptional: Boolean = true
  override def getRawLink: RawLink = this
}
