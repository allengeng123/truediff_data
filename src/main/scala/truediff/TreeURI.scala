package truediff

object TreeURI {
  type NodeTag = Class[_]
}

sealed trait Node
//case object RootNode extends Node
class NodeURI extends Node {
  override def toString: String = {
    val s = super.toString
    s.substring(s.lastIndexOf('@')+1)
  }
}
case class Literal[T](value: T) extends Node

sealed trait Link
case object RootLink extends Link
case class NamedLink(name: String) extends Link {
  override def toString: String = name
}
case object ListNextLink extends Link