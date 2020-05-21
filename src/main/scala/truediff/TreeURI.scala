package truediff

object TreeURI {
  type NodeTag = Class[_]
}

trait Node
case object RootNode extends Node
class NodeURI extends Node {
  override def toString: String = {
    val s = super.toString
    s.substring(s.lastIndexOf('@')+1)
  }
}
case class Literal[T](value: T) extends Node

trait Link
case object RootLink extends Link
case class NamedLink(name: String) extends Link {
  override def toString: String = name
}
case class ListIndexLink(list: Link, ix: Int) extends Link {
  override def toString: String = s"$list[$ix]"
}
