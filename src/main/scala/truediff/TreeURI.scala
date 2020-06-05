package truediff

import truediff.TreeURI.NodeTag

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
case object RootLink extends Link {
  override def toString: String = "#root"
}
case class NamedLink(tag: NodeTag, name: String) extends Link {
  override def toString: String = s"${tag.getSimpleName}.$name"
}
case class OptionalLink(link: Link) extends Link {
  override def toString: String = s"$link?"
}
case object ListFirstLink extends Link {
  override def toString: String = "#first"
}
case object ListNextLink extends Link {
  override def toString: String = "#next"
}