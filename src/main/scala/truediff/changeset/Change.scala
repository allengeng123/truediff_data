package truediff.changeset

import truediff.TreeURI.NodeTag
import truediff.{Link, Literal, NodeURI}

sealed trait Change

sealed trait NegativeChange extends Change

object DetachOrUnload {
  def unapply(neg: Change): Option[(NodeURI, Link, NodeURI, NodeTag)] = neg match {
    case DetachNode(parent, link, node, nodeTag) => Some((parent, link, node, nodeTag))
    case UnloadNode(parent, link, node, nodeTag) => Some((parent, link, node, nodeTag))
    case _ => None
  }
}

sealed trait PositiveChange extends Change

case class DetachNode(parent: NodeURI, link: Link, node: NodeURI, nodeTag: NodeTag) extends NegativeChange {
  override def toString: String = s"detach $node:$nodeTag from $parent.$link"
}

case class UnloadNode(parent: NodeURI, link: Link, node: NodeURI, nodeTag: NodeTag) extends NegativeChange {
  override def toString: String = s"unload $node:$nodeTag from $parent.$link"
}

case class AttachNode(parent: NodeURI, link: Link, node: NodeURI) extends PositiveChange {
  override def toString: String = s"attach $node to $parent.$link"
}

case class LoadNode(node: NodeURI, tag: NodeTag, kids: Iterable[(String, NodeURI)], lits: Iterable[(String, Literal[_])]) extends PositiveChange {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"load   $tag($kidsString$kidsLitsSep$litsString) as $node"
  }
}
