package truediff.changeset

import truediff.TreeURI.NodeTag
import truediff.{Link, Node, NodeURI}

sealed trait Change
trait NegativeChange extends Change
trait PositiveChange extends Change

case class DetachNode(parent: NodeURI, l: Link, node: NodeURI) extends NegativeChange
case class UnloadNode(parent: NodeURI, l: Link, node: NodeURI, kidLinks: Iterable[Link]) extends NegativeChange

case class AttachNode(parent: NodeURI, l: Link, node: NodeURI) extends PositiveChange
case class LoadNode(node: NodeURI, tag: NodeTag, kids: Iterable[(Link, Node)]) extends PositiveChange {
  override def toString: String = s"LoadNode($node, $tag, ${kids.map(p => p._1 + "=" + p._2).mkString(", ")})"
}
