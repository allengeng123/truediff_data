package truediff.changeset

import truediff.TreeURI.NodeTag
import truediff.{Link, Literal, NodeURI}

sealed trait Change

sealed trait DetachOrUnload extends Change {
  val parent: NodeURI
  val link: Link
  val node: NodeURI
  val nodeTag: NodeTag
}
object DetachOrUnload {
  def unapply(neg: DetachOrUnload): Option[(NodeURI, Link, NodeURI, NodeTag)] = neg match {
    case DetachNode(parent, link, node, nodeTag) => Some((parent, link, node, nodeTag))
    case UnloadNode(parent, link, node, nodeTag) => Some((parent, link, node, nodeTag))
  }
}

sealed trait PositiveChange extends Change

case class DetachNode(parent: NodeURI, link: Link, node: NodeURI, nodeTag: NodeTag) extends DetachOrUnload
case class UnloadNode(parent: NodeURI, link: Link, node: NodeURI, nodeTag: NodeTag) extends DetachOrUnload

case class AttachNode(parent: NodeURI, l: Link, node: NodeURI) extends PositiveChange
case class LoadNode(node: NodeURI, tag: NodeTag, kids: Iterable[(String, NodeURI)], lits: Iterable[(String, Literal[_])]) extends PositiveChange {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep1 = if (kids.nonEmpty || lits.nonEmpty) ", " else ""
    val kidsLitsSep2 = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"LoadNode($node, $tag$kidsLitsSep1$kidsString$kidsLitsSep2$litsString)"
  }
}
