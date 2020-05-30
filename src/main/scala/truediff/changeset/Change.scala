package truediff.changeset

import truediff.TreeURI.NodeTag
import truediff.{Link, Literal, NodeURI}

sealed trait Change
sealed trait NegativeChange extends Change
sealed trait PositiveChange extends Change

case class DetachNode(parent: NodeURI, l: Link, node: NodeURI) extends NegativeChange
case class UnloadNode(parent: NodeURI, l: Link, node: NodeURI, kidLinks: Iterable[Link]) extends NegativeChange

case class AttachNode(parent: NodeURI, l: Link, node: NodeURI) extends PositiveChange
case class LoadNode(node: NodeURI, tag: NodeTag, kids: Iterable[(Link, NodeURI)], lits: Iterable[(Link, Literal[_])]) extends PositiveChange {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep1 = if (kids.nonEmpty || lits.nonEmpty) ", " else ""
    val kidsLitsSep2 = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"LoadNode($node, $tag$kidsLitsSep1$kidsString$kidsLitsSep2$litsString)"
  }
}
