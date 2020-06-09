package truechange

sealed trait Change

sealed trait NegativeChange extends Change
sealed trait PositiveChange extends Change





case class DetachNode(parent: NodeURI, link: Link, node: NodeURI, tag: NodeTag) extends NegativeChange {
  override def toString: String = s"detach ${tag}_$node from $parent.$link"
}

case class UnloadNode(node: NodeURI, tag: NodeTag, kids: Iterable[(String, NodeURI)], lits: Iterable[(String, Literal[_])]) extends NegativeChange {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"unload ${tag}_$node($kidsString$kidsLitsSep$litsString)"
  }
}

case class AttachNode(parent: NodeURI, link: Link, node: NodeURI) extends PositiveChange {
  override def toString: String = s"attach $node to $parent.$link"
}

case class LoadNode(node: NodeURI, tag: NodeTag, kids: Iterable[(String, NodeURI)], lits: Iterable[(String, Literal[_])]) extends PositiveChange {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"load   ${tag}_$node($kidsString$kidsLitsSep$litsString)"
  }
}
