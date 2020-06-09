package truechange

sealed trait Change

case class Detach(parent: NodeURI, link: Link, node: NodeURI, tag: NodeTag) extends Change {
  override def toString: String = s"detach ${tag}_$node from $parent.$link"
}

case class Unload(node: NodeURI, tag: NodeTag, kids: Iterable[(String, NodeURI)], lits: Iterable[(String, Literal[_])]) extends Change {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"unload ${tag}_$node($kidsString$kidsLitsSep$litsString)"
  }
}

case class Attach(parent: NodeURI, link: Link, node: NodeURI) extends Change {
  override def toString: String = s"attach $node to $parent.$link"
}

case class Load(node: NodeURI, tag: NodeTag, kids: Iterable[(String, NodeURI)], lits: Iterable[(String, Literal[_])]) extends Change {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"load   ${tag}_$node($kidsString$kidsLitsSep$litsString)"
  }
}
