package truechange

sealed trait Change

case class Detach(node: NodeURI, tag: NodeTag, link: Link, parent: NodeURI, ptag: NodeTag) extends Change {
  override def toString: String = s"detach ${tag}_$node from ${ptag}_$parent.$link"
}
case class Attach(node: NodeURI, tag: NodeTag, link: Link, parent: NodeURI, ptag: NodeTag) extends Change {
  override def toString: String = s"attach ${tag}_$node to ${ptag}_$parent.$link"
}

case class Unload(node: NodeURI, tag: NodeTag, kids: Iterable[(String, NodeURI)], lits: Iterable[(String, Any)]) extends Change {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"unload ${tag}_$node($kidsString$kidsLitsSep$litsString)"
  }
}
case class Load(node: NodeURI, tag: NodeTag, kids: Iterable[(String, NodeURI)], lits: Iterable[(String, Any)]) extends Change {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"load   ${tag}_$node($kidsString$kidsLitsSep$litsString)"
  }
}
