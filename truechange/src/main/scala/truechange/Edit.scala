package truechange

sealed trait Edit {
  def asCoreEdits: Seq[CoreEdit]
}

sealed trait CoreEdit extends Edit {
  override def asCoreEdits: Seq[CoreEdit] = Seq(this)
}

case class Detach(node: URI, tag: Tag, link: Link, parent: URI, ptag: Tag) extends CoreEdit {
  override def toString: String = s"detach ${tag}_$node from ${ptag}_$parent.$link"
}
case class Attach(node: URI, tag: Tag, link: Link, parent: URI, ptag: Tag) extends CoreEdit {
  override def toString: String = s"attach ${tag}_$node to ${ptag}_$parent.$link"
}

case class Unload(node: URI, tag: Tag, kids: Iterable[(String, URI)], lits: Iterable[(String, Any)]) extends CoreEdit {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"unload ${tag}_$node($kidsString$kidsLitsSep$litsString)"
  }
}
case class Load(node: URI, tag: Tag, kids: Iterable[(String, URI)], lits: Iterable[(String, Any)]) extends CoreEdit {
  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"load   ${tag}_$node($kidsString$kidsLitsSep$litsString)"
  }
}

case class LoadAttach(node: URI, tag: Tag, kids: Iterable[(String, URI)], lits: Iterable[(String, Any)], link: Link, parent: URI, ptag: Tag) extends Edit {
  override def asCoreEdits: Seq[CoreEdit] = Seq(
    Load(node, tag, kids, lits),
    Attach(node, tag, link, parent, ptag)
  )

  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"load+attach   ${tag}_$node($kidsString$kidsLitsSep$litsString) to ${ptag}_$parent.$link"
  }
}

case class DetachUnload(node: URI, tag: Tag, kids: Iterable[(String, URI)], lits: Iterable[(String, Any)], link: Link, parent: URI, ptag: Tag) extends Edit {
  override def asCoreEdits: Seq[CoreEdit] = Seq(
    Detach(node, tag, link, parent, ptag),
    Unload(node, tag, kids, lits)
  )

  override def toString: String = {
    val kidsString = s"${kids.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val litsString = s"${lits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val kidsLitsSep = if (kids.nonEmpty && lits.nonEmpty) ", " else ""
    s"detach+unload ${tag}_$node($kidsString$kidsLitsSep$litsString) from ${ptag}_$parent.$link"
  }
}
