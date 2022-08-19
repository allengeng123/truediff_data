package truechange

import scala.collection.SortedMap


/** Core edits: detach, attach, unload, load, update */
sealed trait CoreEdit

case class Detach(node: URI, tag: Tag, link: Link, parent: URI, ptag: Tag) extends CoreEdit with Edit {
  override def asCoreEdits(buf: CoreEditScriptBuffer): Unit = buf += this
  override def toString: String = s"detach ${tag}_$node from ${ptag}_$parent.$link"
}
case class Attach(node: URI, tag: Tag, link: Link, parent: URI, ptag: Tag) extends CoreEdit with Edit {
  override def asCoreEdits(buf: CoreEditScriptBuffer): Unit = buf += this
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

case class Update(node: URI, tag: Tag, oldlits: Iterable[(String, Any)], newlits: Iterable[(String, Any)]) extends CoreEdit with Edit {
  override def asCoreEdits(buf: CoreEditScriptBuffer): Unit = buf += this
  override def toString: String = {
    val oldlitsString = s"${oldlits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val newlitsString = s"${newlits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    s"update ${tag}_$node($oldlitsString -> $newlitsString)"
  }
}


/** Edits: detach, attach, update, insert, remove */
sealed trait Edit {
  def asCoreEdits(buf: CoreEditScriptBuffer): Unit
}


trait Insert extends Edit {
  def node: URI
  def tag: Tag
  def makeString(buf: StringBuffer): Unit
  override def toString: String = {
    val buf = new StringBuffer()
    makeString(buf)
    s"insert $buf"
  }
}

/** Inserts all children marked for loading, then inserts this node */
case class InsertNode(node: URI, tag: Tag, kids: Iterable[(String, Either[Insert, URI])], lits: Iterable[(String, Any)]) extends Insert {
  def asCoreEdits(buf: CoreEditScriptBuffer): Unit = {
    kids.foreach {
      case (_, Left(ins)) => ins.asCoreEdits(buf)
      case (_, Right(_)) => // keep detached child
    }
    buf += Load(node, tag, kids.map {
      case (k, Left(ins)) => (k, ins.node)
      case (k, Right(n)) => (k, n)
    }, lits)
  }

  def makeString(buf: StringBuffer): Unit = {
    buf.append(tag).append('_').append(node).append('(')
    kids.foreach {
      case (k, Left(ins)) =>
        buf.append(k).append('=')
        ins.makeString(buf)
        buf.append(", ")
      case (k, Right(uri)) =>
        buf.append(k).append('=').append(uri).append(", ")
    }
    if (kids.nonEmpty && lits.isEmpty) {
      buf.deleteCharAt(buf.length() - 1)
      buf.deleteCharAt(buf.length() - 1)
    }
    buf.append(lits.map(p => p._1 + "=" + p._2).mkString(", "))
    buf.append(')')
  }
}

case class InsertList(node: URI, tag: Tag, list: Iterable[Either[Insert, (URI, Tag)]], atype: Type) extends Insert {
  override def asCoreEdits(buf: CoreEditScriptBuffer): Unit = {
    buf += Load(node, tag, Seq(), Seq())
    this.list.foldLeft[(URI,Tag,Link)]((node, tag, ListFirstLink(atype))) {
      case (pred, Left(ins)) =>
        ins.asCoreEdits(buf)
        buf += Attach(ins.node, ins.tag, pred._3, pred._1, pred._2)
        (ins.node, ins.tag, ListNextLink(atype))
      case (pred, Right((elUri, elTag))) =>
        buf += Attach(elUri, elTag, pred._3, pred._1, pred._2)
        (elUri, elTag, ListNextLink(atype))
    }
  }

  override def makeString(buf: StringBuffer): Unit = {
    buf.append("List(")
    list.foreach {
      case Left(rem) =>
        rem.makeString(buf)
        buf.append(", ")
      case Right((uri,_)) =>
        buf.append(uri).append(", ")
    }
    buf.deleteCharAt(buf.length() - 1)
    buf.deleteCharAt(buf.length() - 1)
    buf.append(')')
  }
}

trait Remove extends Edit {
  def node: URI
  def tag: Tag
  def makeString(buf: StringBuffer): Unit

  override def toString: String = {
    val buf = new StringBuffer()
    makeString(buf)
    s"remove $buf"
  }
}

/** Removes node, then removes all children marked for unloading */
case class RemoveNode(node: URI, tag: Tag, kids: SortedMap[String, Either[Remove, URI]], lits: Iterable[(String, Any)]) extends Remove {
  def asCoreEdits(buf: CoreEditScriptBuffer): Unit = {
    buf += Unload(node, tag, kids.map {
      case (k, Left(r)) => (k, r.node)
      case (k, Right(n)) => (k, n)
    }, lits)
    kids.foreach {
      case (_, Left(rem)) => rem.asCoreEdits(buf)
      case (_, Right(_)) => // keep detached child
    }
  }

  def makeString(buf: StringBuffer): Unit = {
    buf.append(tag).append('_').append(node).append('(')
    kids.foreach {
      case (k, Left(rem)) =>
        buf.append(k).append('=')
        rem.makeString(buf)
        buf.append(", ")
      case (k, Right(uri)) =>
        buf.append(k).append('=').append(uri).append(", ")
    }
    if (kids.nonEmpty && lits.isEmpty) {
      buf.deleteCharAt(buf.length() - 1)
      buf.deleteCharAt(buf.length() - 1)
    }
    buf.append(lits.map(p => p._1 + "=" + p._2).mkString(", "))
    buf.append(')')
  }
}
object Remove {
  def apply(node: URI, tag: Tag, kids: Iterable[(String, URI)], lits: Iterable[(String, Any)]): RemoveNode =
    RemoveNode(node, tag, SortedMap.from(kids.view.map(kv => kv._1 -> Right(kv._2))), lits)
}

case class RemoveList(node: URI, tag: Tag, list: Iterable[Either[Remove, (URI, Tag)]], atype: Type) extends Remove {
  override def asCoreEdits(buf: CoreEditScriptBuffer): Unit = {
    this.list.foldLeft[(URI,Tag,Link)]((node, tag, ListFirstLink(atype))) {
      case (pred, Left(rem)) =>
        buf += Detach(rem.node, rem.tag, pred._3, pred._1, pred._2)
        rem.asCoreEdits(buf)
        (rem.node, rem.tag, ListNextLink(atype))
      case (pred, Right((elUri, elTag))) =>
        buf += Detach(elUri, elTag, pred._3, pred._1, pred._2)
        (elUri, elTag, ListNextLink(atype))
    }
    buf += Unload(node, tag, Seq(), Seq())
  }

  override def makeString(buf: StringBuffer): Unit = {
    buf.append("List(")
    list.foreach {
      case Left(rem) =>
        rem.makeString(buf)
        buf.append(", ")
      case Right(uri) =>
        buf.append(uri).append(", ")
    }
    buf.deleteCharAt(buf.length() - 1)
    buf.deleteCharAt(buf.length() - 1)
    buf.append(')')
  }
}
