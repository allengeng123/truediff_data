package truechange

import scala.collection.SortedMap
import scala.collection.mutable.ListBuffer

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

case class Update(node: URI, tag: Tag, oldlits: Iterable[(String, Any)], newlits: Iterable[(String, Any)]) extends CoreEdit {
  override def toString: String = {
    val oldlitsString = s"${oldlits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    val newlitsString = s"${newlits.map(p => p._1 + "=" + p._2).mkString(", ")}"
    s"update ${tag}_$node($oldlitsString -> $newlitsString)"
  }
}

/** Inserts all children marked for loading, then inserts this node */
case class Insert(node: URI, tag: Tag, kids: Iterable[(String, Either[Insert, URI])], lits: Iterable[(String, Any)]) extends Edit {
  private def asCoreEdits(buf: ListBuffer[CoreEdit]): Unit = {
    kids.foreach {
      case (_, Left(ins)) => ins.asCoreEdits(buf)
      case (_, Right(_)) => // keep detached child
    }
    buf += Load(node, tag, kids.map {
      case (k, Left(ins)) => (k, ins.node)
      case (k, Right(n)) => (k, n)
    }, lits)
  }
  override def asCoreEdits: Seq[CoreEdit] = {
    val buf = ListBuffer[CoreEdit]()
    asCoreEdits(buf)
    buf.toSeq
  }

  private def makeString(buf: StringBuffer): Unit = {
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

  override def toString: String = {
    val buf = new StringBuffer()
    makeString(buf)
    s"insert $buf"
  }
}

/** Removes node, then removes all children marked for unloading */
case class Remove(node: URI, tag: Tag, kids: SortedMap[String, Either[Remove, URI]], lits: Iterable[(String, Any)]) extends Edit {
  private def asCoreEdits(buf: ListBuffer[CoreEdit]): Unit = {
    buf += Unload(node, tag, kids.map {
      case (k, Left(r)) => (k, r.node)
      case (k, Right(n)) => (k, n)
    }, lits)
    kids.foreach {
      case (_, Left(r)) => r.asCoreEdits(buf)
      case (_, Right(_)) => // keep detached child
    }
  }
  override def asCoreEdits: Seq[CoreEdit] = {
    val buf = ListBuffer[CoreEdit]()
    asCoreEdits(buf)
    buf.toSeq
  }

  private def makeString(buf: StringBuffer): Unit = {
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

  override def toString: String = {
    val buf = new StringBuffer()
    makeString(buf)
    s"remove $buf"
  }
}

