package truediff

import truechange._

import scala.collection.mutable.ListBuffer

trait GenericDiffable extends Diffable {
  def name: String
  def children: Seq[(String, Any)]
  def make(children: Seq[Any]): GenericDiffable

  private def diffable(children: Seq[(String, Any)]): Seq[(String, Diffable)] =
    children.filter(_._2.isInstanceOf[Diffable]).asInstanceOf[Seq[(String, Diffable)]]
  private def literal(children: Seq[(String, Any)]) =
    children.filter(!_._2.isInstanceOf[Diffable])

  override val tag: Tag = NamedTag(name)

  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(this.tag.toString, digest)
    literal(children).foreach(l => Hashable.hash(l, digest))
    this.directSubtrees.foreach(t => digest.update(t.literalHash))
    digest.digest()
  }

  override def sig: Signature = ???

  override def treeheight: Int = 1 + diffable(children).map(_._2.treeheight).maxOption.getOrElse(0)

  override def treesize: Int = 1 + diffable(children).map(_._2.treesize).sum

  override def toStringWithURI: String = {
    val subs = children.map {
      case (_,diff:Diffable) => diff.toStringWithURI
      case (_,c) => c.toString
    }
    s"${name}_$uri(${subs.mkString(",")})"
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable =
    if (this.assigned != null)
      this.assigned.updateLiterals(this, edits)
    else {
      val newChildren = ListBuffer[Any]()
      val (newchildrenInserts, newlits) = children.map {
        case (k, v:Diffable) =>
          val loaded = v.loadUnassigned(edits)
          newChildren += loaded
          Left(k -> edits.mergeKidInsert(loaded.uri))
        case (k, v) =>
          newChildren += v
          Right(k -> v)
      }.partitionMap(identity)
      val newtree = make(newChildren.toSeq)
      edits += InsertNode(newtree.uri, tag, newchildrenInserts, newlits)
      newtree
    }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit =
    if (this.assigned == null) {
      edits += Remove(this.uri, this.tag, diffable(children).map(kv => kv._1 -> kv._2.uri), literal(children))
      diffable(children).foreach { case (k, v) =>
        v.unloadUnassigned(edits)
        edits.mergeKidRemove(v.uri, k)
      }
    }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    val childrenInserts = diffable(children).map { case (k, v) =>
      v.loadInitial(edits)
      k -> edits.mergeKidInsert(v.uri)
    }
    edits += InsertNode(uri, tag, childrenInserts, literal(children))
  }

  override def updateLiterals(that: Diffable, edits: EditScriptBuffer): Diffable = that match {
    case that: GenericDiffable =>
      val thislits = literal(children)
      val thatlits = literal(that.children)
      if (thislits != thatlits)
        edits += Update(uri, tag, thislits, thatlits)
      val newchildren = children.zip(that.children).map {
        case ((k, v:Diffable), (_, w: Diffable)) => v.updateLiterals(w, edits)
        case (_, (_, w)) => w
      }
      make(newchildren).withURI(this.uri)
  }


  override protected def directSubtrees: Iterable[Diffable] =
    diffable(children).map(_._2)

  override def literals: Iterable[Any] =
    literal(children).map(_._2)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: GenericDiffable if tag == that.tag && children.size == that.children.size =>
      val newchildren = this.children.zip(that.children).map {
        case ((k, v:Diffable), (_, w: Diffable)) =>
          v.computeEditScript(w, this.uri, this.tag, NamedLink(k), edits)
        case ((k, v), (k2, w)) =>
          if (k != k2 || v != w)
            return null
          w
      }
      make(newchildren).withURI(this.uri)
    case _ => null
  }
}
