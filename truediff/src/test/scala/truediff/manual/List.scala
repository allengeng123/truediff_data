package truediff.manual

import truechange._
import truediff.{SubtreeRegistry, _}

case class Many(es: DiffableList[Exp]) extends Exp {

  lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(es.hash)
    digest.digest()
  }

  override val treeheight: Int = 1 + es.treeheight

  override def treesize: Int = 1 + es.treesize

  override def toStringWithURI: String = s"Many_$uri(${es.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map("es" -> ListType(SortType(classOf[Exp].getCanonicalName))), Map())

  override def foreachSubtree(f: Diffable => Unit): Unit = {
    f(this.es)
    this.es.foreachSubtree(f)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Many =>
      this.es.assignShares(that.es, subtreeReg)
    case _ =>
      this.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
  }

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(es)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: Many =>
      val es = this.es.computeEditScript(that.es, this.uri, this.tag, NamedLink("es"), edits).asInstanceOf[DiffableList[Exp]]
      val newtree = Many(es)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val es = this.es.loadUnassigned(edits).asInstanceOf[DiffableList[Exp]]
    val newtree = Many(es)
    edits += Load(newtree.uri, this.tag, Seq(
      "es" -> es.uri
    ), Seq())
    newtree
  }


  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.es.loadInitial(edits)
    edits += Load(this.uri, this.tag, Seq(
      "es" -> es.uri
    ), Seq())
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      edits += Unload(this.uri, this.tag, Seq(
        "es" -> es.uri
      ), Seq())
      this.es.unloadUnassigned(edits)
    }
  }
}

object Many {
  def apply(es: Seq[Exp]): Many = Many(DiffableList.from(es, SortType(classOf[Exp].getCanonicalName)))
}