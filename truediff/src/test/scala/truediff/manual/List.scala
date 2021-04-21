package truediff.manual

import truechange._
import truediff._

case class Many(es: DiffableList[Exp]) extends Exp {

  override val treeheight: Int = 1 + es.treeheight

  override def treesize: Int = 1 + es.treesize

  override def toStringWithURI: String = s"Many_$uri(${es.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map("es" -> ListType(SortType(classOf[Exp].getCanonicalName))), Map())

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(es)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: Many =>
      val es = this.es.computeEditScript(that.es, this.uri, this.tag, NamedLink("es"), edits).asInstanceOf[DiffableList[Exp]]
      Many(es).withURI(this.uri)
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned.updateLiterals(that, edits)
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

  override def updateLiterals(that: Diffable, edits: EditScriptBuffer): Diffable = {
    val newlist = this.es.updateLiterals(that.asInstanceOf[Many].es, edits).asInstanceOf[DiffableList[Exp]]
    Many(newlist).withURI(this.uri)
  }
}

object Many {
  def apply(es: Seq[Exp]): Many = Many(DiffableList.from(es, SortType(classOf[Exp].getCanonicalName)))
}