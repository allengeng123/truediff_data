package truediff.manual

import truechange._
import truediff._

import scala.collection.immutable.SortedMap

case class Maybe(a: DiffableOption[Exp]) extends Exp {

  override val treeheight: Int = 1 + a.treeheight

  override def treesize: Int = 1 + a.treesize

  override def toStringWithURI: String = s"Maybe_$uri(${a.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map("a" -> OptionType(SortType(classOf[Exp].getCanonicalName))), Map())

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(a)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: Maybe =>
      val a = this.a.computeEditScript(that.a, this.uri, this.tag, NamedLink("a"), edits).asInstanceOf[DiffableOption[Exp]]
      Maybe(a).withURI(this.uri)
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned.updateLiterals(that, edits)
    }

    val a = this.a.loadUnassigned(edits).asInstanceOf[DiffableOption[Exp]]
    val aInsert = edits.mergeKidInsert(a.uri)
    val newtree = Maybe(a)
    edits += Insert(newtree.uri, this.tag, Seq(
      "a" -> aInsert
    ), Seq())
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.a.loadInitial(edits)
    val aInsert = edits.mergeKidInsert(this.a.uri)
    edits += Insert(this.uri, this.tag, Seq(
      "a" -> aInsert
    ), Seq())
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      edits += Remove(this.uri, this.tag, SortedMap("a" -> Right(a.uri)), Seq())
      this.a.unloadUnassigned(edits)
      edits.mergeKidRemove(this.a.uri,"a")
    }
  }

  override def updateLiterals(that: Diffable, edits: EditScriptBuffer): Diffable = {
    val newlist = this.a.updateLiterals(that.asInstanceOf[Maybe].a, edits).asInstanceOf[DiffableOption[Exp]]
    Maybe(newlist).withURI(this.uri)
  }
}

object Maybe {
  def apply(a: Option[Exp]): Maybe = Maybe(DiffableOption.from(a.map(a => a), SortType(classOf[Exp].getCanonicalName)))
}