package truediff.manual

import truechange._
import truediff.{SubtreeRegistry, _}

case class Maybe(a: DiffableOption[Exp]) extends Exp {

  lazy val cryptoHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(a.cryptoHash)
    digest.digest()
  }

  override val treeheight: Int = 1 + a.treeheight

  override def treesize: Int = 1 + a.treesize

  override def toStringWithURI: String = s"Maybe_$uri(${a.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map("a" -> OptionType(SortType(classOf[Exp].getCanonicalName))), Map())

  override def foreachSubtree(f: Diffable => Unit): Unit = {
    f(this.a)
    this.a.foreachSubtree(f)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Maybe =>
      this.a.assignShares(that.a, subtreeReg)
    case _ =>
      this.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
  }

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(a)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: Maybe =>
      val a = this.a.computeEditScript(that.a, this.uri, this.tag, NamedLink("a"), edits).asInstanceOf[DiffableOption[Exp]]
      val newtree = Maybe(a)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val a = this.a.loadUnassigned(edits).asInstanceOf[DiffableOption[Exp]]
    val newtree = Maybe(a)
    edits += Load(newtree.uri, this.tag, Seq(
      "a" -> a.uri
    ), Seq())
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.a.loadInitial(edits)
    edits += Load(this.uri, this.tag, Seq(
      "a" -> a.uri
    ), Seq())
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      edits += Unload(this.uri, this.tag, Seq(
        "a" -> a.uri
      ), Seq())
      this.a.unloadUnassigned(edits)
    }
  }
}

object Maybe {
  def apply(a: Option[Exp]): Maybe = Maybe(DiffableOption.from(a.map(a => a), SortType(classOf[Exp].getCanonicalName)))
}