package truediff.manual

import truediff._
import truechange._
import truediff.diffable.{Diffable, DiffableOption, SubtreeRegistry}

case class Maybe(a: DiffableOption[Exp]) extends Exp {

  lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(a.hash)
    digest.digest()
  }

  override val treeheight: Int = 1 + a.treeheight

  override def treesize: Int = 1 + a.treesize

  override def toStringWithURI: String = s"Maybe_$uri(${a.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map("a" -> OptionType(SortType(classOf[Exp]))), Map())

  override def foreachDiffable(f: Diffable => Unit): Unit = {
    f(this)
    this.a.foreachDiffable(f)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Maybe =>
      this.a.assignShares(that.a, subtreeReg)
    case _ =>
      this.a.foreachDiffable(t => subtreeReg.shareFor(t).registerAvailableTree(t))
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override protected def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.single(a)

  override protected def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case that: Maybe =>
      val a = this.a.computeChangeset(that.a, this.uri, NamedLink(this.tag, "a"), changes).asInstanceOf[DiffableOption[Exp]]
      val newtree = Maybe(a)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val a = this.a.loadUnassigned(changes).asInstanceOf[DiffableOption[Exp]]
    val newtree = Maybe(a)
    changes += LoadNode(newtree.uri, this.tag, Seq(
      "a" -> a.uri
    ), Seq())
    newtree
  }

  override def loadInitial(changes: ChangesetBuffer): Unit = {
    this.a.loadInitial(changes)
    changes += LoadNode(this.uri, this.tag, Seq(
      "a" -> a.uri
    ), Seq())
  }

  override def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      changes += DetachNode(parent, link, this.uri, this.tag)
      this.assigned = null
    } else
      this.a.unloadUnassigned(this.uri, NamedLink(this.tag, "a"), changes)
      changes += UnloadNode(parent, link, this.uri, this.tag)
  }
}

object Maybe {
  def apply(a: Option[Exp]): Maybe = Maybe(DiffableOption.from(a.map(a => a), SortType(classOf[Exp])))
}