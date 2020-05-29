package truediff.manual

import truediff._
import truediff.changeset.{ChangesetBuffer, DetachNode, LoadNode, UnloadNode}
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

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    f(this)
    this.a.foreachDiffable(f)
  }

  override def toStringWithURI: String = s"Maybe_$uri(${a.toStringWithURI})"

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Maybe =>
      this.a.assignShares(that.a, subtreeReg)
    case _ =>
      this.a.foreachDiffable(t => subtreeReg.shareFor(t).registerAvailableTree(t))
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override private[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.single(a)

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
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
    changes += LoadNode(newtree.uri, classOf[Maybe], Seq(
      NamedLink(this.tag, "a") -> a.uri
    ))
    newtree
  }

  override def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      changes += DetachNode(parent, link, this.uri)
      this.assigned = null
    } else
      this.a.unloadUnassigned(this.uri, NamedLink(this.tag, "a"), changes)
      changes += UnloadNode(parent, link, this.uri, Seq(NamedLink(this.tag, "a")))
  }
}

object Maybe {
  def apply(a: Option[Exp]): Maybe = Maybe(DiffableOption.from(a.map(a => a)))
}