package truediff.manual

import truediff._
import truediff.changeset.{ChangesetBuffer, DetachNode, LoadNode, UnloadNode}
import truediff.diffable.{Diffable, DiffableList, SubtreeRegistry}

case class Many(es: DiffableList[Exp]) extends Exp {

  lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(es.hash)
    digest.digest()
  }

  override val treeheight: Int = 1 + es.treeheight

  override def treesize: Int = 1 + es.treesize

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    f(this)
    this.es.foreachDiffable(f)
  }

  override def toStringWithURI: String = s"Many_$uri(${es.toStringWithURI})"

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Many =>
      this.es.assignShares(that.es, subtreeReg)
    case _ =>
      this.es.foreachDiffable(subtreeReg.registerShareFor)
      that.foreachDiffable(subtreeReg.shareFor)
  }

  override private[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.single(es)

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case that: Many =>
      val es = this.es.computeChangeset(that.es, this.uri, NamedLink(this.tag, "es"), changes).asInstanceOf[DiffableList[Exp]]
      val newtree = Many(es)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val es = this.es.loadUnassigned(changes).asInstanceOf[DiffableList[Exp]]
    val newtree = Many(es)
    changes += LoadNode(newtree.uri, classOf[Many], Seq(
      NamedLink(this.tag, "es") -> es.uri
    ), Seq())
    newtree
  }

  override def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      changes += DetachNode(parent, link, this.uri)
      this.assigned = null
    } else
      this.es.unloadUnassigned(this.uri, NamedLink(this.tag, "es"), changes)
      changes += UnloadNode(parent, link, this.uri, Seq(NamedLink(this.tag, "es")))
  }
}

object Many {
  def apply(es: Seq[Exp]): Many = Many(DiffableList.from(es))
}