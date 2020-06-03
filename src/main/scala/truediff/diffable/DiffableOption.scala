package truediff.diffable
import truediff.changeset.{AttachNode, ChangesetBuffer}
import truediff.{Hashable, Link, NodeURI}

sealed trait DiffableOption[+A <: Diffable] extends DiffableCollection {
  final override private[truediff] def skipNode = true
}
object DiffableOption {
  def from[A <: Diffable](a: Option[A]): DiffableOption[A] = a match {
    case Some(value) => DiffableSome(value)
    case None => DiffableNone
  }
}

case object DiffableNone extends DiffableOption[Nothing] {
  override val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.digest()
  }

  override def uri: NodeURI = null

  override val treeheight: Int = 0

  override def treesize: Int = 0

  override val toStringWithURI: String = "None"

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    // nothing
  }

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case DiffableNone =>
    case _ =>
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override private[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.empty

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case DiffableNone => this
    case that: DiffableSome[_] =>
      val newtree = that.loadUnassigned(changes)
      changes += AttachNode(parent, link, newtree.uri)
      newtree
  }

  override private[truediff] def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    // nothing to load for None
    this
  }

  override private[truediff] def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    // nothing to unload for None
  }
}

final case class DiffableSome[+A <: Diffable](a: A) extends DiffableOption[A] {
  override val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(a.hash)
    digest.digest()
  }

  override def uri: NodeURI = a.uri

  override def treeheight: Int = a.treeheight

  override def treesize: Int = a.treesize

  override def toStringWithURI: String = s"Some(${a.toStringWithURI})"

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    this.a.foreachDiffable(f)
  }

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: DiffableSome[_] =>
      this.a.assignShares(that.a, subtreeReg)
    case _ =>
      this.a.foreachDiffable(t => subtreeReg.shareFor(t).registerAvailableTree(t))
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override private[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.single(a)

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case that: DiffableSome[_] =>
      val a = this.a.computeChangeset(that.a, parent, link, changes)
      DiffableSome(a.asInstanceOf[A])
    case DiffableNone =>
      this.a.unloadUnassigned(parent, link, changes)
      that
  }

  override private[truediff] def loadUnassigned(changes: ChangesetBuffer): DiffableSome[A] = {
    val a = this.a.loadUnassigned(changes).asInstanceOf[A]
    DiffableSome(a)
  }

  override private[truediff] def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    a.unloadUnassigned(parent, link, changes)
  }
}