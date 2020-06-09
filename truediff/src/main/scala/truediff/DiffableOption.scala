package truediff

import truechange._

sealed trait DiffableOption[+A <: Diffable] extends Diffable {
  final override private[truediff] def skipNode = true
}
object DiffableOption {
  def from[A <: Diffable](a: Option[A], atype: Type): DiffableOption[A] = a match {
    case Some(value) => DiffableSome(value, atype)
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

  final override def tag: NodeTag = OptionType(NothingType)
  override def sig: Signature = Signature(OptionType(NothingType), this.tag, Map(), Map())

  override def foreachDiffable(f: Diffable => Unit): Unit = {
    // nothing
  }

  override protected[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case DiffableNone =>
    case _ =>
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override protected[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.empty

  override protected[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case DiffableNone => this
    case that: DiffableSome[_] =>
      val newtree = that.loadUnassigned(changes)
      changes += AttachNode(parent, link, newtree.uri)
      newtree
  }

  override def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    // nothing to load for None
    this
  }

  override def loadInitial(changes: ChangesetBuffer): Unit = {
    // nothing to load for None
  }

  override def unloadUnassigned(changes: ChangesetBuffer): Unit = {
    // nothing to unload for None
  }
}

final case class DiffableSome[+A <: Diffable](a: A, atype: Type) extends DiffableOption[A] {
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

  override def tag: NodeTag = OptionType(atype)
  override def sig: Signature = Signature(OptionType(atype), this.tag, Map(), Map())

  override def foreachDiffable(f: Diffable => Unit): Unit = {
    this.a.foreachDiffable(f)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: DiffableSome[_] =>
      this.a.assignShares(that.a, subtreeReg)
    case _ =>
      this.a.foreachDiffable(t => subtreeReg.shareFor(t).registerAvailableTree(t))
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override protected def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.single(a)

  override protected def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case that: DiffableSome[_] =>
      val a = this.a.computeChangeset(that.a, parent, link, changes)
      DiffableSome(a.asInstanceOf[A], atype)
    case DiffableNone =>
      changes += DetachNode(parent, link, this.a.uri, this.a.tag)
      this.a.unloadUnassigned(changes)
      that
  }

  override def loadUnassigned(changes: ChangesetBuffer): DiffableSome[A] = {
    val a = this.a.loadUnassigned(changes).asInstanceOf[A]
    DiffableSome(a, atype)
  }

  override def loadInitial(changes: ChangesetBuffer): Unit = {
    this.a.loadInitial(changes)
  }

  override def unloadUnassigned(changes: ChangesetBuffer): Unit = {
    a.unloadUnassigned(changes)
  }
}