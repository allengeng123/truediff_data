package truediff.diffable

import truediff.changeset.{AttachNode, ChangesetBuffer}
import truediff.{Hashable, Link, ListNextLink, NodeURI}

sealed trait DiffableList[+A <: Diffable] extends Diffable {
  override private[truediff] def isCollection = true

  private[truediff] def asList: List[A]
}
object DiffableList {
  def from[A <: Diffable](list: Seq[A]): DiffableList[A] =
    list.foldRight[DiffableList[A]](DiffableNil)((head,tail) => DiffableCons(head, tail))
}

case object DiffableNil extends DiffableList[Nothing] {
  override private[truediff] def asList: List[Nothing] = Nil

  override val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.digest()
  }

  override def uri: NodeURI = null

  override val height: Int = 0

  override val toStringWithURI: String = "Nil"

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    // nothing
  }

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case DiffableNil =>
    case _ =>
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override private[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.empty

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case DiffableNil => this
    case that: DiffableCons[_] =>
      val newtree = that.loadUnassigned(changes)
      changes += AttachNode(parent, link, newtree.head.uri)
      newtree
  }

  override private[truediff] def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    // nothing to load for Nil
    this
  }

  override private[truediff] def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    // nothing to unload for Nil
  }
}

final case class DiffableCons[+A <: Diffable](head: A, tail: DiffableList[A]) extends DiffableList[A] {
  override private[truediff] def asList: List[A] = head::tail.asList

  override val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(head.hash)
    digest.update(tail.hash)
    digest.digest()
  }

  override def uri: NodeURI = head.uri

  override def height: Int = Math.max(head.height, tail.height)

  override def toStringWithURI: String = s"${head.toStringWithURI}::${tail.toStringWithURI}"

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    this.head.foreachDiffable(f)
    this.tail.foreachDiffable(f)
  }

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: DiffableCons[_] =>
      this.head.assignShares(that.head, subtreeReg)
      this.tail.assignShares(that.tail, subtreeReg)
    case _ =>
      this.head.foreachDiffable(t => subtreeReg.shareFor(t).registerAvailableTree(t))
      this.tail.foreachDiffable(t => subtreeReg.shareFor(t).registerAvailableTree(t))
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override private[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = this.asList

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case that: DiffableCons[_] =>
      val head = this.head.computeChangeset(that.head, parent, link, changes).asInstanceOf[A]
      val tail = this.tail.computeChangeset(that.tail, head.uri, ListNextLink, changes).asInstanceOf[DiffableList[A]]
      DiffableCons(head, tail)
    case DiffableNil =>
      this.unloadUnassigned(parent, link, changes)
      that
  }

  override private[truediff] def loadUnassigned(changes: ChangesetBuffer): DiffableCons[A] = {
    val head = this.head.loadUnassigned(changes).asInstanceOf[A]
    val tail = this.tail.loadUnassigned(changes).asInstanceOf[DiffableList[A]]
    tail match {
      case DiffableNil =>
      case tail: DiffableCons[_] =>
        changes += AttachNode(head.uri, ListNextLink, tail.head.uri)
    }
    DiffableCons(head, tail)
  }

  override private[truediff] def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    this.head.unloadUnassigned(parent, link, changes)
    this.tail.unloadUnassigned(this.head.uri, ListNextLink, changes)
  }
}