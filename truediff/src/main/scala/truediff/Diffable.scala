package truediff

import truechange._

import scala.collection.mutable

trait Diffable extends Hashable {

  protected var _uri: NodeURI = new NodeURI
  def uri: NodeURI = _uri

  def tag: NodeTag = ConstrTag(this.getClass.getCanonicalName)
  def sig: Signature

  def treeheight: Int
  def treesize: Int
  def toStringWithURI: String

  private[truediff] def skipNode: Boolean = false

  private[truediff] var share: SubtreeShare = _
  protected[truediff] var assigned: Diffable = _

  final def foreachDiffable(f: Diffable => Unit): Unit = {
    f(this)
    this.foreachDiffableKid(f)
  }
  def foreachDiffableKid(f: Diffable => Unit): Unit
  def loadUnassigned(changes: ChangesetBuffer): Diffable
  def unloadUnassigned(changes: ChangesetBuffer): Unit
  def loadInitial(changes: ChangesetBuffer): Unit

  protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit
  private[truediff] def _assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = this.assignSharesRecurse(that, subtreeReg)

  protected def assignSubtreesRecurse(): Iterable[Diffable]
  private[truediff] def _assignSubtreesRecurse(): Iterable[Diffable] = this.assignSubtreesRecurse()

  protected def computeChangesetRecurse(that: Diffable, parent: NodeURI, parentTag:NodeTag, link: Link, changes: ChangesetBuffer): Diffable
  private[truediff] def _computeChangesetRecurse(that: Diffable, parent: NodeURI, parentTag: NodeTag, link: Link, changes: ChangesetBuffer): Diffable = this.computeChangesetRecurse(that, parent, parentTag, link, changes)

  final def assignShares(that: Diffable, subtreeReg: SubtreeRegistry): Unit = {
    if (this.skipNode) {
      that.foreachDiffable(subtreeReg.shareFor)
      return
    }
    if (that.skipNode) {
      this.foreachDiffable(subtreeReg.registerShareFor)
      return
    }

    val thisShare = subtreeReg.shareFor(this)
    val thatShare = subtreeReg.shareFor(that)
    if (thisShare == thatShare) {
      // equal trees => preemptive assign
      this.assigned = that
      this.share = null
      that.assigned = this
    } else {
      thisShare.registerAvailableTree(this)
      assignSharesRecurse(that, subtreeReg)
    }
  }

  protected final def assignSubtrees(subtreeReg: SubtreeRegistry): Unit = {
    val queue = new mutable.PriorityQueue[Diffable]()(Diffable.heightFirstOrdering)
    queue += this

    while (queue.nonEmpty) {
      val that = queue.dequeue()
      if (that.skipNode) {
        // skip
      } else if (that.assigned == null) {
        that.share.takeAvailableTree(subtreeReg) match {
          case Some(src) =>
            that.assigned = src
            src.assigned = that
            src.share = null
          case None =>
            queue ++= that.assignSubtreesRecurse()
        }
      }
    }
  }

  final def computeChangeset(that: Diffable, parent: NodeURI, parentTag: NodeTag, link: Link, changes: ChangesetBuffer): Diffable = {
    // this == that
    if (that.assigned != null && that.assigned.uri == this.uri) {
      this.assigned = null
      return this
    }

    if (this.assigned == null && that.assigned == null) {
      val newtree = this.computeChangesetRecurse(that, parent, parentTag, link, changes)
      if (newtree != null)
        return newtree
    }

    changes += Detach(parent, parentTag, link, this.uri, this.tag)
    this.unloadUnassigned(changes)
    val newtree = that.loadUnassigned(changes)
    changes += Attach(parent, parentTag, link, newtree.uri, newtree.tag)
    newtree
  }

  final def compareTo[T <: Diffable](that: T): (Changeset, T) = {
    val subtreeReg = new SubtreeRegistry
    this.assignShares(that, subtreeReg)

    that.assignSubtrees(subtreeReg)

    val buf = new ChangesetBuffer
    val newtree = this.computeChangeset(that, new NodeURI, RootTag, RootLink, buf)
    (buf.toChangeset, newtree.asInstanceOf[T])
  }
}

object Diffable {
  val heightFirstOrdering: Ordering[Diffable] = Ordering.by[Diffable,Int](_.treeheight)

  def load[T <: Diffable](t: T): Changeset = {
    val buf = new ChangesetBuffer
    t.loadInitial(buf)
    buf.toChangeset
  }
}