package truediff.diffable

import truediff.TreeURI.NodeTag
import truediff._
import truediff.changeset.{AttachNode, Changeset, ChangesetBuffer}

import scala.collection.mutable

trait Diffable extends Hashable {

  private[truediff] var _uri: NodeURI = new NodeURI
  def uri: NodeURI = _uri

  def tag: NodeTag = this.getClass

  def treeheight: Int
  def treesize: Int
  def toStringWithURI: String

  private[truediff] def skipNode: Boolean = false

  private[truediff] var share: SubtreeShare = _
  private[truediff] var assigned: Diffable = _

  private[truediff] def foreachDiffable(f: Diffable => Unit): Unit
  private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit
  private[truediff] def assignSubtreesRecurse(): Iterable[Diffable]
  private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable
  private[truediff] def loadUnassigned(changes: ChangesetBuffer): Diffable
  private[truediff] def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit


  private[truediff] final def assignShares(that: Diffable, subtreeReg: SubtreeRegistry): Unit = {
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

  private[truediff] final def assignSubtrees(subtreeReg: SubtreeRegistry): Unit = {
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

  private[truediff] final def computeChangeset(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = {
    // this == that
    if (that.assigned != null && that.assigned.uri == this.uri) {
      this.assigned = null
      return this
    }

    if (this.assigned == null && that.assigned == null) {
      val newtree = this.computeChangesetRecurse(that, parent, link, changes)
      if (newtree != null)
        return newtree
    }

    this.unloadUnassigned(parent, link, changes)
    val newtree = that.loadUnassigned(changes)
    changes += AttachNode(parent, link, newtree.uri)
    newtree
  }

  final def compareTo[T <: Diffable](that: T): (Changeset, T) = {
    val subtreeReg = new SubtreeRegistry
    this.assignShares(that, subtreeReg)
    that.assignSubtrees(subtreeReg)

    val buf = new ChangesetBuffer
    val newtree = this.computeChangeset(that, null, RootLink, buf)
    (buf.toChangeset, newtree.asInstanceOf[T])
  }
}

object Diffable {
  val heightFirstOrdering: Ordering[Diffable] = Ordering.by[Diffable,Int](_.treeheight)

  def load[T <: Diffable](t: T): (Changeset, T) = {
    val buf = new ChangesetBuffer
    val newtree = t.loadUnassigned(buf)
    (buf.toChangeset, newtree.asInstanceOf[T])
  }
}