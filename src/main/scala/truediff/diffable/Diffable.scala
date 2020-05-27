package truediff.diffable

import truediff.changeset.{AttachNode, Changeset, ChangesetBuffer}
import truediff.{Hashable, Link, NodeURI, RootLink}

trait Diffable extends Hashable {

  private[truediff] var _uri: NodeURI = new NodeURI
  def uri: NodeURI = _uri

  def height: Int
  def toStringWithURI: String

  private[truediff] def isCollection: Boolean = false

  private[truediff] var share: SubtreeShare = _
  private[truediff] var assigned: Diffable = _

  private[truediff] def foreachDiffable(f: Diffable => Unit): Unit
  private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit
  private[truediff] def assignSubtreesRecurse(): Unit
  private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable
  private[truediff] def loadUnassigned(changes: ChangesetBuffer): Diffable

  private[truediff] def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit

  private[truediff] final def assignShares(that: Diffable, subtreeReg: SubtreeRegistry): Unit = {
    if (this.isCollection)
      return assignSharesRecurse(that, subtreeReg)

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

  private[truediff] final def assignSubtrees(): Unit = {
    if (this.isCollection)
      return assignSubtreesRecurse()

    val that = this
    if (that.assigned == null) {
      that.share.takeAvailableTree() match {
        case Some(src) =>
          that.assigned = src
          src.assigned = that
          src.share = null
        case None =>
          that.assignSubtreesRecurse()
      }
    }
  }

  private[truediff] final def computeChangeset(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = {
    if (this.isCollection)
      return computeChangesetRecurse(that, parent, link, changes)

    // this == that
    if (that.assigned != null && that.assigned.uri == this.uri) {
      this.assigned = null
      return this
    }

    if (this.assigned == null && that.assigned == null) {
      val newtree = computeChangesetRecurse(that, parent, link, changes)
      if (newtree != null)
        return newtree
    }

    this.unloadUnassigned(parent, link, changes)
    val newtree = that.loadUnassigned(changes)
    changes += AttachNode(parent, link, newtree.uri)
    newtree
  }

  final def compareTo(that: Diffable): (Changeset, Diffable) = {
    assignShares(that, new SubtreeRegistry)
    that.assignSubtrees()

    val buf = new ChangesetBuffer
    val newtree = this.computeChangeset(that, null, RootLink, buf)
    (buf.toChangeset, newtree)
  }

}
