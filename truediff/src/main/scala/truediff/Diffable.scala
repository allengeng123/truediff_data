package truediff

import truechange._

import scala.collection.mutable

trait Diffable extends Hashable {

  protected var _uri: URI = new JVMURI
  def uri: URI = _uri

  def tag: Tag = NamedTag(this.getClass.getCanonicalName)
  def sig: Signature

  def treeheight: Int
  def treesize: Int
  def toStringWithURI: String

  private[truediff] def skipNode: Boolean = false

  private[truediff] var share: SubtreeShare = _
  protected[truediff] var assigned: Diffable = _

  final def foreachTree(f: Diffable => Unit): Unit = {
    f(this)
    this.foreachSubtree(f)
  }
  def foreachSubtree(f: Diffable => Unit): Unit
  def loadUnassigned(changes: EditscriptBuffer): Diffable
  def unloadUnassigned(changes: EditscriptBuffer): Unit
  def loadInitial(changes: EditscriptBuffer): Unit

  protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit
  private[truediff] def _assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = this.assignSharesRecurse(that, subtreeReg)

  protected def directSubtrees: Iterable[Diffable]
  private[truediff] def _directSubtrees: Iterable[Diffable] = this.directSubtrees

  protected def computeEditscriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, changes: EditscriptBuffer): Diffable
  private[truediff] def _computeEditscriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, changes: EditscriptBuffer): Diffable = this.computeEditscriptRecurse(that, parent, parentTag, link, changes)

  @inline
  final def assignTree(that: Diffable): Unit = {
    this.assigned = that
    that.assigned = this
    this.share = null // reset to prevent memory leaks
  }

  final def assignShares(that: Diffable, subtreeReg: SubtreeRegistry): Unit = {
    if (this.skipNode) {
      that.foreachTree(subtreeReg.assignShare)
      return
    }
    if (that.skipNode) {
      this.foreachTree(subtreeReg.assignShareAndRegisterTree)
      return
    }

    val thisShare = subtreeReg.assignShare(this)
    val thatShare = subtreeReg.assignShare(that)
    if (thisShare == thatShare) {
      // equal trees => preemptive assign
      this.assignTree(that)
    } else {
      thisShare.registerAvailableTree(this)
      assignSharesRecurse(that, subtreeReg)
    }
  }

  protected final def assignSubtrees(that: Diffable, subtreeReg: SubtreeRegistry): Unit = {
    val queue = new mutable.PriorityQueue[Diffable]()(Diffable.highestFirstOrdering)
    queue += that

    while (queue.nonEmpty) {
      val next = queue.dequeue()
      if (next.skipNode) {
        // skip
      } else if (next.assigned == null) {
        next.share.takeAvailableTree(subtreeReg) match {
          case Some(src) => src.assignTree(next)
          case None => queue ++= next.directSubtrees
        }
      }
    }
  }

  final def computeEditscript(that: Diffable, parent: URI, parentTag: Tag, link: Link, changes: EditscriptBuffer): Diffable = {
    // this == that
    if (that.assigned != null && that.assigned.uri == this.uri) {
      this.assigned = null
      return this
    }

    if (this.assigned == null && that.assigned == null) {
      val newtree = this.computeEditscriptRecurse(that, parent, parentTag, link, changes)
      if (newtree != null)
        return newtree
    }

    changes += Detach(this.uri, this.tag, link, parent, parentTag)
    this.unloadUnassigned(changes)
    val newtree = that.loadUnassigned(changes)
    changes += Attach(newtree.uri, newtree.tag, link, parent, parentTag)
    newtree
  }

  final def compareTo[T <: Diffable](that: T): (EditScript, T) = {
    val subtreeReg = new SubtreeRegistry
    this.assignShares(that, subtreeReg)

    this.assignSubtrees(that, subtreeReg)

    val buf = new EditscriptBuffer
    val newtree = this.computeEditscript(that, null, RootTag, RootLink, buf)
    (buf.toEditscript, newtree.asInstanceOf[T])
  }
}

object Diffable {
  val highestFirstOrdering: Ordering[Diffable] = Ordering.by[Diffable,Int](_.treeheight)

  def load[T <: Diffable](t: T): EditScript = {
    val buf = new EditscriptBuffer
    t.loadInitial(buf)
    buf += Attach(t.uri, t.tag, RootLink, null, RootTag)
    buf.toEditscript
  }
}