package truediff

import truechange._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Diffable extends Hashable {

  private var _uri: URI = new JVMURI

  def uri: URI = _uri
  protected def withURI(uri: URI): this.type = {
    _uri = uri
    this
  }

  val _tag: Tag = NamedTag(this.getClass.getCanonicalName)
  def tag: Tag = _tag

  final lazy val structureHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.tag match {
      case NamedTag(c) => Hashable.hash(c, digest)
      case lt: ListTag => Hashable.hash(lt.toString, digest)
    }
    this.directSubtrees.foreach(t => digest.update(t.structureHash))
    digest.digest()
  }

  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.directSubtrees.foreach(t => digest.update(t.literalHash))
    digest.digest()
  }

  def sig: Signature
  def collectSignatures: Map[Tag, Signature] = {
    var sigs: Map[Tag, Signature] = Map(RootTag -> RootSig)
    this.foreachTree(t => if (!t.skipNode) sigs += t.tag -> t.sig)
    sigs
  }

  def treeheight: Int
  def treesize: Int
  def toStringWithURI: String

  private[truediff] def skipNode: Boolean = false

  private[truediff] var share: SubtreeShare = _
  protected[truediff] var assigned: Diffable = _

  final def foreachTree(f: Diffable => Unit): Unit = {
    if (!this.skipNode)
      f(this)
    this.foreachSubtree(f)
  }
  final def foreachSubtree(f: Diffable => Unit): Unit =
    directSubtrees.foreach { t =>
      if (!t.skipNode)
        f(t)
      t.foreachSubtree(f)
    }

  def loadUnassigned(edits: EditScriptBuffer): Diffable
  def unloadUnassigned(edits: EditScriptBuffer): Unit
  def loadInitial(edits: EditScriptBuffer): Unit
  def updateLiterals(that: Diffable, edits: EditScriptBuffer): Diffable

  protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit =
    if (this.tag == that.tag) {
      this.share.registerAvailableTree(this)
      val subs = this.directSubtrees.zip(that.directSubtrees)
      subs.foreach {case (l,r) => l.assignShares(r, subtreeReg)}
    } else {
      this.foreachTree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
    }
  private[truediff] def _assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = this.assignSharesRecurse(that, subtreeReg)

  protected def directSubtrees: Iterable[Diffable]
  private[truediff] def _directSubtrees: Iterable[Diffable] = this.directSubtrees

  protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable
  private[truediff] def _computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = this.computeEditScriptRecurse(that, parent, parentTag, link, edits)

  @inline
  private[truediff] final def assignTree(that: Diffable): Unit = {
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
    if (thisShare == thatShare) // equal trees => preemptive assign
      this.assignTree(that)
    else
      assignSharesRecurse(that, subtreeReg)
  }

  protected final def assignSubtrees(that: Diffable, subtreeReg: SubtreeRegistry): Unit = {
    val queue = new mutable.PriorityQueue[Diffable]()(Diffable.highestFirstOrdering)
    queue += that

    while (queue.nonEmpty) {
      val level = queue.head.treeheight
      val nextNodes = ListBuffer[Diffable]()
      while (queue.nonEmpty && queue.head.treeheight == level) {
        val next = queue.dequeue()
        if (next.assigned == null)
          nextNodes += next
      }

      val remainingMatchedNodes = assignPreferredSubtrees(nextNodes, subtreeReg)
      val unassignedNodes = assignSubtrees(remainingMatchedNodes, subtreeReg)
      unassignedNodes.foreach(queue ++= _.directSubtrees)
    }
  }

  private def assignPreferredSubtrees(nodes: Iterable[Diffable], subtreeReg: SubtreeRegistry): Iterable[Diffable] =
    nodes.filter { node =>
      if (node.skipNode)
        true
      else node.share.takePreferredAvailableTree(node, subtreeReg) match {
        case Some(availableTree) =>
          availableTree.assignTree(node)
          false // discard
        case None =>
          true // keep
      }
    }

  private def assignSubtrees(nodes: Iterable[Diffable], subtreeReg: SubtreeRegistry): Iterable[Diffable] =
    nodes.filter { node =>
      if (node.skipNode)
        true
      else node.share.takeApproxAvailableTree(node, subtreeReg) match {
        case Some(availableTree) =>
          availableTree.assignTree(node)
          false // discard
        case None =>
          true // keep
      }
    }


  final def computeEditScript(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = {
    if (this.assigned != null && this.assigned.uri == that.uri) {
      // this == that
      val newtree = this.updateLiterals(that, edits)
      this.assigned = null
      return newtree
    }

    if (this.assigned == null && that.assigned == null) {
      val newtree = this.computeEditScriptRecurse(that, parent, parentTag, link, edits)
      if (newtree != null)
        return newtree
    }

    edits += Detach(this.uri, this.tag, link, parent, parentTag)
    this.unloadUnassigned(edits)
    val newtree = that.loadUnassigned(edits)
    edits += Attach(newtree.uri, newtree.tag, link, parent, parentTag)
    newtree
  }

  final def compareTo[T <: Diffable](that: T): (EditScript, T) = {
    val subtreeReg = new SubtreeRegistry
    this.assignShares(that, subtreeReg)

    this.assignSubtrees(that, subtreeReg)

    val edits = new EditScriptBuffer
    val newtree = this.computeEditScript(that, null, RootTag, RootLink, edits)
    (edits.toEditScript, newtree.asInstanceOf[T])
  }
}

object Diffable {
  val highestFirstOrdering: Ordering[Diffable] = Ordering.by[Diffable,Int](_.treeheight)

  def load(t: Diffable): EditScript = {
    val edits = new EditScriptBuffer
    t.loadInitial(edits)
    edits += Attach(t.uri, t.tag, RootLink, null, RootTag)
    edits.toEditScript
  }
}