package truediff.diffable

import truediff.TreeURI.NodeTag
import truediff._
import truediff.changeset._

import scala.collection.mutable.ListBuffer

final case class DiffableList[+A <: Diffable](list: Seq[A], atype: Type) extends Diffable {
  def length: Int = list.length
  def apply(i: Int): A = list(i)
  def updated[B >: A <: Diffable](i: Int, elem: B): DiffableList[B] = DiffableList(list.updated(i, elem), atype)
  def indices: Range = Range(0, length)

  override def tag: NodeTag = ListType(atype)
  override def sig: Signature = Signature(ListType(atype), this.tag, Map(), Map())

  override val treeheight: Int = 1 + this.list.foldRight(0)((t, max) => Math.max(t.treeheight, max))

  override lazy val treesize: Int = 1 + this.list.foldRight(0)((t, sum) => t.treesize + sum)

  override def toString: String = s"List(" + list.map(_.toString).mkString(", ") + ")"
  override def toStringWithURI: String = s"List_$uri(" + list.map(_.toStringWithURI).mkString(", ") + ")"

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    f(this)
    this.list.foreach(_.foreachDiffable(f))
  }

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: DiffableList[A] =>
      // pre-assign subtrees that are list elements in this and that

      var thisShares: Map[SubtreeShare, ListBuffer[A]] = Map()

      this.list.foreach { thisNode =>
        val share = subtreeReg.shareFor(thisNode)
        thisShares.get(share) match {
          case Some(buf) => buf += thisNode
          case None => thisShares += share -> ListBuffer(thisNode)
        }
      }

      that.list.foreach { thatNode =>
        val thatShare = subtreeReg.shareFor(thatNode)
        thisShares.get(thatShare) match {
          case Some(buf) =>
            val thisNode = buf.remove(0)
            thisNode.assigned = thatNode
            thisNode.share = null
            thatNode.assigned = thisNode
          case None =>
        }
      }

      this.list.filter(_.assigned == null).zipAll[Diffable,Diffable](that.list.filter(_.assigned == null), null, null).foreach { case (thisnode, thatnode) =>
        if (thisnode == null) {
          thatnode.foreachDiffable(subtreeReg.shareFor)
        } else if (thatnode == null) {
          thisnode.foreachDiffable(subtreeReg.registerShareFor)
        } else {
          thisnode.share.registerAvailableTree(thisnode)
          thisnode.assignSharesRecurse(thatnode, subtreeReg)
        }
      }

  }

  override private[truediff] def assignSubtreesRecurse(): Iterable[A] = this.list

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): DiffableList[Diffable] = that match {
    case that: DiffableList[A] =>
      val newlist = computeChangesetLists(this.list, that.list, this.uri, this.uri, ListFirstLink(atype), changes)
      val newtree = DiffableList(newlist, atype)
      newtree._uri = this.uri
      newtree
    case _ =>
      null
  }

  private[diffable] def computeChangesetLists(thislist: Seq[Diffable], thatlist: Seq[Diffable], thisparent: NodeURI, thatparent: NodeURI, link: Link, changes: ChangesetBuffer): Seq[Diffable] = (thislist, thatlist) match {
    case (Nil, Nil) => Nil
    case (Nil, thatnode::thatlist) =>
      val newtree = thatnode.loadUnassigned(changes)
      changes += AttachNode(thatparent, link, newtree.uri)
      val newlist = computeChangesetLists(Nil, thatlist, null, newtree.uri, ListNextLink(atype), changes)
      newtree +: newlist
    case (thisnode::thislist, Nil) =>
      computeChangesetLists(thislist, Nil, thisnode.uri, null, ListNextLink(atype), changes)
      thisnode.unloadUnassigned(thisparent, link, changes)
      Seq()
    case (thisnode::thislist, thatnode::thatlist) =>
      tryReuseListElem(thisnode, thatnode, thisparent, link, changes) match {
        case Some(reusednode) =>
          // could reuse node
          val hasParentChanged = thisparent != thatparent
          if (hasParentChanged || thisnode.uri != reusednode.uri) {
            changes += DetachNode(thisparent, link, reusednode.uri, reusednode.tag)
            changes += AttachNode(thatparent, link, reusednode.uri)
          }
          val newlist = computeChangesetLists(thislist, thatlist, thisnode.uri, reusednode.uri, ListNextLink(atype), changes)
          reusednode +: newlist
        case None =>
          // need to unload thisnode and load thatnode
          val newtree = thatnode.loadUnassigned(changes)
          changes += AttachNode(thatparent, link, newtree.uri)
          val newlist = computeChangesetLists(thislist, thatlist, thisnode.uri, newtree.uri ,ListNextLink(atype), changes)
          thisnode.unloadUnassigned(thisparent, link, changes)
          newtree +: newlist
      }
  }

  private[diffable] def tryReuseListElem(thisnode: Diffable, thatnode: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Option[Diffable] = {
    // this == that
    if (thatnode.assigned != null && thatnode.assigned.uri == thisnode.uri) {
      thisnode.assigned = null
      return Some(thisnode)
    }

    if (thisnode.assigned == null && thatnode.assigned == null) {
      val newtree = thisnode.computeChangesetRecurse(thatnode, parent, link, changes)
      if (newtree != null)
        return Some(newtree)
    }

    None
  }

  override private[truediff] def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val newlist = that.list.map(_.loadUnassigned(changes))
    val newtree = DiffableList(newlist, atype)
    changes += LoadNode(newtree.uri, this.tag, Seq(), Seq())
    newlist.foldLeft[(NodeURI,Link)]((newtree.uri, ListFirstLink(atype))){(pred, el) =>
      changes += AttachNode(pred._1, pred._2, el.uri)
      (el.uri, ListNextLink(atype))
    }

    newtree
  }

  override private[truediff] def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      changes += DetachNode(parent, link, this.uri, this.tag)
      this.assigned = null
    } else {
      unload(this.list, this.uri, ListFirstLink(atype), changes)
      changes += UnloadNode(parent, link, this.uri, this.tag)
    }
  }

  private def unload(l: Seq[Diffable], parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = l match {
    case Nil =>
    case a :: as =>
      unload(as, a.uri, ListNextLink(atype), changes)
      a.unloadUnassigned(parent, link, changes)
  }

  override def hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    this.list.foreach(t => digest.update(t.hash))
    digest.digest()
  }
}
object DiffableList {
  def from[A <: Diffable](list: Seq[A], atype: Type): DiffableList[A] = DiffableList(list, atype)
}
