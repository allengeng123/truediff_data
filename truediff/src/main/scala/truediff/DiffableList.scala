package truediff

import truechange._

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

  override def foreachDiffableKid(f: Diffable => Unit): Unit = {
    this.list.foreach { t =>
      f(t)
      t.foreachDiffableKid(f)
    }
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: DiffableList[A] =>
      // pre-assign subtrees that are list elements in this and that

      var thisShares: Map[SubtreeShare, ListBuffer[A]] = Map()

      this.list.foreach { thisNode =>
        val thisShare = subtreeReg.shareFor(thisNode)
        thisShares.get(thisShare) match {
          case Some(buf) => buf += thisNode
          case None => thisShares += thisShare -> ListBuffer(thisNode)
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
          thatnode.foreachDiffableKid(subtreeReg.shareFor)
        } else if (thatnode == null) {
          thisnode.share.registerAvailableTree(thisnode)
          thisnode.foreachDiffableKid(subtreeReg.registerShareFor)
        } else {
          thisnode.share.registerAvailableTree(thisnode)
          thisnode._assignSharesRecurse(thatnode, subtreeReg)
        }
      }

  }

  override protected def assignSubtreesRecurse(): Iterable[A] = this.list

  override protected def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): DiffableList[Diffable] = that match {
    case that: DiffableList[A] =>
      val newlist = computeChangesetLists(this.list, that.list, this.uri, this.uri, ListFirstLink(atype), changes)
      val newtree = DiffableList(newlist, atype)
      newtree._uri = this.uri
      newtree
    case _ =>
      null
  }

  private[truediff] def computeChangesetLists(thislist: Seq[Diffable], thatlist: Seq[Diffable], thisparent: NodeURI, thatparent: NodeURI, link: Link, changes: ChangesetBuffer): Seq[Diffable] = (thislist, thatlist) match {
    case (Nil, Nil) => Nil
    case (Nil, thatnode::thatlist) =>
      val newtree = thatnode.loadUnassigned(changes)
      changes += Attach(thatparent, link, newtree.uri, newtree.tag)
      val newlist = computeChangesetLists(Nil, thatlist, null, newtree.uri, ListNextLink(atype), changes)
      newtree +: newlist
    case (thisnode::thislist, Nil) =>
      changes += Detach(thisparent, link, thisnode.uri, thisnode.tag)
      thisnode.unloadUnassigned(changes)
      computeChangesetLists(thislist, Nil, thisnode.uri, null, ListNextLink(atype), changes)
      Seq()
    case (thisnode::thislist, thatnode::thatlist) =>
      tryReuseListElem(thisnode, thatnode, thisparent, link, changes) match {
        case Some(reusednode) =>
          // could reuse node
          val hasParentChanged = thisparent != thatparent
          if (hasParentChanged || thisnode.uri != reusednode.uri) {
            changes += Detach(thisparent, link, reusednode.uri, reusednode.tag)
            changes += Attach(thatparent, link, reusednode.uri, reusednode.tag)
          }
          val newlist = computeChangesetLists(thislist, thatlist, thisnode.uri, reusednode.uri, ListNextLink(atype), changes)
          reusednode +: newlist
        case None =>
          // need to unload thisnode and load thatnode
          changes += Detach(thisparent, link, thisnode.uri, thisnode.tag)
          thisnode.unloadUnassigned(changes)
          val newtree = thatnode.loadUnassigned(changes)
          changes += Attach(thatparent, link, newtree.uri, newtree.tag)
          val newlist = computeChangesetLists(thislist, thatlist, thisnode.uri, newtree.uri ,ListNextLink(atype), changes)
          newtree +: newlist
      }
  }

  private[truediff] def tryReuseListElem(thisnode: Diffable, thatnode: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Option[Diffable] = {
    // this == that
    if (thatnode.assigned != null && thatnode.assigned.uri == thisnode.uri) {
      thisnode.assigned = null
      return Some(thisnode)
    }

    if (thisnode.assigned == null && thatnode.assigned == null) {
      val newtree = thisnode._computeChangesetRecurse(thatnode, parent, link, changes)
      if (newtree != null)
        return Some(newtree)
    }

    None
  }

  override def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val newlist = that.list.map(_.loadUnassigned(changes))
    val newtree = DiffableList(newlist, atype)
    changes += Load(newtree.uri, this.tag, Seq(), Seq())
    newlist.foldLeft[(NodeURI,Link)]((newtree.uri, ListFirstLink(atype))){(pred, el) =>
      changes += Attach(pred._1, pred._2, el.uri, el.tag)
      (el.uri, ListNextLink(atype))
    }

    newtree
  }


  override def loadInitial(changes: ChangesetBuffer): Unit = {
    changes += Load(this.uri, this.tag, Seq(), Seq())
    this.list.foldLeft[(NodeURI,Link)]((this.uri, ListFirstLink(atype))){(pred, el) =>
      el.loadInitial(changes)
      changes += Attach(pred._1, pred._2, el.uri, el.tag)
      (el.uri, ListNextLink(atype))
    }
  }

  override def unloadUnassigned(changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      changes += Unload(this.uri, this.tag, Seq(), Seq())
      this.list.foldLeft[(NodeURI,Link)]((this.uri, ListFirstLink(atype))){(pred, el) =>
        changes += Detach(pred._1, pred._2, el.uri, el.tag)
        el.unloadUnassigned(changes)
        (el.uri, ListNextLink(atype))
      }
    }
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
