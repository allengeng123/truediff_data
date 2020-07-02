package truediff

import truechange._

import scala.collection.mutable.ListBuffer

object DiffableList {
  def from[A <: Diffable](list: Seq[A], atype: Type): DiffableList[A] = DiffableList(list, atype)
}
final case class DiffableList[+A <: Diffable](list: Seq[A], atype: Type) extends Diffable {
  def length: Int = list.length
  def apply(i: Int): A = list(i)
  def updated[B >: A <: Diffable](i: Int, elem: B): DiffableList[B] = DiffableList(list.updated(i, elem), atype)
  def indices: Range = Range(0, length)

  override def tag: Tag = ListTag(atype)
  override def sig: Signature = Signature(ListType(atype), this.tag, Map(), Map())

  override val treeheight: Int = 1 + this.list.foldRight(0)((t, max) => Math.max(t.treeheight, max))

  override lazy val treesize: Int = 1 + this.list.foldRight(0)((t, sum) => t.treesize + sum)

  override def toString: String = s"List(" + list.map(_.toString).mkString(", ") + ")"
  override def toStringWithURI: String = s"List_$uri(" + list.map(_.toStringWithURI).mkString(", ") + ")"

  override def foreachSubtree(f: Diffable => Unit): Unit = {
    this.list.foreach { t =>
      f(t)
      t.foreachSubtree(f)
    }
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: DiffableList[A] =>
      // pre-assign subtrees that are list elements in this and that

      var thisShares: Map[SubtreeShare, ListBuffer[A]] = Map()

      this.list.foreach { thisNode =>
        val thisShare = subtreeReg.assignShare(thisNode)
        thisShares.get(thisShare) match {
          case Some(buf) => buf += thisNode
          case None => thisShares += thisShare -> ListBuffer(thisNode)
        }
      }

      that.list.foreach { thatNode =>
        val thatShare = subtreeReg.assignShare(thatNode)
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
          thatnode.foreachSubtree(subtreeReg.assignShare)
        } else if (thatnode == null) {
          thisnode.share.registerAvailableTree(thisnode)
          thisnode.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
        } else {
          thisnode.share.registerAvailableTree(thisnode)
          thisnode._assignSharesRecurse(thatnode, subtreeReg)
        }
      }

  }

  override protected def directSubtrees: Iterable[A] = this.list

  override protected def computeEditscriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, changes: EditscriptBuffer): DiffableList[Diffable] = that match {
    case that: DiffableList[A] =>
      val newlist = computeEditscriptLists(this.list, that.list, this.uri, this.tag, this.uri, this.tag, ListFirstLink(atype), changes)
      val newtree = DiffableList(newlist, atype)
      newtree._uri = this.uri
      newtree
    case _ =>
      null
  }

  private[truediff] def computeEditscriptLists(thislist: Seq[Diffable], thatlist: Seq[Diffable], thisparent: URI, thisparentTag: Tag, thatparent: URI, thatparentTag: Tag, link: Link, changes: EditscriptBuffer): Seq[Diffable] = (thislist, thatlist) match {
    case (Nil, Nil) => Nil
    case (Nil, thatnode::thatlist) =>
      val newtree = thatnode.loadUnassigned(changes)
      changes += Attach(newtree.uri, newtree.tag, link, thatparent, thatparentTag)
      val newlist = computeEditscriptLists(Nil, thatlist, null, null, newtree.uri, newtree.tag, ListNextLink(atype), changes)
      newtree +: newlist
    case (thisnode::thislist, Nil) =>
      changes += Detach(thisnode.uri, thisnode.tag, link, thisparent, thisparentTag)
      thisnode.unloadUnassigned(changes)
      computeEditscriptLists(thislist, Nil, thisnode.uri, thisnode.tag, null, null, ListNextLink(atype), changes)
      Seq()
    case (thisnode::thislist, thatnode::thatlist) =>
      tryReuseListElem(thisnode, thatnode, thisparent, thisparentTag, link, changes) match {
        case Some(reusednode) =>
          // could reuse node
          val hasParentChanged = thisparent != thatparent
          if (hasParentChanged || thisnode.uri != reusednode.uri) {
            changes += Detach(reusednode.uri, reusednode.tag, link, thisparent, thisparentTag)
            changes += Attach(reusednode.uri, reusednode.tag, link, thatparent, thatparentTag)
          }
          val newlist = computeEditscriptLists(thislist, thatlist, thisnode.uri, thisnode.tag, reusednode.uri, reusednode.tag, ListNextLink(atype), changes)
          reusednode +: newlist
        case None =>
          // need to unload thisnode and load thatnode
          changes += Detach(thisnode.uri, thisnode.tag, link, thisparent, thisparentTag)
          thisnode.unloadUnassigned(changes)
          val newtree = thatnode.loadUnassigned(changes)
          changes += Attach(newtree.uri, newtree.tag, link, thatparent, thatparentTag)
          val newlist = computeEditscriptLists(thislist, thatlist, thisnode.uri, thisnode.tag, newtree.uri, newtree.tag ,ListNextLink(atype), changes)
          newtree +: newlist
      }
  }

  private[truediff] def tryReuseListElem(thisnode: Diffable, thatnode: Diffable, parent: URI, parentTag: Tag, link: Link, changes: EditscriptBuffer): Option[Diffable] = {
    // this == that
    if (thatnode.assigned != null && thatnode.assigned.uri == thisnode.uri) {
      thisnode.assigned = null
      return Some(thisnode)
    }

    if (thisnode.assigned == null && thatnode.assigned == null) {
      val newtree = thisnode._computeEditscriptRecurse(thatnode, parent, parentTag, link, changes)
      if (newtree != null)
        return Some(newtree)
    }

    None
  }

  override def loadUnassigned(changes: EditscriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val newlist = that.list.map(_.loadUnassigned(changes))
    val newtree = DiffableList(newlist, atype)
    changes += Load(newtree.uri, this.tag, Seq(), Seq())
    newlist.foldLeft[(URI,Tag,Link)]((newtree.uri, newtree.tag, ListFirstLink(atype))){ (pred, el) =>
      changes += Attach(el.uri, el.tag, pred._3, pred._1, pred._2)
      (el.uri, el.tag, ListNextLink(atype))
    }

    newtree
  }


  override def loadInitial(changes: EditscriptBuffer): Unit = {
    changes += Load(this.uri, this.tag, Seq(), Seq())
    this.list.foldLeft[(URI,Tag,Link)]((this.uri, this.tag, ListFirstLink(atype))){ (pred, el) =>
      el.loadInitial(changes)
      changes += Attach(el.uri, el.tag, pred._3, pred._1, pred._2)
      (el.uri, el.tag, ListNextLink(atype))
    }
  }

  override def unloadUnassigned(changes: EditscriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      changes += Unload(this.uri, this.tag, Seq(), Seq())
      this.list.foldLeft[(URI,Tag,Link)]((this.uri, this.tag, ListFirstLink(atype))){ (pred, el) =>
        changes += Detach(el.uri, el.tag, pred._3, pred._1, pred._2)
        el.unloadUnassigned(changes)
        (el.uri, el.tag, ListNextLink(atype))
      }
    }
  }

  override lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    this.list.foreach(t => digest.update(t.hash))
    digest.digest()
  }
}
