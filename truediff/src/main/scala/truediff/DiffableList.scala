package truediff

import truechange._

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

object DiffableList {
  def from[A <: Diffable](list: Seq[A], atype: Type): DiffableList[A] = DiffableList(list, atype)
}
final case class DiffableList[+A <: Diffable](list: Seq[A], atype: Type) extends Diffable {
  def length: Int = list.length
  def apply(i: Int): A = list(i)
  def updated[B >: A <: Diffable](i: Int, elem: B): DiffableList[B] = DiffableList(list.updated(i, elem), atype)
  def map[B <: Diffable](f: A => B)(implicit tag: ClassTag[B]): DiffableList[B] = {
    val btype = SortType(tag.runtimeClass)
    DiffableList(list.map(f), btype)
  }
  def indices: Range = Range(0, length)

  override val tag: Tag = ListTag(atype)
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

  // trim equal elements off the front of the lists
  private def trimFront[B <: Diffable](l1: Seq[B], l2: Seq[B], subtreeReg: SubtreeRegistry) : (Seq[B], Seq[B]) = {
    var thislist = l1
    var thatlist = l2
    while (thislist.nonEmpty && thatlist.nonEmpty) {
      val thisShare = subtreeReg.assignShare(thislist.head)
      val thatShare = subtreeReg.assignShare(thatlist.head)
      if (thisShare == thatShare) {
        thislist.head.assignTree(thatlist.head)
        thislist = thislist.tail
        thatlist = thatlist.tail
      } else {
        return (thislist, thatlist)
      }
    }
    (thislist, thatlist)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: DiffableList[A] =>

      val (thislist1, thatlist1) = trimFront(this.list, that.list, subtreeReg)
      val (thislist2, thatlist2) = trimFront(thislist1.reverse, thatlist1.reverse, subtreeReg)

      thislist2.zipAll[Diffable,Diffable](thatlist2, null, null).foreach { case (thisnode, thatnode) =>
        if (thisnode == null) {
          subtreeReg.assignShare(thatnode)
          thatnode.foreachSubtree(subtreeReg.assignShare)
        } else if (thatnode == null) {
          subtreeReg.assignShareAndRegisterTree(thisnode)
          thisnode.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
        } else {
          subtreeReg.assignShareAndRegisterTree(thisnode)
          subtreeReg.assignShare(thatnode)
          thisnode._assignSharesRecurse(thatnode, subtreeReg)
        }
      }

    // pre-assign subtrees that are list elements in this and that
//      var thisShares: Map[SubtreeShare, ListBuffer[A]] = Map()
//
//      this.list.foreach { thisNode =>
//        val thisShare = subtreeReg.assignShare(thisNode)
//        thisShares.get(thisShare) match {
//          case Some(edits) => edits += thisNode
//          case None => thisShares += thisShare -> ListBuffer(thisNode)
//        }
//      }
//
//      that.list.foreach { thatNode =>
//        val thatShare = subtreeReg.assignShare(thatNode)
//        thisShares.get(thatShare) match {
//          case Some(edits) =>
//            val thisNode = edits.remove(0)
//            if (edits.isEmpty)
//              thisShares -= thatShare
//            thisNode.assigned = thatNode
//            thisNode.share = null
//            thatNode.assigned = thisNode
//          case None =>
//        }
//      }
//
//      this.list.filter(_.assigned == null).zipAll[Diffable,Diffable](that.list.filter(_.assigned == null), null, null).foreach { case (thisnode, thatnode) =>
//        if (thisnode == null) {
//          thatnode.foreachSubtree(subtreeReg.assignShare)
//        } else if (thatnode == null) {
//          thisnode.share.registerAvailableTree(thisnode)
//          thisnode.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
//        } else {
//          thisnode.share.registerAvailableTree(thisnode)
//          thisnode._assignSharesRecurse(thatnode, subtreeReg)
//        }
//      }

  }

  override protected def directSubtrees: Iterable[A] = this.list

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): DiffableList[Diffable] = that match {
    case that: DiffableList[A] =>
      val newlist = computeEditScriptLists(this.list, that.list, this.uri, this.tag, this.uri, this.tag, ListFirstLink(atype), edits)
      val newtree = DiffableList(newlist, atype)
      newtree._uri = this.uri
      newtree
    case _ =>
      null
  }

  private[truediff] def computeEditScriptLists(thislist: Seq[Diffable], thatlist: Seq[Diffable], thisparent: URI, thisparentTag: Tag, thatparent: URI, thatparentTag: Tag, link: Link, edits: EditScriptBuffer): Seq[Diffable] = {
    var vthislist: Seq[Diffable] = thislist
    var vthatlist: Seq[Diffable] = thatlist
    var vthisparent: URI = thisparent
    var vthisparentTag: Tag = thisparentTag
    var vthatparent: URI = thatparent
    var vthatparentTag: Tag = thatparentTag
    var vlink: Link = link
    val NEXT = ListNextLink(atype)

    val result = ListBuffer[Diffable]()

    while (vthislist.nonEmpty && vthatlist.nonEmpty) {
      val thisnode = vthislist.head
      vthislist = vthislist.tail
      val thatnode = vthatlist.head
      vthatlist = vthatlist.tail
      tryReuseListElem(thisnode, thatnode, vthisparent, vthisparentTag, vlink, edits) match {
        case Some(reusednode) =>
          // could reuse node
          val hasParentChanged = vthisparent != vthatparent
          if (hasParentChanged || thisnode.uri != reusednode.uri) {
            edits += Detach(reusednode.uri, reusednode.tag, vlink, vthisparent, vthisparentTag)
            edits += Attach(reusednode.uri, reusednode.tag, vlink, vthatparent, vthatparentTag)
          }
          result += reusednode
          vthisparent = thisnode.uri
          vthisparentTag = thisnode.tag
          vthatparent = reusednode.uri
          vthatparentTag = reusednode.tag
          vlink = NEXT
        case None =>
          // need to unload thisnode and load thatnode
          edits += Detach(thisnode.uri, thisnode.tag, vlink, vthisparent, vthisparentTag)
          thisnode.unloadUnassigned(edits)
          val newtree = thatnode.loadUnassigned(edits)
          edits += Attach(newtree.uri, newtree.tag, vlink, vthatparent, vthatparentTag)
          result += newtree
          vthisparent = thisnode.uri
          vthisparentTag = thisnode.tag
          vthatparent = newtree.uri
          vthatparentTag = newtree.tag
          vlink = NEXT
      }
    }

    // load remaining thatlist nodes
    while (vthatlist.nonEmpty) {
      val thatnode = vthatlist.head
      vthatlist = vthatlist.tail
      val newtree = thatnode.loadUnassigned(edits)
      edits += Attach(newtree.uri, newtree.tag, vlink, vthatparent, vthatparentTag)
      result += newtree
      vthatparent = newtree.uri
      vthatparentTag = newtree.tag
      vlink = NEXT
    }

    // unload remaining thislist nodes
    while (vthislist.nonEmpty) {
      val thisnode = vthislist.head
      vthislist = vthislist.tail
      edits += Detach(thisnode.uri, thisnode.tag, vlink, vthisparent, vthisparentTag)
      thisnode.unloadUnassigned(edits)
      vthisparent = thisnode.uri
      vthisparentTag = thisnode.tag
    }

    result.toList
  }

  private[truediff] def tryReuseListElem(thisnode: Diffable, thatnode: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Option[Diffable] = {
    // this == that
    if (thatnode.assigned != null && thatnode.assigned.uri == thisnode.uri) {
      thisnode.assigned = null
      return Some(thisnode)
    }

    if (thisnode.assigned == null && thatnode.assigned == null) {
      val newtree = thisnode._computeEditScriptRecurse(thatnode, parent, parentTag, link, edits)
      if (newtree != null)
        return Some(newtree)
    }

    None
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val newlist = that.list.map(_.loadUnassigned(edits))
    val newtree = DiffableList(newlist, atype)
    edits += Load(newtree.uri, this.tag, Seq(), Seq())
    newlist.foldLeft[(URI,Tag,Link)]((newtree.uri, newtree.tag, ListFirstLink(atype))){ (pred, el) =>
      edits += Attach(el.uri, el.tag, pred._3, pred._1, pred._2)
      (el.uri, el.tag, ListNextLink(atype))
    }

    newtree
  }


  override def loadInitial(edits: EditScriptBuffer): Unit = {
    edits += Load(this.uri, this.tag, Seq(), Seq())
    this.list.foldLeft[(URI,Tag,Link)]((this.uri, this.tag, ListFirstLink(atype))){ (pred, el) =>
      el.loadInitial(edits)
      edits += Attach(el.uri, el.tag, pred._3, pred._1, pred._2)
      (el.uri, el.tag, ListNextLink(atype))
    }
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      edits += Unload(this.uri, this.tag, Seq(), Seq())
      this.list.foldLeft[(URI,Tag,Link)]((this.uri, this.tag, ListFirstLink(atype))){ (pred, el) =>
        edits += Detach(el.uri, el.tag, pred._3, pred._1, pred._2)
        el.unloadUnassigned(edits)
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
