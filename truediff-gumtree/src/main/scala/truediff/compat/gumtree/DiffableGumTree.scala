package truediff.compat.gumtree

import com.github.gumtreediff.tree.Tree
import truechange._
import truediff.{Diffable, Hashable, SubtreeRegistry}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

class DiffableGumTree(val typeLabel: String, _label: String) extends Tree(typeLabel.hashCode, _label) with Diffable {

  override val _tag: Tag = NamedTag(typeLabel)

  @inline def label: String = getLabel

  def this(typ: String, label: String, children: Iterable[DiffableGumTree]) = {
    this(typ, label)
    children.foreach(this.addChild)
  }

  def dchildren: Iterable[DiffableGumTree] =
    children.iterator().asScala.to(Iterable).asInstanceOf[Iterable[DiffableGumTree]]

  override lazy val cryptoHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(typeLabel, digest)
    Hashable.hash(label, digest)
    dchildren.foreach(t => digest.update(t.cryptoHash))
    digest.digest()
  }

  override def sig: Signature = ???

  override lazy val treeheight: Int = 1 + dchildren.map(_.treeheight).maxOption.getOrElse(0)

  override lazy val treesize: Int = 1 + dchildren.map(_.treesize).sum

  override def toStringWithURI: String = s"${typeLabel}_$uri($label, ${dchildren.map(_.toStringWithURI).mkString(", ")})"
  override def toString: String = s"$typeLabel($label, ${dchildren.map(_.toString).mkString(", ")})"

  override def foreachSubtree(f: Diffable => Unit): Unit = dchildren.foreach { t =>
    f(t)
    t.foreachSubtree(f)
  }

  override def loadUnassigned(edits: EditScriptBuffer): DiffableGumTree = {
    val that = this
    if (that.assigned != null) {
      return that.assigned.asInstanceOf[DiffableGumTree]
    }

    val newchildren = dchildren.map(_.loadUnassigned(edits))
    val newtree = new DiffableGumTree(typeLabel, label, newchildren)

    edits += Load(newtree.uri, this.tag,
      newchildren.zipWithIndex.map {
        case (t, ix) => ix.toString -> t.uri
      }.to(Iterable),
      Seq("label" -> label)
    )

    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    dchildren.foreach(_.loadInitial(edits))

    edits += Load(this.uri, this.tag,
      dchildren.zipWithIndex.map {
        case (t, ix) => ix.toString -> t.uri
      },
      Seq("label" -> label)
    )
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      edits += Unload(this.uri, this.tag,
        dchildren.zipWithIndex.map {
          case (t, ix) => ix.toString -> t.uri
        },
        Seq("label" -> label)
      )
      dchildren.foreach(_.unloadUnassigned(edits))
    }
  }

  // trim equal elements off the front of the lists
  private def trimFront[B <: Diffable](l1: Iterable[B], l2: Iterable[B], subtreeReg: SubtreeRegistry) : (Iterable[B], Iterable[B]) = {
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

  override protected def assignSharesRecurse(thatX: Diffable, subtreeReg: SubtreeRegistry): Unit = {
    val that = thatX.asInstanceOf[DiffableGumTree]

    if (this.typeLabel == that.typeLabel && this.label == that.label) {
      val (thislist1, thatlist1) = trimFront(this.dchildren, that.dchildren, subtreeReg)
      val (thislist2, thatlist2) = trimFront(thislist1.toSeq.reverse, thatlist1.toSeq.reverse, subtreeReg)

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
    } else {
      this.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
    }
  }

  override protected def directSubtrees: Iterable[Diffable] = dchildren

  override protected def computeEditScriptRecurse(thatX: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = {
    val that = thatX.asInstanceOf[DiffableGumTree]

    if (this.typeLabel == that.typeLabel && this.label == that.label) {
      val newchildren = computeEditScriptLists(this.dchildren, that.dchildren, this.uri, this.tag, this.uri, this.tag, ListFirstLink(null), edits)
      val newtree = new DiffableGumTree(typeLabel, label, newchildren)
      newtree._uri = this.uri
      newtree
    } else {
      null
    }
  }


  private def computeEditScriptLists(thislist: Iterable[DiffableGumTree], thatlist: Iterable[DiffableGumTree], thisparent: URI, thisparentTag: Tag, thatparent: URI, thatparentTag: Tag, link: Link, edits: EditScriptBuffer): Iterable[DiffableGumTree] = {
    var vthislist: Iterable[DiffableGumTree] = thislist
    var vthatlist: Iterable[DiffableGumTree] = thatlist
    var vthisparent: URI = thisparent
    var vthisparentTag: Tag = thisparentTag
    var vthatparent: URI = thatparent
    var vthatparentTag: Tag = thatparentTag
    var vlink: Link = link
    val NEXT = ListNextLink(null)

    val result = ListBuffer[DiffableGumTree]()

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

    result.toIterable
  }

  private def tryReuseListElem(thisnode: DiffableGumTree, thatnode: DiffableGumTree, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Option[DiffableGumTree] = {
    // this == that
    if (thatnode.assigned != null && thatnode.assigned.uri == thisnode.uri) {
      thisnode.assigned = null
      return Some(thisnode)
    }

    if (thisnode.assigned == null && thatnode.assigned == null) {
      val newtree = thisnode._computeEditScriptRecurse(thatnode, parent, parentTag, link, edits).asInstanceOf[DiffableGumTree]
      if (newtree != null)
        return Some(newtree)
    }

    None
  }

}
