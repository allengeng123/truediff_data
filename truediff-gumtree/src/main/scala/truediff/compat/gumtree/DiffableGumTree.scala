package truediff.compat.gumtree

import com.github.gumtreediff.tree.Tree
import truechange._
import truediff.{Diffable, Hashable, SubtreeRegistry}

import scala.jdk.CollectionConverters._

class DiffableGumTree(_typ: Int, _label: String) extends Tree(_typ, _label) with Diffable {

  override val _tag: Tag = NamedTag(typ.toString)

  @inline def typ: Int = getType
  @inline def label: String = getLabel

  def this(typ: Int, label: String, children: Iterable[DiffableGumTree]) = {
    this(typ, label)
    children.foreach(this.addChild)
  }

  def dchildren: Iterable[DiffableGumTree] =
    children.iterator().asScala.to(Iterable).asInstanceOf[Iterable[DiffableGumTree]]

  override lazy val cryptoHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(typ, digest)
    Hashable.hash(label, digest)
    dchildren.foreach(t => digest.update(t.cryptoHash))
    digest.digest()
  }

  override def sig: Signature = ???

  override lazy val treeheight: Int = 1 + dchildren.map(_.treeheight).maxOption.getOrElse(0)

  override lazy val treesize: Int = 1 + dchildren.map(_.treesize).sum

  override def toStringWithURI: String = s"${typ}_$uri($label, ${dchildren.map(_.toStringWithURI).mkString(", ")})"

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
    val newtree = new DiffableGumTree(typ, label, newchildren)

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

  override protected def assignSharesRecurse(thatX: Diffable, subtreeReg: SubtreeRegistry): Unit = {
    val that = thatX.asInstanceOf[DiffableGumTree]

    if (this.typ == that.typ && this.label == that.label && this.children.size == that.children.size) {
      this.dchildren.zip(that.dchildren).foreach(tt => tt._1.assignShares(tt._2, subtreeReg))
    } else {
      this.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
    }
  }

  override protected def directSubtrees: Iterable[Diffable] = dchildren

  override protected def computeEditScriptRecurse(thatX: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = {
    val that = thatX.asInstanceOf[DiffableGumTree]

    if (this.typ == that.typ && this.label == that.label && this.children.size == that.children.size) {
      val newchildren = this.dchildren.zip(that.dchildren).zipWithIndex.map {
        case ((thisSub, thatSub), ix) =>
          thisSub.computeEditScript(thatSub, this.uri, this.tag, NamedLink(ix.toString), edits).asInstanceOf[DiffableGumTree]
      }
      val newtree = new DiffableGumTree(typ, label, newchildren)
      newtree._uri = this.uri
      newtree
    } else {
      null
    }
  }
}
