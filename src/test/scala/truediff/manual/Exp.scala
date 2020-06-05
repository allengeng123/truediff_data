package truediff.manual

import truediff._
import truediff.changeset._
import truediff.diffable.{Diffable, SubtreeRegistry}

trait Exp extends Diffable

object Exp {
  case class Hole() extends Exp {
    override def treeheight: Int = 1

    override def treesize: Int = 1

    override def toStringWithURI: String = s"Hole_$uri()"

    override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map(), Map())

    override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
      f(this)
    }

    override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
      case that: Hole =>
      case _ =>
        that.foreachDiffable(t => subtreeReg.shareFor(t))
    }

    override private[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.empty

    override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
      case Hole() =>
        val newtree = Hole()
        newtree._uri = this.uri
        newtree
      case _ => null
    }

    override private[truediff] def loadUnassigned(changes: ChangesetBuffer): Diffable = {
      val that = this
      if (that.assigned != null) {
        return that.assigned
      }

      val newtree = Hole()
      changes += LoadNode(newtree.uri, this.tag, Seq(), Seq())
      newtree
    }

    override private[truediff] def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
      if (this.assigned != null) {
        changes += DetachNode(parent, link, this.uri, this.tag)
        this.assigned = null
      } else
        changes += UnloadNode(parent, link, this.uri, this.tag)
    }

    lazy val hash: Array[Byte] = {
      val digest = Hashable.mkDigest
      this.getClass.getCanonicalName.getBytes
      digest.digest()
    }
  }
}


case class Num(n: Int) extends Exp {

  lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    Hashable.hash(this.n, digest)
    digest.digest()
  }

  override val treeheight: Int = 1

  override def treesize: Int = 1

  override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map(), Map("n" -> classOf[Int]))

  override def toStringWithURI: String = s"Num_$uri($n)"

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    f(this)
  }

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Num if this.n == that.n =>
    case _ =>
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override private[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.empty

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case Num(n) if this.n == n =>
      val newtree = Num(n)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val newtree = Num(this.n)
    changes += LoadNode(newtree.uri, this.tag, Seq(), Seq(
      "n" -> Literal(this.n)
    ))
    newtree
  }

  override def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      changes += DetachNode(parent, link, this.uri, this.tag)
      this.assigned = null
    } else
      changes += UnloadNode(parent, link, this.uri, this.tag)
  }
}

case class Add(e1: Exp, e2: Exp) extends Exp {

  override def hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(this.e1.hash)
    digest.update(this.e2.hash)
    digest.digest()
  }

  override val treeheight: Int = 1 + Math.max(e1.treeheight, e2.treeheight)

  override def treesize: Int = 1 + e1.treesize + e2.treesize

  override def toStringWithURI: String = s"Add_$uri(${e1.toStringWithURI}, ${e2.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map("e1" -> SortType(classOf[Exp]), "e2" -> SortType(classOf[Exp])), Map())

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    f(this)
    this.e1.foreachDiffable(f)
    this.e2.foreachDiffable(f)
  }

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Add =>
      this.e1.assignShares(that.e1, subtreeReg)
      this.e2.assignShares(that.e2, subtreeReg)
    case _ =>
      this.e1.foreachDiffable(subtreeReg.registerShareFor)
      this.e2.foreachDiffable(subtreeReg.registerShareFor)
      that.foreachDiffable(subtreeReg.shareFor)
  }

  override private[truediff] def assignSubtreesRecurse(): Iterable[Diffable] = Iterable(e1, e2)

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case that: Add =>
      val e1 = this.e1.computeChangeset(that.e1, this.uri, NamedLink(this.tag, "e1"), changes).asInstanceOf[Exp]
      val e2 = this.e2.computeChangeset(that.e2, this.uri, NamedLink(this.tag, "e2"), changes).asInstanceOf[Exp]
      val newtree = Add(e1, e2)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val e1 = that.e1.loadUnassigned(changes).asInstanceOf[Exp]
    val e2 = that.e2.loadUnassigned(changes).asInstanceOf[Exp]
    val newtree = Add(e1, e2)
    changes += LoadNode(newtree.uri, this.tag, Seq(
      "e1" -> e1.uri,
      "e2" -> e2.uri
    ), Seq())
    newtree
  }

  override def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      changes += DetachNode(parent, link, this.uri, this.tag)
      this.assigned = null
    } else {
      this.e1.unloadUnassigned(this.uri, NamedLink(this.tag, "e1"), changes)
      this.e2.unloadUnassigned(this.uri, NamedLink(this.tag, "e2"), changes)
      changes += UnloadNode(parent, link, this.uri, this.tag)
    }
  }
}
