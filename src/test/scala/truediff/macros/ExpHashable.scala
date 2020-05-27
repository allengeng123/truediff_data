package truediff.macros
package hashableTest

import truediff._
import truediff.changeset._
import truediff.diffable.{Diffable, SubtreeRegistry}


@hashable
trait Exp extends Diffable

object Exp {
  case class Hole() extends Exp {
    override def height: Int = 1

    override def toStringWithURI: String = s"None_$uri()"

    override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
      f(this)
    }

    override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
      case that: Hole =>
      case _ =>
        that.foreachDiffable(t => subtreeReg.shareFor(t))
    }

    override private[truediff] def assignSubtreesRecurse(): Unit = {
      // nothing to do
    }

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
      changes += LoadNode(newtree.uri, classOf[Hole], Seq())
      newtree
    }

    override private[truediff] def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
      if (this.assigned != null) {
        changes += DetachNode(parent, link, this.uri)
        this.assigned = null
      } else
        changes += UnloadNode(parent, link, this.uri, Seq())
    }
  }

}

@hashable
case class Num(n: Int) extends Exp {

  override val height: Int = 1

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    f(this)
  }

  override def toStringWithURI: String = s"Num_$uri($n)"

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Num if this.n == that.n =>
    case _ =>
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override private[truediff] def assignSubtreesRecurse(): Unit = {
    // nothing to do
  }

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
    changes += LoadNode(newtree.uri, classOf[Num], Seq(
      NamedLink("n") -> Literal(this.n)
    ))
    newtree
  }

  override def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      changes += DetachNode(parent, link, this.uri)
      this.assigned = null
    } else
      changes += UnloadNode(parent, link, this.uri, Seq())
  }
}

@hashable
case class Add(e1: Exp, e2: Exp) extends Exp {

  override val height: Int = 1 + Math.max(e1.height, e2.height)

  override private[truediff] def foreachDiffable(f: Diffable => Unit): Unit = {
    f(this)
    this.e1.foreachDiffable(f)
    this.e2.foreachDiffable(f)
  }

  override def toStringWithURI: String = s"Add_$uri(${e1.toStringWithURI}, ${e2.toStringWithURI})"

  override private[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Add =>
      this.e1.assignShares(that.e1, subtreeReg)
      this.e2.assignShares(that.e2, subtreeReg)
    case _ =>
      this.e1.foreachDiffable(t => subtreeReg.shareFor(t).registerAvailableTree(t))
      this.e2.foreachDiffable(t => subtreeReg.shareFor(t).registerAvailableTree(t))
      that.foreachDiffable(t => subtreeReg.shareFor(t))
  }

  override private[truediff] def assignSubtreesRecurse(): Unit =
    if (this.e1.height >= this.e2.height) {
      this.e1.assignSubtrees()
      this.e2.assignSubtrees()
    } else {
      this.e2.assignSubtrees()
      this.e1.assignSubtrees()
    }

  override private[truediff] def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case that: Add =>
      val e1 = this.e1.computeChangeset(that.e1, this.uri, NamedLink("e1"), changes).asInstanceOf[Exp]
      val e2 = this.e2.computeChangeset(that.e2, this.uri, NamedLink("e2"), changes).asInstanceOf[Exp]
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
    changes += LoadNode(newtree.uri, classOf[Add], Seq(
      NamedLink("e1") -> e1.uri,
      NamedLink("e2") -> e2.uri
    ))
    newtree
  }

  override def unloadUnassigned(parent: NodeURI, link: Link, changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      changes += DetachNode(parent, link, this.uri)
      this.assigned = null
    } else {
      this.e1.unloadUnassigned(this.uri, NamedLink("e1"), changes)
      this.e2.unloadUnassigned(this.uri, NamedLink("e2"), changes)
      changes += UnloadNode(parent, link, this.uri, Seq(NamedLink("e1"), NamedLink("e2")))
    }
  }
}
