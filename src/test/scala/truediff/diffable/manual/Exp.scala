package truediff.diffable.manual

import truediff._
import truediff.changeset._
import truediff.diffable.Diffable

trait Exp extends Diffable

case class Num(n: Int) extends Exp {

  lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    Hashable.hash(this.n, digest)
    digest.digest()
  }

  override val height: Int = 1

  override private[truediff] def diffableKids: Vector[Diffable] = Vector()

  override def toStringWithURI: String = s"Num_$uri($n)"

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
    changes += LoadNode(newtree.uri, newtree.tag, Seq(
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

case class Add(e1: Exp, e2: Exp) extends Exp {

  override def hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(this.e1.hash)
    digest.update(this.e2.hash)
    digest.digest()
  }

  override val height: Int = 1 + Math.max(e1.height, e2.height)

  override private[truediff] def diffableKids: Vector[Diffable] = Vector(e1, e2)

  override def toStringWithURI: String = s"Add_$uri(${e1.toStringWithURI}, ${e2.toStringWithURI})"

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
    changes += LoadNode(newtree.uri, newtree.tag, Seq(
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
