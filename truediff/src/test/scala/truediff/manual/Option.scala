package truediff.manual

import truechange._
import truediff.{SubtreeRegistry, _}

case class Maybe(a: DiffableOption[Exp]) extends Exp {

  lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(a.hash)
    digest.digest()
  }

  override val treeheight: Int = 1 + a.treeheight

  override def treesize: Int = 1 + a.treesize

  override def toStringWithURI: String = s"Maybe_$uri(${a.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map("a" -> OptionType(SortType(classOf[Exp]))), Map())

  override def foreachDiffableKid(f: Diffable => Unit): Unit = {
    f(this.a)
    this.a.foreachDiffableKid(f)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Maybe =>
      this.a.assignShares(that.a, subtreeReg)
    case _ =>
      this.foreachDiffableKid(subtreeReg.registerShareFor)
      that.foreachDiffableKid(subtreeReg.shareFor)
  }

  override protected def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.single(a)

  override protected def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case that: Maybe =>
      val a = this.a.computeChangeset(that.a, this.uri, NamedLink("a"), changes).asInstanceOf[DiffableOption[Exp]]
      val newtree = Maybe(a)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val a = this.a.loadUnassigned(changes).asInstanceOf[DiffableOption[Exp]]
    val newtree = Maybe(a)
    changes += Load(newtree.uri, this.tag, Seq(
      "a" -> a.uri
    ), Seq())
    newtree
  }

  override def loadInitial(changes: ChangesetBuffer): Unit = {
    this.a.loadInitial(changes)
    changes += Load(this.uri, this.tag, Seq(
      "a" -> a.uri
    ), Seq())
  }

  override def unloadUnassigned(changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      changes += Unload(this.uri, this.tag, Seq(
        "a" -> a.uri
      ), Seq())
      this.a.unloadUnassigned(changes)
    }
  }
}

object Maybe {
  def apply(a: Option[Exp]): Maybe = Maybe(DiffableOption.from(a.map(a => a), SortType(classOf[Exp])))
}