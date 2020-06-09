package truediff.manual

import truechange._
import truediff.{SubtreeRegistry, _}

case class Many(es: DiffableList[Exp]) extends Exp {

  lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(es.hash)
    digest.digest()
  }

  override val treeheight: Int = 1 + es.treeheight

  override def treesize: Int = 1 + es.treesize

  override def toStringWithURI: String = s"Many_$uri(${es.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map("es" -> ListType(SortType(classOf[Exp]))), Map())

  override def foreachDiffableKid(f: Diffable => Unit): Unit = {
    f(this.es)
    this.es.foreachDiffableKid(f)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Many =>
      this.es.assignShares(that.es, subtreeReg)
    case _ =>
      this.foreachDiffableKid(subtreeReg.registerShareFor)
      that.foreachDiffableKid(subtreeReg.shareFor)
  }

  override protected def assignSubtreesRecurse(): Iterable[Diffable] = Iterable.single(es)

  override protected def computeChangesetRecurse(that: Diffable, parent: NodeURI, link: Link, changes: ChangesetBuffer): Diffable = that match {
    case that: Many =>
      val es = this.es.computeChangeset(that.es, this.uri, NamedLink("es"), changes).asInstanceOf[DiffableList[Exp]]
      val newtree = Many(es)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(changes: ChangesetBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val es = this.es.loadUnassigned(changes).asInstanceOf[DiffableList[Exp]]
    val newtree = Many(es)
    changes += Load(newtree.uri, this.tag, Seq(
      "es" -> es.uri
    ), Seq())
    newtree
  }


  override def loadInitial(changes: ChangesetBuffer): Unit = {
    this.es.loadInitial(changes)
    changes += Load(this.uri, this.tag, Seq(
      "es" -> es.uri
    ), Seq())
  }

  override def unloadUnassigned(changes: ChangesetBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      changes += Unload(this.uri, this.tag, Seq(
        "es" -> es.uri
      ), Seq())
      this.es.unloadUnassigned(changes)
    }
  }
}

object Many {
  def apply(es: Seq[Exp]): Many = Many(DiffableList.from(es, SortType(classOf[Exp])))
}