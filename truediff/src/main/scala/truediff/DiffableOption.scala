package truediff

import truechange._

sealed trait DiffableOption[+A <: Diffable] extends Diffable {
  final override private[truediff] def skipNode = true
}
object DiffableOption {
  def from[A <: Diffable](a: Option[A], atype: Type): DiffableOption[A] = a match {
    case Some(value) => DiffableSome(value, atype)
    case None => DiffableNone
  }

}

case object DiffableNone extends DiffableOption[Nothing] {
  override val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.digest()
  }

  override def uri: URI = null

  override val treeheight: Int = 0

  override def treesize: Int = 0

  override val toStringWithURI: String = "None"

  final override val tag: Tag = OptionTag(NothingType)
  override def sig: Signature = Signature(OptionType(NothingType), this.tag, Map(), Map())

  override def foreachSubtree(f: Diffable => Unit): Unit = {
    // nothing
  }

  override protected[truediff] def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case DiffableNone =>
    case _ =>
      that.foreachSubtree(subtreeReg.assignShare)
  }

  override protected[truediff] def directSubtrees: Iterable[Diffable] = Iterable.empty

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case DiffableNone => this
    case that: DiffableSome[_] =>
      val newtree = that.loadUnassigned(edits)
      edits += Attach(newtree.uri, newtree.tag, OptionalLink(link), parent, parentTag)
      newtree
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    // nothing to load for None
    this
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    // nothing to load for None
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    // nothing to unload for None
  }
}

final case class DiffableSome[+A <: Diffable](a: A, atype: Type) extends DiffableOption[A] {
  override val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(a.hash)
    digest.digest()
  }

  override def uri: URI = a.uri

  override def treeheight: Int = a.treeheight

  override def treesize: Int = a.treesize

  override def toStringWithURI: String = s"Some(${a.toStringWithURI})"

  override val tag: Tag = OptionTag(atype)
  override def sig: Signature = Signature(OptionType(atype), this.tag, Map(), Map())

  override def foreachSubtree(f: Diffable => Unit): Unit = {
    f(this.a)
    this.a.foreachSubtree(f)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: DiffableSome[_] =>
      this.a.assignShares(that.a, subtreeReg)
    case _ =>
      this.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
  }

  override protected def directSubtrees: Iterable[Diffable] = Iterable.single(a)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: DiffableSome[_] =>
      val a = this.a.computeEditScript(that.a, parent, parentTag, OptionalLink(link), edits)
      DiffableSome(a.asInstanceOf[A], atype)
    case DiffableNone =>
      edits += Detach(this.a.uri, this.a.tag, OptionalLink(link), parent, parentTag)
      this.a.unloadUnassigned(edits)
      that
  }

  override def loadUnassigned(edits: EditScriptBuffer): DiffableSome[A] = {
    val a = this.a.loadUnassigned(edits).asInstanceOf[A]
    DiffableSome(a, atype)
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.a.loadInitial(edits)
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    a.unloadUnassigned(edits)
  }
}