package truediff
import truechange._

object Ref {
  case class RefSort()
}

final case class Ref[T <: Diffable](target: T) extends Diffable {
  override def sig: Signature = Signature(RefType(target.sig.sort), this.tag, Map(), Map("target" -> JavaLitType(classOf[URI])))

  override def treeheight: Int = 1

  override def treesize: Int = 1

  override lazy val literalHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    digest.update(target.uri.serialized)
    digest.digest()
  }

  override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

  override def toStringWithURI: String = s"Ref(${target.uri})"

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    edits += InsertNode(this.uri, this.tag, Seq(), Seq("target" -> target.uri))
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned.updateLiterals(that, edits)
    }

    val newtree = Ref(target)
    edits += InsertNode(newtree.uri, this.tag, Seq(), Seq("target" -> target.uri))
    newtree
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      //      this.assigned = null
    } else {
      edits += Remove(this.uri, this.tag, Seq(), Seq("target" -> target.uri))
    }
  }

  override def updateLiterals(thatX: Diffable, edits: EditScriptBuffer): Ref[T] = {
    val that = thatX.asInstanceOf[Ref[T]]
    if (this.target.uri != that.target.uri) {
      edits += Update(this.uri, this.tag, Seq("target" -> this.target.uri), Seq("target" -> that.target.uri))
      Ref(that.target).withURI(this.uri)
    } else
      this
  }

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: Ref[T] if this.target.uri == that.target.uri =>
      this
    case _ => null
  }
}
