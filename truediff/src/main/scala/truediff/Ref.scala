package truediff
import truechange._

import scala.collection.MapView

final case class Ref[T <: Diffable](var target: T) extends Diffable {

  override def equals(obj: Any): Boolean = obj match {
    case that: Ref[_] => this.target eq that.target
    case _ => false
  }

  private[truediff] def equalReferences(that: Diffable, refs: MapView[URI, URI]): Boolean = that match {
    case that: Ref[_] => refs.get(this.target.uri) match {
      case Some(thatUri) => thatUri == that.target.uri
      case None => false
    }
    case _ => false
  }

  override def hashCode(): Int = 7 * System.identityHashCode(target)

  def setTarget(trg: Diffable): Unit =
    this.target = trg.asInstanceOf[T]

  override def sig: Signature = Signature(RefType(target.sig.sort), this.tag, Map(), Map("target" -> JavaLitType(classOf[URI])))

  override protected def literals: Iterable[Any] = Iterable(target)

  override def treeheight: Int = 0

  override def treesize: Int = 1

  override lazy val literalHash: Array[Byte] = Hashable.hash(System.identityHashCode(target))

  override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

  override def toString: String = s"Ref(${target.getClass.getSimpleName}_${target.uri})"

  override def toStringWithURI: String = s"Ref_${this.uri}(${target.getClass.getSimpleName}_${target.uri})"

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
    val thatTargetAssigned = that.target.assigned.asInstanceOf[T]

    if (thatTargetAssigned == null)
      throw new UnsupportedOperationException()

    if (this.target.uri != thatTargetAssigned.uri) {
      edits += Update(this.uri, this.tag, Seq("target" -> this.target.uri), Seq("target" -> thatTargetAssigned.uri))
    }
    if (thatTargetAssigned eq this.target) {
      // this is a cyclic reference
      this.target.cyclicReferences += this
    }
    Ref(thatTargetAssigned).withURI(this.uri)
  }

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: Ref[T] if this.target.uri == that.target.uri =>
      this
    case _ => null
  }
}
