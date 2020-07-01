package truediff.consumers

import truechange._

import scala.collection.mutable

class Prev extends Consumer {
  // maps list element nodes to their predecessor element nodes
  val prevs: mutable.Map[URI, URI] = mutable.Map()

  override def toString: String = s"Prev(${prevs.mkString(",")})"

  def apply(node: URI): Option[URI] = prevs.get(node)


  override def update(changeset: EditScript): Unit = {
    changeset.foreach {
      case Detach(succ, _, ListNextLink(_), _, _) =>
        prevs -= succ
      case Attach(succ, _, ListNextLink(_), pred, _) =>
        prevs += ((succ, pred))
      case _ =>
    }
  }
}
