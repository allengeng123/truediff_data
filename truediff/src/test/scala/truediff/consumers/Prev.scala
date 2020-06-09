package truediff.consumers

import truechange._

import scala.collection.mutable

class Prev extends Consumer {
  // maps list element nodes to their predecessor element nodes
  val prevs: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"Prev(${prevs.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = prevs.get(node)


  override def update(changeset: Changeset): Unit = {
    changeset.neg.foreach {
      case Detach(_, ListNextLink(_), succ, _) =>
        prevs -= succ
      case _ =>
    }
    changeset.pos.foreach {
      case Attach(pred, ListNextLink(_), succ) =>
        prevs += ((succ, pred))
      case _ =>
    }
  }
}
