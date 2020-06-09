package truediff.consumers

import truechange._

import scala.collection.mutable

class Prev extends Consumer {
  // maps list element nodes to their predecessor element nodes
  val prevs: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"Prev(${prevs.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = prevs.get(node)


  override def update(changeset: Changeset): Unit = {
    changeset.foreach {
      case Detach(_, ListNextLink(_), succ, _) =>
        prevs -= succ
      case Attach(pred, ListNextLink(_), succ, _) =>
        prevs += ((succ, pred))
      case _ =>
    }
  }
}
