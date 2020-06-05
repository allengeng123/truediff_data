package truediff.consumers

import truediff.changeset._
import truediff.{ListFirstLink, NodeURI}

import scala.collection.mutable

class Last extends Consumer {
  // maps list nodes to their last element node
  val lasts: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"Last(${lasts.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = lasts.get(node)


  override def update(changeset: Changeset): Unit = {
    changeset.neg.foreach {
      case DetachNode(list, ListFirstLink, _) =>
        lasts -= list
      case UnloadNode(list, ListFirstLink, _, _) =>
        lasts -= list
      case _ =>
    }
    changeset.pos.foreach {
      case AttachNode(list, ListFirstLink, node) =>
        lasts += ((list, node))
      case _ =>
    }
  }
}
