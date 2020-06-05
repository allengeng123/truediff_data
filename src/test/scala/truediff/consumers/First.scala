package truediff.consumers

import truediff.changeset._
import truediff.{ListFirstLink, NodeURI}

import scala.collection.mutable

class First extends Consumer {
  // maps list nodes to their first element node
  val firsts: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"First(${firsts.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = firsts.get(node)


  override def update(changeset: Changeset): Unit = {
    changeset.neg.foreach {
      case DetachNode(list, ListFirstLink, _) =>
        firsts -= list
      case UnloadNode(list, ListFirstLink, _, _) =>
        firsts -= list
      case _ =>
    }
    changeset.pos.foreach {
      case AttachNode(list, ListFirstLink, node) =>
        firsts += ((list, node))
      case _ =>
    }
  }
}
