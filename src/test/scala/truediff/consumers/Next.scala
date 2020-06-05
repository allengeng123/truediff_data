package truediff.consumers

import truediff.changeset._
import truediff.{ListNextLink, NodeURI}

import scala.collection.mutable

class Next extends Consumer {
  // maps list element nodes to their successor element nodes
  val nexts: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"Next(${nexts.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = nexts.get(node)


  override def update(changeset: Changeset): Unit = {
    changeset.neg.foreach {
      case DetachNode(pred, ListNextLink, _) =>
        nexts -= pred
      case UnloadNode(pred, ListNextLink, _, _) =>
        nexts -= pred
      case _ =>
    }
    changeset.pos.foreach {
      case AttachNode(pred, ListNextLink, succ) =>
        nexts += ((pred, succ))
      case _ =>
    }
  }
}
