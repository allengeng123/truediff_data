package truediff.consumers

import truechange._

import scala.collection.mutable

class First extends Consumer {
  // maps list nodes to their first element node
  val firsts: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"First(${firsts.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = firsts.get(node)


  override def update(changeset: Changeset): Unit = {
    changeset.neg.foreach {
      case DetachNode(list, ListFirstLink(_), _, _) =>
        firsts -= list
      case _ =>
    }
    changeset.pos.foreach {
      case AttachNode(list, ListFirstLink(_), node) =>
        firsts += ((list, node))
      case _ =>
    }
  }
}
