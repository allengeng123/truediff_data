package truediff.consumers

import truechange._

import scala.collection.mutable

class First extends Consumer {
  // maps list nodes to their first element node
  val firsts: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"First(${firsts.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = firsts.get(node)


  override def update(changeset: Changeset): Unit = {
    changeset.foreach {
      case Detach(list, ListFirstLink(_), _, _) =>
        firsts -= list
      case Attach(list, ListFirstLink(_), node, _) =>
        firsts += ((list, node))
      case _ =>
    }
  }
}
