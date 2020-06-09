package truediff.consumers

import truechange._

import scala.collection.mutable

class ParentNoLists extends Consumer {

  val parents: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"Parent(${parents.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = parents.get(node)


  override def update(changeset: Changeset): Unit = {
    changeset.neg.foreach {
      case DetachNode(_, _, node, _) =>
        parents -= node
      case UnloadNode(_, _, kids, _) =>
        for ((_, kid) <- kids)
          parents -= kid
    }
    changeset.pos.foreach {
      case AttachNode(parent, _, node) =>
        parents += ((node, parent))
      case LoadNode(node, _, kids, _) =>
        for ((_, kid) <- kids)
          parents += ((kid, node))
    }
  }
}
