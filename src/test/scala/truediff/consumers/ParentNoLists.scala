package truediff.consumers

import truediff.NodeURI
import truediff.changeset._

import scala.collection.mutable

class ParentNoLists extends Consumer {

  val parents: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"Parent(${parents.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = parents.get(node)


  override def update(changeset: Changeset): Unit = {
    changeset.neg.foreach {
      case DetachNode(_, _, node) =>
        parents -= node
      case UnloadNode(_, _, node, _) =>
        parents -= node
    }
    changeset.pos.foreach {
      case AttachNode(parent, _, node) =>
        parents += ((node, parent))
      case LoadNode(node, _, kids, _) => kids.foreach {
        case (_,kid) =>
          parents += ((kid, node))
      }
    }
  }
}
