package truediff.consumers

import truechange._

import scala.collection.mutable

class ParentNoLists extends Consumer {

  val parents: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"Parent(${parents.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = parents.get(node)


  override def update(changeset: Editscript): Unit = {
    changeset.foreach {
      case Detach(node, _, _, _, _) =>
        parents -= node
      case Unload(_, _, kids, _) =>
        for ((_, kid) <- kids)
          parents -= kid
      case Attach(node, _, _, parent, _) =>
        parents += ((node, parent))
      case Load(node, _, kids, _) =>
        for ((_, kid) <- kids)
          parents += ((kid, node))
    }
  }
}
