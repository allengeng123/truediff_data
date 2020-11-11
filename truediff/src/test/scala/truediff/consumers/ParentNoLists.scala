package truediff.consumers

import truechange._

import scala.collection.mutable

class ParentNoLists extends Consumer {

  val parents: mutable.Map[URI, URI] = mutable.Map()

  override def toString: String = s"Parent(${parents.mkString(",")})"

  def apply(node: URI): Option[URI] = parents.get(node)


  override def update(editscript: EditScript): Unit = {
    editscript.foreachCoreEdit {
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
      case _: Update =>
        // nothing
    }
  }
}
