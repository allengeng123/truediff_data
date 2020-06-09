package truediff.consumers

import truechange._

import scala.collection.mutable

class ParentNextLast extends Consumer {

  // maps nodes to their parent node
  val parents: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  // maps list nodes to their first element node
  val nexts: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  // maps list nodes to their last element node
  val lasts: mutable.Map[NodeURI, NodeURI] = mutable.Map()

  override def toString: String = s"Parent(${parents.mkString(",")})"

  def apply(node: NodeURI): Option[NodeURI] = parents.get(node)

  private def iterateNext(from: NodeURI)(f: NodeURI => Unit): Unit = {
    f(from)
    var nextNode = nexts.get(from)
    while (nextNode.isDefined) {
      val node = nextNode.get
      f(node)
      nextNode = nexts.get(node)
    }
  }

  override def update(changeset: Changeset): Unit = {
    changeset.foreach {
      case Detach(list, ListFirstLink(_), first, _) =>
        // update parent: remove all list elements
        iterateNext(first){parents -= _}
        // update next: nothing
        // update last: list has no last element anymore
        lasts -= list

      case Detach(pred, ListNextLink(_), next, _) =>
        // update parent: remove all successor elements
        iterateNext(next){parents -= _}
        // update next: remove next
        nexts -= pred
        // update last: pred is new last
        val list = parents.get(pred)
        if (list.isDefined)
          lasts += ((list.get, pred))

      case Detach(_, _, node, _) =>
        // non-list change: only update parent
        parents -= node
      case Unload(_, _, kids, _) =>
        for ((_, kid) <- kids)
          parents -= kid

      case Attach(list, ListFirstLink(_), first) =>
        // update parent: add all list elements
        iterateNext(first){n => parents += ((list, n))}
        // update next: nothing
        // update last: find and add last
        var last = first
        iterateNext(first){last = _}
        lasts += ((list, last))

      case Attach(pred, ListNextLink(_), succ) =>
        // update parent: add all successor elements
        val list = parents.get(pred)
        if (list.isDefined)
          iterateNext(succ){n => parents += ((list.get, n))}
        // update next: add next
        nexts += ((pred, succ))
        // update last: find and replace last
        if (list.isDefined) {
          var last = succ
          iterateNext(succ){last = _}
          lasts += ((list.get, last))
        }

      case Attach(parent, _, node) =>
        // non-list change
        parents += ((node, parent))
      case Load(node, _, kids, _) =>
        for ((_, kid) <- kids)
          parents += ((kid, node))
    }
  }
}
