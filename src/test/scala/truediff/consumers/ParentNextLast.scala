package truediff.consumers

import truediff.changeset._
import truediff.{ListFirstLink, ListNextLink, NodeURI}

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
    changeset.neg.foreach {
      case neg if neg.link == ListFirstLink =>
        val first = neg.node
        val list = neg.parent
        // update parent: remove all list elements
        iterateNext(first){parents -= _}
        // update next: nothing
        // update last: list has no last element anymore
        lasts -= list

      case neg if neg.link == ListNextLink =>
        val next = neg.node
        val pred = neg.parent
        // update parent: remove all successor elements
        iterateNext(next){parents -= _}
        // update next: remove next
        nexts -= pred
        // update last: pred is new last
        val list = parents.get(pred)
        if (list.isDefined)
          lasts += ((list.get, pred))

      case neg =>
        // non-list change: only update parent
        parents -= neg.node
    }
    changeset.pos.foreach {
      case AttachNode(list, ListFirstLink, first) =>
        // update parent: add all list elements
        iterateNext(first){n => parents += ((list, n))}
        // update next: nothing
        // update last: find and add last
        var last = first
        iterateNext(first){last = _}
        lasts += ((list, last))

      case AttachNode(pred, ListNextLink, succ) =>
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

      case AttachNode(parent, _, node) =>
        parents += ((node, parent))
      case LoadNode(node, _, kids, _) => kids.foreach {
        case (_,kid) =>
          parents += ((kid, node))
      }
    }
  }
}
