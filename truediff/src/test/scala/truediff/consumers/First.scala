package truediff.consumers

import truechange._

import scala.collection.mutable

class First extends Consumer {
  // maps list nodes to their first element node
  val firsts: mutable.Map[URI, URI] = mutable.Map()

  override def toString: String = s"First(${firsts.mkString(",")})"

  def apply(node: URI): Option[URI] = firsts.get(node)


  override def update(changeset: EditScript): Unit = {
    changeset.foreach {
      case Detach(_, _, ListFirstLink(_), list, _) =>
        firsts -= list
      case Attach(node, _, ListFirstLink(_), list, _) =>
        firsts += ((list, node))
      case _ =>
    }
  }
}
