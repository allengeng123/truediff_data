package truediff.consumers

import truechange._

import scala.collection.mutable

class Next extends Consumer {
  // maps list element nodes to their successor element nodes
  val nexts: mutable.Map[URI, URI] = mutable.Map()

  override def toString: String = s"Next(${nexts.mkString(",")})"

  def apply(node: URI): Option[URI] = nexts.get(node)


  override def update(editscript: EditScript): Unit = {
    editscript.foreach {
      case Detach(_, _, ListNextLink(_), pred, _) =>
        nexts -= pred
      case Attach(succ, _, ListNextLink(_), pred, _) =>
        nexts += ((pred, succ))
      case _ =>
    }
  }
}
