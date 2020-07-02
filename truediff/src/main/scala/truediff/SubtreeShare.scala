package truediff

import truechange.URI

class SubtreeShare() {
  private var availableTrees: Map[URI, Diffable] = Map()

  def registerAvailableTree(t: Diffable): Unit = {
    if (!t.skipNode)
      this.availableTrees += ((t.uri, t))
  }

  def takeAvailableTree(subtreeReg: SubtreeRegistry): Option[Diffable] =
    availableTrees.headOption.map { case (uri, tree) =>
      deregisterAvailableTree(tree, subtreeReg)
      tree
    }

  def deregisterAvailableTree(t: Diffable, subtreeReg: SubtreeRegistry): Unit =
    if (t.share != null) {
      t.share.availableTrees -= t.uri
      t.share = null  // reset to prevent memory leaks
      t._directSubtrees.foreach(deregisterAvailableTree(_, subtreeReg))
    } else if (t.assigned != null) {
      // t was pre-emptively assigned, but it is part of a larger subtree that is being moved
      val that = t.assigned
      t.assigned = null // undo pre-emptive assignment
      that.assigned = null // undo pre-emptive assignment
      that.foreachTree(subtreeReg.assignShare) // mark that and its subtrees as required
    }

  override def toString: String = s"SubtreeShare(${availableTrees.headOption.map(_._2).getOrElse("???")})"
}