package truediff

import truechange.NodeURI

class SubtreeShare() {
  private var availableTrees: Map[NodeURI, Diffable] = Map()

  def registerAvailableTree(t: Diffable): Unit = {
    if (t.treeheight > 0)
      this.availableTrees += ((t.uri, t))
  }

  def takeAvailableTree(subtreeReg: SubtreeRegistry): Option[Diffable] = {
    val treeAvailable = availableTrees.nonEmpty
    if (treeAvailable) {
      // we need to yield the available tree for re-attachment
      val (uri, tree) = availableTrees.head // alternative selection strategy possible here
      availableTrees = availableTrees.tail
      tree.share = null
      tree.foreachDiffableKid { t =>
        if (t.assigned != null) {
          // t and t.assigned were unchanged, but t is part of a larger subtree that is being moved
          val that = t.assigned
          t.assigned = null
          that.assigned = null
          that.foreachDiffable(subtreeReg.shareFor)
        } else if (t.share != null) {
          t.share.availableTrees -= t.uri
          t.share = null
        }
      }
      Some(tree)
    } else {
      None
    }
  }

  override def toString: String = s"SubtreeShare(${availableTrees.headOption.map(_._2).getOrElse("???")})"
}