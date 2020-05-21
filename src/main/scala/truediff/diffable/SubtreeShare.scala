package truediff.diffable

import truediff.NodeURI

class SubtreeShare() {
  var availableTrees: Map[NodeURI, Diffable] = Map()

  def registerAvailableTree(t: Diffable): Unit = {
    this.availableTrees += ((t.uri, t))
  }

  def deregisterAvailableTree(t: Diffable): Unit = {
    this.availableTrees -= t.uri
    t.share = null
  }

  def takeAvailableTree(): Option[Diffable] = {
    val treeAvailable = availableTrees.nonEmpty
    if (treeAvailable) {
      // we need to yield the available trees for re-attachment
      val (uri, tree) = availableTrees.head // alternative selection strategy possible here
      tree.foreachDiffable(t => t.share.deregisterAvailableTree(t))
      Some(tree)
    } else {
      None
    }
  }

  override def toString: String = s"SubtreeShare(${availableTrees.headOption.map(_._2).getOrElse("???")})"
}