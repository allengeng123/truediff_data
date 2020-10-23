package truediff

import org.apache.commons.collections4.trie
import org.apache.commons.collections4.trie.PatriciaTrie
import truechange.URI

class SubtreeShare() {
  private var availableTrees: Map[URI, Diffable] = Map()
  private var identityTrees: trie.PatriciaTrie[Diffable] = null

  def registerAvailableTree(t: Diffable): Unit = {
    if (!t.skipNode)
      this.availableTrees += ((t.uri, t))
  }

  def takeExactAvailableTree(that: Diffable, subtreeReg: SubtreeRegistry): Option[Diffable] = {
    if (identityTrees == null) {
      identityTrees = new PatriciaTrie[Diffable]()
      availableTrees.values.foreach(t => identityTrees.put(t.identityHashString, t))
    }

    identityTrees.get(that.identityHashString) match {
      case null => None
      case tree =>
        takeTree(tree, that, subtreeReg)
        Some(tree)
    }
  }

  def takeApproxAvailableTree(that: Diffable, subtreeReg: SubtreeRegistry): Option[Diffable] = {
    val foundTree = availableTrees.headOption

    foundTree.map { case (_, tree) =>
      takeTree(tree, that, subtreeReg)
      tree
    }
  }

  private def takeTree(tree: Diffable, that: Diffable, subtreeReg: SubtreeRegistry): Unit = {
    tree.share.availableTrees -= tree.uri
    tree.share.identityTrees.remove(tree.identityHashString)
    tree.share = null  // reset to prevent memory leaks
    tree._directSubtrees.foreach(deregisterAvailableTree(_, subtreeReg))

    that.foreachSubtree { thatnode =>
      if (thatnode.assigned != null) {
        val thisnode = thatnode.assigned
        subtreeReg.assignShareAndRegisterTree(thisnode)
      }
    }
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