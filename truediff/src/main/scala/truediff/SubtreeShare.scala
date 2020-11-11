package truediff

import org.apache.commons.collections4.trie
import org.apache.commons.collections4.trie.PatriciaTrie
import truechange.URI

class SubtreeShare() {
  private var availableTrees: Map[URI, Diffable] = Map()
  private var _preferredTrees: trie.PatriciaTrie[Diffable] = null

  def registerAvailableTree(t: Diffable): Unit = {
    if (!t.skipNode) {
      this.availableTrees += ((t.uri, t))
      if (_preferredTrees != null) {
        _preferredTrees.put(t.literalHashString, t)
      }
    }
  }

  private def preferredTrees: PatriciaTrie[Diffable] = {
    if (_preferredTrees == null) {
      _preferredTrees = new PatriciaTrie[Diffable]()
      availableTrees.values.foreach(t => _preferredTrees.put(t.literalHashString, t))
    }
    _preferredTrees
  }

  def takeAvailableTree(that: Diffable, preferred: Boolean, subtreeReg: SubtreeRegistry): Option[Diffable] = {
    val found: Option[Diffable] =
      if (preferred)
        Option(preferredTrees.get(that.literalHashString))
      else
        availableTrees.values.headOption
    found.map(takeTree(_, that, subtreeReg))
  }

  private def takeTree(tree: Diffable, that: Diffable, subtreeReg: SubtreeRegistry): Diffable = {
    tree.share.availableTrees -= tree.uri
    if (tree.share._preferredTrees != null)
      tree.share._preferredTrees.remove(tree.literalHashString)
    tree.share = null  // reset to prevent memory leaks
    tree._directSubtrees.foreach(deregisterAvailableTree(_, subtreeReg))

    that.foreachSubtree { thatnode =>
      if (thatnode.assigned != null) {
        val thisnode = thatnode.assigned
        subtreeReg.assignShareAndRegisterTree(thisnode)
      }
    }
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