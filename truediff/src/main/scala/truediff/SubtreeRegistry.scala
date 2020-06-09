package truediff

import org.apache.commons.collections4.trie.PatriciaTrie

class SubtreeRegistry {
  private val subtrees = new PatriciaTrie[SubtreeShare]()

  def shareFor(t: Diffable): SubtreeShare = {
    if (t.skipNode)
      return null

    val share = Option(subtrees.get(t.hashString)) match {
      case Some(existingShare) =>
        existingShare
      case None =>
        val newShare = new SubtreeShare()
        subtrees.put(t.hashString, newShare)
        newShare
    }

    t.share = share
    t.assigned = null
    share
  }

  def registerShareFor(t: Diffable): SubtreeShare = {
    if (t.skipNode)
      return null

    val share = shareFor(t)
    share.registerAvailableTree(t)
    share
  }
}
