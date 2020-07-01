package truediff

import org.apache.commons.collections4.trie.PatriciaTrie

class SubtreeRegistry {
  private val subtrees = new PatriciaTrie[SubtreeShare]()

  def assignShare(t: Diffable): SubtreeShare = {
    if (t.skipNode)
      return null

    t.assigned = null

    subtrees.get(t.hashString) match {
      case null =>
        val newShare = new SubtreeShare()
        subtrees.put(t.hashString, newShare)
        t.share = newShare
        newShare
      case existingShare =>
        t.share = existingShare
        existingShare
    }
  }

  def assignShareAndRegisterTree(t: Diffable): SubtreeShare = {
    if (t.skipNode)
      return null

    val share = assignShare(t)
    share.registerAvailableTree(t)
    share
  }
}
