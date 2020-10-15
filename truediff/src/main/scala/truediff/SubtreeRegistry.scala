package truediff

import org.apache.commons.collections4.trie

class SubtreeRegistry {
  private val subtrees = new trie.PatriciaTrie[SubtreeShare]()

  def assignShare(t: Diffable): SubtreeShare = {
    if (t.skipNode)
      return null

    t.assigned = null

    subtrees.get(t.cryptoString) match {
      case null =>
        val newShare = new SubtreeShare()
        subtrees.put(t.cryptoString, newShare)
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
