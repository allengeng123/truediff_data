package truediff

import truediff.trie.ByteArrayPatriciaTrie

class SubtreeRegistry {
  private val subtrees = new ByteArrayPatriciaTrie[SubtreeShare]()

  def assignShare(t: Diffable): SubtreeShare = {
    if (t.skipNode)
      return null

    t.assigned = null

    subtrees.get(t.cryptoHash) match {
      case null =>
        val newShare = new SubtreeShare()
        subtrees.put(t.cryptoHash, newShare)
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
