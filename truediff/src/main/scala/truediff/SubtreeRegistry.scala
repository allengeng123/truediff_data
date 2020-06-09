package truediff

import org.apache.commons.collections4.trie.PatriciaTrie

class SubtreeRegistry {
  private val subtrees = new PatriciaTrie[SubtreeShare]()

  def shareFor(t: Diffable): SubtreeShare = {
    if (t.share != null)
      return t.share

    val share = Option(subtrees.get(t.hashString)) match {
      case Some(existingShare) =>
        existingShare
      case None =>
        val newShare = new SubtreeShare()
        subtrees.put(t.hashString, newShare)
        newShare
    }

    t.share = share
    share
  }

  def registerShareFor(t: Diffable): Unit = {
    shareFor(t).registerAvailableTree(t)
  }
}
