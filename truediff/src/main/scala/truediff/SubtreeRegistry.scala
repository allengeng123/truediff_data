package truediff

import org.apache.commons.collections4.trie.PatriciaTrie

class SubtreeRegistry {
  val subtrees = new PatriciaTrie[SubtreeShare]()

  def shareFor(t: Diffable): SubtreeShare = {
    if (t.share != null)
      return t.share

    val existingShare = subtrees.get(t.hashString)
    val share = if (existingShare != null)
      existingShare.asInstanceOf[SubtreeShare]
    else {
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
