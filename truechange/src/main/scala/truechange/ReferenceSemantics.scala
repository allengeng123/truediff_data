package truechange

import scala.collection.mutable

/**
 * The reference semantics of Changeset reproduces the original tree from the Changeset alone.
 */

case class ReferenceNode(tag: NodeTag, kids: mutable.Map[String, ReferenceNode], lits: mutable.Map[String, Any])

class ReferenceTree {
  // the root of this tree
  var root: ReferenceNode = null
  // index of all loaded nodes
  private val nodes: mutable.Map[NodeURI, ReferenceNode] = mutable.Map()

  // applies a changeset to this
  def applyChangeset(changeset: Changeset): Unit =
    changeset.foreach(this.change)

  // applies a single change to this
  def change(change: Change): Unit = change match {
    case Load(nodeURI, tag, kids, lits) =>
      val node = ReferenceNode(tag,
        mutable.Map() ++ (for ((n,uri) <- kids) yield (n, nodes(uri))),
        mutable.Map() ++ (for ((n,lit) <- lits) yield (n, lit.value)))
      nodes += (nodeURI -> node)
    case Unload(node, _, _, _) => nodes -= node
    case Detach(parent, NamedLink(name), _, _) => nodes(parent).kids(name) = null
    case Attach(parent, NamedLink(name), node, _) => nodes(parent).kids(name) = nodes(node)

    case Detach(_, RootLink, _, _) => root = null
    case Attach(_, RootLink, node, _) => root = nodes(node)
  }
}

