package truechange

import scala.collection.mutable

/**
 * The reference semantics of Changeset reproduces the original tree from the Changeset alone.
 */

case class MNode(tag: NodeTag, kids: mutable.Map[String, MNode], lits: Map[String, Any])

class StandardTree {
  // the root of this tree
  var root: MNode = null
  // index of all loaded nodes
  private val index: mutable.Map[NodeURI, MNode] = mutable.Map()

  // applies a changeset to this
  def patch(changeset: Changeset): Unit =
    changeset.foreach(patch)

  // applies a single change to this
  def patch(change: Change): Unit = change match {
    case Load(node, tag, kids, lits) =>
      val subtree = MNode(tag,
        mutable.Map() ++ kids.map{case (n, uri) => (n, index(uri))},
        lits.toMap)
      index += (node -> subtree)
    case Unload(node, _, _, _) => index -= node

    case Detach(_, _, RootLink, _, _) => root = null
    case Attach(_, _, RootLink, node, _) => root = index(node)
    case Detach(parent, _, NamedLink(name), _, _) => index(parent).kids(name) = null
    case Attach(parent, _, NamedLink(name), node, _) => index(parent).kids(name) = index(node)

  }
}

