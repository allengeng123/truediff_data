package truechange.changeable

import truechange._

class ReferenceTree {
  var nodes: Map[NodeURI, Node] = Map()
  var root: Node = _

  def withNode(p: (String, NodeURI)): (String, Node) = (p._1, nodes(p._2))
  def withLit(p: (String, Literal[_])): (String, Any) = (p._1, p._2.value)

  def change(change: Change): Unit = change match {
    case Load(nodeURI, tag, kids, lits) =>
      val node = Node(tag,
            kids.map(p => (p._1, nodes(p._2))).toMap,
            lits.map(p => (p._1, p._2.value)).toMap)
      nodes += (nodeURI -> node)
    case Unload(nodeURI, _, _, _) =>
      nodes -= nodeURI
    case Detach(parentURI, NamedLink(name), _, _) =>
      val parent = nodes(parentURI)
      parent.kids = parent.kids.updated(name, null)
    case Attach(parentURI, NamedLink(name), node, _) =>
      val parent = nodes(parentURI)
      parent.kids = parent.kids.updated(name, nodes(node))

    case Attach(_, RootLink, node, _) =>
      root = nodes(node)
  }

  def applyChangeset(changeset: Changeset): Unit =
    changeset.foreach(this.change)
}

case class Node(tag: NodeTag, var kids: Map[String, Node], var lits: Map[String, Any])
