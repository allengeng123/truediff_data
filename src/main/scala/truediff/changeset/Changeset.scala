package truediff.changeset

import truediff._

class Changeset(val neg: Seq[NegativeChange], val pos: Seq[PositiveChange]) {
  def cmds: Iterable[Change] = neg.view.concat(pos.view)
  def size: Int = neg.size + pos.size

  def welltyped: Option[String] = {
    var roots = Set[NodeURI]()
    var stubs = Map[NodeURI, Set[Link]]()

    def ignore(link: Link): Boolean = link match {
      case ListFirstLink => true
      case ListNextLink => true
      case OptionalLink(_) => true
      case _ => false
    }

    def addStub(node: NodeURI, link: Link): Unit =
      if (!ignore(link))
        stubs = stubs.updatedWith(node) {
          case Some(set) => Some(set + link)
          case None => Some(Set(link))
        }
    def removeStub(node: NodeURI, link: Link): Unit =
      if (!ignore(link))
        stubs = stubs.updatedWith(node) {
          case Some(set) =>
            val newset = set - link
            if (newset.isEmpty) None else Some(newset)
          case None => None
        }

    neg.foreach {
      case DetachNode(parent, link, node) =>
        // kid is not a root yet
        if (roots.contains(node))
          return Some(s"Duplicate detach of node $node")
        // parent.link is not a stub yet
        val parentStubs = stubs.getOrElse(parent, Set())
        if (!ignore(link) && parentStubs.contains(link))
          return Some(s"Detach of $node from $parent with already free $link")
        roots += node
        addStub(parent, link)

      case UnloadNode(parent, link, node, kidLinks) =>
        // parent.link is not a stub yet
        val parentStubs = stubs.getOrElse(parent, Set())
        if (parentStubs.contains(link))
          return Some(s"Unload of $node from $parent with already free $link")
        val nodeStubs = stubs.getOrElse(node, Set())
        for (link <- kidLinks)
          if (!ignore(link) && !nodeStubs.contains(link))
            return Some(s"Unload of $node from $parent with unfree slot $link")
        stubs -= node
        addStub(parent, link)
    }

    pos.foreach {
      case AttachNode(parent, link, node) =>
        // kid is a root
        if (!roots.contains(node))
          return Some(s"Attach of unfree node $node")
        // parent.link is a stub
        val parentStubs = stubs.getOrElse(parent, Set())
        if (!ignore(link) && !parentStubs.contains(link))
          return Some(s"Attach of $node to $parent with unfree slot $link")
        roots -= node
        removeStub(parent, link)

      case LoadNode(node, _, kids, _) =>
        // node is not a root yet
        if (roots.contains(node))
          return Some(s"Duplicate load for $node")
        // all kid nodes are roots
        for ((_,kid) <- kids if kid.isInstanceOf[NodeURI]) {
          if (!roots.contains(kid.asInstanceOf[NodeURI]))
            return Some(s"Load of $node with unfree kid $kid")
          roots -= kid.asInstanceOf[NodeURI]
        }
        roots += node
    }

    // no roots left over
    if (roots.nonEmpty)
      return Some(s"Leaked roots $roots")

    // no empty slots left over
    if (stubs.nonEmpty)
      return Some(s"Dangling slots $stubs")

    None
  }
}
