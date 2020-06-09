package truechange

case class Changeset(changes: Seq[Change]) {
  def size: Int = changes.size

  def foreach(f: Change => Unit): Unit = changes.foreach(f)

  /**
   * Checks if this changeset is well-typed.
   *
   * A changeset is well-typed if ...
   *
   * Assumption: If the changeset contains `Detach/Unload(parent, link, node, nodeTag)`, then
   *   1. `parent` exists in the base tree and has a link `link` that points to `node`
   *   2. the tag of `node` is `nodeTag`
   */
  def welltyped(sigs: Map[NodeTag, Signature]): Option[String] = {

//    def linkType(link: Link): Type = link match {
//      case RootLink(ty) => ty
//      case NamedLink(tag, name) => sigs(tag).kids(name)
//      case ListFirstLink(ty) => ty
//      case ListNextLink(ty) => ty
//    }

    var roots = Map[NodeURI, Type]()
    var stubs = Map[NodeURI, Set[Link]]()

    def isListLink(link: Link) = link.isInstanceOf[ListNextLink] || link.isInstanceOf[ListFirstLink]
    def addStub(node: NodeURI, link: Link): Unit =
      if (!link.isOptional)
        stubs = stubs.updatedWith(node) {
          case Some(set) => Some(set + link)
          case None => Some(Set(link))
        }
    def removeStub(node: NodeURI, link: Link): Unit =
      if (!link.isOptional)
        stubs = stubs.updatedWith(node) {
          case Some(set) =>
            val newset = set - link
            if (newset.isEmpty) None else Some(newset)
          case None => None
        }

    changes.foreach {
      case Detach(parent, link, node, tag) =>
        // kid is not a root yet
        if (roots.contains(node))
          return Some(s"Duplicate detach of node $node")

        // parent.link is not a stub yet
        val parentStubs = stubs.getOrElse(parent, Set())
        if (!link.isOptional && parentStubs.contains(link))
          return Some(s"Detach of $node from $parent with already free $link")
        val nodeType = sigs.getOrElse(tag, return Some(s"No signature for $tag found")).sort
        roots += node -> nodeType
        addStub(parent, link)

      case Unload(node, tag, kids, lits) =>
        // node is a root
        roots.getOrElse(node, return Some(s"Unload of unfree node $node"))

        // all kids become roots
        val sig = sigs.getOrElse(tag, return Some(s"No signature for $tag found"))
        for ((kidname, kidnode) <- kids) {
          val kidType = sig.kids.getOrElse(kidname, return Some(s"Cannot unload $node, unexpected kid $kidname"))
          roots += kidnode -> kidType
        }

        roots -= node

      case Attach(parent, link, node, tag) =>
        // kid is a root
        val nodeType = roots.getOrElse(node, return Some(s"Attach of unfree node $node"))

        // kid is attachable to parent.link
        val expectedType = sigs.getOrElse(tag, return Some(s"No signature for $tag found")).sort
        if (!isListLink(link) && !expectedType.isAssignableFrom(nodeType))
          return Some(s"Cannot attach $node to $link, incompatible types: Expected $expectedType but got $nodeType.")
        else if (isListLink(link) && !expectedType.isAssignableFrom(nodeType))
          return Some(s"Cannot attach $node to $link, incompatible types: Expected $expectedType but got ${nodeType}.")

        // parent.link is a stub
        val parentStubs = stubs.getOrElse(parent, Set())
        if (!link.isOptional && !parentStubs.contains(link))
          return Some(s"Attach of $node to $parent with unfree slot $link")
        roots -= node
        removeStub(parent, link)

      case Load(node, tag, kids, lits) =>
        val sig = sigs.getOrElse(tag, return Some(s"No signature for $tag found"))

        // node is not a root yet
        if (roots.contains(node))
          return Some(s"Duplicate load for $node")

        // provided kids match signature
        if (sig.kids.size != kids.size)
          return Some(s"Cannot load $tag, expected ${sig.kids.size} kids but got ${kids.size} kids")
        for ((kidname, kidnode) <- kids) {
          val expectedType = sig.kids.getOrElse(kidname, return Some(s"Cannot load $tag, unexpected kid $kidname"))
          val kidType =
            if (kidnode == null) NothingType
            else roots.getOrElse(kidnode, return Some(s"Load of $node with unfree kid $kidnode"))
          if (!expectedType.isAssignableFrom(kidType))
            return Some(s"Cannot load $tag, incompatible type for kid $kidname: Expected $expectedType but got $kidType.")
        }

        // provided lits match signature
        if (sig.lits.size != lits.size)
          return Some(s"Cannot load $tag, expected ${sig.lits.size} lits but got ${lits.size} lits")
        for ((litname, lit) <- lits) {
          val expectedType = sig.lits.getOrElse(litname, return Some(s"Cannot load $tag, unexpected lit $litname"))
          val litType = lit.tag
          if (!expectedType.isAssignableFrom(litType))
            return Some(s"Cannot load $tag, incompatible type for lit $litname: Expected $expectedType but got $litType.")
        }
        roots --= kids.map(_._2)
        roots += node -> sig.sort
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
