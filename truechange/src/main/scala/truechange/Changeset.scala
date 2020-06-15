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

    var roots = Map[NodeURI, Type]()
    var stubs = Map[(NodeURI, Link), Type]()

    changes.foreach {
      case Detach(parent, ptag, NamedLink(name), node, tag) =>
        // kid is not a root yet
        if (roots.contains(node))
          return Some(s"Duplicate detach of node $node")

        // parent.link is not a stub yet
        if (stubs.contains((parent, NamedLink(name))))
          return Some(s"Detach of $node from $parent, but $ptag.$name is already a stub")

        val nodeType = sigs.getOrElse(tag, return Some(s"No signature for $tag found")).sort
        roots += node -> nodeType
        val stubType = sigs.getOrElse(ptag, return Some(s"No signature for $ptag found")).kids(name)
        stubs += (parent, NamedLink(name)) -> stubType

      case Detach(parent, ptag, OptionalLink(link), node, tag) =>
        // kid is not a root yet
        if (roots.contains(node))
          return Some(s"Duplicate detach of node $node")

        val nodeType = sigs.getOrElse(tag, return Some(s"No signature for $tag found")).sort
        roots += node -> nodeType

      case Detach(parent, ptag, ListFirstLink(ty), node, tag) =>
        // kid is not a root yet
        if (roots.contains(node))
          return Some(s"Duplicate detach of node $node")

        val nodeType = sigs.getOrElse(tag, return Some(s"No signature for $tag found")).sort
        roots += node -> nodeType

      case Detach(parent, ptag, ListNextLink(ty), node, tag) =>
        // kid is not a root yet
        if (roots.contains(node))
          return Some(s"Duplicate detach of node $node")

        val nodeType = sigs.getOrElse(tag, return Some(s"No signature for $tag found")).sort
        roots += node -> nodeType

      case Unload(node, tag, kids, lits) =>
        // node is a root
        roots.getOrElse(node, return Some(s"Unload of unfree node $node"))

        // all kids become roots
        val sig = sigs.getOrElse(tag, return Some(s"No signature for $tag found"))
        for ((kidname, kidnode) <- kids) {
          if (roots.contains(kidnode))
            return Some(s"Cannot unload $node because $kidnode is already free")
          val kidType = sig.kids.getOrElse(kidname, return Some(s"Cannot unload $node, unexpected kid $kidname"))
          roots += kidnode -> kidType
        }

        roots -= node

      case Attach(parent, ptag, NamedLink(name), node, tag) =>
        // kid is a root
        val nodeType = roots.getOrElse(node, return Some(s"Attach of unfree node $node"))

        // kid is attachable to parent.link
        val expectedType = sigs.getOrElse(ptag, return Some(s"No signature for $ptag found")).kids(name)
        if (!expectedType.isAssignableFrom(nodeType))
          return Some(s"Cannot attach $node to $ptag.$name, incompatible types: Expected $expectedType but got $nodeType.")

        // parent.link is a stub
        if (!stubs.contains((parent, NamedLink(name))))
          return Some(s"Attach of $node to $parent with unfree slot $ptag.$name")
        roots -= node
        stubs -= ((parent, NamedLink(name)))

      case Attach(parent, ptag, OptionalLink(NamedLink(name)), node, tag) =>
        // kid is a root
        val nodeType = roots.getOrElse(node, return Some(s"Attach of unfree node $node"))

        // kid is attachable to parent.link
        val expectedType = sigs.getOrElse(ptag, return Some(s"No signature for $ptag found")).kids(name)
        if (!expectedType.isAssignableFrom(nodeType))
          return Some(s"Cannot attach $node to $ptag.$name, incompatible types: Expected $expectedType but got $nodeType.")

        roots -= node

      case Attach(parent, ptag, link@ListFirstLink(ty), node, tag) =>
        // kid is a root
        val nodeType = roots.getOrElse(node, return Some(s"Attach of unfree node $node"))

        // kid is attachable to parent.link
        val expectedType = ty
        if (!expectedType.isAssignableFrom(nodeType))
          return Some(s"Cannot attach $node to $parent.$link, incompatible types: Expected $expectedType but got $nodeType.")

        roots -= node

      case Attach(parent, ptag, link@ListNextLink(ty), node, tag) =>
        // kid is a root
        val nodeType = roots.getOrElse(node, return Some(s"Attach of unfree node $node"))

        // kid is attachable to parent.link
        val expectedType = ty
        if (!expectedType.isAssignableFrom(nodeType))
          return Some(s"Cannot attach $node to $parent.$link, incompatible types: Expected $expectedType but got $nodeType.")

        roots -= node

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
          val expectedLitType = sig.lits.getOrElse(litname, return Some(s"Cannot load $tag, unexpected lit $litname"))
          if (!expectedLitType.accepts(lit))
            return Some(s"Cannot load $tag, incompatible literal for $litname: Expected $expectedLitType but got $lit.")
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
