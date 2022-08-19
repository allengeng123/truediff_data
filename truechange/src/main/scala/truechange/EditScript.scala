package truechange

import truechange.Type.Subsorts

import scala.language.implicitConversions

case class CoreEditScript(edits: Seq[CoreEdit]) {
  def size: Int = edits.size

  def foreach(f: CoreEdit => Unit): Unit = edits.foreach(f)

  /**
   * Checks if this editscript is well-typed.
   *
   * A editscript is well-typed if ...
   *
   * Assumption: If the editscript contains `Detach/Unload(parent, link, node, nodeTag)`, then
   *   1. `parent` exists in the base tree and has a link `link` that points to `node`
   *   2. the tag of `node` is `nodeTag`
   */
  def welltyped(sigs: Map[Tag, Signature], initRoots: Map[URI, Type] = Map(), initStubs: Map[(URI, Link), Type] = Map()): Option[String] = {

    var roots = initRoots
    var stubs = initStubs

    implicit val subsorts: Subsorts = Map()

    edits.foreach {
      case Detach(node, tag, link, parent, ptag) =>
        // kid is not a root yet
        if (roots.contains(node))
          return Some(s"Duplicate detach of node $node")

        if (trackStub(link, ptag, sigs)) {
          // parent.link is not a stub yet
          if (stubs.contains((parent, link)))
            return Some(s"Detach of $node from $parent, but $ptag.$link is already a stub")
          targetType(link, ptag, sigs) match {
            case Left(stubType) => stubs += (parent, link) -> stubType
            case Right(err) => return Some(err)
          }
        }

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


      case Attach(node, tag, link, parent, ptag) =>
        // kid is a root
        val nodeType = roots.getOrElse(node, return Some(s"Attach of unfree node $node"))

        // kid is attachable to parent.link
        targetType(link, ptag, sigs) match {
          case Left(expectedType) =>
            if (!expectedType.isAssignableFrom(nodeType))
              return Some(s"Cannot attach $node to $ptag.$link, incompatible types: Expected $expectedType but got $nodeType.")
          case Right(err) => return Some(err)
        }

        if (trackStub(link, ptag, sigs)) {
          // parent.link is a stub
          if (!stubs.contains((parent, link)))
            return Some(s"Attach of $node to $parent with unfree slot $ptag.$link")
          stubs -= ((parent, link))
        }

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


      case Update(node, tag, oldlits, newlits) =>
        val sig = sigs.getOrElse(tag, return Some(s"No signature for $tag found"))
        // provided lits match signature
        if (sig.lits.size != newlits.size)
          return Some(s"Cannot update $tag, expected ${sig.lits.size} lits but got ${newlits.size} lits")
        for ((litname, lit) <- newlits) {
          val expectedLitType = sig.lits.getOrElse(litname, return Some(s"Cannot load $tag, unexpected lit $litname"))
          if (!expectedLitType.accepts(lit))
            return Some(s"Cannot update $tag, incompatible literal for $litname: Expected $expectedLitType but got $lit.")
        }
    }

    // no roots left over
    if (roots.nonEmpty)
      return Some(s"Leaked roots $roots")

    // no empty slots left over
    if (stubs.nonEmpty)
      return Some(s"Dangling slots $stubs")

    None
  }

  def trackStub(link: Link, ptag: Tag, sigs: Map[Tag, Signature]): Boolean = link match {
    case NamedLink(name) =>
      val expectedType = sigs.getOrElse(ptag, return true).kids(name)
      expectedType match {
        case _: OptionType => false
        case _ => true
      }
    case ListFirstLink(_) => false
    case ListNextLink(_) => false
  }

  def targetType(link: Link, ptag: Tag, sigs: Map[Tag, Signature]): Either[Type, String] = link match {
    case NamedLink(name) =>
      sigs.get(ptag) match {
        case Some(sig) => Left(sig.kids(name))
        case None => Right(s"No signature for $ptag found")
      }
    case ListFirstLink(ty) => Left(ty)
    case ListNextLink(ty) => Left(ty)
  }
}

case class EditScript(edits: Seq[Edit]) {
  lazy val coreEdits: CoreEditScript = {
    val buf = new CoreEditScriptBuffer
    edits.foreach(_.asCoreEdits(buf))
    buf.toEditScript
  }

  def size: Int = edits.size
  def coresize: Int = coreEdits.size

  def foreach(f: Edit => Unit): Unit = edits.foreach(f)
  def foreachCoreEdit(f: CoreEdit => Unit): Unit = coreEdits.foreach(f)

  /**
   * Checks if this editscript is well-typed.
   *
   * A editscript is well-typed if ...
   *
   * Assumption: If the editscript contains `Detach/Unload(parent, link, node, nodeTag)`, then
   *   1. `parent` exists in the base tree and has a link `link` that points to `node`
   *   2. the tag of `node` is `nodeTag`
   */
  def welltyped(sigs: Map[Tag, Signature], initRoots: Map[URI, Type] = Map(), initStubs: Map[(URI, Link), Type] = Map()): Option[String] =
    coreEdits.welltyped(sigs, initRoots, initStubs)

  def print(): Unit = edits.foreach(e => println("  " + e))
}
