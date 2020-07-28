package truechange

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * The standard semantics of EditScript reproduces the original tree from the EditScript alone.
 */

case class MNode(uri: URI, tag: Tag, kids: mutable.Map[String, MNode], lits: Map[String, Any]) {
  def conformsTo(sigs: Map[Tag, Signature], expectedSort: Type): Unit = {
    val sig = sigs.getOrElse(tag, throw new Exception(s"Cannot find signature of $tag"))
    if (!expectedSort.isAssignableFrom(sig.sort)(Map())) throw new Exception(s"Wrong sort, expected $expectedSort for ${tag}_$uri ~ $sig")

    if (sig.kids.size != kids.size) throw new Exception(s"Wrong number of kids in ${tag}_$uri ~ $sig")
    for ((name, kidtype) <- sig.kids) {
      val kid = kids.getOrElse(name, throw new Exception(s"Missing kid $name in ${tag}_$uri ~ $sig"))
      kid.conformsTo(sigs, kidtype)
    }

    if (sig.lits.size != lits.size) throw new Exception(s"Wrong number of lits in ${tag}_$uri ~ $sig")
    for ((name, littype) <- sig.lits) {
      val lit = lits.getOrElse(name, throw new Exception(s"Missing lit $name in ${tag}_$uri ~ $sig"))
      if (!littype.accepts(lit)) throw new Exception(s"Wrong lit, expected $littype but got $lit in ${tag}_$uri ~ $sig")
    }
  }
}

class MTree {
  // the root of this tree
  val root: MNode = MNode(null, RootTag, mutable.Map((RootLink.name, null)), Map())
  // index of all loaded nodes
  private val index: mutable.Map[URI, MNode] = mutable.Map((null, root))

  // applies an editscript to this
  def patch(edits: EditScript): MTree = {
    edits.foreach(processEdit)
    this
  }

  // applies a single change to this
  def processEdit(edit: Edit): Unit = edit match {
    case Detach(_, _, NamedLink(name), parent, _) => index(parent).kids(name) = null
    case Attach(node, _, NamedLink(name), parent, _) => index(parent).kids(name) = index(node)
    case Unload(node, _, _, _) => index -= node
    case Load(node, tag, kids, lits) =>
      val subtree = MNode(node, tag,
        mutable.Map() ++ kids.map{case (n, uri) => (n, index(uri))},
        lits.toMap)
      index += (node -> subtree)
  }

  def conformsTo(sigs: Map[Tag, Signature]): Option[String] = root.kids.get(RootLink.name) flatMap { t =>
    Try(t.conformsTo(sigs, sigs(root.tag).kids(RootLink.name))) match {
      case Failure(exception) => Some(exception.getMessage)
      case Success(value) => None
    }
  }
}

