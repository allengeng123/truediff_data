package truechange

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/**
 * The standard semantics of Changeset reproduces the original tree from the Changeset alone.
 */

case class MNode(uri: NodeURI, tag: NodeTag, kids: mutable.Map[String, MNode], lits: Map[String, Any]) {
  def conformsTo(sigs: Map[NodeTag, Signature], expectedSort: Type): Unit = {
    val sig = sigs.getOrElse(tag, throw new Exception(s"Cannot find signature of $tag"))
    if (!expectedSort.isAssignableFrom(sig.sort)) throw new Exception(s"Wrong sort, expected $expectedSort for ${tag}_$uri ~ $sig")

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

class StandardTree {
  // the root of this tree
  val root: MNode = MNode(null, RootTag, mutable.Map((RootLink.name, null)), Map())
  // index of all loaded nodes
  private val index: mutable.Map[NodeURI, MNode] = mutable.Map((null, root))

  // applies a changeset to this
  def process(changeset: Changeset): Unit =
    changeset.foreach(processChange)

  // applies a single change to this
  def processChange(change: Change): Unit = change match {
    case Load(node, tag, kids, lits) =>
      val subtree = MNode(node, tag,
        mutable.Map() ++ kids.map{case (n, uri) => (n, index(uri))},
        lits.toMap)
      index += (node -> subtree)
    case Unload(node, _, _, _) => index -= node

    case Detach(_, _, NamedLink(name), parent, _) => index(parent).kids(name) = null
    case Attach(node, _, NamedLink(name), parent, _) => index(parent).kids(name) = index(node)
  }

  def conformsTo(sigs: Map[NodeTag, Signature]): Option[String] = root.kids.get(RootLink.name) flatMap { t =>
    Try(t.conformsTo(sigs, sigs(root.tag).kids(RootLink.name))) match {
      case Failure(exception) => Some(exception.getMessage)
      case Success(value) => None
    }
  }
}

