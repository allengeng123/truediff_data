package truediff.compat.antlr

import org.antlr.v4.runtime.tree.{ParseTree, RuleNode, TerminalNode}
import org.antlr.v4.runtime.{ParserRuleContext, RuleContext}
import truechange._
import truediff.{Diffable, Hashable, SubtreeRegistry}

import scala.collection.mutable.ListBuffer


class DiffableRuleContext(val rulename: String, val ctx: RuleContext, mapper: RuleContextMapper) extends Diffable {

  override val _tag: Tag = NamedTag(rulename)

  private def children: Iterable[ParseTree] =
    for (i <- 0 until ctx.getChildCount)
      yield ctx.getChild(i)

  override lazy val directSubtrees: Iterable[DiffableRuleContext] =
    children.flatMap {
      case node: RuleNode => Some(mapper.diffable(node.getRuleContext))
      case _ => None
    }

  def directSubtreesIndexed: Iterable[(Int, DiffableRuleContext)] =
    children.zipWithIndex.flatMap {
      case (node,i) if node.isInstanceOf[RuleNode] =>
        Some(i -> mapper.diffable(node.asInstanceOf[RuleNode].getRuleContext))
      case _ => None
    }

  def lits: Iterable[String] =
    children.flatMap {
      case node: TerminalNode => Some(node.getSymbol.getText)
      case _ => None
    }

  override val cryptoHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    Hashable.hash(ctx.getRuleIndex, digest)
    for (i <- 0 until ctx.getChildCount) {
      ctx.getChild(i) match {
        case node: RuleNode =>
          digest.update(mapper.diffable(node.getRuleContext).cryptoHash)
        case node: TerminalNode =>
          Hashable.hash(node.getSymbol.getText, digest)
      }
    }
    digest.digest()
  }

  override def sig: Signature = ???

  override val treeheight: Int = 1 + directSubtrees.map(_.treeheight).maxOption.getOrElse(0)

  override def treesize: Int = 1 + directSubtrees.map(_.treeheight).sum

  override def toStringWithURI: String = ???

  override def foreachSubtree(f: Diffable => Unit): Unit =
    directSubtrees.foreach { node =>
      f(node)
      node.foreachSubtree(f)
    }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    var newtreeKids: Map[String, URI] = Map()
    var newtreeLits: Map[String, Any] = Map()
    val newctx = new ParserRuleContext() {
      override def getRuleIndex: Int = ctx.getRuleIndex
    }

    var i = 0
    children.foreach {
      case node: RuleNode =>
        val newkid = mapper.diffable(node).loadUnassigned(edits).asInstanceOf[DiffableRuleContext]
        newctx.addAnyChild(newkid.ctx)
        newtreeKids += i.toString -> newkid.uri
        i += 1
      case node: TerminalNode =>
        newctx.addAnyChild(node)
        newtreeLits += i.toString -> node.getSymbol.getText
        i += 1
    }

    val newtree = mapper.diffable(newctx)
    edits += Load(newtree.uri, newtree.tag, newtreeKids, newtreeLits)
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    var kids: Map[String, URI] = Map()
    var lits: Map[String, Any] = Map()
    var i = 0
    children.foreach {
      case node: RuleNode =>
        val diffNode = mapper.diffable(node)
        diffNode.loadInitial(edits)
        kids += i.toString -> diffNode.uri
        i += 1
      case node: TerminalNode =>
        lits += i.toString -> node.getSymbol.getText
        i += 1
    }
    edits += Load(this.uri, this.tag, kids, lits)
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
      return
    }

    var kids: Map[String, URI] = Map()
    val kidsSeq: ListBuffer[DiffableRuleContext] = ListBuffer()
    var lits: Map[String, Any] = Map()
    var i = 0
    children.foreach {
      case node: RuleNode =>
        val diffNode = mapper.diffable(node).asInstanceOf[DiffableRuleContext]
        kids += i.toString -> diffNode.uri
        kidsSeq += diffNode
        i += 1
      case node: TerminalNode =>
        lits += i.toString -> node.getSymbol.getText
        i += 1
    }

    edits += Unload(this.uri, this.tag, kids, lits)
    kidsSeq.foreach(_.unloadUnassigned(edits))
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: DiffableRuleContext
      if this.ctx.getRuleIndex == that.ctx.getRuleIndex
        && this.directSubtrees.size == that.directSubtrees.size
        && this.lits == that.lits =>
      this.directSubtrees.zip(that.directSubtrees).foreach { case (thisnode, thatnode) =>
        thisnode.assignShares(thatnode, subtreeReg)
      }

    case _ =>
      this.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
  }

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: DiffableRuleContext
      if this.ctx.getRuleIndex == that.ctx.getRuleIndex
        && this.directSubtrees.size == that.directSubtrees.size
        && this.lits == that.lits =>

      val newctx = new ParserRuleContext() {
        override def getRuleIndex: Int = ctx.getRuleIndex
      }

      var i = 0
      this.children.zip(that.children).foreach {
        case (thisRuleNode:RuleNode, thatRuleNode:RuleNode) =>
          val thisnode = mapper.diffable(thisRuleNode)
          val thatnode = mapper.diffable(thatRuleNode)
          val newnode = thisnode.computeEditScript(thatnode, this.uri, this.tag, NamedLink(i.toString), edits).asInstanceOf[DiffableRuleContext]
          newctx.addAnyChild(newnode.ctx)
          i += 1
        case (thisTerminalNode:TerminalNode, _) =>
          newctx.addAnyChild(thisTerminalNode)
          i += 1
      }

      val newtree = mapper.diffable(newctx)
      newtree._uri = this.uri
      newtree
    case _ => null
  }
}
