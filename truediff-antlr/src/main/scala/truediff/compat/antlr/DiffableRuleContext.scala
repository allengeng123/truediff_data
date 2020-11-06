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

  lazy val lits: Map[String, String] =
    children.zipWithIndex.flatMap {
      case (node, i) if node.isInstanceOf[TerminalNode] =>
        Some(i.toString -> node.asInstanceOf[TerminalNode].getSymbol.getText)
      case _ =>
        None
    }.toMap

  override lazy val literalsHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    for (i <- 0 until ctx.getChildCount) {
      ctx.getChild(i) match {
        case node: RuleNode =>
          digest.update(mapper.diffable(node.getRuleContext).literalsHash)
        case node: TerminalNode =>
          Hashable.hash(node.getSymbol.getText, digest)
      }
    }
    digest.digest()
  }

  override def sig: Signature = ???

  override val treeheight: Int = 1 + directSubtrees.map(_.treeheight).maxOption.getOrElse(0)

  override def treesize: Int = 1 + directSubtrees.map(_.treeheight).sum

  override def toString: String = s"${rulename}(${directSubtrees.map(_.toStringWithURI).mkString(", ")}, $lits)"
  override def toStringWithURI: String = s"${rulename}_$uri(${directSubtrees.map(_.toStringWithURI).mkString(", ")}, $lits)"

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    var newtreeKids: Map[String, URI] = Map()
    val newctx = new ParserRuleContext() {
      override def getRuleIndex: Int = ctx.getRuleIndex
    }

    directSubtreesIndexed.foreach {
      case (i, subtree) =>
        val newkid = subtree.loadUnassigned(edits).asInstanceOf[DiffableRuleContext]
        newctx.addAnyChild(newkid.ctx)
        newtreeKids += i.toString -> newkid.uri
    }

    val newtree = mapper.diffable(newctx)
    edits += Load(newtree.uri, newtree.tag, newtreeKids, this.lits)
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
    edits += Load(this.uri, this.tag, kids, this.lits)
  }


  override def updateLiterals(thatX: Diffable, edits: EditScriptBuffer): Diffable = {
    val that = thatX.asInstanceOf[DiffableRuleContext]
    if (this.lits != that.lits)
      edits += UpdateLiterals(this.uri, this.tag, this.lits, that.lits)

    val newctx = new ParserRuleContext() {
      override def getRuleIndex: Int = ctx.getRuleIndex
    }

    this.children.zip(that.children).foreach {
      case (thisRuleNode:RuleNode, thatRuleNode:RuleNode) =>
        val thisnode = mapper.diffable(thisRuleNode)
        val thatnode = mapper.diffable(thatRuleNode)
        val newnode = thisnode.updateLiterals(thatnode, edits).asInstanceOf[DiffableRuleContext]
        newctx.addAnyChild(newnode.ctx)
      case (_, thatTerminalNode:TerminalNode) =>
        newctx.addAnyChild(thatTerminalNode)
    }

    val newtree = mapper.diffable(newctx)
    newtree._uri = this.uri
    newtree
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
