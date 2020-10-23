package truediff.compat.treesitter

import truechange._
import truediff.compat.treesitter.TreeSitterLibrary.lib
import truediff.{Diffable, Hashable, SubtreeRegistry}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DiffableTSNode(val nodeType: String, val literals: List[String], val fieldChildren: Map[String, DiffableTSNode], val otherChildren: List[DiffableTSNode]) extends Diffable {

  override def tag: NamedTag = NamedTag(nodeType)

  override lazy val identityHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    digest.update(nodeType.getBytes)
    literals.foreach(lit => digest.update(lit.getBytes()))
    directSubtrees.foreach(sub => digest.update(sub.identityHash))
    digest.digest()
  }

  override def sig: Signature = ???

  override val treeheight: Int = 1 + (fieldChildren.values.map(_.treeheight) ++ otherChildren.map(_.treeheight)).maxOption.getOrElse(0)

  override def treesize: Int = 1 + fieldChildren.values.map(_.treesize).sum + otherChildren.map(_.treeheight).sum

  override def toString: String = {
    val literalsStrings = if (literals.isEmpty) List() else literals.map(s => '"' + s.replace('\n', '\t') + '"')
    val childrenStrings = directSubtreesWithName.map(kv => s"${kv._1}=${kv._2}")
    val content = (literalsStrings ++ childrenStrings).mkString(", ")
    s"$tag($content)"
  }

  override def toStringWithURI: String = {
    val literalsStrings = if (literals.isEmpty) List() else literals.map(s => '"' + s.replace('\n', '\t') + '"')
    val childrenStrings = directSubtreesWithName.map(kv => s"${kv._1}=${kv._2.toStringWithURI}")
    val content = (literalsStrings ++ childrenStrings).mkString(", ")
    s"${tag}_$uri($content)"
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val newkids = ListBuffer[(String, URI)]()

    val newFieldChildren = fieldChildren.map {
      case (name, kid) =>
        val newkid = kid.loadUnassigned(edits).asInstanceOf[DiffableTSNode]
        newkids += name -> newkid.uri
        name -> newkid
    }
    val newOtherChildren = otherChildrenWithName.map {
      case (name, kid) =>
        val newkid = kid.loadUnassigned(edits).asInstanceOf[DiffableTSNode]
        newkids += name -> newkid.uri
        newkid
    }

    val newtree = new DiffableTSNode(nodeType, literals, newFieldChildren, newOtherChildren)
    edits += Load(newtree.uri, this.tag, newkids.toList, literalsWithName)
    newtree
  }


  override def updateLiterals(thatX: Diffable, edits: EditScriptBuffer): Diffable = {
    val that = thatX.asInstanceOf[DiffableTSNode]
    if (this.literals != that.literals) {
      edits += UpdateLiterals(this.uri, this.tag, this.literalsWithName, that.literalsWithName)
    }
    val newFieldChildren = fieldChildren.map {
      case (name, kid) =>
        val newkid = kid.updateLiterals(that.fieldChildren(name), edits).asInstanceOf[DiffableTSNode]
        name -> newkid
    }
    val newOtherChildren = otherChildrenWithName.zip(that.otherChildren).map {
      case ((name, thiskid), thatkid) =>
        val newkid = thiskid.updateLiterals(thatkid, edits).asInstanceOf[DiffableTSNode]
        newkid
    }

    val newtree = new DiffableTSNode(nodeType, that.literals, newFieldChildren, newOtherChildren)
    newtree._uri = this.uri
    newtree
  }

  override def loadInitial(edits: EditScriptBuffer): Unit = {
    directSubtrees.foreach(_.loadInitial(edits))
    edits += Load(this.uri, this.tag, directSubtreeURIsWithName, literalsWithName)
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      edits += Unload(this.uri, this.tag, directSubtreeURIsWithName, literalsWithName)
      directSubtrees.foreach(_.unloadUnassigned(edits))
    }
  }

  override protected def assignSharesRecurse(_that: Diffable, subtreeReg: SubtreeRegistry): Unit = {
    val that = _that.asInstanceOf[DiffableTSNode]

    if (this.nodeType == that.nodeType && this.literals == that.literals && this.fieldChildren.keys == that.fieldChildren.keys && this.otherChildren.size == that.otherChildren.size) {
      this.fieldChildren.keys.foreach(field => this.fieldChildren(field).assignShares(that.fieldChildren(field), subtreeReg))
      this.otherChildren.zip(that.otherChildren).foreach(kk => kk._1.assignShares(kk._2, subtreeReg))
    } else {
      this.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
    }
  }

  override protected def directSubtrees: Iterable[Diffable] = fieldChildren.values ++ otherChildren

  private def literalsWithName: List[(String, String)] = literals.zipWithIndex.map(kv => kv._2.toString -> kv._1)
  private def otherChildrenWithName: List[(String, DiffableTSNode)] = otherChildren.zipWithIndex.map(kv => kv._2.toString -> kv._1)
  private def directSubtreesWithName: Iterable[(String, Diffable)] = fieldChildren ++ otherChildrenWithName
  private def directSubtreeURIsWithName: Iterable[(String, URI)] = fieldChildren.view.mapValues(_.uri) ++ otherChildren.zipWithIndex.map(kv => kv._2.toString -> kv._1.uri)

  override protected def computeEditScriptRecurse(_that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = {
    val that = _that.asInstanceOf[DiffableTSNode]
    val thisuri = this.uri
    val thistag = this.tag

    if (this.nodeType == that.nodeType && this.literals == that.literals && this.fieldChildren.keys == that.fieldChildren.keys && this.otherChildren.size == that.otherChildren.size) {
      val newFieldChildren = fieldChildren.map {
        case (name, kid) =>
          val newkid = kid.computeEditScript(that.fieldChildren(name), thisuri, thistag, NamedLink(name), edits).asInstanceOf[DiffableTSNode]
          name -> newkid
      }
      val newOtherChildren = otherChildrenWithName.zip(that.otherChildren).map {
        case ((name, thiskid), thatkid) =>
          val newkid = thiskid.computeEditScript(thatkid, thisuri, thistag, NamedLink(name), edits).asInstanceOf[DiffableTSNode]
          newkid
      }

      val newtree = new DiffableTSNode(nodeType, literals, newFieldChildren, newOtherChildren)
      newtree._uri = thisuri
      newtree
    } else {
      null
    }
  }
}

object DiffableTSNode {
  def importTreeSitter(cursor: TSTreeCursor.ByReference, node: TSNode.ByValue, tokenNodes: Set[String], code: String): DiffableTSNode = {
    val nodeType = lib.ts_node_type(node)
    val (literals, fieldChildren, otherChildren): (List[String], Map[String, DiffableTSNode], List[DiffableTSNode]) = {
      val fieldChildrenBuffer = mutable.Map[String, DiffableTSNode]()
      val otherChildrenBuffer = ListBuffer[DiffableTSNode]()
      val isTokenNode = tokenNodes.contains(nodeType)

      if (!isTokenNode) {
        // move to first child
        val hasFirst = lib.ts_tree_cursor_goto_first_child(cursor)

        // traverse all children
        var hasMore = hasFirst
        while (hasMore) {
          val child = lib.ts_tree_cursor_current_node(cursor)
          val isNamed = lib.ts_node_is_named(child)
          if (isNamed) {
            val subnode = DiffableTSNode.importTreeSitter(cursor, child, tokenNodes, code)
            val field = lib.ts_tree_cursor_current_field_name(cursor)
            if (field != null)
              fieldChildrenBuffer += field -> subnode
            else
              otherChildrenBuffer += subnode
          }
          hasMore = lib.ts_tree_cursor_goto_next_sibling(cursor)
        }

        // step back to this node
        if (hasFirst)
          lib.ts_tree_cursor_goto_parent(cursor)

        (List(), fieldChildrenBuffer.toMap, otherChildrenBuffer.toList)
      } else {
        val literalBuffer = ListBuffer[String]()

        var currentPos = lib.ts_node_start_byte(node)

        // move to first child
        val hasFirst = lib.ts_tree_cursor_goto_first_child(cursor)

        // traverse all children
        var hasMore = hasFirst
        while (hasMore) {
          val child = lib.ts_tree_cursor_current_node(cursor)

          val childStart = lib.ts_node_start_byte(child)
          if (currentPos < childStart)
            literalBuffer += code.substring(currentPos, childStart)

          val isNamed = lib.ts_node_is_named(child)
          if (isNamed) {
            val subnode = DiffableTSNode.importTreeSitter(cursor, child, tokenNodes, code)
            val field = lib.ts_tree_cursor_current_field_name(cursor)
            if (field != null)
              fieldChildrenBuffer += field -> subnode
            else
              otherChildrenBuffer += subnode
          }

          currentPos = lib.ts_node_end_byte(child)
          hasMore = lib.ts_tree_cursor_goto_next_sibling(cursor)
        }

        // step back to this node
        if (hasFirst)
          lib.ts_tree_cursor_goto_parent(cursor)

        val endPos = lib.ts_node_end_byte(node)
        if (currentPos < endPos)
          literalBuffer += code.substring(currentPos, endPos)

        (literalBuffer.toList, fieldChildrenBuffer.toMap, otherChildrenBuffer.toList)
      }
    }
    new DiffableTSNode(nodeType, literals, fieldChildren, otherChildren)
  }
}