package externalpackage

import truechange._
import truediff.{SubtreeRegistry, _}

trait Exp extends Diffable

object Exp {
  case class Hole() extends Exp {
    override def treeheight: Int = 1

    override def treesize: Int = 1

    override def toStringWithURI: String = s"Hole_$uri()"

    override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map(), Map())

    override def foreachSubtree(f: Diffable => Unit): Unit = {
      f(this)
    }

    override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
      case that: Hole =>
      case _ =>
        that.foreachSubtree(t => subtreeReg.assignShare(t))
    }

    override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

    override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
      case Hole() =>
        val newtree = Hole()
        newtree._uri = this.uri
        newtree
      case _ => null
    }

    override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
      val that = this
      if (that.assigned != null) {
        return that.assigned
      }

      val newtree = Hole()
      edits += Load(newtree.uri, this.tag, Seq(), Seq())
      newtree
    }


    override def loadInitial(edits: EditScriptBuffer): Unit = {
      edits += Load(this.uri, this.tag, Seq(), Seq())
    }

    override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
      if (this.assigned != null) {
        this.assigned = null
      } else
        edits += Unload(this.uri, this.tag, Seq(), Seq())
    }

    lazy val cryptoHash: Array[Byte] = {
      val digest = Hashable.mkDigest
      this.getClass.getCanonicalName.getBytes
      digest.digest()
    }
  }
}


case class Num(n: Int) extends Exp {

  lazy val cryptoHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    Hashable.hash(this.n, digest)
    digest.digest()
  }

  override val treeheight: Int = 1

  override def treesize: Int = 1

  override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map(), Map("n" -> JavaLitType(classOf[Integer])))

  override def toStringWithURI: String = s"Num_$uri($n)"

  override def foreachSubtree(f: Diffable => Unit): Unit = {
    f(this)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Num if this.n == that.n =>
    case _ =>
      that.foreachSubtree(t => subtreeReg.assignShare(t))
  }

  override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case Num(n) if this.n == n =>
      val newtree = Num(n)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val newtree = Num(this.n)
    edits += Load(newtree.uri, this.tag, Seq(), Seq(
      "n" -> this.n
    ))
    newtree
  }


  override def loadInitial(edits: EditScriptBuffer): Unit = {
    edits += Load(this.uri, this.tag, Seq(), Seq(
      "n" -> this.n
    ))
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else
      edits += Unload(this.uri, this.tag, Seq(), Seq("n" -> this.n))
  }
}

case class Add(e1: Exp, e2: Exp) extends Exp {

  override lazy val cryptoHash: Array[Byte] = {
    val digest = Hashable.mkDigest
    this.getClass.getCanonicalName.getBytes
    digest.update(this.e1.cryptoHash)
    digest.update(this.e2.cryptoHash)
    digest.digest()
  }

  override val treeheight: Int = 1 + Math.max(e1.treeheight, e2.treeheight)

  override def treesize: Int = 1 + e1.treesize + e2.treesize

  override def toStringWithURI: String = s"Add_$uri(${e1.toStringWithURI}, ${e2.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp].getCanonicalName), this.tag, Map("e1" -> SortType(classOf[Exp].getCanonicalName), "e2" -> SortType(classOf[Exp].getCanonicalName)), Map())

  override def foreachSubtree(f: Diffable => Unit): Unit = {
    f(this)
    this.e1.foreachSubtree(f)
    this.e2.foreachSubtree(f)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Add =>
      this.e1.assignShares(that.e1, subtreeReg)
      this.e2.assignShares(that.e2, subtreeReg)
    case _ =>
      this.e1.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      this.e2.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
  }

  override protected def directSubtrees: Iterable[Diffable] = Iterable(e1, e2)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, edits: EditScriptBuffer): Diffable = that match {
    case that: Add =>
      val e1 = this.e1.computeEditScript(that.e1, this.uri, this.tag, NamedLink("e1"), edits).asInstanceOf[Exp]
      val e2 = this.e2.computeEditScript(that.e2, this.uri, this.tag, NamedLink("e2"), edits).asInstanceOf[Exp]
      val newtree = Add(e1, e2)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(edits: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val e1 = that.e1.loadUnassigned(edits).asInstanceOf[Exp]
    val e2 = that.e2.loadUnassigned(edits).asInstanceOf[Exp]
    val newtree = Add(e1, e2)
    edits += Load(newtree.uri, this.tag, Seq(
      "e1" -> e1.uri,
      "e2" -> e2.uri
    ), Seq())
    newtree
  }


  override def loadInitial(edits: EditScriptBuffer): Unit = {
    this.e1.loadInitial(edits)
    this.e2.loadInitial(edits)
    edits += Load(this.uri, this.tag, Seq(
      "e1" -> this.e1.uri,
      "e2" -> this.e2.uri
    ), Seq())
  }

  override def unloadUnassigned(edits: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      edits += Unload(this.uri, this.tag, Seq(
      "e1" -> this.e1.uri,
      "e2" -> this.e2.uri
      ), Seq())
      this.e1.unloadUnassigned(edits)
      this.e2.unloadUnassigned(edits)
    }
  }
}
