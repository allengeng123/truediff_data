package truediff.manual

import truechange._
import truediff.{SubtreeRegistry, _}

trait Exp extends Diffable

object Exp {
  case class Hole() extends Exp {
    override def treeheight: Int = 1

    override def treesize: Int = 1

    override def toStringWithURI: String = s"Hole_$uri()"

    override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map(), Map())

    override def foreachSubtree(f: Diffable => Unit): Unit = {
    }

    override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
      case that: Hole =>
      case _ =>
        that.foreachSubtree(subtreeReg.assignShare)
    }

    override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

    override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, buf: EditScriptBuffer): Diffable = that match {
      case Hole() =>
        val newtree = Hole()
        newtree._uri = this.uri
        newtree
      case _ => null
    }

    override def loadUnassigned(buf: EditScriptBuffer): Diffable = {
      val that = this
      if (that.assigned != null) {
        return that.assigned
      }

      val newtree = Hole()
      buf += Load(newtree.uri, this.tag, Seq(), Seq())
      newtree
    }


    override def loadInitial(buf: EditScriptBuffer): Unit = {
      buf += Load(this.uri, this.tag, Seq(), Seq())
    }

    override def unloadUnassigned(buf: EditScriptBuffer): Unit = {
      if (this.assigned != null) {
        this.assigned = null
      } else {
        buf += Unload(this.uri, this.tag, Seq(), Seq())
      }
    }

    lazy val hash: Array[Byte] = {
      val digest = Hashable.mkDigest
      digest.update(this.getClass.getCanonicalName.getBytes)
      digest.digest()
    }
  }
}


case class Num(n: Int) extends Exp {

  lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes)
    Hashable.hash(this.n, digest)
    digest.digest()
  }

  override val treeheight: Int = 1

  override def treesize: Int = 1

  override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map(), Map("n" -> JavaLitType(classOf[Integer])))

  override def toStringWithURI: String = s"Num_$uri($n)"

  override def foreachSubtree(f: Diffable => Unit): Unit = {
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Num if this.n == that.n =>
    case _ =>
      that.foreachSubtree(subtreeReg.assignShare)
  }

  override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, buf: EditScriptBuffer): Diffable = that match {
    case Num(n) if this.n == n =>
      val newtree = Num(n)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(buf: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val newtree = Num(this.n)
    buf += Load(newtree.uri, this.tag, Seq(), Seq(
      "n" -> this.n
    ))
    newtree
  }


  override def loadInitial(buf: EditScriptBuffer): Unit = {
    buf += Load(this.uri, this.tag, Seq(), Seq(
      "n" -> this.n
    ))
  }

  override def unloadUnassigned(buf: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      buf += Unload(this.uri, this.tag, Seq(), Seq(
        "n" -> this.n
      ))
    }
  }
}

case class Add(e1: Exp, e2: Exp) extends Exp {

  override lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes)
    digest.update(this.e1.hash)
    digest.update(this.e2.hash)
    digest.digest()
  }

  override val treeheight: Int = 1 + Math.max(e1.treeheight, e2.treeheight)

  override def treesize: Int = 1 + e1.treesize + e2.treesize

  override def toStringWithURI: String = s"Add_$uri(${e1.toStringWithURI}, ${e2.toStringWithURI})"

  override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map("e1" -> SortType(classOf[Exp]), "e2" -> SortType(classOf[Exp])), Map())

  override def foreachSubtree(f: Diffable => Unit): Unit = {
    f(this.e1)
    this.e1.foreachSubtree(f)
    f(this.e2)
    this.e2.foreachSubtree(f)
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Add =>
      this.e1.assignShares(that.e1, subtreeReg)
      this.e2.assignShares(that.e2, subtreeReg)
    case _ =>
      this.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
      that.foreachSubtree(subtreeReg.assignShare)
  }

  override protected def directSubtrees: Iterable[Diffable] = Iterable(e1, e2)

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, buf: EditScriptBuffer): Diffable = that match {
    case that: Add =>
      val e1 = this.e1.computeEditScript(that.e1, this.uri, this.tag, NamedLink("e1"), buf).asInstanceOf[Exp]
      val e2 = this.e2.computeEditScript(that.e2, this.uri, this.tag, NamedLink("e2"), buf).asInstanceOf[Exp]
      val newtree = Add(e1, e2)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(buf: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val e1 = that.e1.loadUnassigned(buf).asInstanceOf[Exp]
    val e2 = that.e2.loadUnassigned(buf).asInstanceOf[Exp]
    val newtree = Add(e1, e2)
    buf += Load(newtree.uri, this.tag, Seq(
      "e1" -> e1.uri,
      "e2" -> e2.uri
    ), Seq())
    newtree
  }


  override def loadInitial(buf: EditScriptBuffer): Unit = {
    this.e1.loadInitial(buf)
    this.e2.loadInitial(buf)
    buf += Load(this.uri, this.tag, Seq(
      "e1" -> this.e1.uri,
      "e2" -> this.e2.uri
    ), Seq())
  }

  override def unloadUnassigned(buf: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      buf += Unload(this.uri, this.tag, Seq(
        "e1" -> this.e1.uri,
        "e2" -> this.e2.uri
      ), Seq())
      this.e1.unloadUnassigned(buf)
      this.e2.unloadUnassigned(buf)
    }
  }
}

case class Var(name: String) extends Exp {
  override lazy val hash: Array[Byte] = {
    val digest = Hashable.mkDigest
    digest.update(this.getClass.getCanonicalName.getBytes)
    Hashable.hash(this.name, digest)
    digest.digest()
  }

  override val treeheight: Int = 1

  override def treesize: Int = 1

  override def sig: Signature = Signature(SortType(classOf[Exp]), this.tag, Map(), Map("name" -> JavaLitType(classOf[String])))

  override def toStringWithURI: String = s"Var_$uri($name)"

  override def foreachSubtree(f: Diffable => Unit): Unit = {
  }

  override protected def assignSharesRecurse(that: Diffable, subtreeReg: SubtreeRegistry): Unit = that match {
    case that: Var if this.name == that.name =>
    case _ =>
      that.foreachSubtree(subtreeReg.assignShare)
  }

  override protected def directSubtrees: Iterable[Diffable] = Iterable.empty

  override protected def computeEditScriptRecurse(that: Diffable, parent: URI, parentTag: Tag, link: Link, buf: EditScriptBuffer): Diffable = that match {
    case Var(n) if this.name == name =>
      val newtree = Var(name)
      newtree._uri = this.uri
      newtree
    case _ => null
  }

  override def loadUnassigned(buf: EditScriptBuffer): Diffable = {
    val that = this
    if (that.assigned != null) {
      return that.assigned
    }

    val newtree = Var(this.name)
    buf += Load(newtree.uri, this.tag, Seq(), Seq(
      "name" -> this.name
    ))
    newtree
  }


  override def loadInitial(buf: EditScriptBuffer): Unit = {
    buf += Load(this.uri, this.tag, Seq(), Seq(
      "name" -> this.name
    ))
  }

  override def unloadUnassigned(buf: EditScriptBuffer): Unit = {
    if (this.assigned != null) {
      this.assigned = null
    } else {
      buf += Unload(this.uri, this.tag, Seq(), Seq(
        "name" -> this.name
      ))
    }
  }
}