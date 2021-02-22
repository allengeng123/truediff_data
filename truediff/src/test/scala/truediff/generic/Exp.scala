package truediff.generic

import truediff._

trait Exp extends Diffable

object Exp {
  case class Hole() extends Exp with GenericDiffable {
    override def name: String = "Hole"
    override def children: Seq[(String, Any)] = Seq()
    override def make(children: Seq[Any]): GenericDiffable = Hole()
  }
}

case class Num(n: Int) extends Exp with GenericDiffable {
  override def name: String = "Num"
  override def children: Seq[(String, Any)] = Seq("n" -> n)
  override def make(children: Seq[Any]): GenericDiffable =
    Num(children(0).asInstanceOf[Int])
}

case class Add(e1: Exp, e2: Exp) extends Exp with GenericDiffable {
  override def name: String = "Add"
  override def children: Seq[(String, Any)] = Seq("e1" -> e1, "e2" -> e2)
  override def make(children: Seq[Any]): GenericDiffable =
    Add(children(0).asInstanceOf[Exp], children(1).asInstanceOf[Exp])
}

case class Sub(e1: Exp, e2: Exp) extends Exp with GenericDiffable {
  override def name: String = "Sub"
  override def children: Seq[(String, Any)] = Seq("e1" -> e1, "e2" -> e2)
  override def make(children: Seq[Any]): GenericDiffable =
    Sub(children(0).asInstanceOf[Exp], children(1).asInstanceOf[Exp])
}

case class Mul(e1: Exp, e2: Exp) extends Exp with GenericDiffable {
  override def name: String = "Mul"
  override def children: Seq[(String, Any)] = Seq("e1" -> e1, "e2" -> e2)
  override def make(children: Seq[Any]): GenericDiffable =
    Mul(children(0).asInstanceOf[Exp], children(1).asInstanceOf[Exp])
}

case class Var(vname: String) extends Exp with GenericDiffable {
  override def name: String = "Var"
  override def children: Seq[(String, Any)] = Seq("name" -> vname)
  override def make(children: Seq[Any]): GenericDiffable =
    Var(children(0).asInstanceOf[String])
}

case class Let(x: String, e: Exp, body: Exp) extends Exp with GenericDiffable {
  override def name: String = "Let"
  override def children: Seq[(String, Any)] = Seq("x" -> x, "e" -> e, "body" -> body)
  override def make(children: Seq[Any]): GenericDiffable =
    Let(children(0).asInstanceOf[String], children(1).asInstanceOf[Exp], children(2).asInstanceOf[Exp])
}
