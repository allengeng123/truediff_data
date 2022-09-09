package truediff.graph

import truediff.{Diffable, Ref}
import truediff.macros.diffable

@diffable sealed trait Exp extends Diffable {
  def resolve(env: Map[String, Let]): Exp
  def resolved: Exp = resolve(Map())
}

@diffable case class Num(n: Int) extends Exp {
  override def resolve(env: Map[String, Let]): Exp = this
}
@diffable case class Add(e1: Exp, e2: Exp) extends Exp {
  override def resolve(env: Map[String, Let]): Exp = Add(e1.resolve(env), e2.resolve(env))
}
@diffable case class Sub(e1: Exp, e2: Exp) extends Exp {
  override def resolve(env: Map[String, Let]): Exp = Sub(e1.resolve(env), e2.resolve(env))
}
@diffable case class Mul(e1: Exp, e2: Exp) extends Exp {
  override def resolve(env: Map[String, Let]): Exp = Mul(e1.resolve(env), e2.resolve(env))
}

@diffable case class Let(x: String, e: Exp, body: Exp) extends Exp {
  override def resolve(env: Map[String, Let]): Exp = Let(x, e.resolve(env), body.resolve(env + (x -> this)))
}
@diffable case class Var(decl: Ref[Let]) extends Exp {
  override def resolve(env: Map[String, Let]): Exp = this
}
@diffable case class UVar(name: String) extends Exp {
  override def resolve(env: Map[String, Let]): Exp = env.get(name).map(l => Var(Ref(l))).getOrElse(this)
}
