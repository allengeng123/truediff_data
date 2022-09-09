package truediff.graph

import truediff.Diffable
import truediff.Ref
import truediff.macros.diffable

@diffable sealed trait Exp extends Diffable {
  def resolve(env: Map[String, VarDecl]): Exp
  def resolved: Exp = resolve(Map())
}

@diffable case class Num(n: Int) extends Exp {
  override def resolve(env: Map[String, VarDecl]): Exp = this
}
@diffable case class Add(e1: Exp, e2: Exp) extends Exp {
  override def resolve(env: Map[String, VarDecl]): Exp = Add(e1.resolve(env), e2.resolve(env))
}
@diffable case class Sub(e1: Exp, e2: Exp) extends Exp {
  override def resolve(env: Map[String, VarDecl]): Exp = Sub(e1.resolve(env), e2.resolve(env))
}
@diffable case class Mul(e1: Exp, e2: Exp) extends Exp {
  override def resolve(env: Map[String, VarDecl]): Exp = Mul(e1.resolve(env), e2.resolve(env))
}

@diffable case class VarDecl(name: String)

@diffable case class Let(x: VarDecl, e: Exp, body: Exp) extends Exp {
  override def resolve(env: Map[String, VarDecl]): Exp = Let(x, e.resolve(env), body.resolve(env + (x.name -> x)))
}
@diffable case class Var(decl: Ref[VarDecl]) extends Exp {
  override def resolve(env: Map[String, VarDecl]): Exp = this
}
@diffable case class UVar(name: String) extends Exp {
  override def resolve(env: Map[String, VarDecl]): Exp = env.get(name).map(l => Var(Ref(l))).getOrElse(this)
}
