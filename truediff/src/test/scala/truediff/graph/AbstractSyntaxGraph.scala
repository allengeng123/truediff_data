package truediff.graph

import truediff.Diffable
import truediff.Ref
import truediff.macros.diffable

@diffable sealed trait Exp extends Diffable {
  def resolve(env: Map[String, Declaration]): Exp
  def resolved: Exp = resolve(Map())
}

@diffable case class Num(n: Int) extends Exp {
  override def resolve(env: Map[String, Declaration]): Exp = this
}
@diffable case class Add(e1: Exp, e2: Exp) extends Exp {
  override def resolve(env: Map[String, Declaration]): Exp = Add(e1.resolve(env), e2.resolve(env))
}
@diffable case class Sub(e1: Exp, e2: Exp) extends Exp {
  override def resolve(env: Map[String, Declaration]): Exp = Sub(e1.resolve(env), e2.resolve(env))
}
@diffable case class Mul(e1: Exp, e2: Exp) extends Exp {
  override def resolve(env: Map[String, Declaration]): Exp = Mul(e1.resolve(env), e2.resolve(env))
}

trait Declaration extends Diffable

/** Let creates a non-cyclic data structure with cross references (x has a parent and is linked by references). */
@diffable case class Let(x: VarDecl, e: Exp, body: Exp) extends Exp {
  override def resolve(env: Map[String, Declaration]): Exp = Let(x, e.resolve(env), body.resolve(env + (x.name -> x)))
}
@diffable case class VarDecl(name: String) extends Declaration

/** Lambda creates a cyclic data structure when the lambda is linked by references within the body */
@diffable case class Lambda(x: String, private var _body: Exp) extends Exp with Declaration {
  def body: Exp = _body

  override def resolve(env: Map[String, Declaration]): Exp = {
    val lam = Lambda(x, _body)
    lam._body = _body.resolve(env + (x -> lam))
    lam
  }
}

@diffable case class Var(decl: Ref[Declaration]) extends Exp {
  override def resolve(env: Map[String, Declaration]): Exp = this
}
@diffable case class UVar(name: String) extends Exp {
  override def resolve(env: Map[String, Declaration]): Exp = env.get(name).map(l => Var(Ref(l))).getOrElse(this)
}

