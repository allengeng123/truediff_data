package truediff.macros.propagate

import truediff.Diffable
import truediff.macros.diffable

@diffable
trait Exp extends Diffable
object Exp {
  case class BooleanLit(value: Boolean) extends Exp
  case class IntegerLit(value: Int) extends Exp
  case class LongLit(value: Long) extends Exp
  case class Mul(lhs: Exp, rhs: Exp) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp
  case class Not(e: Exp) extends Exp
  case class And(lhs: Exp, rhs: Exp) extends Exp
  case class Or(lhs: Exp, rhs: Exp) extends Exp
  case class Many(exps: List[Exp]) extends Exp
  case class Let(name: String, bound: Exp, body: Exp) extends Exp
}