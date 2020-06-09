package truediff.macros

import truediff.Diffable

@diffable
trait Exp extends Diffable

object Exp {
  case class Hole() extends Exp
}

@diffable
case class Num(n: Int) extends Exp

@diffable
case class Add(e1: Exp, e2: Exp) extends Exp