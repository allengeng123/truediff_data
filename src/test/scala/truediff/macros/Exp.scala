package truediff.macros

@diffable
trait Exp

object Exp {
  case class Hole() extends Exp
}

@diffable
case class Num(n: Int) extends Exp

@diffable
case class Add(e1: Exp, e2: Exp) extends Exp
