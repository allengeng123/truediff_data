package truediff.macros

@diffable
case class Many(es: List[Exp]) extends Exp

@diffable
case class ManyMany(es: List[List[Exp]]) extends Exp

@diffable
case class ManyManyMany(es: List[List[List[Exp]]]) extends Exp
