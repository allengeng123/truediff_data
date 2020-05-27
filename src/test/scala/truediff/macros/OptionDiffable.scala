package truediff.macros

@diffable
case class Maybe(a: Option[Exp]) extends Exp

@diffable
case class MaybeMaybe(a: Option[Option[Exp]]) extends Exp

@diffable
case class MaybeMaybeMaybe(a: Option[Option[Option[Exp]]]) extends Exp

