package truediff.macros
package diffableTest

import truediff.diffable.DiffableOption

@diffable
case class Maybe(a: Option[Exp]) extends Exp

object Maybe {
  def apply(a: Option[Exp]): Maybe = Maybe(DiffableOption.from(a))
}