package truediff.json

import truediff.macros.diffable

// adapted from https://github.com/lihaoyi/fastparse/blob/master/fastparse/test/src/fastparse/JsonTests.scala

@diffable
sealed trait Js {
  def value: Any
}
object Js {
  case class Str(value: java.lang.String) extends Js
  case class Obj(value: Seq[Field]) extends Js {
//    override def toString: String = super.toString
  }
  case class Arr(value: Seq[Js]) extends Js {
//    override def toString: String = super.toString
  }
  case class Num(value: Double) extends Js
  case class False() extends Js {
    def value = false
  }
  case class True() extends Js {
    def value = true
  }
  case class Null() extends Js {
    def value = null
  }
}

@diffable
case class Field(name: java.lang.String, value: Js)
