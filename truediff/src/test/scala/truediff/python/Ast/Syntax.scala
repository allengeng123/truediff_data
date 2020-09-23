package truediff.python.Ast

import truediff.Diffable
import truediff.macros.diffable

@diffable case class identifier(name: String)

@diffable trait file extends Diffable
object file {
  case class File(body: Seq[stmt]) extends file
}

@diffable trait mod
object mod{
  case class Module(body: Seq[stmt]) extends mod
  case class Interactive(body: Seq[stmt]) extends mod
  case class Expression(body: Seq[stmt]) extends mod
}

@diffable sealed trait stmt
object stmt{
  case class FunctionDef(name: identifier, args: arguments, body: Seq[stmt], decorator_list: Seq[expr]) extends stmt
  case class ClassDef(name: identifier, bases: Seq[expr], body: Seq[stmt], decorator_list: Seq[expr]) extends stmt
  case class Return(value: Option[expr]) extends stmt

  case class Delete(targets: Seq[expr]) extends stmt
  case class Assign(targets: Seq[expr], value: expr) extends stmt
  case class AugAssign(target: expr, op: operator, value: expr) extends stmt

  // not sure if bool allowed: is, can always use int
  case class Print(dest: Option[expr], values: Seq[expr], nl: bool) extends stmt

  // use 'orelse' because else is a keyword in target languages
  case class For(target: expr, iter: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
  case class While(test: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
  case class If(test: expr, body: Seq[stmt], orelse: Seq[stmt]) extends stmt
  case class With(context_expr: expr, optional_vars: Option[expr], body: Seq[stmt]) extends stmt

  // 'type' is a bad name
  case class Raise(`type`: Option[expr], inst: Option[expr], tback: Option[expr]) extends stmt
  case class TryExcept(body: Seq[stmt], handlers: Seq[excepthandler], orelse: Seq[stmt]) extends stmt
  case class TryFinally(body: Seq[stmt], finalbody: Seq[stmt]) extends stmt
  case class Assert(test: expr, msg: Option[expr]) extends stmt

  case class Import(names: Seq[alias]) extends stmt
  case class ImportFrom(module: Option[identifier], names: Seq[alias], level: Option[Int]) extends stmt

  // Doesn't capture requirement that locals must be
  // defined if globals is
  // still supports use as a function!
  case class Exec(body: expr, globals: Option[expr], locals: Option[expr]) extends stmt

  case class Global(names: Seq[identifier]) extends stmt
  case class Expr(value: expr) extends stmt
  case class Pass() extends stmt
  case class Break() extends stmt
  case class Continue() extends stmt

  // XXX Jython will be different
  // col_offset is the byte offset in the utf8 string the parser uses
  // case class attributes(lineno: Int, col_offset: Int)
}

// BoolOp() can use left & right?
@diffable sealed trait expr
object expr{
  case class BoolOp(op: boolop, values: Seq[expr]) extends expr
  case class BinOp(left: expr, op: operator, right: expr) extends expr
  case class UnaryOp(op: unaryop, operand: expr) extends expr
  case class Lambda(args: arguments, body: expr) extends expr
  case class IfExp(test: expr, body: expr, orelse: expr) extends expr
  case class Dict(keys: Seq[expr], values: Seq[expr]) extends expr
  case class Set(elts: Seq[expr]) extends expr
  case class ListComp(elt: expr, generators: Seq[comprehension]) extends expr
  case class SetComp(elt: expr, generators: Seq[comprehension]) extends expr
  case class DictComp(key: expr, value: expr, generators: Seq[comprehension]) extends expr
  case class GeneratorExp(elt: expr, generators: Seq[comprehension]) extends expr
  // the grammar constrains where yield expressions can occur
  case class Yield(value: Option[expr]) extends expr
  // need sequences for compare to distinguish between
  // x < 4 < 3 and (x < 4) < 3
  case class Compare(left: expr, ops: Seq[cmpop], comparators: Seq[expr]) extends expr
  case class Call(func: expr, args: Seq[expr], keywords: Seq[keyword], starargs: Option[expr], kwargs: Option[expr]) extends expr
  case class Repr(value: expr) extends expr
  case class Num(n: Any) extends expr // a number as a PyObject.
  case class Str(s: string) extends expr // need to raw: specify, unicode, etc?
  // other bools: Option[literals]?

  // the following expression can appear in assignment context
  case class Attribute(value: expr, attr: identifier, ctx: expr_context) extends expr
  case class Subscript(value: expr, slice: slice, ctx: expr_context) extends expr
  case class Name(id: identifier, ctx: expr_context) extends expr
  case class List(elts: Seq[expr], ctx: expr_context) extends expr
  case class Tuple(elts: Seq[expr], ctx: expr_context) extends expr
}
// col_offset is the byte offset in the utf8 string the parser uses
case class attributes(lineno: int, col_offset: int)

@diffable sealed trait expr_context
object expr_context{
  case class Load() extends expr_context
  case class Store() extends expr_context
  case class Del() extends expr_context
  case class AugLoad() extends expr_context
  case class AugStore() extends expr_context
  case class Param() extends expr_context
}

@diffable sealed trait slice
object slice{
  case class Ellipsis() extends slice
  case class Slice(lower: Option[expr], upper: Option[expr], step: Option[expr]) extends slice
  case class ExtSlice(dims: Seq[slice]) extends slice
  case class Index(value: expr) extends slice
}

@diffable sealed trait boolop
object boolop{
  case class And() extends boolop
  case class Or() extends boolop
}

@diffable sealed trait operator
case object operator{
  case class Add() extends operator
  case class Sub()  extends operator
  case class Mult()  extends operator
  case class Div()  extends operator
  case class Mod()  extends operator
  case class Pow()  extends operator
  case class LShift()  extends operator
  case class RShift()  extends operator
  case class BitOr()  extends operator
  case class BitXor()  extends operator
  case class BitAnd()  extends operator
  case class FloorDiv() extends operator
}

@diffable sealed trait unaryop
object unaryop{
  case class Invert() extends unaryop
  case class Not() extends unaryop
  case class UAdd() extends unaryop
  case class USub() extends unaryop
}

@diffable sealed trait cmpop
object cmpop{
  case class Eq() extends cmpop
  case class NotEq() extends cmpop
  case class Lt() extends cmpop
  case class LtE() extends cmpop
  case class Gt() extends cmpop
  case class GtE() extends cmpop
  case class Is() extends cmpop
  case class IsNot() extends cmpop
  case class In() extends cmpop
  case class NotIn() extends cmpop
}

@diffable case class comprehension(target: expr, iter: expr, ifs: Seq[expr])

// not sure what to call the first argument for raise and except
@diffable sealed trait excepthandler
object excepthandler{
  case class ExceptHandler(`type`: Option[expr], name: Option[expr], body: Seq[stmt]) extends excepthandler
}

@diffable case class arguments(args: Seq[expr], vararg: Option[identifier], kwarg: Option[identifier], defaults: Seq[expr])

// keyword arguments supplied to call
@diffable case class keyword(arg: identifier, value: expr)

// import name with optional 'as' alias.
@diffable case class alias(name: identifier, asname: Option[identifier])