package truediff.dot

import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser => P, Parser0 => P0}

import scala.language.implicitConversions

/**
 *  Parser for TIP programs, adapted for cats-parse from https://github.com/cs-au-dk/TIP/blob/master/src/tip/parser/TipParser.scala
 */
object Parser {

  def parse(source: String): Graph =
    graph.parseAll(source) match {
      case Right(p) => p
      case Left(err) => throw new IllegalArgumentException(s"Parse error at ${source.slice(err.failedAtOffset, err.failedAtOffset + 10)}: $err")
    }

  /* LEXICAL */

  val lineComment: P[Unit] = P.string("//") *> P.charsWhile0(c => c != '\n' && c != '\r').void
  val blockComment: P[Unit] = P.string("/*") *> P.recursive[Unit](rec =>
    P.product01(P.charsWhile0(c => c != '*').void, P.string("*/") | P.char('*') ~ rec).void
  )
  val comment: P[Unit] = lineComment | blockComment
  val whitespace: P[Unit] = (P.charIn(" \t\r\n").void | comment)
  val whitespaces0: P0[Unit] = whitespace.rep0.void

  def spaced[A](p: P[A]): P[A] =
    p <* whitespaces0

  def keyword(s: String): P[Unit] =
    spaced(P.string(s) *> P.not(letterDigit))

  val letter: P[Unit] = P.ignoreCaseCharIn('a' to 'z').void | P.char('_')
  val digit: P[Unit] = P.charIn('0' to '9').void
  val letterDigit: P[Unit] = P.charIn(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')).void

  val id: P[String] =
    (letter ~ letterDigit.rep0)
      .string

  val stringChar: P[Unit] =
    P.charWhere(_ != '"').void | P.string("\\\"")

  val string: P[String] =
    inQuotes(stringChar.rep0.void.string)

  val identifier: P[Identifier] =
    spaced {
      id.map(Named.apply) |
      Numbers.signedIntString.map(s => Numbered(s.toLong)) |
      string.map(Quoted.apply)
    }

  def inParens[A](p: P0[A]): P[A] =
    op('(') *> p <* op(')')

  def inBraces[A](p: P0[A]): P[A] =
    op('{') *> p <* op('}')

  def inBrackets[A](p: P0[A]): P[A] =
    op('[') *> p <* op(']')

  def inQuotes[A](p: P0[A]): P[A] =
    op('"') *> p <* op('"')

  def list0[A](p: P[A]): P0[List[A]] =
    p.repSep0(op(','))

  def list[A](p: P[A]): P[List[A]] =
    p.repSep(op(',')).map(_.toList)

  val semi: P[Unit] =
    op(';')

  def op(c: Char): P[Unit] =
    spaced(P.char(c))

  def op(s: String): P[Unit] =
    spaced(P.string(s))

  /* STRUCTURAL */

  val attribute: P[Attribute] =
    (identifier ~ (op('=') *> identifier)).map { case (l, r) => Attribute(l, r) }

  val attributes: P0[Attributes] =
    inBrackets(
      (attribute <* (op(',') | op(';')).?).rep0
    ).rep0.map(attrs => Attributes(attrs.flatten))

  val attributes0: P0[Attributes] =
    attributes.?.map(_.getOrElse(Attributes(Seq())))

  val edgeRhs: P[NonEmptyList[Identifier]] =
    ((op("--") | op("->")) *> identifier).rep(1)

  val stmt: P[Any] =
    (identifier ~ edgeRhs.? ~ attributes0 <* op(';').?).map {
      case ((from, Some(NonEmptyList(to, more))), attrs) =>
        EdgeUnresolved(from, to, more, attrs)
      case ((name, None), attrs) =>
        Node(name, attrs)
    }

  val graph: P0[Graph] =
    whitespaces0 *>
    ( keyword("strict").? ~
      (keyword("graph").string | keyword("digraph").string) ~
      identifier.? ~
      inBraces(stmt.rep0)
    ).map {
      case (((strict, graph), name), stmts) =>
        val nodes = stmts.collect { case n: Node => n }
        val nodeMap = nodes.map(n => n.name -> n).toMap
        val edges = stmts.collect { case e: EdgeUnresolved => e.resolve(nodeMap) }
        Graph(strict.isDefined, graph == "digraph", name, nodes, edges)
    } <* P.end
}