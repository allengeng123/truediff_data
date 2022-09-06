package truediff.dot

import truediff.macros.diffable
import truediff.{Diffable, Ref}

@diffable
case class Graph(strict: Boolean, digraph: Boolean, name: Option[Identifier],
                 nodes: Seq[Node], edges: Seq[Edge]) extends Diffable

@diffable
case class Attribute(lhs: Identifier, rhs: Identifier)

@diffable
case class Attributes(attrs: Seq[Attribute])

@diffable
case class Node(name: Identifier, attrs: Attributes)

@diffable
case class Edge(from: Ref[Node], to: Ref[Node], more: Seq[Ref[Node]], attrs: Attributes)

case class EdgeUnresolved(from: Identifier, to: Identifier, more: Seq[Identifier], attrs: Attributes) {
  def resolve(nodeMap: Map[Identifier, Node]): Edge =
    Edge(Ref(nodeMap(from)), Ref(nodeMap(to)), more.map(n => Ref(nodeMap(n))), attrs)
}

sealed trait Identifier
case class Named(s: String) extends Identifier {
  override def toString: String = s
}
case class Numbered(l: Long) extends Identifier {
  override def toString: String = l.toString
}
case class Quoted(s: String) extends Identifier {
  override def toString: String = '"' + s + '"'
}
