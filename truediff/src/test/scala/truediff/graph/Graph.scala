package truediff.graph

import truediff.macros.diffable
import truediff.{Diffable, Ref}

@diffable case class Graph(nodes: List[Node], edges: List[Edge]) extends Diffable
@diffable case class Node(label: NodeName, color: NodeColor)
@diffable case class NodeName(name: String)
@diffable case class NodeColor(color: String)
@diffable case class Edge(from: Ref[Node], to: Ref[Node])
