package truediff.graph

import truediff.Diffable
import truediff.macros.diffable

@diffable case class TGraph(nodes: List[TNode], edges: List[TEdge]) extends Diffable
@diffable case class TNode(label: TNodeName, color: TNodeColor)
@diffable case class TNodeName(name: String)
@diffable case class TNodeColor(color: String)
@diffable case class TEdge(from: TNode, to: TNode)
