package truediff.graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truechange._
import truediff.Diffable

class TreeGraphTests extends AnyFlatSpec with Matchers {
  def testEditScript(src: Diffable, dest: Diffable, expectedChanges: Int): Unit = {
    println("Comparing:")
    println(s"  ${src.toStringWithURI}")
    println(s"  ${dest.toStringWithURI}")

    val (editscript,newtree) = src.compareTo(dest)
    println("EditScript:")
    editscript.foreach(c => println("  " + c))
    println("New tree:")
    println("  " + newtree.toStringWithURI)

    assertResult(dest)(newtree)

    val sigs: Map[Tag, Signature] = src.collectSignatures ++ dest.collectSignatures
    assertResult(None)(editscript.welltyped(sigs))

    assertResult(expectedChanges)(editscript.coresize)
    newtree.foreachTree(t => assert(t.share == null, s", share of ${t.toStringWithURI} was not reset"))
    newtree.foreachTree(t => assert(t.assigned == null, s", assigned of ${t.toStringWithURI} was not reset"))

    val reverseEditScript = dest.compareTo(src)._1
    println("Reverse editscript:")
    reverseEditScript.foreach(c => println("  " + c))
    assertResult(expectedChanges)(reverseEditScript.coresize)

    val loadEditScript = Diffable.load(src)
    println("Load editscript:")
    loadEditScript.foreach(c => println("  " + c))
    assertResult(None)(loadEditScript.welltyped(sigs, initStubs = Map((null, RootLink) -> AnyType)))

    println()

  }

  def na = TNode(TNodeName("a"), TNodeColor("red"))
  def nb = TNode(TNodeName("b"), TNodeColor("blue"))
  def nc = TNode(TNodeName("c"), TNodeColor("red"))
  def nd = TNode(TNodeName("d"), TNodeColor("blue"))

  "tree graph diff" should "compare nodes" in {
    testEditScript(
      TGraph(
        List(na, nb, nc),
        List()),
      TGraph(
        List(na, nb, nc, nd),
        List()),
      4
    )

    testEditScript(
      TGraph(
        List(na, nb, nc, nd),
        List()),
      TGraph(
        List(nd, nc, nb, na),
        List()),
      8
    )
  }

  it should "compare edges" in {
    testEditScript(
      TGraph(
        List(na, nb, nc),
        List()),
      TGraph(
        List(na, nb, nc),
        List(TEdge(na, nb), TEdge(nb, nc))),
      16
    )

    testEditScript(
      TGraph(
        List(na, nb, nc),
        List(TEdge(na, nb), TEdge(nb, nc))),
      TGraph(
        List(na, nb, nc),
        List(TEdge(na, nc), TEdge(nb, nb))),
      4
    )
  }
}
