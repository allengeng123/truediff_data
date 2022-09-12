package truediff.graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truediff.Diffable
import truediff.Ref

class GraphTests extends AnyFlatSpec with Matchers {
  def testEditScript(src: Diffable, dest: Diffable, expectedChanges: Int): Unit = {
    println("Comparing:")
    println(s"  ${src.toStringWithURI}")
    println(s"  ${dest.toStringWithURI}")

    val (editscript,newtree) = src.compareTo(dest)
    println("EditScript:")
    editscript.foreach(c => println("  " + c))
    println("New tree:")
    println("  " + newtree.toStringWithURI)

    assert(dest.referenceEquality(newtree))

//    val sigs: Map[Tag, Signature] = src.collectSignatures ++ dest.collectSignatures
//    assertResult(None)(editscript.welltyped(sigs))

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
//    assertResult(None)(loadEditScript.welltyped(sigs, initStubs = Map((null, RootLink) -> AnyType)))

    println()

  }

  def na = Node(NodeName("a"), NodeColor("red"))
  def nb = Node(NodeName("b"), NodeColor("blue"))
  def nc = Node(NodeName("c"), NodeColor("red"))
  def nd = Node(NodeName("d"), NodeColor("blue"))

  "graph diff" should "compare nodes" in {
    testEditScript(
      Graph(
        List(na, nb, nc),
        List()),
      Graph(
        List(na, nb, nc, nd),
        List()),
      4
    )

    testEditScript(
      Graph(
        List(na, nb, nc, nd),
        List()),
      Graph(
        List(nd, nc, nb, na),
        List()),
      8
    )
  }

  it should "compare edges" in {
    val a = na
    val b = nb
    val c = nc
    val d = nd

    testEditScript(
      Graph(
        List(a, b, c),
        List()),
      Graph(
        List(a, b, c),
        List(Edge(Ref(a), Ref(b)), Edge(Ref(b), Ref(c)))),
      8
    )

    testEditScript(
      Graph(
        List(a, b, c),
        List(Edge(Ref(a), Ref(b)), Edge(Ref(b), Ref(c)))),
      Graph(
        List(a, b, c),
        List(Edge(Ref(a), Ref(c)), Edge(Ref(b), Ref(b)))),
      2
    )
  }

  it should "compare nodes and edges" in {
    val a = na
    val b = nb
    val c = nc
    val d = nd

    testEditScript(
      Graph(
        List(a, b, c, d),
        List(Edge(Ref(a), Ref(b)), Edge(Ref(c), Ref(d)))),
      Graph(
        List(a, b, c, d),
        List(Edge(Ref(a), Ref(b)), Edge(Ref(d), Ref(c)))),
      2
    )
  }
}
