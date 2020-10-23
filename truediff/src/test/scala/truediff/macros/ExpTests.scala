package truediff.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truechange._
import truediff.Diffable
import truediff.macros.Exp.Hole

class ExpTests extends AnyFlatSpec with Matchers {
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


  "diff" should "fill and create holes" in {
    testEditScript(
      Hole(),
      Add(Num(1), Num(2)),
      6
    )

    testEditScript(
      Add(Hole(), Num(3)),
      Add(Add(Num(1), Num(2)), Num(3)),
      6
    )

    testEditScript(
      Add(Num(1), Num(2)),
      Hole(),
      6
    )

    testEditScript(
      Add(Add(Num(1), Num(2)), Num(3)),
      Add(Hole(), Num(3)),
      6
    )
  }

  "diff" should "leave subtrees in place" in {
    // should yield editscript:
    //   [detach 10, unload 10, load 13, attach 13]
    testEditScript(
      Add(Num(10), Num(13)),
      Add(Num(13), Num(13)),
      4
    )

    // should yield editscript:
    //   [unload 10 from x, load 13 as y, attach y to x]
    testEditScript(
      Add(Num(13), Num(10)),
      Add(Num(13), Num(13)),
      4
    )
  }

  "diff" should "leave subtrees intact" in {
    // should yield editscript:
    //   [detach 1, unload 1 from x, load 2 as y, attach y to x,
    //    unload z, unload 3, detach (2+3) from z, attach (2+3) to x]
    testEditScript(
      Add(Num(1), Add(Num(3), Add(Num(2), Num(3)))),
      Add(Num(2), Add(Num(2), Num(3))),
      4 + 4
    )

    // [detach Add(a,b), detach a, unload a,
    //  load b, attach b, attach Add(a,b)]
    testEditScript(
      Add(Add(Var("a"), Var("b")), Var("c")),
      Add(Var("a"), Add(Var("a"), Var("b"))),
      6
    )

    // Note that the tree in which `2` occurs is higher than the one of `2+3`.
    // This test requires a piecewise height-first traversal of subtrees, such that `2+3` is visited before `2`.
    // [detach 1, unload 1,
    //  detach (3+(2+3)), unload (3+(2+3)), unload 3,
    //  load 2, load 0, load 0, load (0+0), load (2+(0+0)),
    //  attach (2+(0+0)), attach (2+3)]
    testEditScript(
      Add(Num(1), Add(Num(3), Add(Num(2), Num(3)))),
      Add(Add(Num(2), Add(Num(0), Num(0))), Add(Num(2), Num(3))),
      12
    )
  }

  "diff" should "reuse all subtrees" in {
    // should yield editscript:
    //   [detach 2, unload 3, unload 2+3 from x, attach 2 to x]
    testEditScript(
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      Add(Add(Num(2), Num(3)), Num(2)),
      4
    )

    // should yield editscript:
    //   [detach 2, unload 3, unload 2+3 from x, attach 2 to x]
    testEditScript(
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      Add(Num(2), Add(Num(2), Num(3))),
      4
    )

    // should yield editscript:
    //   [detach 2 from x, detach 3 from y, detach (2+3) from y,
    //    attach (2+3) to x, attach 2 to y, attach 3 to y]
    testEditScript(
      Add(Num(2), Add(Num(3), Add(Num(2), Num(3)))),
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      6
    )
  }

  "diff" should "wrap subtrees" in {
    // should yield editscript:
    //   [detach 2+3, load 1, load 1 + (2+3), attach 1 + (2+3)]
    testEditScript(
      Add(Num(2), Num(3)),
      Add(Num(1), Add(Num(2), Num(3))),
      4
    )

    // should yield editscript:
    //   [detach 2+3, load 1, load 1 + (2+3), attach 1 + (2+3)]
    testEditScript(
      Add(Num(2), Num(3)),
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      6
    )
  }


  "diff" should "support example from paper" in {
    testEditScript(
      Add(Sub(Var("a"), Num(1)), Mul(Var("b"), Var("c"))),
      Add(Var("c"), Mul(Var("b"), Sub(Var("a"), Num(1)))),
      4
    )
  }
}
