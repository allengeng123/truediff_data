package truediff.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truechange._
import truediff.Diffable
import truediff.macros.Exp.Hole

class ExpTests extends AnyFlatSpec with Matchers {
  def testEditscript(src: Diffable, dest: Diffable, expectedChanges: Int): Unit = {
    println("Comparing:")
    println(s"  ${src.toStringWithURI}")
    println(s"  ${dest.toStringWithURI}")

    val (changeset,newtree) = src.compareTo(dest)
    println("Editscript:")
    changeset.foreach(c => println("  " + c))
    println("New tree:")
    println("  " + newtree.toStringWithURI)

    assertResult(dest)(newtree)

    var sigs: Map[Tag, Signature] = Map(RootTag -> RootSig)
    src.foreachTree(t => sigs += t.tag -> t.sig)
    dest.foreachTree(t => sigs += t.tag -> t.sig)
    assertResult(None)(changeset.welltyped(sigs))

    assertResult(expectedChanges)(changeset.size)
    newtree.foreachTree(t => assert(t.share == null, s", share of ${t.toStringWithURI} was not reset"))
    newtree.foreachTree(t => assert(t.assigned == null, s", assigned of ${t.toStringWithURI} was not reset"))

    val reverseEditscript = dest.compareTo(src)._1
    println("Reverse changeset:")
    reverseEditscript.foreach(c => println("  " + c))
    assertResult(expectedChanges)(reverseEditscript.size)

    val loadEditscript = Diffable.load(src)
    println("Load changeset:")
    loadEditscript.foreach(c => println("  " + c))
    assertResult(None)(loadEditscript.welltyped(sigs, initStubs = Map((null, RootLink) -> AnyType)))

    println()

  }


  "diff" should "fill and create holes" in {
    testEditscript(
      Hole(),
      Add(Num(1), Num(2)),
      6
    )

    testEditscript(
      Add(Hole(), Num(3)),
      Add(Add(Num(1), Num(2)), Num(3)),
      6
    )

    testEditscript(
      Add(Num(1), Num(2)),
      Hole(),
      6
    )

    testEditscript(
      Add(Add(Num(1), Num(2)), Num(3)),
      Add(Hole(), Num(3)),
      6
    )
  }

  "diff" should "leave subtrees in place" in {
    // should yield changeset:
    //   [detach 10, unload 10, load 13, attach 13]
    testEditscript(
      Add(Num(10), Num(13)),
      Add(Num(13), Num(13)),
      4
    )

    // should yield changeset:
    //   [unload 10 from x, load 13 as y, attach y to x]
    testEditscript(
      Add(Num(13), Num(10)),
      Add(Num(13), Num(13)),
      4
    )
  }

  "diff" should "leave subtrees intact" in {
    // should yield changeset:
    //   [detach 1, unload 1 from x, load 2 as y, attach y to x,
    //    unload z, unload 3, detach (2+3) from z, attach (2+3) to x]
    testEditscript(
      Add(Num(1), Add(Num(3), Add(Num(2), Num(3)))),
      Add(Num(2), Add(Num(2), Num(3))),
      4 + 4
    )

    // [detach Add(a,b), detach a, unload a,
    //  load b, attach b, attach Add(a,b)]
    testEditscript(
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
    testEditscript(
      Add(Num(1), Add(Num(3), Add(Num(2), Num(3)))),
      Add(Add(Num(2), Add(Num(0), Num(0))), Add(Num(2), Num(3))),
      12
    )
  }

  "diff" should "reuse all subtrees" in {
    // should yield changeset:
    //   [detach 2, unload 3, unload 2+3 from x, attach 2 to x]
    testEditscript(
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      Add(Add(Num(2), Num(3)), Num(2)),
      4
    )

    // should yield changeset:
    //   [detach 2, unload 3, unload 2+3 from x, attach 2 to x]
    testEditscript(
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      Add(Num(2), Add(Num(2), Num(3))),
      4
    )

    // should yield changeset:
    //   [detach 2 from x, detach 3 from y, detach (2+3) from y,
    //    attach (2+3) to x, attach 2 to y, attach 3 to y]
    testEditscript(
      Add(Num(2), Add(Num(3), Add(Num(2), Num(3)))),
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      6
    )
  }

  "diff" should "wrap subtrees" in {
    // should yield changeset:
    //   [detach 2+3, load 1, load 1 + (2+3), attach 1 + (2+3)]
    testEditscript(
      Add(Num(2), Num(3)),
      Add(Num(1), Add(Num(2), Num(3))),
      4
    )

    // should yield changeset:
    //   [detach 2+3, load 1, load 1 + (2+3), attach 1 + (2+3)]
    testEditscript(
      Add(Num(2), Num(3)),
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      6
    )
  }


  "diff" should "support example from paper" in {
    testEditscript(
      Add(Sub(Var("a"), Num(1)), Mul(Var("b"), Var("c"))),
      Add(Var("c"), Mul(Var("b"), Sub(Var("a"), Num(1)))),
      4
    )
  }
}
