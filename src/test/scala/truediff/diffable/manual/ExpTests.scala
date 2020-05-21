package truediff.diffable.manual

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExpTests extends AnyFlatSpec with Matchers {
  def testChangeset(src: Exp, dest: Exp, expectedChanges: Int): Unit = {
    println("Comparing:")
    println(s"  ${src.toStringWithURI}")
    println(s"  ${dest.toStringWithURI}")

    val (changeset,newtree) = src.compareTo(dest)
    println("Changeset:")
    changeset.cmds.foreach(c => println("  " + c))
    println("New tree:")
    println("  " + newtree.toStringWithURI)
    println()

    assertResult(dest)(newtree)
    assertResult(None)(changeset.welltyped)
    assertResult(expectedChanges)(changeset.size)
    newtree.foreachDiffable(t => assert(t.share == null, s", share of ${t.toStringWithURI} was not reset"))
    newtree.foreachDiffable(t => assert(t.assigned == null, s", assigned of ${t.toStringWithURI} was not reset"))
  }

  "diff" should "leave subtrees in place" in {
    // should yield changeset:
    //   [unload 10 from x, load 13 as y, attach y to x]
    testChangeset(
      Add(Num(10), Num(13)),
      Add(Num(13), Num(13)),
      3
    )

    // should yield changeset:
    //   [unload 10 from x, load 13 as y, attach y to x]
    testChangeset(
      Add(Num(13), Num(10)),
      Add(Num(13), Num(13)),
      3
    )
  }

  "diff" should "leave subtrees intact" in {
    // should yield changeset:
    //   [unload 1 from x, load 2 as y, attach y to x,
    //    unload 3 from z, detach (2+3) from z, unload z, attach (2+3) to x]
    testChangeset(
      Add(Num(1), Add(Num(3), Add(Num(2), Num(3)))),
      Add(Num(2), Add(Num(2), Num(3))),
      3 + 4
    )
  }

  "diff" should "reuse all subtrees" in {
    // should yield changeset:
    //   [detach 2, unload 3, unload 2+3 from x, attach 2 to x]
    testChangeset(
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      Add(Add(Num(2), Num(3)), Num(2)),
      4
    )

    // should yield changeset:
    //   [detach 2, unload 3, unload 2+3 from x, attach 2 to x]
    testChangeset(
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      Add(Num(2), Add(Num(2), Num(3))),
      4
    )

    // should yield changeset:
    //   [detach 2 from x, detach 3 from y, detach (2+3) from y,
    //    attach (2+3) to x, attach 2 to y, attach 3 to y]
    testChangeset(
      Add(Num(2), Add(Num(3), Add(Num(2), Num(3)))),
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      6
    )
  }

  "diff" should "wrap subtrees" in {
    // should yield changeset:
    //   [detach 2+3, load 1, load 1 + (2+3), attach 1 + (2+3)]
    testChangeset(
      Add(Num(2), Num(3)),
      Add(Num(1), Add(Num(2), Num(3))),
      4
    )

    // should yield changeset:
    //   [detach 2+3, load 1, load 1 + (2+3), attach 1 + (2+3)]
    testChangeset(
      Add(Num(2), Num(3)),
      Add(Add(Num(2), Num(3)), Add(Num(2), Num(3))),
      6
    )

  }
}
