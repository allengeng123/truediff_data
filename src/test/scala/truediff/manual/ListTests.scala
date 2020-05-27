package truediff.manual

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truediff.diffable.Diffable

class ListTests extends AnyFlatSpec with Matchers {
  def testChangeset(src: Diffable, dest: Diffable, expectedChanges: Int): Unit = {
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
//    assertResult(None)(changeset.welltyped)
    assertResult(expectedChanges)(changeset.size)
    newtree.foreachDiffable(t => assert(t.share == null, s", share of ${t.toStringWithURI} was not reset"))
    newtree.foreachDiffable(t => assert(t.assigned == null, s", assigned of ${t.toStringWithURI} was not reset"))
  }


  "diff" should "fill and clear lists" in {
    testChangeset(
      Many(Nil),
      Many(Num(1) :: Num(2) :: Nil),
      4
    )

    testChangeset(
      Add(Many(Nil), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      4
    )

    testChangeset(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Nil),
      2
    )

    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Many(Nil), Num(3)),
      2
    )
  }

  "diff" should "prune and extend lists" in {
    // load 2, attach 2 to 1.next
    testChangeset(
      Many(Num(1) :: Nil),
      Many(Num(1) :: Num(2) :: Nil),
      2
    )

    // load 2, attach 2 to 1.next
    testChangeset(
      Add(Many(Num(1) :: Nil), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      2
    )

    // detach 2 from many, load 1, attach 1 to many, attach 2 to 1.next
    testChangeset(
      Many(Num(2) :: Nil),
      Many(Num(1) :: Num(2) :: Nil),
      4
    )

    // detach 2 from many, load 1, attach 1 to many, attach 2 to 1.next
    testChangeset(
      Add(Many(Num(2) :: Nil), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      4
    )

    // load 2, detach 3 from 1.next, attach 2 to 1.next, attach 3 to 2.next
    testChangeset(
      Many(Num(1) :: Num(3) :: Nil),
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      4
    )

    // load 2, detach 3 from 1.next, attach 2 to 1.next, attach 3 to 2.next
    testChangeset(
      Add(Many(Num(1) :: Num(3) :: Nil), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Num(3) :: Nil), Num(3)),
      4
    )

    // unload 2
    testChangeset(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Num(1) :: Nil),
      1
    )

    // unload 2
    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Many(Num(1) :: Nil), Num(3)),
      1
    )

    // detach 2 from 1.next, unload 2, attach 2 to many
    testChangeset(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Num(2) :: Nil),
      3
    )

    // detach 2 from 1.next, unload 2, attach 2 to many
    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Many(Num(2) :: Nil), Num(3)),
      3
    )

    // detach 3 from 2.next, unload 2, attach 3 to 1.next
    testChangeset(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(1) :: Num(3) :: Nil),
      3
    )

    // detach 3 from 2.next, unload 2, attach 3 to 1.next
    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Num(3) :: Nil), Num(3)),
      Add(Many(Num(1) :: Num(3) :: Nil), Num(3)),
      3
    )

  }

  "diff" should "load and unload lists" in {
    testChangeset(
      Num(0),
      Many(Num(1) :: Num(2) :: Nil),
      6
    )

    testChangeset(
      Add(Num(0), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      6
    )

    testChangeset(
      Many(Num(1) :: Num(2) :: Nil),
      Num(0),
      5
    )

    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Num(0), Num(3)),
      5
    )
  }

}
