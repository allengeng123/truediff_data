package truediff.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truechange._
import truediff.Diffable

class ListTests extends AnyFlatSpec with Matchers {
  def testChangeset(src: Diffable, dest: Diffable, expectedChanges: Int): Unit = {
    println("Comparing:")
    println(s"  ${src.toStringWithURI}")
    println(s"  ${dest.toStringWithURI}")

    val (changeset,newtree) = src.compareTo(dest)
    println("Changeset:")
    changeset.foreach(c => println("  " + c))
    println("New tree:")
    println("  " + newtree.toStringWithURI)
    println()

    assertResult(dest)(newtree)

    var sigs: Map[NodeTag, Signature] = Map(RootTag -> RootSig)
    src.foreachDiffable(t => sigs += t.tag -> t.sig)
    dest.foreachDiffable(t => sigs += t.tag -> t.sig)
    assertResult(None)(changeset.welltyped(sigs))

    assertResult(expectedChanges)(changeset.size)
    newtree.foreachDiffable(t => assert(t.share == null, s", share of ${t.toStringWithURI} was not reset"))
    newtree.foreachDiffable(t => assert(t.assigned == null, s", assigned of ${t.toStringWithURI} was not reset"))
  }


  "diff" should "fill and clear lists" in {
    // load 1, attach 1 to many.es
    testChangeset(
      Many(Nil),
      Many(Num(1) :: Nil),
      2
    )

    // load 1, load 2, attach 1 to many.es, attach 2 to 1.next
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
      4
    )

    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Many(Nil), Num(3)),
      4
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
      2
    )

    // unload 2
    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Many(Num(1) :: Nil), Num(3)),
      2
    )

    // detach 2 from 1.next, unload 1, attach 2 to many
    testChangeset(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Num(2) :: Nil),
      4
    )

    // detach 2 from 1.next, unload 1, attach 2 to many
    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Many(Num(2) :: Nil), Num(3)),
      4
    )

    // detach 3 from 2.next, unload 2, attach 3 to 1.next
    testChangeset(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(1) :: Num(3) :: Nil),
      4
    )

    // detach 3 from 2.next, unload 2, attach 3 to 1.next
    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Num(3) :: Nil), Num(3)),
      Add(Many(Num(1) :: Num(3) :: Nil), Num(3)),
      4
    )

    // load 3, attach 3 to 2.next
    testChangeset(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      2
    )

    // detach 2 from many, load 1, attach 1 to many, attach 2 to 1
    testChangeset(
      Many(Num(2) :: Num(3) :: Nil),
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      4
    )

    // detach 3 from 1.next, load 2, attach 3 to 2.next, attach 2 to 1.next
    testChangeset(
      Many(Num(1) :: Num(3) :: Num(4) :: Nil),
      Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil),
      4
    )
  }

  "diff" should "replace lists" in {
    testChangeset(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(4) :: Num(5) :: Num(6) :: Nil),
      12
    )

    testChangeset(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(4) :: Num(5) :: Num(3) :: Nil),
      10
    )

    testChangeset(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(4) :: Num(5) :: Num(6) :: Num(7) :: Nil),
      14
    )

    testChangeset(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(3) :: Num(4) :: Num(5) :: Num(6) :: Nil),
      12
    )

    testChangeset(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(4) :: Num(3) :: Num(2) :: Nil),
      8
    )

    testChangeset(
      Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil),
      Many(Num(2) :: Num(3) :: Num(5) :: Nil),
      8
    )
  }

  "diff" should "load and unload lists" in {
    testChangeset(
      Num(0),
      Many(Num(1) :: Num(2) :: Nil),
      9
    )

    testChangeset(
      Add(Num(0), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      9
    )

    testChangeset(
      Many(Num(1) :: Num(2) :: Nil),
      Num(0),
      9
    )

    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Num(0), Num(3)),
      9
    )
  }

  "diff" should "work across lists" in {
    testChangeset(
      Add(Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil), Many(Nil)),
      Add(Many(Nil), Many(Num(2) :: Num(3) :: Nil)),
      14
    )
  }

  "diff" should "move list elements in and out of lists" in {
    testChangeset(
      Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil),
      Add(Add(Num(1), Num(2)), Add(Num(4), Num(3))),
      11
    )

    testChangeset(
      Add(Add(Num(1), Num(2)), Add(Num(4), Num(3))),
      Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil),
      11
    )
  }
}
