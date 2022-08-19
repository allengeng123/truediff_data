package truediff.manual

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truechange._
import truediff.Diffable

class ListTests extends AnyFlatSpec with Matchers {
  def testEditScript(src: Diffable, dest: Diffable, expectedChanges: Int): Unit = {
    println("Comparing:")
    println(s"  ${src.toStringWithURI}")
    println(s"  ${dest.toStringWithURI}")

    val (editscript,newtree) = src.compareTo(dest)
    println("EditScript:")
    editscript.foreach(c => println("  " + c))
    println("Core EditScript:")
    editscript.coreEdits.foreach(c => println("  " + c))
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


  "diff" should "fill and clear lists" in {
    // load 1, attach 1 to many.es
    testEditScript(
      Many(Nil),
      Many(Num(1) :: Nil),
      2
    )

    // load 1, load 2, attach 1 to many.es, attach 2 to 1.next
    testEditScript(
      Many(Nil),
      Many(Num(1) :: Num(2) :: Nil),
      4
    )

    testEditScript(
      Add(Many(Nil), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      4
    )

    testEditScript(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Nil),
      4
    )

    testEditScript(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Many(Nil), Num(3)),
      4
    )
  }

  "diff" should "prune and extend lists" in {
    // load 2, attach 2 to 1.next
    testEditScript(
      Many(Num(1) :: Nil),
      Many(Num(1) :: Num(2) :: Nil),
      2
    )

    // load 2, attach 2 to 1.next
    testEditScript(
      Add(Many(Num(1) :: Nil), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      2
    )

    // detach 2 from many, load 1, attach 1 to many, attach 2 to 1.next
    testEditScript(
      Many(Num(2) :: Nil),
      Many(Num(1) :: Num(2) :: Nil),
      3
    )

    // detach 2 from many, load 1, attach 1 to many, attach 2 to 1.next
    testEditScript(
      Add(Many(Num(2) :: Nil), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      3
    )

    // load 2, detach 3 from 1.next, attach 2 to 1.next, attach 3 to 2.next
    testEditScript(
      Many(Num(1) :: Num(3) :: Nil),
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      3
    )

    // load 2, detach 3 from 1.next, attach 2 to 1.next, attach 3 to 2.next
    testEditScript(
      Add(Many(Num(1) :: Num(3) :: Nil), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Num(3) :: Nil), Num(3)),
      3
    )

    // unload 2
    testEditScript(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Num(1) :: Nil),
      2
    )

    // unload 2
    testEditScript(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Many(Num(1) :: Nil), Num(3)),
      2
    )

    // detach 2 from 1.next, unload 1, attach 2 to many
    testEditScript(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Num(2) :: Nil),
      3
    )

    // detach 2 from 1.next, unload 1, attach 2 to many
    testEditScript(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Many(Num(2) :: Nil), Num(3)),
      3
    )

    // detach 3 from 2.next, unload 2, attach 3 to 1.next
    testEditScript(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(1) :: Num(3) :: Nil),
      3
    )

    // detach 3 from 2.next, unload 2, attach 3 to 1.next
    testEditScript(
      Add(Many(Num(1) :: Num(2) :: Num(3) :: Nil), Num(3)),
      Add(Many(Num(1) :: Num(3) :: Nil), Num(3)),
      3
    )

     // load 3, attach 3 to 2.next
    testEditScript(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      2
    )

    // detach 2 from many, load 1, attach 1 to many, attach 2 to 1
    testEditScript(
      Many(Num(2) :: Num(3) :: Nil),
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      4
    )

    // detach 3 from 1.next, load 2, attach 3 to 2.next, attach 2 to 1.next
    testEditScript(
      Many(Num(1) :: Num(3) :: Num(4) :: Nil),
      Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil),
      4
    )


    testEditScript(
      Many(Num(1) :: Num(2) :: Nil),
      Many(Num(3) :: Num(1) :: Nil),
      2
    )
  }

  "diff" should "replace lists" in {
//    testEditScript(
//      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
//      Many(Num(4) :: Num(5) :: Num(6) :: Nil),
//      3
//    )
//
//    testEditScript(
//      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
//      Many(Num(4) :: Num(5) :: Num(3) :: Nil),
//      2
//    )

    testEditScript(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(4) :: Num(5) :: Num(6) :: Num(7) :: Nil),
      5
    )

    testEditScript(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(3) :: Num(4) :: Num(5) :: Num(6) :: Nil),
      5
    )

    testEditScript(
      Many(Num(1) :: Num(2) :: Num(3) :: Nil),
      Many(Num(4) :: Num(3) :: Num(2) :: Nil),
      3
    )

    testEditScript(
      Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil),
      Many(Num(2) :: Num(3) :: Num(5) :: Nil),
      5
    )
  }

  "diff" should "load and unload lists" in {
    testEditScript(
      Num(0),
      Many(Num(1) :: Num(2) :: Nil),
      8
    )

    testEditScript(
      Add(Num(0), Num(3)),
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      8
    )

    testEditScript(
      Many(Num(1) :: Num(2) :: Nil),
      Num(0),
      8
    )

    testEditScript(
      Add(Many(Num(1) :: Num(2) :: Nil), Num(3)),
      Add(Num(0), Num(3)),
      8
    )
  }

  "diff" should "work across lists" in {
    testEditScript(
      Add(Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil), Many(Nil)),
      Add(Many(Nil), Many(Num(2) :: Num(3) :: Nil)),
      14
    )
  }

  "diff" should "move list elements in and out of lists" in {
    testEditScript(
      Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil),
      Add(Add(Num(1), Num(2)), Add(Num(4), Num(3))),
      11
    )

    testEditScript(
      Add(Add(Num(1), Num(2)), Add(Num(4), Num(3))),
      Many(Num(1) :: Num(2) :: Num(3) :: Num(4) :: Nil),
      11
    )
  }
}
