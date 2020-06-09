package truediff.manual

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truechange._
import truediff.Diffable

class OptionTests extends AnyFlatSpec with Matchers {
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

    var sigs: Map[NodeTag, Signature] = Map()
    src.foreachDiffable(t => sigs += t.tag -> t.sig)
    dest.foreachDiffable(t => sigs += t.tag -> t.sig)
    assertResult(None)(changeset.welltyped(sigs))

    assertResult(expectedChanges)(changeset.size)
    newtree.foreachDiffable(t => assert(t.share == null, s", share of ${t.toStringWithURI} was not reset"))
    newtree.foreachDiffable(t => assert(t.assigned == null, s", assigned of ${t.toStringWithURI} was not reset"))
  }


  "diff" should "fill and clear options" in {
    testChangeset(
      Maybe(None),
      Maybe(Some(Add(Num(1), Num(2)))),
      4
    )

    testChangeset(
      Add(Maybe(None), Num(3)),
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      4
    )

    testChangeset(
      Maybe(Some(Add(Num(1), Num(2)))),
      Maybe(None),
      3
    )

    testChangeset(
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      Add(Maybe(None), Num(3)),
      3
    )
  }

  "diff" should "load and unload options" in {
    testChangeset(
      Num(0),
      Maybe(None),
      3
    )

    testChangeset(
      Num(0),
      Maybe(Some(Add(Num(1), Num(2)))),
      6
    )

    testChangeset(
      Add(Num(0), Num(3)),
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      6
    )

    testChangeset(
      Maybe(Some(Add(Num(1), Num(2)))),
      Num(0),
      6
    )

    testChangeset(
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      Add(Num(0), Num(3)),
      6
    )
  }

}
