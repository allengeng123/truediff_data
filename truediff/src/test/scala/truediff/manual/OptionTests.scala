package truediff.manual

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truechange._
import truediff.Diffable

class OptionTests extends AnyFlatSpec with Matchers {
  def testEditscript(src: Diffable, dest: Diffable, expectedChanges: Int): Unit = {
    println("Comparing:")
    println(s"  ${src.toStringWithURI}")
    println(s"  ${dest.toStringWithURI}")

    val (changeset,newtree) = src.compareTo(dest)
    println("Editscript:")
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

    val reverseEditscript = dest.compareTo(src)._1
    println("Reverse changeset:")
    reverseEditscript.foreach(c => println("  " + c))
    assertResult(expectedChanges)(reverseEditscript.size)

    val loadEditscript = Diffable.load(src)
    println("Load changeset:")
    loadEditscript.foreach(c => println("  " + c))
    assertResult(None)(loadEditscript.welltyped(sigs, initStubs = Map((null, RootLink) -> AnyType)))

  }


  "diff" should "fill and clear options" in {
    testEditscript(
      Maybe(None),
      Maybe(Some(Add(Num(1), Num(2)))),
      4
    )

    testEditscript(
      Add(Maybe(None), Num(3)),
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      4
    )

    testEditscript(
      Maybe(Some(Add(Num(1), Num(2)))),
      Maybe(None),
      4
    )

    testEditscript(
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      Add(Maybe(None), Num(3)),
      4
    )
  }

  "diff" should "load and unload options" in {
    testEditscript(
      Num(0),
      Maybe(None),
      4
    )

    testEditscript(
      Num(0),
      Maybe(Some(Add(Num(1), Num(2)))),
      7
    )

    testEditscript(
      Add(Num(0), Num(3)),
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      7
    )

    testEditscript(
      Maybe(Some(Add(Num(1), Num(2)))),
      Num(0),
      7
    )

    testEditscript(
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      Add(Num(0), Num(3)),
      7
    )
  }

}
