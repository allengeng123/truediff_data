package truediff.macros

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truechange._
import truediff.Diffable

class OptionTests extends AnyFlatSpec with Matchers {
  def testEditScript(src: Diffable, dest: Diffable, expectedChanges: Int): Unit = {
    println("Comparing:")
    println(s"  ${src.toStringWithURI}")
    println(s"  ${dest.toStringWithURI}")

    val (editscript,newtree) = src.compareTo(dest)
    println("EditScript:")
    editscript.foreach(c => println("  " + c))
    println("New tree:")
    println("  " + newtree.toStringWithURI)
    println()

    assertResult(dest)(newtree)

    val sigs: Map[Tag, Signature] = src.collectSignatures ++ dest.collectSignatures
    assertResult(None)(editscript.welltyped(sigs))

    assertResult(expectedChanges)(editscript.size)
    newtree.foreachTree(t => assert(t.share == null, s", share of ${t.toStringWithURI} was not reset"))
    newtree.foreachTree(t => assert(t.assigned == null, s", assigned of ${t.toStringWithURI} was not reset"))

    val reverseEditScript = dest.compareTo(src)._1
    println("Reverse editscript:")
    reverseEditScript.foreach(c => println("  " + c))
    assertResult(expectedChanges)(reverseEditScript.size)

    val loadEditScript = Diffable.load(src)
    println("Load editscript:")
    loadEditScript.foreach(c => println("  " + c))
    assertResult(None)(loadEditScript.welltyped(sigs, initStubs = Map((null, RootLink) -> AnyType)))

  }


  "diff" should "fill and clear options" in {
    testEditScript(
      Maybe(None),
      Maybe(Some(Add(Num(1), Num(2)))),
      4
    )

    testEditScript(
      Add(Maybe(None), Num(3)),
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      4
    )

    testEditScript(
      Maybe(Some(Add(Num(1), Num(2)))),
      Maybe(None),
      4
    )

    testEditScript(
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      Add(Maybe(None), Num(3)),
      4
    )
  }

  "diff" should "load and unload options" in {
    testEditScript(
      Num(0),
      Maybe(Some(Add(Num(1), Num(2)))),
      7
    )

    testEditScript(
      Add(Num(0), Num(3)),
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      7
    )

    testEditScript(
      Maybe(Some(Add(Num(1), Num(2)))),
      Num(0),
      7
    )

    testEditScript(
      Add(Maybe(Some(Add(Num(1), Num(2)))), Num(3)),
      Add(Num(0), Num(3)),
      7
    )
  }

}
