package truediff.dot

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truediff.Diffable

class TestDotDiff extends AnyFlatSpec with Matchers {
  def testEditScript(src: Diffable, dest: Diffable, expectedChanges: Int): Unit = {
    println("Comparing:")
    println(s"  ${src.toStringWithURI}")
    println(s"  ${dest.toStringWithURI}")

    val (editscript, newtree) = src.compareTo(dest)
    println("EditScript:")
    editscript.foreach(c => println("  " + c))
    println("New tree:")
    println("  " + newtree.toStringWithURI)

    assertResult(dest)(newtree)

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


}
