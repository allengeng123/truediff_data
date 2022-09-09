package truediff.graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truediff.{Diffable, Ref}

class AbstractSyntaxGraphTests extends AnyFlatSpec with Matchers {
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

  "graph diff" should "ASG 1" in {
    testEditScript(
      Let("x", Add(Add(Num(1), Num(2)), Add(Num(3), Num(4))),
        Let("y", Add(Add(Num(4), Num(3)), Add(Num(2), Num(1))),
          Add(UVar("x"), UVar("y"))
        )
      ).resolved,
      Let("x", Add(Add(Num(4), Num(3)), Add(Num(2), Num(1))),
        Let("y", Add(Add(Num(1), Num(2)), Add(Num(3), Num(4))),
          Add(UVar("x"), UVar("y"))
        )
      ).resolved,
      4
    )
    

    val cfg1 = makeCfg(parse(file1))
    val cfg2 = makeCfg(parse(file2))

    cfg1.compareTo(cfg2)

  }



}
