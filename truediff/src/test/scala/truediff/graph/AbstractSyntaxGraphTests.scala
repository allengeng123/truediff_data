package truediff.graph

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import truediff.Diffable

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

    println(dest == newtree)
    assertResult(dest)(newtree)

//    val sigs: Map[Tag, Signature] = src.collectSignatures ++ dest.collectSignatures
//    assertResult(None)(editscript.welltyped(sigs))

    assertResult(expectedChanges)(editscript.coresize)
//    newtree.foreachTree(t => assert(t.share == null, s", share of ${t.toStringWithURI} was not reset"))
//    newtree.foreachTree(t => assert(t.assigned == null, s", assigned of ${t.toStringWithURI} was not reset"))

    val reverseEditScript = newtree.compareTo(src)._1
    println("Reverse editscript:")
    reverseEditScript.foreach(c => println("  " + c))
    assertResult(expectedChanges)(reverseEditScript.coresize)

    val loadEditScript = Diffable.load(src)
    println("Load editscript:")
    loadEditScript.foreach(c => println("  " + c))
//    assertResult(None)(loadEditScript.welltyped(sigs, initStubs = Map((null, RootLink) -> AnyType)))

    println()

  }

  behavior of "graph diff"

  it should "detect unchanged acyclic graph" in {
    testEditScript(
      Let(VarDecl("x"), Num(1), Let(VarDecl("y"), Num(2), UVar("x"))).resolved,
      Let(VarDecl("x"), Num(1), Let(VarDecl("y"), Num(2), UVar("x"))).resolved,
      0
    )
  }

  it should "detect changed ref in acyclic graph" in {
    testEditScript(
      Let(VarDecl("x"), Num(1), Let(VarDecl("y"), Num(2), UVar("x"))).resolved,
      Let(VarDecl("x"), Num(1), Let(VarDecl("y"), Num(2), UVar("y"))).resolved,
      1
    )
  }

  it should "detect changed decl in acyclic graph" in {
    testEditScript(
      Let(VarDecl("x"), Num(1), Let(VarDecl("y"), Num(2), UVar("x"))).resolved,
      Let(VarDecl("x"), Num(1), Let(VarDecl("z"), Num(2), UVar("x"))).resolved,
      1
    )
  }

  it should "detect consistent renaming in acyclic graph" in {
    testEditScript(
      Let(VarDecl("x"), Num(1), Let(VarDecl("y"), Num(2), UVar("x"))).resolved,
      Let(VarDecl("z"), Num(1), Let(VarDecl("y"), Num(2), UVar("z"))).resolved,
      1
    )
  }

  it should "detect unchanged cyclic graph" in {
    testEditScript(
      Lambda("x", Lambda("y", UVar("x"))).resolved,
      Lambda("x", Lambda("y", UVar("x"))).resolved,
      0
    )
  }

  it should "detect changed ref in cyclic graph" in {
    testEditScript(
      Lambda("x", Lambda("y", UVar("x"))).resolved,
      Lambda("x", Lambda("y", UVar("y"))).resolved,
      1
    )
  }

  it should "detect changed decl in cyclic graph" in {
    testEditScript(
      Lambda("x", Lambda("y", UVar("x"))).resolved,
      Lambda("x", Lambda("z", UVar("x"))).resolved,
      1
    )
  }

  it should "detect consistent renaming in cyclic graph" in {
    testEditScript(
      Lambda("x", Lambda("y", UVar("x"))).resolved,
      Lambda("z", Lambda("y", UVar("z"))).resolved,
      1
    )
  }

  it should "ASG 1" in {
    testEditScript(
      Let(VarDecl("x"), Add(Add(Num(1), Num(2)), Add(Num(3), Num(4))),
        Let(VarDecl("y"), Add(Add(Num(4), Num(3)), Add(Num(2), Num(1))),
          Add(UVar("x"), UVar("y"))
        )
      ).resolved,
      Let(VarDecl("x"), Add(Add(Num(4), Num(3)), Add(Num(2), Num(1))),
        Let(VarDecl("y"), Add(Add(Num(1), Num(2)), Add(Num(3), Num(4))),
          Add(UVar("x"), UVar("y"))
        )
      ).resolved,
      4
    )
    
  }



}
