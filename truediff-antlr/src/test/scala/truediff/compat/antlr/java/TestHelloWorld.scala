package truediff.compat.antlr.java

import org.scalatest.funsuite.AnyFunSuite
import truediff.compat.antlr.RuleContextMapper
import truediff.compat.antlr.java.parser.Java8Parser

class TestHelloWorld extends AnyFunSuite {


  def testDiff(path1: String, path2: String, expectedDiffSize: Int): Unit =
    test(s"diff($path1, $path2) has size $expectedDiffSize") {
      val tree1 = Java8.parseCompilationUnit(path1)
      val tree2 = Java8.parseCompilationUnit(path2)

      val mapper = new RuleContextMapper(Java8Parser.ruleNames)
      val diff1 = mapper.diffable(tree1)
      val diff2 = mapper.diffable(tree2)
      val (editScript, _) = diff1.compareTo(diff2)

      editScript.foreach(c => println("  " + c))
      assert(editScript.edits.size == expectedDiffSize)
    }

  testDiff(
    "truediff/compat/antlr/java/HelloWorld.java",
    "truediff/compat/antlr/java/HelloWorld.java",
    0)

  testDiff(
    "truediff/compat/antlr/java/HelloWorld.java",
    "truediff/compat/antlr/java/HelloWorld_renameParam.java",
    4)

  testDiff(
    "truediff/compat/antlr/java/HelloWorld.java",
    "truediff/compat/antlr/java/HelloWorld_changeStringLit.java",
    4)

  testDiff(
    "truediff/compat/antlr/java/HelloWorld_renameParam.java",
    "truediff/compat/antlr/java/HelloWorld_changeStringLit.java",
    8)

  testDiff(
    "truediff/compat/antlr/java/HelloWorld.java",
    "truediff/compat/antlr/java/HelloWorld_changedLayout.java",
    0)

  testDiff(
    "truediff/compat/antlr/java/HelloWorld.java",
    "truediff/compat/antlr/java/HelloWorld_renameClass.java",
    4)

  testDiff(
    "truediff/compat/antlr/java/HelloWorld.java",
    "truediff/compat/antlr/java/HelloWorld_extractMethod.java",
    28)

}
