package truediff.compat.treesitter

import truechange.EditScript

import scala.io.Source

object Main extends App {
//  val goParser = GoLang.newParser()
////  val pyParser = new TSParser("tree-sitter-python")
//
//  val code =
//    """func hello() { "abc" }
//      |""".stripMargin
//
//  val tree = goParser.parse(code)
//  println(tree)
//  println(tree.toStringWithURI)
//  goParser.destroy()


  val pythonParser = PythonLang.newParser()

  val fooTree = pythonParser.parse(
    """ foo() + print(f'Hello {name}! This is {program}')
      |""".stripMargin)
  println(fooTree)
  println(fooTree.toStringWithURI)

  val barTree = pythonParser.parse(
    """ bar() + print(f'Hello {name}! This is {program}')
      |""".stripMargin)
  println(barTree)
  println(barTree.toStringWithURI)

  val (edits, newtree) = fooTree.compareTo(barTree)
  println(newtree)
  edits.foreach(println)


  val source1 = Source.fromFile("/Users/seba/tmp/optimizers.py")
  val tree1 = pythonParser.parse(source1.mkString)
  println(tree1)
  source1.close()

  val source2 = Source.fromFile("/Users/seba/tmp/optimizers2.py")
  val tree2 = pythonParser.parse(source2.mkString)
  println(tree2)
  source2.close()

  val editsOpt: EditScript = tree1.compareTo(tree2)._1
  editsOpt.foreach(println)
  println(editsOpt.size)

  pythonParser.destroy()
}
