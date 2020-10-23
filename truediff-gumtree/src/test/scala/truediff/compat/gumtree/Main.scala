package truediff.compat.gumtree

import java.io.File

import com.github.gumtreediff.actions.ActionGenerator
import com.github.gumtreediff.matchers.Matchers

object Main extends App {

  val projectName = "keras"
  val rootDir = new File(s"/Users/seba/tmp")

  val file1 = new File(rootDir, "tensorflow_backend1.py")
//  val xmlFile1 = new File(file1.getAbsolutePath + ".xml")
//  val xml1 = readFile(xmlFile1.getAbsolutePath)
//  val code1 = readFile(file1.getAbsolutePath).split('\n')

  val file2 = new File(rootDir, "tensorflow_backend2.py")
//  val xmlFile2 = new File(file2.getAbsolutePath + ".xml")
//  val xml2 = readFile(xmlFile2.getAbsolutePath)
//  val code2 = readFile(file2.getAbsolutePath).split('\n')

  {
    val currTree = new PythonGumTreeGenerator().generateFromFile(file2).getRoot.asInstanceOf[DiffableGumTree]
    val prevTree = new PythonGumTreeGenerator().generateFromFile(file1).getRoot.asInstanceOf[DiffableGumTree]

    val (truediffEdits, _) = prevTree.compareTo(currTree)
    println(s"Truediff: ${truediffEdits.size}")
//    truediffEdits.edits.foreach(println(_))
  }

  println()
  println("####################")
  println()

  {
    val currTree = new PythonGumTreeGenerator().generateFromFile(file2).getRoot.asInstanceOf[DiffableGumTree]
    val prevTree = new PythonGumTreeGenerator().generateFromFile(file1).getRoot.asInstanceOf[DiffableGumTree]

    val matcher = Matchers.getInstance.getMatcher(prevTree, currTree)
    matcher.`match`()
    val actionGenerator = new ActionGenerator(prevTree, currTree, matcher.getMappings)
    val (gumtreeEdits) = actionGenerator.generate
    println(s"Gumtree: ${gumtreeEdits.size}")
//    import scala.jdk.CollectionConverters._
//    gumtreeEdits.asScala.foreach(println(_))
  }
}
