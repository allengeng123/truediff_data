package truediff.compat.gumtree

import java.io.File

import com.github.gumtreediff.actions.ActionGenerator
import com.github.gumtreediff.actions.model.{Delete, Update}
import com.github.gumtreediff.matchers.Matchers
import truechange.{Attach, Detach, DetachUnload, Unload}

object Main extends App {

  val projectName = "keras"

//  val rootDir = new File(s"/Users/seba/tmp")
//  val file1 = new File(rootDir, "tensorflow_backend1.py")
//  val file2 = new File(rootDir, "tensorflow_backend2.py")

  val rootDir = new File(s"/Users/seba/projects/truediff/benchmark/python_keras")
  val file1 = new File(rootDir, "keras-7-7a39b6c6/recurrent.py")
  val file2 = new File(rootDir, "keras-6-b5cb82c6/recurrent.py")

  val truediff = {
    val currTree = new PythonGumTreeGenerator().generateFromFile(file2).getRoot.asInstanceOf[DiffableGumTree]
    val prevTree = new PythonGumTreeGenerator().generateFromFile(file1).getRoot.asInstanceOf[DiffableGumTree]
//    val xml2 = BenchmarkUtils.readFile(file2.getAbsolutePath + ".xml")
//    val xml1 = BenchmarkUtils.readFile(file1.getAbsolutePath + ".xml")
//    val currTree = new PythonGumTreeGenerator().generateFromXml(xml2, null).getRoot.asInstanceOf[DiffableGumTree]
//    val prevTree = new PythonGumTreeGenerator().generateFromXml(xml1, null).getRoot.asInstanceOf[DiffableGumTree]

    val (truediffEdits, _) = prevTree.compareTo(currTree)
    truediffEdits.edits.foreach(println(_))
    truediffEdits
  }

  println()
  println("####################")
  println()

  val gumtree = {
    val currTree = new PythonGumTreeGenerator().generateFromFile(file2).getRoot.asInstanceOf[DiffableGumTree]
    val prevTree = new PythonGumTreeGenerator().generateFromFile(file1).getRoot.asInstanceOf[DiffableGumTree]
//    val xml2 = BenchmarkUtils.readFile(file2.getAbsolutePath + ".xml")
//    val xml1 = BenchmarkUtils.readFile(file1.getAbsolutePath + ".xml")
//    val currTree = new PythonGumTreeGenerator().generateFromXml(xml2, null).getRoot.asInstanceOf[DiffableGumTree]
//    val prevTree = new PythonGumTreeGenerator().generateFromXml(xml1, null).getRoot.asInstanceOf[DiffableGumTree]

    val matcher = Matchers.getInstance.getMatcher(prevTree, currTree)
    matcher.`match`()
    val actionGenerator = new ActionGenerator(prevTree, currTree, matcher.getMappings)
    val (gumtreeEdits) = actionGenerator.generate
    import scala.jdk.CollectionConverters._
    gumtreeEdits.asScala.foreach(println(_))
    gumtreeEdits.asScala
  }

  println()
  println("####################")
  println()
  println(s"Truediff: ${truediff.size}")
  println(s"Truediff neg: ${truediff.edits.count{
    case _:Unload => true
    case _:Detach => true
    case _:DetachUnload => true
    case _ => false
  }}")
  println(s"Gumtree: ${gumtree.size}")
  println(s"Gumtree neg: ${gumtree.count {
    case _:Delete => true; case _ => false
  }}")
  println(s"Gumtree upd: ${gumtree.count {
    case _:Update => true; case _ => false
  }}")

  val attachs = truediff.edits.flatMap {
    case Attach(node, _, _, _, _) => Some(node)
    case _ => None
  }.toSet
  val detachs = truediff.edits.flatMap {
    case Detach(node, _, _, _, _) => Some(node)
    case _ => None
  }
  val moves = detachs.count(attachs.contains)
  println(s"Missed moves: $moves")
}
