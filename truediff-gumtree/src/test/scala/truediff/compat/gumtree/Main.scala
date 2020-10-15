package truediff.compat.gumtree

import java.io.File

object Main extends App {

  val projectName = "keras"
  val rootDir = new File(s"benchmark/python_$projectName")

  val file1 = new File(rootDir, "keras-108-61052bc1/cntk_backend.py")
//  val xmlFile1 = new File(file1.getAbsolutePath + ".xml")
//  val xml1 = readFile(xmlFile1.getAbsolutePath)
//  val code1 = readFile(file1.getAbsolutePath).split('\n')

  val file2 = new File(rootDir, "keras-110-f6bdacde/cntk_backend.py")
//  val xmlFile2 = new File(file2.getAbsolutePath + ".xml")
//  val xml2 = readFile(xmlFile2.getAbsolutePath)
//  val code2 = readFile(file2.getAbsolutePath).split('\n')

  val currTree = new PythonGumTreeGenerator().generateFromFile(file1).getRoot.asInstanceOf[DiffableGumTree]
  val prevTree = new PythonGumTreeGenerator().generateFromFile(file2).getRoot.asInstanceOf[DiffableGumTree]

  val (edits, _) = currTree.compareTo(prevTree)
  println(edits.size)
//  edits.edits.foreach(println(_))
}
