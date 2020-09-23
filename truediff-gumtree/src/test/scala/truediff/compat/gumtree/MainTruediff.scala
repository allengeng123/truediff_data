
package truediff.compat.gumtree

import truechange.EditScript

import scala.collection.mutable.ListBuffer

object MainTruediff {
  def main(args: Array[String]): Unit = try {

    val path1 = "/Users/seba/tmp/optimizers.py"
    val path2 = "/Users/seba/tmp/optimizers2.py"

    var edits: EditScript = null
    val times = ListBuffer[Double]()
    var nodes = 0
    val startup = 100
    val repeat = 10

    var count = 0
    while ( count < startup + repeat) {
      val tree1 = new PythonGumTreeGenerator().generateFromFile(path1).getRoot.asInstanceOf[DiffableGumTree]
      val tree2 = new PythonGumTreeGenerator().generateFromFile(path2).getRoot.asInstanceOf[DiffableGumTree]
      nodes = tree1.getSize + tree2.getSize

      val start = System.nanoTime
      val (es, _) = tree1.compareTo(tree2)
      edits = es
      val end = System.nanoTime

      if (count >= startup)
        times += ((end - start).toDouble / 1000 / 1000)
      count += 1
    }

    val time = times.sum / times.size
    val size = edits.size
    println("times(ms):" + time)
    println("nodes:" + nodes)
    println("throughput(nodes/ms):" + (nodes / time))
    println("patch size:" + size)
    edits.foreach(a => println("  " + a))

  } catch {
    case e: Exception =>
      e.printStackTrace()
  }
}