
package truediff.compat.gumtree

import com.github.gumtreediff.actions.ActionGenerator
import com.github.gumtreediff.actions.model._
import com.github.gumtreediff.matchers.Matchers

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object MainGumTree {
  def main(args: Array[String]): Unit = try {

    val path1 = "/Users/seba/tmp/optimizers.py"
    val path2 = "/Users/seba/tmp/optimizers2.py"
    val tree1 = new PythonGumTreeGenerator().generateFromFile(path1).getRoot
    val tree2 = new PythonGumTreeGenerator().generateFromFile(path2).getRoot

    var actions: mutable.Buffer[Action] = null
    val times = ListBuffer[Double]()
    val startup = 100
    val repeat = 10

    var count = 0
    while ( count < startup + repeat) {
      val start = System.nanoTime
      val matcher = Matchers.getInstance.getMatcher(tree1, tree2)
      // Matcher matcher = new OptimizedVersions.Rtedacdef(tree1, tree2, new MappingStore());
      matcher.`match`()
      val actionGenerator = new ActionGenerator(tree1, tree2, matcher.getMappings)
      actions = actionGenerator.generate.asScala
      val end = System.nanoTime

      if (count >= startup)
        times += ((end - start).toDouble / 1000 / 1000)
      count += 1
    }

    val time = times.sum / times.size
    val nodes = tree1.getSize + tree2.getSize
    val size = actions.map {
      case _: Insert => 1
      case _: Delete => 1
      case _: Update => 2
      case _: Move => 2
    }.sum
    println("times(ms):" + time)
    println("nodes:" + nodes)
    println("throughput(nodes/ms):" + (nodes / time))
    println("patch size:" + size)
    actions.foreach(a => println("  " + a))

  } catch {
    case e: Exception =>
      e.printStackTrace()
  }
}