package truediff.python

import truechange.EditScript
import truediff.util.BenchmarkUtils
import truediff.util.BenchmarkUtils.{Timing, ms}

object Main extends App {

  val path1 = "/Users/seba/tmp/optimizers.py"
  val path2 = "/Users/seba/tmp/optimizers2.py"

  implicit val timing = Timing(discard = 0, repeat = 1)

  val (trees, es, _, times) = BenchmarkUtils.timed[(Ast.file, Ast.file), EditScript]({ () =>
    val tree1 = Statements.parse(BenchmarkUtils.readFile(path1))
    val tree2 = Statements.parse(BenchmarkUtils.readFile(path2))
    (tree1, tree2)
  }, { case (tree1, tree2) =>
    val (es, _) = tree1.compareTo(tree2)
    es
  })

  private val size: Int = trees._1.treesize + trees._2.treesize
  private val runningTime: Double = ms(times.sum / times.length)
  println("time(ms):" + runningTime)
  println("nodes:" + size)
  println("throughput(nodes/ms):" + (size.toDouble / runningTime.toDouble))
  println("patch size:" + es.size)
  es.foreach(e => println(s"  $e"))
  println("core edits:")
  es.coreEdits.foreach(e => println(s"  $e"))
  es.welltyped(trees._1.collectSignatures).foreach(m => throw new IllegalStateException(s"Ill-typed edit script: $m"))
}
