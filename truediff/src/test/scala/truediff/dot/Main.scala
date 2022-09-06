package truediff.dot

import truechange.EditScript
import truediff.util.BenchmarkUtils
import truediff.util.BenchmarkUtils.{Timing, ms}

object Main extends App {

  val path1 = "/Users/seba/tmp/const.dot"
  val path2 = path1  // "/Users/seba/projects/sturdy.scala/sturdy-wasm/src/test/scala/sturdy/language/wasm/benchmarksgame/BenchmarksgameConstantCFGTest.scala"

  implicit val timing = Timing(discard = 0, repeat = 1)

  val (trees, es, _, times) = BenchmarkUtils.timed[(Graph, Graph), EditScript]({ () =>
    val tree1 = Parser.parse(BenchmarkUtils.readFile(path1))
    val tree2 = Parser.parse(BenchmarkUtils.readFile(path2))
    (tree1, tree2)
  }, { case (tree1, tree2) =>
    val (es, _) = tree1.compareTo(tree2)
    es
  })

  private val size: Int = trees._1.treesize + trees._2.treesize
  private val runningTime: Double = ms(times.sum / times.length)

  println(trees._1.toStringWithURI)
  println()
  println(trees._2.toStringWithURI)

  println("time(ms):" + runningTime)
  println("nodes:" + size)
  println("throughput(nodes/ms):" + (size.toDouble / runningTime.toDouble))
  println("patch size:" + es.size)
  es.foreach(e => println(s"  $e"))
  println("core edits:")
  es.coreEdits.foreach(e => println(s"  $e"))
  es.welltyped(trees._1.collectSignatures).foreach(m => throw new IllegalStateException(s"Ill-typed edit script: $m"))
}
