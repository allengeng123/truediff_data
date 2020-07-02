package truediff.python

import truediff.BenchmarkUtils._

object BenchmarkLatest extends App {

  private var warmUp = false

  private var resultCount: Int = 0
  private var nodeCounts: Int = 0
  private var parsingTime: Double = 0
  private var diffingTime: Double = 0

  implicit val timing: Timing = Timing(100, 100)

  private def benchPythonFile(path: String): Unit = {
    if (!warmUp) println(s"Benchmarking Python document $path")
    try {
      val str = readFile(path)
      val (nodeCount, parse, diff) = benchPython(path, str)
      resultCount += 1
      nodeCounts += nodeCount
      if (parse.isNaN)
        throw new Exception(s"$path: $nodeCount, $parse, $diff")
      parsingTime += parse
//      diffingTime += diff
      if (!warmUp) println(s"  nodeCount: ${str.linesIterator.size} lines, $nodeCount nodes")
      if (!warmUp) println(s"  parsing: $parse ms")
      if (!warmUp) println(s"  diffing unchanged: $diff ms")
    } catch {
      case e: Exception if e.getMessage!=null && e.getMessage.startsWith("Parse Error")  =>
        if (!warmUp) println(s"  parsing failed, skipping file")
    }
  }

  private def benchPython(name: String, content: String): (Int, Double, Seq[Long]) = {
    val discard = if (warmUp) 10 else 0
    val repeat = if (warmUp) 0 else 10
    val (tree, _, diffTime) = timed(() => Statements.parse(content), (t: Ast.file) => t.compareTo(t))
    // val (patch,diffIdenticalTime) = timed(tree.compareTo(tree))
    (tree.treesize, 0, diffTime)
  }

  // warmup
  this.warmUp = true
  println(s"\nWarming up")
  foreachFile("benchmark/python/django-0-9e14bc2135", pattern = ".*\\.py")(benchPythonFile)


  // benchmark
  this.warmUp = false
  println(s"\nBenchmarking")
  foreachFile("benchmark/python/django-0-9e14bc2135", pattern = ".*\\.py")(benchPythonFile)

  // report
  println(s"\nReport")
  println(s"  parsed files: $resultCount")
  println(s"  average nodes in AST: ${nodeCounts.toDouble / resultCount}")
  println(s"  total parse time: ${parsingTime} ms")
  println(s"  average parse time: ${parsingTime / resultCount} ms")
  println(s"  total diff time: ${diffingTime} ms")
  println(s"  average diff time: ${diffingTime / resultCount} ms")
}
