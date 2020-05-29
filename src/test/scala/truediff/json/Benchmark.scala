package truediff.json

import truediff.BenchmarkUtils._

object Benchmark extends App {

  type JSMeasurement = Measurement[Js]

  private def benchJsonFile(path: String)(implicit timing: Timing) =
    benchJson(path, readFile(s"benchmark/json/$path"))

  private def benchJson(name: String, content: String)(implicit timing: Timing): JSMeasurement = {
    val (tree,parseTime) = timed(Parser.parse(content))
    val ((changeset,_),diffIdenticalTime) = timed(tree.compareTo(tree))
    Measurement(s"diff unchanged $name", tree, tree, diffIdenticalTime, changeset, Map("parse time (ms)" -> parseTime))
  }

  implicit val timing = Timing(discard = 100, repeat = 10)
  println(benchJsonFile("parboiled2bench.json"))
  println(benchJsonFile("citm_catalog.json"))
  println(benchJsonFile("twitter.json"))
  println(benchJsonFile("canada.json")(Timing(discard = 3, repeat = 3)))
}
