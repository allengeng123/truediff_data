package truediff.json

import truediff.BenchmarkUtils._

object BenchmarkUnchanged extends App {

  private def benchUnchanged(path: String)(implicit timing: Timing): Measurement[Js] = {
    val content = readFile(s"benchmark/json/$path")
    val (_,parseTime) = timedNoSetup(Parser.parse(content))
    val (tree,(editscript,_),diffIdenticalTime) = timed(() => Parser.parse(content), (t: Js) => t.compareTo(t))
    Measurement(s"diff unchanged $path", tree, tree, diffIdenticalTime, editscript, Map("parse time (ms)" -> parseTime))
  }

  // unchanged diffing
  println(benchUnchanged("parboiled2bench.json")(Timing(discard = 100, repeat = 10)))
  println(benchUnchanged("citm_catalog.json")(Timing(discard = 100, repeat = 10)))
  println(benchUnchanged("twitter.json")(Timing(discard = 100, repeat = 10)))
  println(benchUnchanged("canada.json")(Timing(discard = 3, repeat = 3)))
}
