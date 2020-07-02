package truediff.json

import truediff.BenchmarkUtils._

object BenchmarkUnchanged extends App {

  implicit val timing: Timing = Timing(discard = 50, repeat = 100)

  val jsons = files("benchmark/json")

  val measurements = jsons.map { json =>
    val content = readFile(json.getAbsolutePath)
    val (tree, (editscript, _), parseTimes, diffTimes) = timed(() => Parser.parse(content), (t: Js) => t.compareTo(t))
    Measurement(json.getAbsolutePath, tree, tree, diffTimes, editscript, Map("Average Parse time (ms)" -> ms(avg(parseTimes))))
  }

  writeFile("benchmark/measurements/json_unchanged.csv", measurementsToCsv(measurements))
}
