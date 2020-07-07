package truediff.json

import truediff.util.BenchmarkUtils._
import truediff.util.CSVUtil.csvRowToString

object BenchmarkCompareJsons extends App {

  implicit val timing: Timing = Timing(discard = 10, repeat = 50)

  val jsons = files("benchmark/json")
  val combinations = jsons.flatMap { json => jsons.filterNot( _ == json).map { (json, _) } }

  val measurements = combinations.map { case (l, r) =>
    val lContent = readFile(l.getAbsolutePath)
    val rContent = readFile(r.getAbsolutePath)
    val (tree, (editscript, _), parseTimes, diffTimes) =
      timed(() => (Parser.parse(lContent), Parser.parse(rContent)), (pair: (Js, Js)) => pair._1.compareTo(pair._2))
    val mes = Measurement(s"${l.getName} to ${r.getName}", tree._1.treesize, tree._1.treeheight, tree._2.treesize, tree._2.treeheight, diffTimes, editscript)
    println(csvRowToString(mes.csv))
    mes
  }

  writeFile("benchmark/measurements/json_changeeverything.csv", measurementsToCSV(measurements))
}
