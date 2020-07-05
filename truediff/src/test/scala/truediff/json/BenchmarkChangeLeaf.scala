package truediff.json

import truechange.EditScript
import truediff.util.BenchmarkUtils._
import truediff.util.CSVUtil.csvRowToString
import truediff.json.Js._

object BenchmarkChangeLeaf extends App {

  def changeLeaf(js: Js, index: Int): Js = {
    var visitedLeaves = 0

    def traverseLeaf[T](l: T, f: T => T): T =
      if (visitedLeaves == index) {
        visitedLeaves += 1
        f(l)
      } else {
        visitedLeaves += 1
        l
      }

    def transJs(js: Js): Js = js match {
      case str: Str => traverseLeaf(str, (s: Str) => Str(s.value + " changed"))
      case num: Num => traverseLeaf(num, (n: Num) => Num(n.value+1))
      case fls: False => traverseLeaf(fls, (_: Js) => True())
      case tru: True => traverseLeaf(tru, (_: Js) => False())
      case nul: Null => traverseLeaf(nul, (_: Js) => Num(1))
      case Arr(values) => Arr(values.list.map(transJs))
      case Obj(fields) => Obj(fields.list.map(transField))
    }

    def transField(field: Field): Field = Field(field.name, transJs(field.value))
    transJs(js)
  }

  def countLeaves(field: Field): Int = countLeaves(field.value)

  def countLeaves(js: Js): Int = js match {
    case Str(value) => 1
    case False() => 1
    case True() => 1
    case Null() => 1
    case Num(value) => 1
    case Obj(value) => value.list.map(countLeaves).sum
    case Arr(value) => value.list.map(countLeaves).sum
  }


  implicit val timing: Timing = Timing(discard=10, repeat = 50)

  val jsons = files("benchmark/json")

  val measurements = jsons.flatMap { json =>
    val content = readFile(json.getAbsolutePath)
    val (tree, (editscript, _), parseTimes, diffTimes) = timed(() => Parser.parse(content), (t: Js) => t.compareTo(t))
    println(tree)

    val initalMeasurement = Measurement(json.getName + s" initial", tree.treesize, tree.treeheight, tree.treesize, tree.treeheight, diffTimes, editscript)

    val numleaves = countLeaves(tree)
    val changedMeasurements = for (i <- 1 to numleaves; if i % 500 == 0) yield {
      val (newTree, editscript, parseTimes, diffTimes) =
        timed[(Js, Js), (EditScript, Js)](
          () => {
            val tree = Parser.parse(content)
            val changed = changeLeaf(tree, i)
            (tree, changed)
          },
          (in: (Js, Js)) => in._1.compareTo(in._2))
      val mes = Measurement(json.getName + s" leaf $i", tree.treesize, tree.treeheight, newTree._2.treesize, newTree._2.treeheight, diffTimes, editscript._1)
      println(csvRowToString(mes.csv))
      mes
    }

    initalMeasurement +: changedMeasurements
  }

  writeFile("benchmark/measurements/json_changeleaves.csv", measurementsToCSV(measurements))
}
