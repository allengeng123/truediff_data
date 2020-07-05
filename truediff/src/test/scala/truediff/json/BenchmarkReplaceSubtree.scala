package truediff.json

import truechange.EditScript
import truediff.util.CSVUtil.csvRowToString
import truediff.util.BenchmarkUtils._
import truediff.json.Js._

object BenchmarkReplaceSubtree extends App {
  def countInner(field: Field): Int = 1 + countInner(field.value)

  def countInner(js: Js): Int = js match {
    case Str(value) => 0
    case False() => 0
    case True() => 0
    case Null() => 0
    case Num(value) => 0
    case Obj(value) => 1 + value.list.map(countInner).sum
    case Arr(value) => 1 + value.list.map(countInner).sum
  }

  def changeInner(js: Js, index: Int, newjs: () => Js, newfield: () => Field): Js = {
    var visitedInner = 0

    def transJs(js: Js): Js = js match {
      case Arr(values) =>
        if (visitedInner == index) {
          visitedInner += 1
          newjs()
        } else {
          visitedInner += 1
          Arr(values.list.map(transJs))
        }
      case Obj(fields) =>
        if (visitedInner == index) {
          visitedInner += 1
          newjs()
        } else {
          visitedInner += 1
          Obj(fields.list.map(transField))
        }
      case _ => js
    }

    def transField(field: Field): Field = {
      if (visitedInner == index) {
        visitedInner += 1
        newfield()
      } else {
        visitedInner += 1
        Field(field.name, transJs(field.value))
      }
    }

    transJs(js)
  }

  val jsons = files("benchmark/json")

  def measure(jstree: () => Js, fieldtree: () => Field)(implicit timing: Timing): Seq[Measurement] = {
    jsons.flatMap { json =>
      val content = readFile(json.getAbsolutePath)
      val (tree, (editscript, _), parseTimes, diffTimes) = timed(() => Parser.parse(content), (t: Js) => t.compareTo(t))

      val initalMeasurement = Measurement(json.getName + s" initial", tree.treesize, tree.treeheight, tree.treesize, tree.treeheight, diffTimes, editscript)

      val numinner = countInner(tree)
      // only measure for every 500th inner node
      val changedMeasurements = for (i <- 1 to numinner; if i % 500 == 0) yield {
        val (newTree, editscript, parseTimes, diffTimes) =
          timed[(Js, Js), (EditScript, Js)](
            () => {
              val tree = Parser.parse(content)
              val changed = changeInner(tree, i, jstree, fieldtree)
              (tree, changed)
            },
            (in: (Js, Js)) => in._1.compareTo(in._2))
        val mes = Measurement(json.getName + s" inner $i", tree.treesize, tree.treeheight, newTree._2.treesize, newTree._2.treeheight, diffTimes, editscript._1)
        println(csvRowToString(mes.csv))
        mes
      }
      initalMeasurement +: changedMeasurements
    }
  }

  val timing = Timing(discard = 10, repeat = 50)

  val singleMeasurements = measure(
    () => Str(s"Replaced subtree"),
    () => Field(s"Replaced subtree", Null()))(timing)

  writeFile("benchmark/measurements/json_replacesubtrees_with_single.csv", measurementsToCSV(singleMeasurements))

  val treeMeasurements = measure(
    () => {
      val replacementTree = Parser.parse(readFile("benchmark/json/twitter.json"))
      replacementTree
    },
    () => {
      val replacementTree = Parser.parse(readFile("benchmark/json/twitter.json"))
      Field(s"Replaced subtree", replacementTree)
    })(timing)

  writeFile("benchmark/measurements/json_replacesubtrees_with_tree.csv", measurementsToCSV(treeMeasurements))
}
