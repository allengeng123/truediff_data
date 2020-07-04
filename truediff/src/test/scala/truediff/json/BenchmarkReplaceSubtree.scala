package truediff.json

import truediff.Diffable
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

      val initalMeasurement = Measurement(json.getAbsolutePath + s" initial", tree.treesize, tree.treeheight, tree.treesize, tree.treeheight, diffTimes, editscript)

      val numinner = countInner(tree)
      // only measure for every 100th inner node
      val measurements = for (i <- 1 until numinner; if i % 100 == 0) yield {
        val changedTree = changeInner(tree, i, jstree, fieldtree)
        val (newTree, (editscript, _), parseTimes, diffTimes) = timed(() => tree, (t: Js) => tree.compareTo(changedTree))
        val mes = Measurement(json.getAbsolutePath + s" inner node $i", tree.treesize, tree.treeheight, changedTree.treesize, changedTree.treeheight, diffTimes, editscript)
        println(csvRowToString(mes.csv))
        mes
      }
      initalMeasurement+:measurements
    }
  }

  val timing = Timing(discard = 5, repeat = 10)

  val singleMeasurements = measure(
    () => Str(s"Replaced subtree"),
    () => Field(s"Replaced subtree", Null()))(timing)

  writeFile("benchmark/measurements/json_replacesubtrees_with_single.csv", measurementsToCSV(singleMeasurements))

  val replacementTree = Parser.parse(readFile("benchmark/json/twitter.json"))
  val treeMeasurements = measure(
    () => replacementTree,
    () => Field(s"Replaced subtree", replacementTree))(timing)

  writeFile("benchmark/measurements/json_replacesubtrees_with_tree.csv", measurementsToCSV(treeMeasurements))
}
