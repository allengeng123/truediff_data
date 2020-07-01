package truediff.json

import truechange.EditScript
import truediff.BenchmarkUtils._
import truediff.json.Js._

import scala.util.Random

object BenchmarkReplaceSubtree extends App {

  private def benchUnchanged(path: String)(implicit timing: Timing): Measurement[Js] = {
    val content = readFile(s"benchmark/json/$path")
    val (_,parseTime) = timedNoSetup(Parser.parse(content))
    val (tree,(changeset,_),diffIdenticalTime) = timed(() => Parser.parse(content), (t: Js) => t.compareTo(t))
    Measurement(s"diff unchanged $path", tree, tree, diffIdenticalTime, changeset, Map("parse time (ms)" -> parseTime))
  }

  // unchanged diffing
//  println(benchUnchanged("parboiled2bench.json")(Timing(discard = 100, repeat = 10)))
//  println(benchUnchanged("citm_catalog.json")(Timing(discard = 100, repeat = 10)))
//  println(benchUnchanged("twitter.json")(Timing(discard = 100, repeat = 10)))
//  println(benchUnchanged("canada.json")(Timing(discard = 3, repeat = 3)))


  // replace one subtree by newtree, indexed in post-order
  def replaceSubtree(index: Int, t: Js, newtree: () => Js): Js = t match {
    case _ if index == 0 => newtree()
    case Obj(fields) =>
      var restindex = index
      for (i <- fields.indices) {
        val field = fields(i)
        if (restindex < field.value.treesize) {
          val replaced = replaceSubtree(restindex, field.value, newtree)
          return Obj(fields.updated(i, Field(field.name, replaced)))
        } else if (restindex < field.treesize) {
          return Obj(fields.updated(i, Field(field.name, newtree())))
        } else {
          restindex -= field.treesize
        }
      }
      if (restindex == 0)
        newtree()
      else
        throw new IndexOutOfBoundsException()
    case Arr(ts) =>
      var restindex = index
      for (i <- ts.indices) {
        val sub = ts(i)
        if (restindex < sub.treesize) {
          val replaced = replaceSubtree(restindex, sub, newtree)
          return Arr(ts.updated(i, replaced))
        } else {
          restindex -= sub.treesize
        }
      }
      if (restindex == 0)
        newtree()
      else
        throw new IndexOutOfBoundsException()
    case _ => throw new IndexOutOfBoundsException()
  }

  private def benchReplacedSubtree(path: String)(implicit timing: Timing): Measurement[Js] = {
    val content = readFile(s"benchmark/json/$path")

    val random = new Random(seed = 0)

    def measure(timing: Timing): Measurement[Js] = {
      val ((tree1, tree2), changeset, diffIdenticalTime) = timed[(Js,Js), EditScript](() => {
        val tree1 = Parser.parse(content)
        val index = random.nextInt(tree1.treesize)
        val tree2 = replaceSubtree(index, tree1, () => Str(s"Replaced subtree at index $index"))
        (tree1, tree2)
      }, { tt =>
        val (changeset, _) = tt._1.compareTo(tt._2)
        changeset
      })(timing)
      Measurement(s"diff unchanged $path", tree1, tree2, diffIdenticalTime, changeset)
    }

    measure(warmup(timing.discard))

    // actual
    Range(0, timing.repeat).foreach(_ => println(measure(nowarmup(1))))
    measure(nowarmup(1))
  }

  println(benchReplacedSubtree("parboiled2bench.json")(Timing(discard = 20, repeat = 100)))
}
