package truediff.util

import java.io.{File, PrintWriter}

import truechange.EditScript
import truediff.util.CSVUtil.{CSVRow, csvRowToString}

import scala.io.Source

object BenchmarkUtils {
  type TruediffMeasurement = Measurement[EditScript]

  object Measurement {
    def apply[ES <: {def size: Int}](name: String, srcSize: Int, srcHeight: Int, destSize: Int, destHeight: Int, vals: Seq[Long], editScript: ES, extra: Map[String, Any] = Map())(implicit timing: Timing): Measurement[ES] = {
      val outliers = timing.outliers
      Measurement(name, srcSize, srcHeight, destSize, destHeight, vals, outliers, editScript, extra)
    }
  }

  case class Measurement[ES <: {def size: Int}](name: String, srcSize: Int, srcHeight: Int, destSize: Int, destHeight: Int, vals: Seq[Long], outliers: Int, editScript: ES, extra: Map[String, Any]) {
    val valsWithoutOutliers: Seq[Long] = {
      val ordered = vals.sorted
      ordered.dropRight(outliers)
    }

    override def toString: String = {
      val diffTime = avg(valsWithoutOutliers)
      val text = s"""
         |Measurement $name
         |  Src tree size:     $srcSize
         |  Dest tree size:    $destSize
         |  EditScript size:    ${editScript.size}
         |  Diffing time (ms): ${ms(diffTime)}""".stripMargin
      if (extra.isEmpty)
        text
      else
        text + extra.map(kv => s"\n  ${kv._1}: ${kv._2}").foldLeft("")(_+_)
    }

    def extendWithAggregated: Measurement[ES] = {
      val combinedTreeSize = srcSize + destSize
      val diffTime = ms(avg(valsWithoutOutliers))
      val nodesPerMs = combinedTreeSize / diffTime
      val editsPerNod = editScript.size.toDouble / combinedTreeSize
      val newExtras = Map(
        "Nodes/ms" -> f"$nodesPerMs%.32f",
        "Edits/Nodes" -> f"$editsPerNod%.32f",
        "Tree size" -> combinedTreeSize
      )
      extend(newExtras)
    }

    def extend(newExtras: Map[String, Any]): Measurement[ES] = {
      Measurement(name, srcSize, srcHeight, destSize, destHeight, vals, outliers, editScript, extra ++ newExtras)
    }

    val csvHeader: String = s"Filename,Src tree size,Dest tree size,Src tree height,Dest tree height,EditScript size,Average Diffing time (ms)${if (extra.isEmpty) "," else extra.keys.mkString(",", ",", ",")}raw data (ns)"

    val csv: CSVRow = {

      val diffTime = ms(avg(valsWithoutOutliers))
//      s"$name, $srcSize, $destSize, ${editScript.size}, $diffTime${if (extra.isEmpty) ", " else extra.values.mkString(", ", ", ", ", ")}${BenchmarkUtils.toCSVRow(vals)}"
      IndexedSeq(name, srcSize, destSize, srcHeight, destHeight, editScript.size, diffTime) ++ extra.values ++ valsWithoutOutliers
    }
  }

  def measurementsToCSV(measurements: Seq[Measurement[_]]): String =
    if (measurements.isEmpty) ""
    else measurements.head.csvHeader + "\n" + measurements.map { m => csvRowToString(m.csv) }.mkString("\n")

  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    val str = source.mkString
    source.close()
    str
  }

  def writeFile(path: String, content: String): Unit = {
    val file = new File(path)
    file.createNewFile()
    val writer = new PrintWriter(file)
    writer.write(content)
    writer.close()
  }

  def foreachFileLine(path: String)(f: String => Unit): Unit = {
    val source = Source.fromFile(path)
    for (line <- source.getLines())
      f(line)
    source.close()
  }

  def files(path: String, transitive: Boolean = true, pattern: String = ".*"): Seq[File] = {
    val file = new File(path)
    if (file.isDirectory) {
      file.listFiles().toList.flatMap { sub =>
        val subpath = s"$path/${sub.getName}"
        if (sub.isFile && sub.getName.matches(pattern)) Seq(sub)
        else if (transitive && sub.isDirectory)
          files(subpath, transitive, pattern)
        else Nil
      }
    } else Nil
  }

  def foreachFile(path: String, transitive: Boolean = true, pattern: String = ".*")(f: String => Unit): Unit = {
    val file = new File(path)
    if (file.isDirectory) {
      file.listFiles().foreach { sub =>
        val subpath = s"$path/${sub.getName}"
        if (sub.isFile && sub.getName.matches(pattern))
          f(subpath)
        else if (transitive && sub.isDirectory)
          foreachFile(subpath, transitive, pattern)(f)
      }
    }
  }

  def ms(l: Double): Double = l/1000/1000

  def time[R](block: => R): (R,Long) = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    (result, t1 - t0)
  }

  case class Timing(discard: Int, repeat: Int, outliers: Int = 0)
  def warmup(discard: Int): Timing = Timing(discard, 0)
  def nowarmup(repeat: Int): Timing = Timing(0, repeat)

  def timedNoSetup[R](block: => R)(implicit timing: Timing): (R, Seq[Long]) = {
    val (_, out, _, t) = timed[Unit, R](() => (), _ => block)
    (out, t)
  }

  def timed[A,R](setup: () => A, block: A => R)(implicit timing: Timing): (A, R, Seq[Long], Seq[Long]) = {
    var input = null.asInstanceOf[A]
    var result = null.asInstanceOf[R]

    // discard first runs
    for (_ <- 1 to timing.discard) {
      input = setup()
      val (r,_) = time(block(input))
      result = r
    }

    var sum: Long = 0
    var setuptimes: Seq[Long] = Nil
    var times: Seq[Long] = Nil
    for (_ <- 1 to timing.repeat) {
      val setupRes = time(setup())
      input = setupRes._1
      val (res, blocktime) = time(block(input))
      result = res
      setuptimes = setuptimes :+ setupRes._2
      times = times :+ blocktime
    }
    (input, result, setuptimes, times)
  }

  def avg[T](vals: Seq[T])(implicit num: Numeric[T]): Double = if (vals.isEmpty) 0 else num.toDouble(vals.sum) / vals.size
}
