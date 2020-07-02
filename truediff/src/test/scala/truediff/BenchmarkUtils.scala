package truediff

import java.io.File

import truechange.EditScript

import scala.io.Source

object BenchmarkUtils {
  case class Measurement[T <: Diffable](name: String, src: T, dest: T, vals: Seq[Long], editscript: EditScript, extra: Map[String, Any] = Map()) {
    override def toString: String = {
      val srcsize = src.treesize
      val destsize = dest.treesize
      val diffTime = avg(vals)
      val text = s"""
         |Measurement $name
         |  Src tree size:     $srcsize
         |  Dest tree size:    $destsize
         |  EditScript size:    ${editscript.size}
         |  Diffing time (ms): ${ms(diffTime)}""".stripMargin
      if (extra.isEmpty)
        text
      else
        text + extra.map(kv => s"\n  ${kv._1}: ${kv._2}").foldLeft("")(_+_)
    }

    val csvHeader: String = "Filename, Src tree size, Dest tree size, EditScript size, Average Diffing time (ms), raw data (ms)"

    val csv: String = {
      val srcsize = src.treesize
      val destsize = dest.treesize
      val diffTime = avg(vals)
      s"$name, $srcsize, $destsize, ${editscript.size}, $diffTime, ${BenchmarkUtils.csv(vals)}"
    }
  }

  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    val str = source.mkString
    source.close()
    str
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

  case class Timing(discard: Int, repeat: Int)
  def warmup(discard: Int): Timing = Timing(discard, 0)
  def nowarmup(repeat: Int): Timing = Timing(0, repeat)

  def timedNoSetup[R](block: => R)(implicit timing: Timing): (R, Seq[Long]) = {
    val (_, out, t) = timed[Unit, R](() => (), _ => block)
    (out, t)
  }

  def timed[A,R](setup: () => A, block: A => R)(implicit timing: Timing): (A, R, Seq[Long]) = {
    var input = null.asInstanceOf[A]
    var result = null.asInstanceOf[R]

    // discard first runs
    for (_ <- 1 to timing.discard) {
      input = setup()
      val (r,_) = time(block(input))
      result = r
    }

    var sum: Long = 0
    var times: Seq[Long] = Nil
    for (_ <- 1 to timing.repeat) {
      input = setup()
      val (res, t) = time(block(input))
      result = res
      times = times :+ t
    }
    (input, result, times)
  }

  def avg[T](vals: Seq[T])(implicit num: Numeric[T]): Double = if (vals.isEmpty) 0 else num.toDouble(vals.sum) / vals.size
  def throughput(vals: Seq[Long], nodes: Seq[Int]): Double = avg(nodes) / avg(vals)

  def csv[T](vals: Seq[T]): String = vals.mkString(", ")
}
