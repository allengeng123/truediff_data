package truediff

import java.io.File

import truediff.changeset.Changeset
import truediff.diffable.Diffable

import scala.io.Source

object BenchmarkUtils {
  case class Measurement[T <: Diffable](name: String, src: T, dest: T, diffTime: Double, changeset: Changeset, extra: Map[String, Any]) {
    override def toString: String = {
      val srcsize = src.size
      val text = s"""
         |Measurement $name
         |  Src tree size:     $srcsize
         |  Dest tree size:    $srcsize
         |  Changeset size:    ${changeset.size}
         |  Diffing time (ms): $diffTime""".stripMargin
      if (extra.isEmpty)
        text
      else
        text + extra.map(kv => s"\n  ${kv._1}: ${kv._2}").foldLeft("")(_+_)
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
  def timed[R](block: => R)(implicit timing: Timing): (R, Double) = {
    var result = null.asInstanceOf[R]

    // discard first 10 runs
    for (_ <- 1 to timing.discard) {
      val (r,_) = time(block)
      result = r
    }

    var sum: Long = 0
    for (_ <- 1 to timing.repeat) {
      val (r, t) = time(block)
      result = r
      sum += t
    }
    (result, if (timing.repeat == 0) 0 else ms(sum.toDouble / timing.repeat))
  }
}
