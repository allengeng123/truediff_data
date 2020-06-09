package truediff

import java.io.File

import truechange.Changeset

import scala.io.Source

object BenchmarkUtils {
  case class Measurement[T <: Diffable](name: String, src: T, dest: T, diffTime: Double, changeset: Changeset, extra: Map[String, Any] = Map()) {
    override def toString: String = {
      val srcsize = src.treesize
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
  def warmup(discard: Int): Timing = Timing(discard, 0)
  def nowarmup(repeat: Int): Timing = Timing(0, repeat)

  def timedNoSetup[R](block: => R)(implicit timing: Timing): (R, Double) = {
    val (_, out, time) = timed[Unit, R](() => (), _ => block)
    (out, time)
  }

  def timed[A,R](setup: () => A, block: A => R)(implicit timing: Timing): (A, R, Double) = {
    var input = null.asInstanceOf[A]
    var result = null.asInstanceOf[R]

    // discard first runs
    for (_ <- 1 to timing.discard) {
      input = setup()
      val (r,_) = time(block(input))
      result = r
    }

    var sum: Long = 0
    for (_ <- 1 to timing.repeat) {
      input = setup()
      val output = time(block(input))
      result = output._1
      sum += output._2
    }
    (input, result, if (timing.repeat == 0) 0 else ms(sum.toDouble / timing.repeat))
  }
}
