package truediff.python

import java.io.File

import truechange.EditScript
import truediff.python.Ast.file
import truediff.util.BenchmarkUtils._
import truediff.util.CSVUtil.csvRowToString

import scala.collection.mutable


object BenchmarkCommits extends App {
  val projectName = "keras"

  private def getCommitNumber(f: File): Int =
    f.getName.substring(s"$projectName-".length, f.getName.lastIndexOf('-')).toInt

  private def benchmark()(implicit timing: Timing): Seq[TruediffMeasurement] = {
    val rootDir = new File(s"benchmark/python_$projectName")
    // Measure the first 100 commits
    val commits = rootDir.listFiles()
      .filter(_.getName.startsWith(s"$projectName-"))
      .sortBy(getCommitNumber)

    val prevCommit: Map[File, File] = commits.tail.zipWithIndex.map { case (commit, i) =>
      commit -> commits(i)
    }.toMap


    val parsedFiles:mutable.Map[File, String] = mutable.Map()
    // iterate over commits
    // iterate over all files of commit and check if previous has this file
    // parse both
    // compare
    def benchmarkFile(commit: File, file: File)(implicit timing: Timing): Option[TruediffMeasurement] = {
      if (isWarmup && warmpupCount <= 0) 
        return None

      val prevCommitFile = new File(prevCommit(commit), file.getName)
      if (prevCommitFile.exists()) {
        val currCommitFileContent = readFile(file.getAbsolutePath)
        parsedFiles(file) = currCommitFileContent
        parsedFiles.get(prevCommitFile) match {
          case Some(content) => // do nothing
          case None =>
            val content = readFile(prevCommitFile.getAbsolutePath)
            parsedFiles(prevCommitFile) = content
        }

        if (parsedFiles(file) != parsedFiles(prevCommitFile)) {
          if (isWarmup) {
            println(s"Warmup remaining $warmpupCount")
            warmpupCount -= 1
          }
          val (tree, (editscript, _), _, diffTimes) = timed[(file, file), (EditScript, file)](
            () => {
              val currTree = Statements.parse(parsedFiles(file))
              val prevTree = Statements.parse(parsedFiles(prevCommitFile))
              (prevTree, currTree)
            },
            (in: (file, file)) => in._1.compareTo(in._2)
          )
          val res = Some(Measurement(s"${commit.getName}/${file.getName}", tree._1.treesize, tree._1.treeheight, tree._2.treesize, tree._2.treeheight, diffTimes, editscript))
          println(csvRowToString(res.get.csv))
          res
        } else Option.empty[TruediffMeasurement]
      } else Option.empty[TruediffMeasurement]
    }

    commits.toList.tail.flatMap { commit =>
      val commitFiles = files(commit.getAbsolutePath, pattern = ".*py")
      commitFiles.flatMap { file =>
        benchmarkFile(commit, file)
      }
    }
  }

  // collect data
  var isWarmup = true
  var warmpupCount = 100
  benchmark()(Timing(discard = 0, repeat = 1)) // <- this is warmup
  isWarmup = false

  val measurements = benchmark()(Timing(discard = 0, repeat = 3, outliers = 2)) // best of 3

  val changingMeasurements = measurements.filter { m => m.editScript.size > 0 }
  val measurementsPerFile = measurements.map ( m => m.name -> m).toMap

  // write data
  writeFile("benchmark/measurements/python_keras_500_measurements-truediff.csv", measurementsToCSV(measurements))
}
