package truediff.python

import java.io.File

import truediff.util.BenchmarkUtils._
import truediff.util.CSVUtil.csvRowToString

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object BenchmarkCommitsHdiffSizes extends App {
  val projectName = "keras"

  case class HPatch(size: Int)

  private def getCommitNumber(f: File): Int =
    f.getName.substring(s"$projectName-".length, f.getName.lastIndexOf('-')).toInt

  val hdiffExec = "/Volumes/home/projects/external/hdiff/.stack-work/install/x86_64-osx/158835463d84d35a50cb189d9160ce6e437299496abd59a51dfdcf659b1f6c07/8.6.5/bin/hdiff"

  private def benchmarkHdiff(file1: File, file2: File)(implicit timing: Timing): (Int, Int, HPatch, Seq[Long]) = {
    import scala.sys.process._
    val args = s"size ${file1.getAbsoluteFile} ${file2.getAbsoluteFile} --with-stats"
    val cmd = s"$hdiffExec $args"
    println(s"calling $cmd")

    val output = cmd.!!.split('\n')

    var line = 0
    var tree1Size = -1
    var tree2Size = -1
    var patchSize = -1
    val times = ListBuffer[Long]()

    for (_ <- 1 to timing.repeat) {
      line += 1

      assert(output(line).startsWith("tree1 size:"))
      tree1Size = output(line).substring("tree1 size:".length).trim.toInt
      line += 1

      assert(output(line).startsWith("tree2 size:"))
      tree2Size = output(line).substring("tree2 size:".length).trim.toInt
      line += 1
    }

    (tree1Size, tree2Size, HPatch(patchSize), times.toSeq)
  }

  private def benchmark()(implicit timing: Timing): Seq[Measurement[HPatch]] = {
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
    def benchmarkFile(commit: File, file: File)(implicit timing: Timing): Option[Measurement[HPatch]] = {
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
          val (tree1Size, tree2Size, patchSize, diffTimes) = benchmarkHdiff(file, prevCommitFile)
          val res = Some(Measurement(s"${commit.getName}/${file.getName}", tree1Size, -1, tree2Size, -1, diffTimes, patchSize).extendWithAggregated)
          println(csvRowToString(res.get.csv))
          res
        } else None
      } else None
    }

    commits.toList.tail.flatMap { commit =>
      val commitFiles = files(commit.getAbsolutePath, pattern = ".*py")
      commitFiles.flatMap { file =>
        benchmarkFile(commit, file)
      }
    }
  }

  // collect data

  val measurements = benchmark()(Timing(discard = 0, repeat = 1)) // best of 3

  val changingMeasurements = measurements.filter { m => m.editScript.size > 0 }
  val measurementsPerFile = measurements.map ( m => m.name -> m).toMap

  // write data
  writeFile("benchmark/measurements/python_keras_500_measurements-hdiff-sizes.csv", measurementsToCSV(measurements))
}
