package truediff.compat.gumtree

import java.io.File

import truechange.EditScript
import truediff.util.BenchmarkUtils._
import truediff.util.CSVUtil.csvRowToString

object BenchmarkPythonCommitsTruediffGumtree extends App {
  val projectName = "keras"

  private def getCommitNumber(f: File): Int =
    f.getName.substring(s"$projectName-".length, f.getName.lastIndexOf('-')).toInt

  type Edits = EditScript


  private def benchmark()(implicit timing: Timing): Seq[Measurement[Edits]] = {
    val rootDir = new File(s"benchmark/python_$projectName")

    val commits = rootDir.listFiles()
      .filter(_.getName.startsWith(s"$projectName-"))
      .sortBy(getCommitNumber)

    val prevCommit: Map[File, File] = commits.tail.zipWithIndex.map { case (commit, i) =>
      commit -> commits(i)
    }.toMap

    def benchmarkFile(commit: File, file: File)(implicit timing: Timing): Option[Measurement[Edits]] = {
      val prevCommitFile = new File(prevCommit(commit), file.getName).getAbsoluteFile
      if (prevCommitFile.exists()) {
        val content = readFile(file.getAbsolutePath)
        val previousContent = readFile(prevCommitFile.getAbsolutePath)

        if (content != previousContent) {
          if (isWarmup) {
            println(s"Warmup remaining $warmpupCount")
            warmpupCount -= 1
          }

          val previousXmlFile = new File(prevCommitFile.getAbsolutePath + ".xml")
          val previousXml = readFile(previousXmlFile.getAbsolutePath)

          val xmlFile = new File(file.getAbsolutePath + ".xml")
          val xml = readFile(xmlFile.getAbsolutePath)

          val (treePair, editscript, _, diffTimes) = timed[(DiffableGumTree, DiffableGumTree), Edits](
            () => {
              val currTree = new PythonGumTreeGenerator().generateFromXml(xml, null).getRoot
              val prevTree = new PythonGumTreeGenerator().generateFromXml(previousXml, null).getRoot
              (prevTree.asInstanceOf[DiffableGumTree], currTree.asInstanceOf[DiffableGumTree])
            },
            (treePair: (DiffableGumTree, DiffableGumTree)) => {
              val prevTree = treePair._1
              val currTree = treePair._2
              prevTree.compareTo(currTree)._1
            }
          )

          val prevTree = treePair._1
          val currTree = treePair._2
          val res = Some(Measurement(s"${commit.getName}/${file.getName}", prevTree.getSize, prevTree.getHeight, currTree.getSize, currTree.getHeight, diffTimes, editscript).extendWithAggregated)
          println(csvRowToString(res.get.csv))
          res
        } else None
      } else None
    }

    val commitList = commits.toList

    // iterate over commits
    // iterate over all files of commit and check if previous has this file
    // parse both
    // compare
    commitList.tail.flatMap { commit =>
      val commitFiles = files(commit.getAbsolutePath, pattern = ".*py")
      commitFiles.flatMap { file =>
        if (isWarmup && warmpupCount <= 0)
          return Seq()

        benchmarkFile(commit, file)
      }
    }
  }

  // collect data
  var isWarmup = true
  var warmpupCount = 100
  benchmark()(Timing(discard = 0, repeat = 1)) // <- this is warmup
  isWarmup = false

  isWarmup = false
  val measurements = benchmark()(Timing(discard = 0, repeat = 3, outliers = 2)) // best of 3

  val changingMeasurements = measurements.filter { m => m.editScript.edits.nonEmpty }
  val measurementsPerFile = measurements.map ( m => m.name -> m).toMap

  // write data
  writeFile("benchmark/measurements/python_keras_500_measurements-truediffGumtree.csv", measurementsToCSV(measurements))
}
