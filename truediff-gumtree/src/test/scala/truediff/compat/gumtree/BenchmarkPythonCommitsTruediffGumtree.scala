package truediff.compat.gumtree

import java.io.File

import truechange.EditScript
import truediff.util.BenchmarkUtils._
import truediff.util.CSVUtil.csvRowToString

import scala.collection.mutable

object BenchmarkPythonCommitsTruediffGumtree extends App {
  val projectName = "keras"

  type Doc = (String, Array[String])
  val emptyXml = "<Module lineno=\"1\" col=\"0\" end_line_no=\"1\" end_col=\"0\"></Module>"
  val emptyDoc: Doc = (emptyXml, Array[String](""))

  private def getCommitNumber(f: File): Int =
    f.getName.substring(s"$projectName-".length, f.getName.lastIndexOf('-')).toInt

  type Edits = EditScript


  private def benchmark()(implicit timing: Timing): Seq[Measurement[Edits]] = {
    val rootDir = new File(s"benchmark/python_$projectName")
    // Measure the first 100 commits
    val commits = rootDir.listFiles()
      .filter(_.getName.startsWith(s"$projectName-"))
      .sortBy(getCommitNumber)

    val prevCommit: Map[File, File] = commits.tail.zipWithIndex.map { case (commit, i) =>
      commit -> commits(i)
    }.toMap


    val parsedFiles:mutable.Map[File, String] = mutable.Map()
    val previousDocs: mutable.Map[String, Doc] = mutable.Map()

    def benchmarkFile(commit: File, file: File)(implicit timing: Timing): Option[Measurement[Edits]] = {
      if (isWarmup && warmpupCount <= 0)
        return None

      val prevCommitFile = new File(prevCommit(commit), file.getName)
      if (prevCommitFile.exists()) {
        val currCommitFileContent = readFile(file.getAbsolutePath)
        parsedFiles(file) = currCommitFileContent
        parsedFiles.get(prevCommitFile) match {
          case Some(_) => // do nothing
          case None =>
            val content = readFile(prevCommitFile.getAbsolutePath)
            parsedFiles(prevCommitFile) = content
        }

        if (parsedFiles(file) != parsedFiles(prevCommitFile)) {
          if (isWarmup) {
            println(s"Warmup remaining $warmpupCount")
            warmpupCount -= 1
          }

          val (previousXml, previousCode) = previousDocs.getOrElse(file.getName, emptyDoc)
          val code = readFile(file.getAbsolutePath).split('\n')
          val xmlFile = new File(file.getAbsolutePath + ".xml")
          val xml = readFile(xmlFile.getAbsolutePath)
          previousDocs(file.getName) = (xml, code)

          val (treePair, editscript, _, diffTimes) = timed[(DiffableGumTree, DiffableGumTree), Edits](
            () => {
              val currTree = new PythonGumTreeGenerator().generateFromXml(xml,code).getRoot
              val prevTree = new PythonGumTreeGenerator().generateFromXml(previousXml, previousCode).getRoot
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
          val res = Some(Measurement(s"${commit.getName}/${file.getName}", prevTree.getSize, prevTree.getHeight, currTree.getSize, currTree.getHeight, diffTimes, editscript))
          println(csvRowToString(res.get.csv))
          res
        } else None
      } else None
    }

    val commitList = commits.toList
    commitList.headOption.foreach { commit =>
      val commitFiles = files(commit.getAbsolutePath, pattern = ".*py")
      commitFiles.foreach { file =>
        val code = readFile(file.getAbsolutePath).split('\n')
        val xmlFile = new File(file.getAbsolutePath + ".xml")
        val xml = readFile(xmlFile.getAbsolutePath)
        previousDocs(file.getName) = (xml, code)
      }
    }

    // iterate over commits
    // iterate over all files of commit and check if previous has this file
    // parse both
    // compare
    commitList.tail.flatMap { commit =>
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

  isWarmup = false
  val measurements = benchmark()(Timing(discard = 0, repeat = 3, outliers = 2)) // best of 3

  val changingMeasurements = measurements.filter { m => m.editScript.edits.nonEmpty }
  val measurementsPerFile = measurements.map ( m => m.name -> m).toMap

  // write data
  writeFile("benchmark/measurements/python_keras_500_measurements-truediffGumtree.csv", measurementsToCSV(measurements))
  writeFile("benchmark/measurements/python_keras_500_changing_measurements-truediffGumtree.csv", measurementsToCSV(changingMeasurements))
}
