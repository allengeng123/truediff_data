package truediff.python

import java.io.File

import truechange.EditScript
import truediff.util.BenchmarkUtils._
import truediff.util.CSVUtil.csvRowToString


object BenchmarkCommits extends App {
  val projectName = "keras"

  private def getCommitNumber(f: File): Int =
    f.getName.substring(s"$projectName-".length, f.getName.lastIndexOf('-')).toInt

  private def benchmark()(implicit timing: Timing): Seq[Measurement] = {
    val rootDir = new File(s"benchmark/python_$projectName")
    val commits = rootDir.listFiles()
      .filter(_.getName.startsWith(s"$projectName-"))
      .sortBy(getCommitNumber)

    val prevCommit: Map[File, File] = commits.tail.zipWithIndex.map { case (commit, i) =>
      commit -> commits(i)
    }.toMap

    // iterate over commits
    // iterate over all files of commit and check if previous has this file
    // parse both
    // compare
    commits.toList.tail.flatMap { commit =>
      val commitFiles = files(commit.getAbsolutePath)
      commitFiles.flatMap { file =>
        val prevCommitFile = new File(prevCommit(commit), file.getName)
        if (prevCommitFile.exists()) {
          val currCommitFileContent = readFile(file.getAbsolutePath)
          val prevCommitFileContent = readFile(prevCommitFile.getAbsolutePath)
          val (tree, (editscript, _), _, diffTimes) = timed[(Ast.file, Ast.file), (EditScript, Ast.file)](
            () => {
              val currTree = Statements.parse(currCommitFileContent)
              val prevTree = Statements.parse(prevCommitFileContent)
              (prevTree, currTree)
            },
            (in: (Ast.file, Ast.file)) => in._1.compareTo(in._2)
          )
          val res = Some(Measurement(s"${commit.getName}/${file.getName}", tree._1.treesize, tree._1.treeheight, tree._2.treesize, tree._2.treeheight, diffTimes, editscript))
          println(csvRowToString(res.get.csv))
          res
        } else Option.empty[Measurement]
      }
    }
  }

  // collect data
  val measurements = benchmark()(Timing(discard = 10, repeat = 50))
  // what data do we want to collect?
//  val derivedMeasurements = measurements.map { m =>
//    val extra = Map("Throughput (ms)" -> ms(throughput(m.vals, Seq(m.destSize))))
//    m.extend(Map())
//  }

  val changingMeasurements = measurements.filter { m => m.editScript.size > 0 }

  val measurementsPerFile = measurements.map ( m => m.name -> m).toMap

  // write data
  writeFile("benchmark/measurements/python_keras_measurements.csv", measurementsToCSV(measurements))
  writeFile("benchmark/measurements/python_keras_changing_measurements.csv", measurementsToCSV(changingMeasurements))
}
