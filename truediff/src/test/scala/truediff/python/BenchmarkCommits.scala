package truediff.python

import java.io.File

import truechange.EditScript
import truediff.util.BenchmarkUtils._
import truediff.util.CSVUtil.csvRowToString


object BenchmarkCommits extends App {
  val projectName = "keras"

  private def benchmark()(implicit timing: Timing): Seq[Measurement] = {
    val rootDir = new File(s"benchmark/python_$projectName")
    val commits = rootDir.listFiles()
      .filter(_.getName.startsWith(s"$projectName-"))
      .sortBy(f=>f.getName.substring(s"$projectName-".length, f.getName.lastIndexOf('-')).toInt)

    val commitFiles = files(commits.head.getAbsolutePath, transitive = false, pattern = ".*py")

    commitFiles.flatMap { file =>
      var currentTree = null.asInstanceOf[Ast.file]
      // TODO we need to compare
      commits.toList.flatMap { commit =>
        val currentFile = new File(commit, file.getName)
        if (currentFile.exists()) {
          val content = readFile(currentFile.getAbsolutePath)
          val (tree, (editscript, _), parseTimes, times) = timed(() =>
            try {
              Statements.parse(content)
            } catch {
              case e: Exception if e.getMessage != null && e.getMessage.startsWith("Parse Error") =>
                println(s"  parsing ${file.getName} failed, skipping file")
                Ast.file.File(Seq())
            },
            (ast: Ast.file) =>
              if (currentTree == null)
                ast.compareTo(ast)
              else
                currentTree.compareTo(ast)
          )
          val extra = Map("Average Parsetime (ms)" -> ms(avg(parseTimes)))
          val measurement =
            if (currentTree == null) Measurement(currentFile.getName, tree.treesize, tree.treeheight, tree.treesize, tree.treeheight, times, editscript, extra)
            else Measurement(currentFile.getAbsolutePath, currentTree.treesize, currentTree.treeheight, tree.treesize, tree.treeheight, times, editscript, extra)
          println(csvRowToString(measurement.csv))
          currentTree = tree
          Seq(measurement)
        } else Seq()
      }
    }
  }

  // collect data
  val measurements = benchmark()(Timing(discard = 1, repeat = 5))
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
