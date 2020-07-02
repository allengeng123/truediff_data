package truediff.python

import java.io.File

import truechange.EditScript
import truediff.BenchmarkUtils._


object BenchmarkCommits extends App {

  private def benchmark()(implicit timing: Timing): Seq[Measurement[Ast.file]] = {
    val rootDir = new File("benchmark/python_test")
    val commits = rootDir.listFiles()
      .filter(_.getName.startsWith("django-"))
      .sortBy(f=>f.getName.substring("django-".length, f.getName.lastIndexOf('-')).toInt)

    val commitFiles = files(commits.head.getAbsolutePath, transitive = false, pattern = ".*py")

    commitFiles.take(5).flatMap { file =>
      var currentTree = null.asInstanceOf[Ast.file]
      commits.toList.map { commit =>
        val currentFile = new File(commit, file.getName)
        val content = readFile(currentFile.getAbsolutePath)
        val (tree, (editscript: EditScript, _), times) = timed(() =>
          try {
            Statements.parse(content)
          } catch {
            case e: Exception if e.getMessage!=null && e.getMessage.startsWith("Parse Error") =>
              println(s"  parsing failed, skipping file")
              Ast.file.File(Seq())
          },
          (ast: Ast.file) =>
            if (currentTree == null)
              ast.compareTo(ast)
            else
              currentTree.compareTo(ast)
        )
        val measurement =
          if (currentTree == null) Measurement(currentFile.getAbsolutePath, tree, tree, times, editscript)
          else Measurement(currentFile.getAbsolutePath, currentTree, tree, times, editscript)
        currentTree = tree
        measurement
      }
    }
  }

  // collect data
  val measurements = benchmark()(Timing(10, 50))
  // TODO process data
  // what data do we want to collect?
  val derivedMeasurements = measurements.map { m =>
    val extra = Map("Throughput (ms)" -> ms(throughput(m.vals, Seq(m.dest.treesize))))
    m.extend(Map())
  }
  val measurementsPerFile = derivedMeasurements.map ( m => m.name -> m).toMap

  // write data
  val filecontent = derivedMeasurements.head.csvHeader + "\n" + derivedMeasurements.map { m => m.csv }.mkString("\n")
  writeFile("benchmark/measurements/python_measurements.csv", filecontent)
}
