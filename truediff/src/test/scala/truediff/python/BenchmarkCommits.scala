package truediff.python

import java.io.File

import truechange.EditScript
import truediff.util.BenchmarkUtils._


object BenchmarkCommits extends App {

  private def benchmark()(implicit timing: Timing): Seq[Measurement] = {
    val rootDir = new File("benchmark/python")
    val commits = rootDir.listFiles()
      .filter(_.getName.startsWith("django-"))
      .sortBy(f=>f.getName.substring("django-".length, f.getName.lastIndexOf('-')).toInt)

    val commitFiles = files(commits.head.getAbsolutePath, transitive = false, pattern = ".*py")

    commitFiles.flatMap { file =>
      var currentTree = null.asInstanceOf[Ast.file]
      commits.take(100).toList.flatMap { commit =>
        val currentFile = new File(commit, file.getName)
        if (currentFile.exists()) {
          val content = readFile(currentFile.getAbsolutePath)
          val (tree, (editscript, _), parseTimes, times) = timed(() =>
            try {
              Statements.parse(content)
            } catch {
              case e: Exception if e.getMessage != null && e.getMessage.startsWith("Parse Error") =>
                println(s"  parsing ${file.getAbsoluteFile} failed, skipping file")
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
            if (currentTree == null) Measurement(currentFile.getAbsolutePath, tree.treesize, tree.treeheight, tree.treesize, tree.treeheight, times, editscript, extra)
            else Measurement(currentFile.getAbsolutePath, currentTree.treesize, currentTree.treeheight, tree.treesize, tree.treeheight, times, editscript, extra)
          currentTree = tree
          Seq(measurement)
        } else Seq()
      }
    }
  }

  // collect data
  val measurements = benchmark()(Timing(10, 50))
  // what data do we want to collect?
  val derivedMeasurements = measurements.map { m =>
    val extra = Map("Throughput (ms)" -> ms(throughput(m.vals, Seq(m.destSize))))
    m.extend(Map())
  }

  val changingMeasurements = measurements.filter { m => m.editScript.size > 0 }

  val measurementsPerFile = derivedMeasurements.map ( m => m.name -> m).toMap

  // write data
  writeFile("benchmark/measurements/python_measurements.csv", measurementsToCSV(derivedMeasurements))
}
