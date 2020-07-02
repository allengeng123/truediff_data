package truediff.python

import java.io.File

import truechange.EditScript
import truediff.BenchmarkUtils._


object BenchmarkCommits extends App {

  private def benchmark()(implicit timing: Timing): Seq[Measurement[Ast.file]] = {
    val rootDir = new File("benchmark/python")
    val commits = rootDir.listFiles()
      .filter(_.getName.startsWith("django-"))
      .sortBy(f=>f.getName.substring("django-".length, f.getName.lastIndexOf('-')).toInt)

    val commitFiles = files(commits.head.getAbsolutePath, transitive = false, pattern = ".*py")

    commitFiles.flatMap { file =>
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
  measurements.foreach { m => println(m.csv) }
  // TODO process data
}
