package truediff.compat.gumtree

import java.io.{File, PrintWriter}

import com.github.gumtreediff.actions.model.Action
import truediff.util.BenchmarkUtils._

import scala.collection.mutable

object PrepareBenchmarkPythonCommits extends App {
  val projectName = "keras"

  private def getCommitNumber(f: File): Int =
    f.getName.substring(s"$projectName-".length, f.getName.lastIndexOf('-')).toInt

  type Edits = mutable.Buffer[Action]

  private def prepareBenchmark(): Unit = {
    val rootDir = new File(s"benchmark/python_$projectName")
    // Measure the first 100 commits
    val commits = rootDir.listFiles()
      .filter(_.getName.startsWith(s"$projectName-"))
      .sortBy(getCommitNumber)

    val prevCommit: Map[File, File] = commits.tail.zipWithIndex.map { case (commit, i) =>
      commit -> commits(i)
    }.toMap


    val parsedFiles:mutable.Map[File, String] = mutable.Map()

    val commitList = commits.toList

    commitList.headOption.foreach { commit =>
      val commitFiles = files(commit.getAbsolutePath, pattern = ".*py")
      commitFiles.foreach { file =>
        val xmlFile = new File(file.getAbsolutePath + ".xml")
        if (!xmlFile.exists()) {
          println(s"Parse $file")
          val xml = new PythonGumTreeGenerator().generateXml(readFile(file.getAbsolutePath))
          new PrintWriter(xmlFile) { write(xml); close() }
          println(s"Wrote $xmlFile")
        }
      }
    }

    commitList.tail.foreach { commit =>
      val commitFiles = files(commit.getAbsolutePath, pattern = ".*py")
      commitFiles.foreach { file =>
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
            val xmlFile = new File(file.getAbsolutePath + ".xml")
            if (!xmlFile.exists()) {
              println(s"Parse $file")
              val xml = new PythonGumTreeGenerator().generateXml(parsedFiles(file))
              new PrintWriter(xmlFile) { write(xml); close() }
              println(s"Wrote $xmlFile")
            }
          } else Option.empty[Measurement[Edits]]
        } else Option.empty[Measurement[Edits]]
      }
    }
  }

  prepareBenchmark()
}
