package truediff.util

import java.io.File

import scala.sys.process._

object FetchCommits extends App {
  val rootDir = new File("")
  val benchmarkDir = new File("benchmark")
  val repo = new File(benchmarkDir, "keras")
  val target = new File(benchmarkDir, "python_keras")
  val filesuffix = "py"
  val numcommits = 500

  // clone repository if keras dir does not exist in benchmark
  if(!repo.exists()) {
    Process("git clone git@github.com:keras-team/keras.git", benchmarkDir).!!
  }

  // prepare python_kreas if dir does not exist in benchmark
  if(!target.exists()) {
    val originalCommit = Process("git rev-parse 1a3ee84", repo).!!

    Process(s"git checkout $originalCommit", repo).!
    val gitlog = Process("git log --oneline", repo).!!

    val commits = gitlog.linesIterator.take(numcommits).map(s => s.substring(0, s.indexOf(' '))).toList

    val basename = repo.getName
    var i = 0
    for (commit <- commits) {
      val dirname = s"$basename-$i-$commit"
      val dir = new File(target, dirname)
      s"mkdir -p $dir".!
      Process(s"git checkout $commit", repo).!
      s"find $repo -path ./.git -prune -o -name '*.$filesuffix' -exec cp {} $dir ;".!
      s"cp $repo/LICENSE $dir".!
      println(s"Copied $repo@$commit to $dir")
      i += 1
    }

    Process(s"git checkout $originalCommit", repo).!
  }
}
