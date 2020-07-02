package truediff

import java.io.File

import sys.process._

object FetchCommits extends App {

  val repo = new File(args(0))
  val target = new File(args(1))
  val filesuffix = args(2)
  val numcommits = args(3).toInt

  val originalCommit = Process("git rev-parse HEAD", repo).!!
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
