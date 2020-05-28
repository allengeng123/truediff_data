//package truediff.json
//
//object Benchmark extends App {
//
//  private def benchJsonFileByLine(path: String): Unit = {
//    var i = 0
//    var sizes: Int = 0
//    var parses: Double = 0
//    var diffs: Double = 0
//    foreachFileLine(s"benchmark/json/$path") { line =>
//      val (size, parse, diff) = benchJson(s"line$i of $path", line)
//      sizes += size
//      parses += parse
//      diffs += diff
//      i += 1
//    }
//
//    println(s"Benchmarking JSON document $path by line")
//    println(s"  tree size: ${sizes.toDouble / i} nodes")
//    println(s"  parsing: ${parses / i} ms")
//    println(s"  diffing unchanged: ${diffs / i} ms")
//  }
//
//  private def benchJsonFile(path: String): Unit = {
//    val (size, parse, diff) = benchJson(path, readFile(s"benchmark/json/$path"))
//    println(s"Benchmarking JSON document $path")
//    println(s"  tree size: $size nodes")
//    println(s"  parsing: $parse ms")
//    println(s"  diffing unchanged: $diff ms")
//  }
//
//  private def benchJson(name: String, content: String): (Int, Double, Double) = {
//    val (tree,parseTime) = timed(Parser.parse(content))
//    val (patch,diffIdenticalTime) = timed(tree.compareTo(tree), discard = 100, repeat = 10)
//    (tree.size, parseTime, diffIdenticalTime)
//  }
//
//  benchJsonFile("parboiled2bench.json")
////  benchJsonFile("canada.json")
//  benchJsonFile("citm_catalog.json")
//  benchJsonFile("twitter.json")
//  benchJsonFileByLine("one-json-per-line.jsons")
//
//}
