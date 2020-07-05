package truediff.json

import truediff.util.CSVUtil._
import truediff.util.BenchmarkUtils._

object DeriveThroughput extends App {
  val csvFiles = files("benchmark/measurements/")
  csvFiles.map { file =>
    val csv = fromCSV(readFile(file.getAbsolutePath))
    val srcSizes = columnOfCSV(1, csv)
    val destSizes = columnOfCSV(2, csv)
    val avgDiffTimes = columnOfCSV(6, csv)
    // println(csvValAsString(columnOfCSV(0, csv)(1)))
    println(file.getAbsolutePath)
    for (i <- 1 until csv.size) {
      val srcSize = csvValAsInt(srcSizes(i))
      val destSize = csvValAsInt(destSizes(i))
      val diffTime = csvValAsDouble(avgDiffTimes(i))
      println((srcSize + destSize) / diffTime)
    }
//    println(csv)
  }
}
