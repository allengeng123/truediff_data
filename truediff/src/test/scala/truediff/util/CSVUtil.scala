package truediff.util

object CSVUtil {
  type CSV = IndexedSeq[CSVRow]
  type CSVRow = IndexedSeq[Any]

  def csvToString(csv: CSV): String = {
    csv.map(csvRowToString).mkString("\n")
  }

  def csvRowToString(vals: CSVRow): String = vals.mkString(",")

  def fromCSV(content: String, skipHeader: Boolean = false): CSV = {
    val lines = content.split("\n").toIndexedSeq
    val filtered =
      if (skipHeader && lines.nonEmpty) lines.tail
      else lines
    filtered.map(_.split(","))
  }

  def columnOfCSV(columnName: String, csv: CSV): Seq[Any] = {
    val index = csv.head.indexOf(columnName)
    columnOfCSV(index, csv.tail)
  }

  def columnOfCSV(index: Int, csv: CSV): Seq[Any] = {
    csv.map(row => row(index))
  }

  def csvValAsString(v: Any): String = v.asInstanceOf[String]
  def csvValAsInt(v: Any): Int = csvValAsString(v).trim().toInt
  def csvValAsDouble(v: Any): Double = csvValAsString(v).trim().toDouble
}
