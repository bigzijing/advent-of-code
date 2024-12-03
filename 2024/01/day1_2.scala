@main
def main(file: String = "input.txt") = {
  case class Row(first: Long, second: Long)
  case class RawRow(first: String, second: String) {
    def toRow = Row(first.toInt.toLong, second.toInt.toLong)
  }

  val lines: List[Row] = scala.io.Source.fromFile(file).getLines
    .map { row =>
      row.split("   ").toList match {
        case List(f, s) => RawRow(f, s).toRow
      }
    }
    .toList

  val leftSorted = lines.map(_.first).sorted
  val rightMap = lines.groupBy(_.second).map {
    case (k, v) => (k, v.size)
  }

  leftSorted.foldLeft(0L) {
    case (acc, next) =>
      val count = rightMap.getOrElse(next, 0)
      acc + (count * next)
  }
}