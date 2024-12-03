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
  val rightSorted = lines.map(_.second).sorted

  val zipped = leftSorted.zip(rightSorted)

  zipped.map {
    case (l, r) =>
      val diff = l - r
      if (diff < 0) -diff else diff
  }.sum
}