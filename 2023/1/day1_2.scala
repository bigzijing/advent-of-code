@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines

  val coords = lines.map {
    row =>
      val rowLen = row.size

      val left = Range(0, rowLen)
        .foldLeft("") {
          case (acc, next) =>
            if (acc == "")
              if (row(next).isDigit) row(next).toString
              else sliceAndMatch(row, next).getOrElse(acc)
            else acc
        }

      val right = Range(rowLen - 1, -1, -1)
        .foldLeft("") {
          case (acc, next) =>
            if (acc == "")
              if (row(next).isDigit) row(next).toString
              else sliceAndMatchReversed(row, next).getOrElse(acc)
            else acc
        }

      (left + right).toInt
  }

  coords.sum
}

def sliceAndMatch(row: String, start: Int): Option[String] =
  row(start) match {
    case 'o' => if (row.slice(start, start + 3) == "one") Some("1") else None
    case 't' => if (row.slice(start, start + 3) == "two") Some("2") else if (row.slice(start, start + 5) == "three") Some("3") else None
    case 'f' => if (row.slice(start, start + 4) == "four") Some("4") else if (row.slice(start, start + 4) == "five") Some("5") else None
    case 's' => if (row.slice(start, start + 3) == "six") Some("6") else if (row.slice(start, start + 5) == "seven") Some("7") else None
    case 'e' => if (row.slice(start, start + 5) == "eight") Some("8") else None
    case 'n' => if (row.slice(start, start + 4) == "nine") Some("9") else None
    case _ => None
  }


def sliceAndMatchReversed(row: String, start: Int): Option[String] =
  row(start) match {
    case 'e' =>
      if (row.slice(start - 2, start + 1) == "one") Some("1")
      else if (row.slice(start - 4, start + 1) == "three") Some("3")
      else if (row.slice(start - 3, start + 1) == "five") Some("5")
      else if (row.slice(start - 3, start + 1) == "nine") Some("9")
      else None
    case 'o' => if (row.slice(start - 2, start + 1) == "two") Some("2") else None
    case 'r' => if (row.slice(start - 3, start + 1) == "four") Some("4") else None
    case 'x' => if (row.slice(start - 2, start + 1) == "six") Some("6") else None
    case 'n' => if (row.slice(start - 4, start + 1) == "seven") Some("7") else None
    case 't' => if (row.slice(start - 4, start + 1) == "eight") Some("8") else None
    case _ => None
  }

private val numbers = List(
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine"
)