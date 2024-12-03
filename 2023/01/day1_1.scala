@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines

  val coords = lines.map {
    row =>
      val left = row.foldLeft("") {
        case (acc, next) =>
          if (acc == "")
            if (next.isDigit) next.toString else acc
          else acc
      }
      val right = row.foldRight("") {
        case (next, acc) =>
          if (acc == "")
            if (next.isDigit) next.toString else acc
          else acc
      }

      (left + right).toInt
  }

  coords.sum
}