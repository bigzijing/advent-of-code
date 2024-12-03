@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List("Time:      7  15   30", "Distance:  9  40  200")

  val parsedLines = lines
    .map(_.split(" ").map(_.filter(s => s.isDigit)))
    .map(_.filterNot(_ == "").toList) match {
    case List(timeStr, speedStr) =>
      timeStr.map(_.toInt).zip(speedStr.map(_.toInt)).zipWithIndex
  }

  val races = parsedLines
    .map {
    case ((time, dist), in) => Race(in + 1, time, dist)
    }

  races.map {
    case Race(_, time, distance) =>
      Range(1, time)
        .filter { charge =>
          (time - charge) * charge > distance
        }
      .toList
    }
    .map(_.size)
    .product
}

case class Race(no: Int, time: Int, distance: Int)