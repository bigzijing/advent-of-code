@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List("Time:      7  15   30", "Distance:  9  40  200")

  val (time, dist) = lines
    .map(_.split(" ").map(_.filter(s => s.isDigit)))
    .map(_.filterNot(_ == "").toList) match {
    case List(timeStr, speedStr) =>
      (timeStr.mkString.toDouble, speedStr.mkString.toDouble)
  }

  // x^2 - (time) * x + (dist)
  val root1 = ((-(time) + scala.math.sqrt(time * time - (4 * dist))) / 2)
  val root2 = (-(time) - scala.math.sqrt(time * time - (4 * dist))) / 2

  (math.abs(root1.ceil - root2.ceil).toInt)
}

case class Race(no: Int, time: Int, distance: Int)