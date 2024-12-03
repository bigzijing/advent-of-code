@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  def recursiveSteps(sequence: List[Int], acc: List[List[Int]]): List[List[Int]] = {
    val steps = getNext(sequence)

    steps.forall(_ == 0) match {
      case true => steps :: acc
      case false => recursiveSteps(steps, steps :: acc)
    }
  }

  lines
    .map { row =>
      val ints = row.split(" ").toList.map(_.toInt)

      recursiveSteps(ints, List(ints))
        .foldLeft(0) {
          case (acc, next) =>
            next.head - acc
        }
    }
    .sum

}

def getNext(sequence: List[Int]): List[Int] =
  sequence.sliding(2).map {
    case List(a, b) => b - a
  }.toList