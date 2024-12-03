@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    "R 6 (#70c710)",
    "D 5 (#0dc571)",
    "L 2 (#5713f0)",
    "D 2 (#d2c081)",
    "R 2 (#59c680)",
    "D 2 (#411b91)",
    "L 5 (#8ceee2)",
    "U 2 (#caa173)",
    "L 1 (#1b58a2)",
    "U 2 (#caa171)",
    "R 2 (#7807d2)",
    "U 3 (#a77fa3)",
    "L 2 (#015232)",
    "U 2 (#7a21e3)"
  )

  val inputs = lines.map { line =>
    line.split(" ") match {
      case Array(dir, steps, colorRaw) =>
        Row(charToDir(dir.head), steps.toInt, colorRaw.drop(1).dropRight(1))
    }
  }

  val isClockwise = inputs.dropWhile {
    case Row(dir, _, _) => List(North, South).contains(dir)
  }.headOption.map(_.direction).contains(East)

  val coords = (inputs).foldLeft(List(Coord(0, 0, Stationary))){
    case (acc, next) => (acc.last, next) match {
      case (Coord(x, y, _), Row(North, steps, _)) => acc concat intoCoord(incrementY(x, y, steps, false), North)
      case (Coord(x, y, _), Row(South, steps, _)) => acc concat intoCoord(incrementY(x, y, steps), South)
      case (Coord(x, y, _), Row(East, steps, _)) => acc concat intoCoord(incrementX(x, y, steps), East)
      case (Coord(x, y, _), Row(West, steps, _)) => acc concat intoCoord(incrementX(x, y, steps, false), West)
    }
  }

  val areaExcludingEdge = coords.sliding(2).toList.foldLeft(0) {
    case (acc, next) => next match {
      case List(Coord(x, y, South), _) if isClockwise =>
        val coordsToTheLeft = coords.filter(_.y == y)
        val spacesInBetween = coordsToTheLeft.map(x - _.x).filter(_ > 0).min - 1

        acc + spacesInBetween

      case List(Coord(x, y, South), _) =>
        val coordsToTheRight = coords.filter(_.y == y)
        val spacesInBetween = coordsToTheRight.map(_.x - x).filter(_ > 0).min - 1

        acc + spacesInBetween

      case List(Coord(x, y, West), Coord(_, _, South)) if isClockwise =>
        val coordsToTheLeft = coords.filter(_.y == y)
        val spacesInBetween = coordsToTheLeft.map(x - _.x).filter(_ > 0).min - 1

        acc + spacesInBetween

      case List(Coord(x, y, West), Coord(_, _, South)) =>
        val coordsToRight = coords.filter(_.y == y)
        val spacesInBetween = coordsToRight.map(_.x - x).filter(_ > 0).min - 1

        acc + spacesInBetween

      case _ => acc
    }
  }

  areaExcludingEdge + inputs.map(_.steps).sum
}

sealed trait Direction

case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction
case object Stationary extends Direction

case class Row(direction: Direction, steps: Int, color: String)

case class Coord(x: Int, y: Int, currDir: Direction)

def charToDir(char: Char): Direction = char match {
  case 'R' => East
  case 'L' => West
  case 'D' => South
  case 'U' => North
}

def incrementX(x: Int, y: Int, n: Int, add: Boolean = true): List[(Int, Int)] =
  Range(1, n + 1).toList.map(inc => (if (add) x + inc else x - inc, y))

def incrementY(x: Int, y: Int, n: Int, add: Boolean = true): List[(Int, Int)] =
  Range(1, n + 1).toList.map(inc => (x, if (add) y + inc else (y - inc)))

def intoCoord(coords: List[(Int, Int)], currDir: Direction): List[Coord] =
  coords.map {
    case (x, y) => Coord(x, y, currDir)
  }

def areaBetweenTwoNearestPoints(x: Int, y: Int, direction: Direction, isClockwise: Boolean, grid: List[(Int, Int)]) =
  direction match {
    case East =>
      val nearestWestCoords = grid.filter {
        case (x2, y2) => x == x2
      }
    case South => ???
    case _ => 0
  }