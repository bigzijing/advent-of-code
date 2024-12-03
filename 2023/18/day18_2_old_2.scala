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

  val inputs = sampleLines.map { line =>
    line.split(" ") match {
      case Array(dir, steps, colorRaw) =>
        Row(charToDir(dir.head), steps.toLong)
    }
  }

  //  val inputs = sampleLines.map { line =>
  //    line.split(" ") match {
  //      case Array(_, _, raw) =>
  //        val instruction = raw.drop(2).dropRight(1)
  //        val meters = Integer.parseInt(instruction.dropRight(1), 16).toLong
  //        val dir = instruction.last match {
  //          case '0' => East
  //          case '1' => South
  //          case '2' => West
  //          case '3' => North
  //        }
  //
  //        Row(dir, meters)
  //    }
  //  }

  val isClockwise = inputs.dropWhile {
    case Row(dir, _) => List(North, South).contains(dir)
  }.headOption.map(_.direction).contains(East)


  val coords = (inputs).foldLeft(List(LongCoord((0L, 0L), (0L, 0L), Stationary))){
    case (acc, next) => (acc.last, next) match {
      case (LongCoord(_, (x, y), _), Row(dir, steps)) => acc.appended(buildCoord((x, y), dir, steps))
    }
  }

  val areaExcludingEdge = coords.sliding(2).toList.foldLeft(0L) {
    case (acc, next) => next match {
      case List(LongCoord((x, yStart), (_, yEnd), South), _) if isClockwise =>

        // find nearest coords to the left
        val coordsToTheLeft = coords.filter {
            case LongCoord((_, y), (_, _), West) => y >= yStart && y <= yEnd
            case LongCoord((_, _), (_, y), currDir) => y >= yStart && y <= yEnd
          }
          .map {
            case LongCoord((x1, y), (_, _), West) => (x - x1) -> InclusiveRange(y, y)
            case LongCoord((_, _), (x1, y), East) => (x - x1) -> InclusiveRange(y, y)
            case LongCoord((x1, y1), (x2, y2), _) => (x - x1) -> InclusiveRange(y1, y2)
          }
          .filter(_._1 > 0)
          .sortBy(_._2.a)

      case List(LongCoord(coordStart, coordEnd, South), _) =>
        // find nearest coords to the right
        ???

      case List(LongCoord(coordStart, (x1, y1), West), LongCoord(_, _, South)) if isClockwise =>
        // find nearest coords to the left of the westernmost coord

        coords.find {
          case LongCoord((x2, y2), (x3, y3), _) => y1 >= y2 && y3 >= y1
        } match {
          case Some(LongCoord((x2, y2), (x3, y3), _)) => acc + List(x1 - x2, x1 - x3).filter(_ > 0).min
        }

      case List(LongCoord(coordStart, coordEnd, East), LongCoord(_, _, South)) =>
        // find nearest coords to the right of the easternmost coord

        coords.find {
          case LongCoords((x2, y2), (x3, y3), _) => y1 >= y2 && y3 >= y1
        } match {
          case Some(LongCoord((x2, y2), (x3, y3), _)) => acc + List(x2 - x1, x3 - x1).filter(_ > 0).min
        }

      case _ => acc
    }
  }

  //  val areaExcludingEdge = coords.sliding(2).toList.foldLeft(0) {
  //    case (acc, next) => next match {
  //      case List(Coord(x, y, South), _) if isClockwise =>
  //        val coordsToTheLeft = coords.filter(_.y == y)
  //        val spacesInBetween = coordsToTheLeft.map(x - _.x).filter(_ > 0).min - 1
  //
  //        acc + spacesInBetween
  //
  //      case List(Coord(x, y, South), _) =>
  //        val coordsToTheRight = coords.filter(_.y == y)
  //        val spacesInBetween = coordsToTheRight.map(_.x - x).filter(_ > 0).min - 1
  //
  //        acc + spacesInBetween
  //
  //      case List(Coord(x, y, West), Coord(_, _, South)) if isClockwise =>
  //        val coordsToTheLeft = coords.filter(_.y == y)
  //        val spacesInBetween = coordsToTheLeft.map(x - _.x).filter(_ > 0).min - 1
  //
  //        acc + spacesInBetween
  //
  //      case List(Coord(x, y, West), Coord(_, _, South)) =>
  //        val coordsToRight = coords.filter(_.y == y)
  //        val spacesInBetween = coordsToRight.map(_.x - x).filter(_ > 0).min - 1
  //
  //        acc + spacesInBetween
  //
  //      case _ => acc
  //    }
  //  }
  //
  //  areaExcludingEdge + inputs.map(_.steps).sum

  coords
}

case class InclusiveRange(a: Long, b: Long)

sealed trait Direction

case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction
case object Stationary extends Direction

case class Row(direction: Direction, steps: Long)

case class LongCoord(coordStart: (Long, Long), coordEnd: (Long, Long), currDir: Direction)

def charToDir(char: Char): Direction = char match {
  case 'R' => East
  case 'L' => West
  case 'D' => South
  case 'U' => North
}

def buildCoord(last: (Long, Long), direction: Direction, step: Long): LongCoord =
  direction match {
    case North => LongCoord((last._1, last._2 - 1), (last._1, last._2 - step), direction)
    case South => LongCoord((last._1, last._2 + 1), (last._1, last._2 + step), direction)
    case East => LongCoord((last._1 + 1, last._2), (last._1 + step, last._2), direction)
    case West => LongCoord((last._1 - 1, last._2), (last._1 - step, last._2), direction)
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