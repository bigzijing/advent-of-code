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

  val clockwiseCoords = if (isClockwise) coords
  else {
    val (head, tail) = coords match {
      case head :: tail => (head, tail)
    }

    head :: tail.reverse.map {
      case c @ LongCoord(_, _, currDir) =>
        currDir match {
          case North => c.copy(currDir = South)
          case South => c.copy(currDir = North)
          case East => c.copy(currDir = West)
          case West => c.copy(currDir = East)
        }
    }
  }

  val westernmost = coords.flatMap(c => List(c.coordStart._1, c.coordEnd._1)).min
  val southernmost = coords.flatMap(c => List(c.coordStart._2, c.coordEnd._2)).max
  coords.foreach(println)
  println(westernmost)
  println(isClockwise)

  val inside = clockwiseCoords.foldLeft(0L) {
    case (acc, next) => next match {
      case LongCoord((x, y1), (_, y2), South) =>


        val toAdd = (x - westernmost + 1) * ((y2 - y1) + 1)
        println(next)
        println(toAdd)

        acc + toAdd
      case LongCoord((x, y1), (_, y2), North) =>

        val toMinus = (x - westernmost + 1) * ((y1 - y2) + 1) - (y1 - y2 + 1 + 2 * (x - westernmost))
        println(next)
        println(toMinus)
        acc - toMinus
      case _ => acc
    }
  }

//  val outside = clockwiseCoords.foldLeft(0L) {
//    case (acc, LongCoord((x1, y1), (x2, y2), _)) =>
//      acc + scala.math.abs(x1 - x2) + 1 + scala.math.abs(y1 - y2)
//  } - 1

  inside

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
    case North => LongCoord((last._1, last._2), (last._1, last._2 - step), direction)
    case South => LongCoord((last._1, last._2), (last._1, last._2 + step), direction)
    case East => LongCoord((last._1, last._2), (last._1 + step, last._2), direction)
    case West => LongCoord((last._1, last._2), (last._1 - step, last._2), direction)
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