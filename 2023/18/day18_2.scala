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

//  val inputs = lines.map { line =>
//    line.split(" ") match {
//      case Array(dir, steps, colorRaw) =>
//        Row(charToDir(dir.head), steps.toLong)
//    }
//  }

  val inputs = sampleLines.map { line =>
    line.split(" ") match {
      case Array(_, _, raw) =>
        val instruction = raw.drop(2).dropRight(1)
        val meters = Integer.parseInt(instruction.dropRight(1), 16).toLong
        val dir = instruction.last match {
          case '0' => East
          case '1' => South
          case '2' => West
          case '3' => North
        }

        Row(dir, meters)
    }
  }

  val isClockwise = inputs.dropWhile {
    case Row(dir, _) => List(North, South).contains(dir)
  }.headOption.map(_.direction).contains(East)

  val inputsReshuffled = {
    val toCheckForOrientation = inputs.head.direction match {
        case East | West => inputs
        case _ =>
          val head = inputs.head
          inputs.drop(1).appended(head)
      }

    if (isClockwise) toCheckForOrientation
    else toCheckForOrientation.reverse.map {
      case r @ Row(North, _) => r.copy(direction = South)
      case r @ Row(South, _) => r.copy(direction = North)
      case r @ Row(East, _) => r.copy(direction = West)
      case r @ Row(West, _) => r.copy(direction = East)
    }
  }

  println(inputsReshuffled)

//  val coords = inputsReshuffled.foldLeft(List((0L, 0L))) {
//    case (acc, Row(direction, steps)) =>
//      (acc.head, direction) match {
//        case ((x, y), North) => (x, y - steps) :: acc
//        case ((x, y), South) => (x, y + steps) :: acc
//        case ((x, y), East) => (x + steps, y) :: acc
//        case ((x, y), West) => (x - steps, y) :: acc
//      }
//  }

  val coords = inputsReshuffled.foldLeft(List.empty[Coord]) {
    case (acc, Row(dir, steps)) =>
      val (accLastX, accLastY) = acc.lastOption.map(c => ((c.x, c.y))).getOrElse((0L, 0L))

      dir match {
        case North => acc.appended(Coord(accLastX, accLastY - steps, dir, steps))
        case South => acc.appended(Coord(accLastX, accLastY + steps, dir, steps))
        case East => acc.appended(Coord(accLastX + steps, accLastY, dir, steps))
        case West => acc.appended(Coord(accLastX - steps, accLastY, dir, steps))
      }
  }

  println(coords)

  val minX = coords.map(_.x).min
  val maxX = coords.map(_.x).max
  val minY = coords.map(_.y).min
  val maxY = coords.map(_.y).max

  println(s"minX: $minX, maxX: $maxX, minY: $minY, maxY: $maxY")

  val area = coords.grouped(2).toList.map {
    case List(Coord(x1, y1, direction1, steps), Coord(x2, y2, direction2, _)) =>
      direction2 match {
        case North if y1 == maxY =>
          -(y1 - y2) * (x2 - minX)
        case North =>
          -(y1 - y2 - 1) * (x2 - minX)
        case South if y2 == maxY =>
          (y2 - y1 + 1) * (x2 - minX + 1) + (if (direction1 == West) steps else 0)
        case South =>
          (y2 - y1) * (x2 - minX + 1) + (if (direction1 == West) steps else 0)
        case _ => throw new Exception("direction2 should not be anything except East or West")
      }
    case List(single) =>
      throw new Exception("grouped by 2 has one single element in last position")
  }


  println(area)

  area.sum

}


sealed trait Direction

case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

case class Row(direction: Direction, steps: Long)
case class Coord(x: Long, y: Long, direction: Direction, steps: Long)

def charToDir(char: Char): Direction = char match {
  case 'R' => East
  case 'L' => West
  case 'D' => South
  case 'U' => North
}