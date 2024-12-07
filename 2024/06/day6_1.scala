@main
def main(file: String = "input.txt") = {

  case class Coord(x: Int, y: Int)

  sealed trait Direction {
    def nextCoord(curr: Coord): Coord
  }

  object Direction {
    case object North extends Direction {
      def nextCoord(curr: Coord) = curr.copy(y = curr.y - 1)
    }

    case object South extends Direction {
      def nextCoord(curr: Coord) = curr.copy(y = curr.y + 1)
    }

    case object East extends Direction {
      def nextCoord(curr: Coord) = curr.copy(x = curr.x + 1)
    }

    case object West extends Direction {
      def nextCoord(curr: Coord) = curr.copy(x = curr.x - 1)
    }
  }

//  val grid =
//    """....#.....
//      |.........#
//      |..........
//      |..#.......
//      |.......#..
//      |..........
//      |.#..^.....
//      |........#.
//      |#.........
//      |......#..."""
//      .stripMargin
//      .split("\n")

  val grid = scala.io.Source.fromFile(file).getLines
    .toList

  val totalY = grid.length
  val maxY = totalY - 1
  val totalX = grid.head.length
  val maxX = totalX - 1

  val startingPoint = {
    val rowWithCaret = grid.find(_.contains("^")).get
    val y = grid.indexOf(rowWithCaret)
    val x = rowWithCaret.indexOf('^')

    Coord(x, y)
  }

  def getGrid(coord: Coord): Char = coord match {
    case Coord(x, y) => grid(y)(x)
  }

  def turnRight90(direction: Direction) = direction match {
    case Direction.North => Direction.East
    case Direction.South => Direction.West
    case Direction.East => Direction.South
    case Direction.West => Direction.North
  }

  def whatDo(curr: Coord, direction: Direction): (Coord, Direction) =
    getGrid(curr) match {
      case '#' =>
        (curr, turnRight90(direction))
      case '.' =>
        (direction.nextCoord(curr), direction)
      case '^' =>
        (direction.nextCoord(curr), direction)
    }

  def takeNextStep(curr: Coord, direction: Direction): Option[(Coord, Direction)] = {
    val nextStep = direction.nextCoord(curr)

    if (winCondition(nextStep)) then None
    else
      getGrid(nextStep) match {
        case '#' => takeNextStep(curr, turnRight90(direction))
        case '.' => Some((direction.nextCoord(curr), direction))
        case '^' => Some((direction.nextCoord(curr), direction))
      }
  }

  def winCondition(coord: Coord) = coord match {
    case Coord(x, y) => x < 0 || x > maxX || y < 0 || y > maxY
  }

  def traverse(curr: Coord, direction: Direction, acc: Set[Coord]): Set[Coord] =
    takeNextStep(curr, direction) match {
      case None => acc + curr
      case Some((newCoordToTake, newDirectionToMove)) => traverse(newCoordToTake, newDirectionToMove, acc + curr)
    }

  traverse(startingPoint, Direction.North, Set.empty).size
}