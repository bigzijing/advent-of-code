@main
def main(file: String = "input.txt") = {

//  val grid = """MMMSXXMASM
//               |MSAMXMSMSA
//               |AMXSXMAAMM
//               |MSAMASMSMX
//               |XMASAMXAMM
//               |XXAMMXXAMA
//               |SMSMSASXSS
//               |SAXAMASAAA
//               |MAMMMXMMMM
//               |MXMXAXMASX""".stripMargin
//    .split("\n")
//    .toList

  val grid = scala.io.Source.fromFile(file).getLines
    .toList

  val totalY = grid.length
  val maxY = totalY - 1
  val totalX = grid.head.length
  val maxX = totalX - 1

  case class Coord(x: Int, y: Int)

  sealed trait Direction {
    def nextCoord(current: Coord): Coord
    def canBeStart(current: Coord): Boolean
  }

    case object GoRight extends Direction {
      def nextCoord(current: Coord) = Coord(current.x + 1, current.y)

      def canBeStart(current: Coord) = current.x + 3 <= maxX
    }

    case object GoLeft extends Direction {
      def nextCoord(current: Coord) = Coord(current.x - 1, current.y)

      def canBeStart(current: Coord) = current.x - 3 >= 0
    }

    case object GoUp extends Direction {
      def nextCoord(current: Coord) = Coord(current.x, current.y - 1)

      def canBeStart(current: Coord) = current.y - 3 >= 0
    }

    case object GoDown extends Direction {
      def nextCoord(current: Coord) = Coord(current.x, current.y + 1)

      def canBeStart(current: Coord) = current.y + 3 <= maxY
    }

    case object GoTopRight extends Direction {
      def nextCoord(current: Coord) = Coord(current.x + 1, current.y - 1)

      def canBeStart(current: Coord) = current.x + 3 <= maxX && current.y - 3 >= 0
    }

    case object GoTopLeft extends Direction {
      def nextCoord(current: Coord) = Coord(current.x - 1, current.y - 1)

      def canBeStart(current: Coord) = current.x - 3 >= 0 && current.y - 3 >= 0
    }

    case object GoBottomRight extends Direction {
      def nextCoord(current: Coord) = Coord(current.x + 1, current.y + 1)

      def canBeStart(current: Coord) = current.x + 3 <= maxX && current.y + 3 <= maxY
    }

    case object GoBottomLeft extends Direction {
      def nextCoord(current: Coord) = Coord(current.x - 1, current.y + 1)

      def canBeStart(current: Coord) = current.x - 3 >= 0 && current.y + 3 <= maxY
    }


  lazy val allDirs = List(GoRight, GoLeft, GoUp, GoDown, GoTopRight, GoTopLeft, GoBottomRight, GoBottomLeft)

  def findXMAS(coord: Coord, xmasIndex: Int, direction: Direction): Int = coord match {
    case Coord(x, y) if grid(y)(x) != "XMAS"(xmasIndex) => 0
    case _ if xmasIndex == 3 => 1
    case Coord(x, y) => findXMAS(direction.nextCoord(coord), xmasIndex + 1, direction)
  }

  def iterateGrid(coord: Coord, acc: Int): Int = {

    val xmases = allDirs.filter(_.canBeStart(coord)).foldLeft(0) {
      case (acc, next) => acc + findXMAS(coord, 0, next)
    }

    coord match {
      case Coord(x, y) if x == maxX && y == maxY => acc + xmases
      case Coord(x, y) if x == maxX => iterateGrid(Coord(0, y + 1), acc + xmases)
      case Coord(x, y) => iterateGrid(Coord(x + 1, y), acc + xmases)
    }
  }

  iterateGrid(Coord(0, 0), 0)
}
