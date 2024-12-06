@main
def main(file: String = "input.txt") = {

//    val grid = """MMMSXXMASM
//                 |MSAMXMSMSA
//                 |AMXSXMAAMM
//                 |MSAMASMSMX
//                 |XMASAMXAMM
//                 |XXAMMXXAMA
//                 |SMSMSASXSS
//                 |SAXAMASAAA
//                 |MAMMMXMMMM
//                 |MXMXAXMASX""".stripMargin
//      .split("\n")
//      .toList

  val grid = scala.io.Source.fromFile(file).getLines
    .toList

  val totalY = grid.length
  val maxY = totalY - 1
  val totalX = grid.head.length
  val maxX = totalX - 1

  case class Coord(x: Int, y: Int) {
    def canBeXMas = (x - 1 >= 0 && y - 1 >= 0 && x + 1 <= maxX && y + 1 <= maxY)

    def topLeftIsXmas = grid(y - 1)(x - 1) match {
      case 'M' => grid(y + 1)(x + 1) == 'S'
      case 'S' => grid(y + 1)(x + 1) == 'M'
      case _ => false
    }

    def topRightIsXmas = grid(y - 1)(x + 1) match {
      case 'M' => grid(y + 1)(x - 1) == 'S'
      case 'S' => grid(y + 1)(x - 1) == 'M'
      case _ => false
    }

    def isXMas = canBeXMas && topLeftIsXmas && topRightIsXmas
  }

  def iterateGrid(coord: Coord, acc: Int): Int = {

    val isA = grid(coord.y)(coord.x) == 'A'
    val currentCoord = if (isA && coord.isXMas) then 1 else 0

    coord match {
      case Coord(x, y) if x == maxX && y == maxY => acc + currentCoord
      case Coord(x, y) if x == maxX => iterateGrid(Coord(0, y + 1), acc + currentCoord)
      case Coord(x, y) => iterateGrid(Coord(x + 1, y), acc + currentCoord)
    }
  }

  iterateGrid(Coord(0, 0), 0)
}
