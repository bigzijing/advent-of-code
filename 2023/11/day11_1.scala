@main
def main(file: String = "input.txt") = {
//  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val lines = List(
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  )

  val grid = lines.map(_.toList)

  val rows = lines.size
  val columns = lines.map(_.length).max

  def expandedColumn(columns: Int): List[Char] = List.fill(columns)('.')

  def expandTheUniverseVertically(i: Int, rows: Int, acc: List[List[Char]]): List[List[Char]] = {
    if (i == rows) acc
    else {
      val currentRow = acc(i)
      if (currentRow.forall(_ == '.')) { // expand
        val right = acc.takeRight(rows - i - 1)
        val columns = acc.size
        val newUniverse = acc.take(i) concat (List(expandedColumn(columns), expandedColumn(columns))) concat right

        expandTheUniverseVertically(i + 2, rows + 1, newUniverse)
      } else expandTheUniverseVertically(i + 1, rows, acc)
    }
  }

  def expandTheUnveriseHorizontally(j: Int, columns: Int, acc: List[List[Char]]): List[List[Char]] = {
    if (j == columns) acc
    else {
      val currentCol = acc.map(_(j))
      if (currentCol.forall(_ == '.')) { // expand
        val newUniverse = acc.map { rows =>
          val right = rows.takeRight(columns - j - 1)
          rows.take(j) concat (List('.', '.')) concat right
        }

        expandTheUnveriseHorizontally(j + 2, columns + 1, newUniverse)
      } else expandTheUnveriseHorizontally(j + 1, columns, acc)
    }
  }

  val expandedUniverse = expandTheUnveriseHorizontally(0, columns, expandTheUniverseVertically(0, rows, grid))

  val starsMap = expandedUniverse.zipWithIndex.map {
    case ((ls), i) =>
      ls.zipWithIndex.collect {
        case ((row), j) if row == '#' => (i, j)
      }
  }.flatten

  val starsNo = Range(0, starsMap.size).toList

  starsNo.foldLeft(List.empty[((Int, Int), (Int, Int))]) {
    case (acc, next) =>
      val toAppend = starsMap.drop(next) match {
        case head :: tail => tail.foldLeft(List.empty[((Int, Int), (Int, Int))]) {
          case (acc, next) => acc appended ((head, next))
        }
        case Nil => acc
      }

      acc concat (toAppend)
  }
    .map {
      case ((a1, b1), (a2, b2)) =>
        (a1 max a2) - (a1 min a2) + ((b1 max b2) - (b1 min b2))
    }
    .sum

//  println(grid.mkString("\n"))
//  println("\n\n\n")
//  println(expandedUniverse.mkString("\n"))

  println(starsMap)
}