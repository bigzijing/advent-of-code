@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

//  val lines = List(
//    "...#......",
//    ".......#..",
//    "#.........",
//    "..........",
//    "......#...",
//    ".#........",
//    ".........#",
//    "..........",
//    ".......#..",
//    "#...#....."
//  )

  val grid = lines.map(_.toList)

  val rows = lines.size
  val columns = lines.map(_.length).max

  def expandedColumn(columns: Int): List[Char] = List.fill(columns)('C')

  def expandTheUniverseVertically(i: Int, rows: Int, acc: List[List[Char]]): List[List[Char]] = {
    if (i == rows) acc
    else {
      val currentRow = acc(i)
      if (currentRow.forall(_ == '.')) { // expand
        val columnSize = currentRow.size
        val newUniverse = acc.updated(i, expandedColumn(columnSize))

        expandTheUniverseVertically(i + 1, rows, newUniverse)
      } else expandTheUniverseVertically(i + 1, rows, acc)
    }
  }

  def expandTheUnveriseHorizontally(j: Int, columns: Int, acc: List[List[Char]]): List[List[Char]] = {
    if (j == columns) acc
    else {
      val currentCol = acc.map(_(j))
      if (currentCol.forall(c => List('.', 'C').contains(c))) { // expand
        val newUniverse = acc.map { rows =>
          rows.updated(j, 'C')
        }

        expandTheUnveriseHorizontally(j + 1, columns, newUniverse)
      } else expandTheUnveriseHorizontally(j + 1, columns, acc)
    }
  }

  val expandedUniverse: List[List[Char]] = expandTheUnveriseHorizontally(0, columns, expandTheUniverseVertically(0, rows, grid))

  val starsMap = expandedUniverse.zipWithIndex.map {
    case ((ls), i) =>
      ls.zipWithIndex.collect {
        case ((row), j) if row == '#' =>
          val newI =
            if (i - 1 < 0) i
            else
              expandedUniverse.take(i).filter(ls => ls.forall(_ == 'C')).size * (1000000 - 1) + i

          val newJ =
            if (j - 1 < 0) j
            else {
              val miniUniverse = expandedUniverse.map(_.take(j))
              Range(0, j).toList.map { j =>
                miniUniverse.map(_(j))
              }.filter(ls => ls.forall(_ == 'C')).size * (1000000 - 1) + j
            }

          (newI, newJ)
      }
  }.flatten

  val starsNo = Range(0, starsMap.size).toList
  val starsMapLong = starsMap.map {
    case (a, b) => (a.toLong, b.toLong)
  }

  starsNo.foldLeft(List.empty[((Long, Long), (Long, Long))]) {
      case (acc, next) =>
        val toAppend = starsMapLong.drop(next) match {
          case head :: tail => tail.foldLeft(List.empty[((Long, Long), (Long, Long))]) {
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

//  println(starsMap)
//  println(grid.mkString("\n"))
//  println("\n\n\n")
//  println(expandedUniverse.map(_.mkString).mkString("\n"))
}