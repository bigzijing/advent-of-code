@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  )

  val grid: List[List[Char]] = lines.map(_.toList)

  val rows = grid.length
  val columns = grid.map(_.length).max

  val totalMoves = Range(0, rows).foldLeft(List.empty[(Int, Int)]) {
    case (acc, next) =>
      acc.concat(Range(0, columns).map(j => (next, j)).toList)
  }

  def checkRows(row: List[Char], currNumAcc: String, j: Int)(i: Int, grid: List[List[Char]]): Option[(Int, Int, Int, Int)] = {

//    println(s"Current i: $i, current j: $j")

//    println(s"Current row: ${row.mkString}")

    if (!row(j).isDigit) // not digit, so process current collected number
      checkSurroundingGrids(i, j, currNumAcc)(row)
    else
      if (j + 1 >= columns)
        checkSurroundingGrids(i, j, currNumAcc + row(j))(row, false)
      else checkRows(row, currNumAcc + row(j), j + 1)(i, grid)
  }

  def checkSurroundingGrids(i: Int, j: Int, currNumAcc: String)(row: List[Char], canGoNext: Boolean = true) = {
//    println(s"Char at ($i, $j): ${row(j)}")

    if (currNumAcc == "") // nothing collected
      None
    else { // process digit
      val collectedNumber = currNumAcc.toInt
      val numberLen = currNumAcc.size

      val (jStart, jEnd) = if (canGoNext) (j - numberLen, j - 1) else (j - numberLen + 1, j)

//      println(s"Number $collectedNumber appears from grid($i)($jStart) to grid($i)($jEnd)")

      val iMinus1Grids = {
        if (i - 1 >= 0) // can check grid above
          if (jStart - 1 >= 0) // can check grid to the left
            if (jEnd + 1 < columns) // can check grid to the right
              Range(jStart - 1, jEnd + 2).toList
            else // cannot check grid to the right
              Range(jStart - 1, jEnd + 1).toList
          else // cannot check grid to the left
            if (jEnd + 1 < columns) // can check grid to the right
              Range(jStart, jEnd + 2).toList
            else // cannot check grid to the right
              Range(jStart, jEnd + 1).toList
        else // cannot check grid above
          List.empty
      }.map(j => (i - 1, j))

      val leftAndRightGrids = {
        if (jStart - 1 >= 0) // can check grid to the left
          if (jEnd + 1 < columns) // can check grid to the right
            List(jStart - 1, jEnd + 1)
          else // cannot check grid to the right
            List(jStart - 1)
        else // cannot check grid to the left
          if (jEnd + 1 < columns) // can check grid to the right
            List(jEnd + 1)
          else // cannot chek grid to the right
            List.empty
      }.map(j => (i, j))

      val iPlus1Grids = {
        if (i + 1 < rows) // can check grid below
          if (jStart - 1 >= 0) // can check grid to the left
            if (jEnd + 1 < columns) // can check grid to the right
              Range(jStart - 1, jEnd + 2).toList
            else // cannot check grid to the right
              Range(jStart - 1, jEnd + 1).toList
          else // cannot check grid to the left
            if (jEnd + 1 < columns) // can check grid to the right
              Range(jStart, jEnd + 2).toList
            else // cannot check grid to the right
              Range(jStart, jEnd + 1).toList
        else // cannot check grid below
          List.empty
      }.map(j => (i + 1, j))

      val gridsToCheck = iMinus1Grids.concat(leftAndRightGrids).concat(iPlus1Grids)

//      println(s"Grids to check: ${gridsToCheck}")

      val containsSymbol = gridsToCheck.exists {
        case (i, j) =>
          val char = grid(i)(j)
          char != '.' && !char.isDigit
      }

//      println(containsSymbol)

      containsSymbol match {
        case true => Some(collectedNumber, i, jStart, jEnd)
        case _ => None
      }
    }
  }

  Range(0, rows).toList.map { i =>
    Range(0, columns).toList.foldLeft(List.empty[(Int, Int, Int, Int)]) {
      case (acc, nextJ) =>
        acc.exists {
          case (_, _, jStart, jEnd) => nextJ >= jStart && nextJ <= jEnd
        } match {
          case true => acc
          case _ => checkRows(grid(i), "", nextJ)(i, grid) match {
            case Some(value) => value :: acc
            case None => acc
          }
        }
    }
  }
    .flatten
    .map(_._1)
    .sum
}