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

  val gridsWithSymbols = totalMoves.collect {
    case (i, j) =>
      val char = grid(i)(j)
      if (!char.isDigit && char != '.')
        Some(i, j, char)
      else
        None
  }
    .flatten

  val (gridsWithAsterisk, _) = gridsWithSymbols.partition(_._3 == '*')

  def canGoLeft(i: Int, j: Int) =
    if (j - 1 >= 0) Some((i, j - 1)) else None

  def canGoAbove(i: Int, j: Int) =
    if (i - 1 >= 0) Some((i - 1, j)) else None

  def canGoRight(i: Int, j: Int) =
    if (j + 1 < columns) Some((i, j + 1)) else None

  def canGoBelow(i: Int, j: Int) =
    if (i + 1 < rows) Some((i + 1, j)) else None

  def getSurroundingCoords(i: Int, j: Int)(rows: Int, columns: Int): List[(Int, Int)] = {
    val rowAbove = {
      if (i - 1 >= 0) // can check above
        if (j - 1 >= 0) // can check left
          if (j + 1 < columns) // can check right
            Range(j - 1, j + 2).toList
          else // cannot check right
            Range(j - 1, j + 1).toList
        else // cannot check left
          if (j + 1 < columns) // can check right
            Range(j, j + 2).toList
          else // cannot check right
            Range(j, j + 1).toList
      else List.empty
    }.map(j => (i - 1, j))

    val sameRow2 = List(canGoLeft(i, j), canGoRight(i, j)).flatten

    val sameRow = {
      if (j - 1 >= 0) // can check left
        if (j + 1 < columns) // can check right
          List(j - 1, j + 1)
        else // cannot check right
          List(j - 1, j)
      else // cannot check left
        if (j + 1 < columns) // can check right
          List(j + 1)
        else // cannot check right
          List.empty
    }.map(j => (i, j))

    if (sameRow == sameRow2) println("SAME SAME!") else println("NOT SAME ):")

    val rowBelow = {
      if (i + 1 < rows) // can check below
        if (j - 1 >= 0) // can check left
          if (j + 1 < columns) // can check right
            Range(j - 1, j + 2).toList
          else // cannot check right
            Range(j - 1, j + 1).toList
        else // cannot check left
          if (j + 1 < columns) // can check right
            Range(j, j + 2).toList
          else // cannot check right
            Range(j, j + 1).toList
      else // cannot check below
        List.empty
    }.map(j => (i + 1, j))

    val surroundingGrids = rowAbove.concat(sameRow).concat(rowBelow)

    surroundingGrids
  }

  def checkSurroundingAsterisk(i: Int, j: Int, grid: List[List[Char]]) = ???

  def traverseLeft(grid: List[List[Char]], i: Int, j: Int)(acc: String): String =
    if (j - 1 >= 0) {
      val newChar = grid(i)(j - 1)
      if (newChar.isDigit) traverseLeft(grid, i, j - 1)(acc.prepended(newChar))
      else acc
    } else acc

  def traverseRight(grid: List[List[Char]], i: Int, j: Int)(acc: String): String =
    if (j + 1 < columns) {
      val newChar = grid(i)(j + 1)
      if (newChar.isDigit) traverseRight(grid, i, j + 1)(acc.appended(newChar))
      else acc
    } else acc

  def groupSurroundingDigits(ungrouped: List[(Int, Int)]) = {
    ungrouped.foldLeft(List(List.empty[(Int, Int)])) {
      case (acc, (i, j)) =>
        acc.find(ls => ls.exists(coord => (coord._1 == i && coord._2 + 1 == j))) match {
          case None => List((i, j)) :: acc
          case Some(v) =>
            val index = acc.indexOf(v)
            acc.updated(index, v.appended((i, j)))
        }
    }.map(_.sortBy(_._2))
      .filterNot(_.isEmpty)
  }

  val aroundStupidAsterisks = gridsWithAsterisk.map {
    case (i, j, _) =>
      val surroundingDigits = getSurroundingCoords(i, j)(rows, columns)
      .filter {
        case (i, j) => grid(i)(j).isDigit
      }

      val surroundDigitsGrouped = surroundingDigits.foldLeft(List(List.empty[(Int, Int)])) {
        case (acc, (i, j)) =>
          acc.find(ls => ls.exists(coord => (coord._1 == i && coord._2 + 1 == j))) match {
            case None => List((i, j)) :: acc
            case Some(v) =>
              val index = acc.indexOf(v)
              acc.updated(index, v.appended((i, j)))
          }
      }.map(_.sortBy(_._2))
        .filterNot(_.isEmpty)

      surroundDigitsGrouped.map { numberStrs =>
        val (headI, headJ) = numberStrs.head

        val traversedLeft = traverseLeft(grid, headI, headJ)(grid(headI)(headJ).toString)
        val traversedRight = traverseRight(grid, headI, headJ)(traversedLeft)

        traversedRight.toInt
      }
  }
    .filter(_.size == 2)
    .map(_.product)
    .sum

  println(gridsWithAsterisk.size)


  aroundStupidAsterisks
}
