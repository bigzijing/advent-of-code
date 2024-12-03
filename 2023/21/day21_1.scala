@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    "...........",
    ".....###.#.",
    ".###.##..#.",
    "..#.#...#..",
    "....#.#....",
    ".##..S####.",
    ".##..#...#.",
    ".......##..",
    ".##.#.####.",
    ".##..##.##.",
    "..........."
  )

  val gridRaw: List[List[Char]] = lines.map(_.toList)
  val grid: List[List[Plot]] = lines.map(_.toList.map(charToPlot))

  val rows = grid.length
  val columns = grid.map(_.length).max

  val start = {
    val (row, i) = gridRaw.zipWithIndex.find {
      case (row, index) => row.contains('S')
    }.get
    val (_, j) = row.zipWithIndex.find {
      case (char, index) => char == 'S'
    }.get

    (i, j)
  }

  val rocks = grid.zipWithIndex.flatMap {
      case (row, i) => row.zipWithIndex.map {
        case (plot, j) => (plot, i, j)
      }
    }.filter {
      case (plot @ Rock, i, j) => true
      case _ => false
    }
    .map {
      case (_, i, j) => (i, j)
    }.toSet

//  val updatedGrid = iterateNTimes(64, List(start), grid)(rows, columns)

  def possibleStepsImproved(i: Int, j: Int, traversedSteps: Set[(Int, Int)]): List[(Int, Int)] = {
    val canGoUp =
      if (i - 1 < 0) None
      else if (rocks.contains((i - 1, j))) None
//      else if (traversedSteps.contains((i - 1, j))) None
      else Some((i - 1, j))

    val canGoDown =
      if (i + 1 > rows - 1) None
      else if (rocks.contains((i + 1, j))) None
//      else if (traversedSteps.contains((i + 1, j))) None
      else Some((i + 1, j))

    val canGoLeft =
      if (j - 1 < 0) None
      else if (rocks.contains((i, j - 1))) None
//      else if (traversedSteps.contains((i, j - 1))) None
      else Some((i, j - 1))

    val canGoRight =
      if (j + 1 > columns - 1) None
      else if (rocks.contains((i, j + 1))) None
//      else if (traversedSteps.contains(i, j + 1)) None
      else Some((i, j + 1))

    List(canGoUp, canGoDown, canGoLeft, canGoRight).flatten
  }

  def iterateNTimesImproved(n: Int, possibleStepsLs: Set[(Int, Int)], traversedSteps: Set[(Int, Int)]): Set[(Int, Int)] =
    if (n == 0) traversedSteps
    else {
//      val newTraversedSteps = possibleStepsLs.foldLeft(traversedSteps) {
//        case (acc, (i, j)) =>
//          acc + ((i, j))
//      }

      val newPossibleSteps = traversedSteps.flatMap {
        case (i, j) => possibleStepsImproved(i, j, Set.empty)
      }.toSet

      iterateNTimesImproved(n - 1, Set.empty, newPossibleSteps)
    }

//  iterateNTimesImproved(5, Set(start), Set.empty).foreach(c => println(grid(c._1)(c._2)))
  iterateNTimesImproved(64, Set(start), Set(start)).size


//  def algorithm(n: Int, iStart: Int, jStart: Int, currJ: Int, acc: Int): Int = {
//    if (currJ < jStart - n) acc
//    else if (currJ < 0) acc
//    else if (currJ >= columns) algorithm(n, iStart, jStart, currJ - 1, acc)
//    else {
//      val gridsUpAndDown =
//        if (currJ > 0 && currJ > jStart) n - (currJ - n)
//        else if (currJ == jStart) n
//        else n - (jStart - currJ)
//
//      val stepsToCheck = Range(0, gridsUpAndDown + 1).toList.flatMap { difference =>
//
//        val canGoUp = if (iStart - difference >= 0) Some((iStart - difference), currJ) else None
//        val canGoDown = if (iStart + difference < rows) Some((iStart + difference), currJ) else None
//
//        List(canGoUp, canGoDown).flatten
//      }.toSet
//      val intersectWithRocks = stepsToCheck.intersect(rocks)
//
//      println((stepsToCheck.size - intersectWithRocks.size))
////      println(s"Intersects: $intersectWithRocks")
//
////      val newAcc = Range(0, gridsUpAndDown + 1).toList.foldLeft(0) {
////        case (acc, next) =>
////
////          val stepsToTry = Range(iStart - next, iStart + next + 1).toList.map((_, currJ)).toSet
////          val intersectWithRocks = stepsToTry.intersect(rocks)
////
////          println(stepsToTry)
////
////          acc + (stepsToTry.size - intersectWithRocks.size)
////      }
//
//      algorithm(n, iStart, jStart, currJ - 1, acc + (stepsToCheck.size - intersectWithRocks.size))
//    }
//  }
//
////  println(s"Rocks: ${rocks}")
//
//  val (iStart, jStart) = start
//
//  algorithm(6, iStart, jStart, jStart + 6, 0)
}

sealed trait Plot
case object Rock extends Plot
case object Path extends Plot
case object TraversedPath extends Plot

def charToPlot(char: Char): Plot = char match {
  case '.' => Path
  case '#' => Rock
  case 'S' => Path
}


//def iterateNTimes(n: Int, possibleStepsLs: List[(Int, Int)], grid: List[List[Plot]])(rows: Int, columns: Int): List[List[Plot]] =
//  if (n == 0) grid
//  else {
//    val updatedGrid = possibleStepsLs.foldLeft(grid) {
//      case (acc, (i, j)) =>
//        val column = grid(i).updated(j, TraversedPath)
//        grid.updated(i, column)
//    }
//
//    val traversedSteps = updatedGrid.zipWithIndex.flatMap {
//      case (row, i) => row.zipWithIndex.map {
//        case (plot, j) => (plot, i, j)
//      }
//    }.filter {
//      case (plot @ TraversedPath, i, j) => true
//      case _ => false
//    }
//      .map {
//        case (_, i, j) => (i, j)
//      }
//
//    val newPossibleSteps = possibleStepsLs.flatMap {
//      case (i, j) => possibleSteps(i, j, traversedSteps, grid)(rows, columns)
//    }
//
//    iterateNTimes(n - 1, newPossibleSteps, updatedGrid)(rows, columns)
//  }
//
//def possibleSteps(i: Int, j: Int, traversedSteps: List[(Int, Int)], grid: List[List[Plot]])(rows: Int, columns: Int): List[(Int, Int)] = {
//  val canGoUp =
//    if (i - 1 < 0) None
//    else if (grid(i - 1)(j) == Rock) None
//    else if (traversedSteps.contains((i - 1, j))) None
//    else Some((i - 1, j))
//
//  val canGoDown =
//    if (i + 1 > rows - 1) None
//    else if (grid(i + 1)(j) == Rock) None
//    else if (traversedSteps.contains((i + 1, j))) None
//    else Some((i + 1, j))
//
//  val canGoLeft =
//    if (j - 1 < 0) None
//    else if (grid(i)(j - 1) == Rock) None
//    else if (traversedSteps.contains((i, j - 1))) None
//    else Some((i, j - 1))
//
//  val canGoRight =
//    if (j + 1 > columns - 1) None
//    else if (grid(i)(j + 1) == Rock) None
//    else if (traversedSteps.contains(i, j + 1)) None
//    else Some((i, j + 1))
//
//  List(canGoUp, canGoDown, canGoLeft, canGoRight).flatten
//}