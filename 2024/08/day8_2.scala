@main
def main(file: String = "input.txt"): Unit = {
  import scala.math.abs

//  val grid: List[String] =
//    """............
//      |........0...
//      |.....0......
//      |.......0....
//      |....0.......
//      |......A.....
//      |............
//      |............
//      |........A...
//      |.........A..
//      |............
//      |............
//      |""".stripMargin
//      .split("\n")
//      .toList

//  val grid: List[String] =
//    """T....#....
//      |...T......
//      |.T....#...
//      |.........#
//      |..#.......
//      |..........
//      |...#......
//      |..........
//      |....#.....
//      |..........
//      |""".stripMargin
//      .split("\n")
//      .toList

  val grid: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val maxY = grid.length - 1
  val maxX = grid.head.length - 1

  def condition(acc: List[(Int, Int)], fx: Int => Int, fy: Int => Int): List[(Int, Int)] = acc.head match {
    case (x, y) =>
      val newX = fx(x)
      val newY = fy(y)

      if newX >= 0 && newX <= maxX && newY >= 0 && newY <= maxY then condition((newX, newY) :: acc, fx, fy)
      else acc
  }

  case class Coord(x: Int, y: Int) {
    def minus(other: Coord) = {
      val xDiff = abs(other.x - x)
      val yDiff = abs(other.y - y)

      val xIsBigger = x > other.x
      val yIsBigger = y > other.y

      val firstXCond: Int => Int = if xIsBigger then (x: Int) => x + xDiff else (x: Int) => x - xDiff
      val firstYCond: Int => Int = if yIsBigger then (y: Int) => y + yDiff else (y: Int) => y - yDiff
      val secondXCond: Int => Int = if xIsBigger then (x: Int) => x - xDiff else (x: Int) => x + xDiff
      val secondYCond: Int => Int = if yIsBigger then (y: Int) => y - yDiff else (y: Int) => y + yDiff

        {
          condition(
            List((x, y)),
            firstXCond,
            firstYCond
          ) ++
            condition(
              List((other.x, other.y)),
              secondXCond,
              secondYCond
            )
        }.map {
          case (x, y) => Coord(x, y)
        }
    }
  }

  def getGrid(x: Int, y: Int): Char =
    grid(y)(x)

  val allLetters = grid.flatten.filterNot(_ == '.').filterNot(_ == '#').toSet.toList

  def findLinesInGrid(grid: List[String], acc: List[Int], char: Char): List[Int] =
    grid.find(line => line.contains(char)) match {
      case Some(line) =>
        findLinesInGrid(grid.patch(grid.indexOf(line), List("."), 1), grid.indexOf(line) :: acc, char)
      case None => acc
    }

  def findCoordsInLine(line: String, acc: List[Int], char: Char): List[Int] =
    line.indexOf(char) match {
      case -1 => acc
      case in => findCoordsInLine(line.patch(in, ".", 1), in :: acc, char)
    }

  val allCoords: List[(Char, List[Coord])] = allLetters.map { l =>
    l ->
      findLinesInGrid(grid, List.empty, l).flatMap { y =>
        findCoordsInLine(grid(y), List.empty, l)
          .map(x => Coord(x, y))
      }
  }

  val antennae = allCoords.map {
    case (char, coords) =>
      Range(0, coords.length).toList.foldLeft[(List[Coord], List[Coord])]((List.empty, coords.drop(1))) {
        case ((acc, toCompare), next) =>
          coords.drop(next) match {
            case head :: Nil => (acc, toCompare.drop(1))
            case head :: tail =>
              (toCompare.flatMap(c => head.minus(c)) ++ acc, toCompare.drop(1))
          }
      }
  }.flatMap(_._1)
    .toSet
    .toList
    .filter {
      case Coord(x, y) =>
        x >= 0 && x <= maxX && y >= 0 && y <= maxY
    }

  val reGrid = antennae.foldLeft(grid) {
    case (acc, Coord(x, y)) =>
      acc.patch(y, List(acc(y).patch(x, "#", 1)), 1)
  }

  println(reGrid.mkString("\n"))
  println(antennae.size)
}