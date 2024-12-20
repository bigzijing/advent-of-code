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

  val grid: List[String] = scala.io.Source.fromFile(file).getLines.toList

  case class Coord(x: Int, y: Int) {
    def minus(other: Coord) = {
      val xDiff = abs(other.x - x)
      val yDiff = abs(other.y - y)

      val xIsBigger = x > other.x
      val yIsBigger = y > other.y

      List(
        Coord(if xIsBigger then x + xDiff else x - xDiff, if yIsBigger then y + yDiff else y - yDiff),
        Coord(if xIsBigger then other.x - xDiff else other.x + xDiff, if yIsBigger then other.y - yDiff else other.y - yDiff)
      )
    }
  }

  val maxY = grid.length - 1
  val maxX = grid.head.length - 1

  def getGrid(x: Int, y: Int): Char =
    grid(y)(x)

  val allLetters = grid.flatten.filterNot(_ == '.').toSet.toList

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

  println(antennae.size)
}