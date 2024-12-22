@main
def main(file: String = "input.txt"): Unit = {

//    val grid = """89010123
//                 |78121874
//                 |87430965
//                 |96549874
//                 |45678903
//                 |32019012
//                 |01329801
//                 |10456732""".stripMargin
//      .split("\n")

  val grid = scala.io.Source.fromFile(file).getLines.toList

  val maxY = grid.size - 1
  val maxX = grid.head.size - 1

  case class Coord(x: Int, y: Int)

  def getGrid(coord: Coord) = grid(coord.y)(coord.x)

  val allStartingPoints = grid.zipWithIndex.foldLeft(List.empty[Coord]) {
    case (acc, (next, y)) =>
      acc ++ next.zipWithIndex.foldLeft(List.empty[Int]) {
        case (acc, (next, x)) =>
          if next == '0' then x :: acc else acc
      }.map(x => Coord(x, y))
  }

  def walk(current: Coord, path: List[Coord], height: Int, acc: List[List[Coord]]): List[List[Coord]] =
    if height == 9 then (path appended current) :: acc
    else {
      val moveNorth = if current.y - 1 >= 0 then Some(current.copy(y = current.y - 1)) else None
      val moveSouth = if current.y + 1 <= maxY then Some(current.copy(y = current.y + 1)) else None
      val moveEast = if current.x + 1 <= maxX then Some(current.copy(x = current.x + 1)) else None
      val moveWest = if current.x - 1 >= 0 then Some(current.copy(x = current.x - 1)) else None

      val movables = List(moveWest, moveEast, moveSouth, moveNorth)
        .flatten
        .filterNot(c => path.contains(c))
        .filter { c =>
          val nextHeight = getGrid(c).toString.toInt

          nextHeight == height + 1
        }

      movables.map(next => walk(next, (path appended current), height + 1, acc)).flatten
    }

  val results = allStartingPoints.map(coord => walk(coord, List(coord), 0, List.empty)).flatten.toSet

  println(results.size)
}