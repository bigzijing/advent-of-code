@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    ".|...\\....",
    "|.-.\\.....",
    ".....|-...",
    "........|.",
    "..........",
    ".........\\",
    "..../.\\\\..",
    ".-.-/..|..",
    ".|....-|.\\",
    "..//.|...."
  )

  val grid = sampleLines.map(_.toList)

  val rows = grid.length
  val columns = grid.map(_.length).max

  val (i, j) = (0, 0)

  def reflect(coord: Coord, char: Char) = {
    //    println("at reflect")
    (coord, char) match {
      case (Coord(i, j, Northwards), '/') =>
        //      println(s"($i, $j)")
        //      List(coord.copy(dir = Stationary), coord.copy(j = j + 1, dir = Eastwards))
        coord.copy(dir = Stationary) :: nextGrid(coord.copy(j = j + 1, dir = Eastwards))
      case (Coord(i, j, Southwards), '/') =>
        //      List(coord.copy(dir = Stationary), coord.copy(j = j - 1, dir = Westwards))
        coord.copy(dir = Stationary) :: nextGrid(coord.copy(j = j - 1, dir = Westwards))
      case (Coord(i, j, Westwards), '/') =>
        //      List(coord.copy(dir = Stationary), coord.copy(i = i + 1, dir = Southwards))
        coord.copy(dir = Stationary) :: nextGrid(coord.copy(i = i + 1, dir = Southwards))
      case (Coord(i, j, Eastwards), '/') =>
        //      List(coord.copy(dir = Stationary), coord.copy(i = i - 1, dir = Northwards))
        coord.copy(dir = Stationary) :: nextGrid(coord.copy(i = i - 1, dir = Northwards))
      case (Coord(i, j, Northwards), '\\') =>
        //      List(coord.copy(dir = Stationary), coord.copy(j = j - 1, dir = Westwards))
        coord.copy(dir = Stationary) :: nextGrid(coord.copy(j = j - 1, dir = Westwards))
      case (Coord(i, j, Southwards), '\\') =>
        //      List(coord.copy(dir = Stationary), coord.copy(j = j + 1, dir = Eastwards))
        coord.copy(dir = Stationary) :: nextGrid(coord.copy(j = j + 1, dir = Eastwards))
      case (Coord(i, j, Westwards), '\\') =>
        //      List(coord.copy(dir = Stationary), coord.copy(i = i - 1, dir = Northwards))
        coord.copy(dir = Stationary) :: nextGrid(coord.copy(i = i - 1, dir = Northwards))
      case (Coord(i, j, Eastwards), '\\') =>
        //      List(coord.copy(dir = Stationary), coord.copy(i = i + 1, dir = Southwards))
        coord.copy(dir = Stationary) :: nextGrid(coord.copy(i = i + 1, dir = Southwards))
    }
  }

  def splitter(coord: Coord, char: Char) = {
    //    println("at splitter")
    (coord, char) match {
      case (Coord(i, j, dir), '|') if dir == Northwards | dir == Southwards =>
        List(coord)
      //    case (Coord(i, j, Southwards), '|') =>
      //      List(coord)
      case (Coord(i, j, dir), '|') if dir == Eastwards | dir == Westwards =>
        //      val canGoUp = if (i - 1 >= 0) Some(Coord(i - 1, j, Northwards)) else None
        //      val canGoDown = if (i + 1 < rows) Some(Coord(i + 1, j, Southwards)) else None

        List(coord.copy(dir = Stationary), coord.copy(dir = Northwards), coord.copy(dir = Southwards))
      //    case (Coord(i, j, Eastwards), '|') =>
      //      if (i - 1 >= 0) List(coord, Coord(i - 1, j, Northwards)) else List(coord)
      case (Coord(i, j, dir), '-') if dir == Northwards | dir == Southwards =>
        //      val canGoLeft = if (j - 1 >= 0) Some(Coord(i, j - 1, Westwards)) else None
        //      val canGoRight = if (j + 1 < columns) Some(Coord(i, j + 1, Eastwards)) else None

        List(coord.copy(dir = Stationary), coord.copy(dir = Westwards), coord.copy(dir = Eastwards))
      //    case (Coord(i, j, Southwards), '-') =>
      //      if (j + 1 < columns) List(coord, Coord(i, j + 1, Eastwards)) else List(coord)
      case (Coord(i, j, Westwards), '-') =>
        List(coord)
      case (Coord(i, j, Eastwards), '-') =>
        List(coord)
    }
  }

  def nextGrid(coord: Coord): List[Coord] = coord match {
    case c @ Coord(i, j, dir) if i >= 0 && j >= 0 && i < rows && j < columns =>
      grid(i)(j) match {
        case '.' => List(c)
        case '/' => reflect(coord, '/')
        case '\\' => reflect(coord, '\\')
        case '|' => splitter(coord, '|')
        case '-' => splitter(coord, '-')
      }
  }

  def moveInDirection(coord: Coord): List[Coord] = coord match {
    case Coord(i, j, Northwards) =>
      if (i - 1 < 0) List.empty
      else nextGrid(Coord(i - 1, j, Northwards))
    case Coord(i, j, Southwards) =>
      if (i + 1 >= rows) List.empty
      else nextGrid(Coord(i + 1, j, Southwards))
    case Coord(i, j, Westwards) =>
      if (j - 1 < 0) List.empty
      else nextGrid(Coord(i, j - 1, Westwards))
    case Coord(i, j, Eastwards) =>
      if (j + 1 >= columns) List.empty
      else nextGrid(Coord(i, j + 1, Eastwards))
    case Coord(_, _, Stationary) => List.empty
  }

  def recursivelyMove(coords: List[Coord], energizedCoords: Set[(Int, Int)]): Set[(Int, Int)] =
    if (coords.isEmpty) energizedCoords
    else {
      println(coords)
      val newCoords = coords.flatMap(moveInDirection).filter(c => !energizedCoords.contains((c.i, c.j)))
      val newEnergizedCoords = coords.foldLeft(energizedCoords) {
        case (acc, Coord(i, j, _)) =>
          acc + ((i, j))
      }

      recursivelyMove(newCoords, newEnergizedCoords)
    }

  recursivelyMove(List(Coord(0, 0, Eastwards)), Set.empty).toList.size
}

case class Coord(i: Int, j: Int, dir: Direction)

sealed trait Direction

case object Northwards extends Direction
case object Southwards extends Direction
case object Eastwards extends Direction
case object Westwards extends Direction
case object Stationary extends Direction
