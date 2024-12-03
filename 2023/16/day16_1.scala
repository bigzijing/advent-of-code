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

  val grid = lines.map(_.toList)

  val rows = grid.length
  val columns = grid.map(_.length).max

  val (i, j) = (0, 0)
  val start = Coord(i, j, Eastwards)


  def reflect(coord: Coord, reflector: Char) = (coord, reflector) match {
    case (Coord(i, j, Northwards), '/') =>
      if (j + 1 < columns) List(coord.copy(j = j + 1, dir = Eastwards)) else List.empty

    case (Coord(i, j, Southwards), '/') =>
      if (j - 1 >= 0) List(coord.copy(j = j - 1, dir = Westwards)) else List.empty

    case (Coord(i, j, Eastwards), '/') =>
      if (i - 1 >= 0) List(coord.copy(i = i - 1, dir = Northwards)) else List.empty

    case (Coord(i, j, Westwards), '/') =>
      if (i + 1 < rows) List(coord.copy(i = i + 1, dir = Southwards)) else List.empty

    case (Coord(i, j, Northwards), '\\') =>
      if (j - 1 >= 0) List(coord.copy(j = j - 1, dir = Westwards)) else List.empty

    case (Coord(i, j, Southwards), '\\') =>
      if (j + 1 < columns) List(coord.copy(j = j + 1, dir = Eastwards)) else List.empty

    case (Coord(i, j, Eastwards), '\\') =>
      if (i + 1 < rows) List(coord.copy(i = i + 1, dir = Southwards)) else List.empty

    case (Coord(i, j, Westwards), '\\') =>
      if (i - 1 >= 0) List(coord.copy(i = i - 1, dir = Northwards)) else List.empty
  }

  def split(coord: Coord, splitter: Char) = (coord, splitter) match {
    case (Coord(i, j, dir), '|') =>
      val canGoUp = if (i - 1 >= 0) Some(coord.copy(i = i - 1, dir = Northwards)) else None
      val canGoDown = if (i + 1 < rows) Some(coord.copy(i = i + 1, dir = Southwards)) else None

      List(canGoUp, canGoDown).flatten
    case (Coord(i, j, dir), '-') =>
      val canGoLeft = if (j - 1 >= 0) Some(coord.copy(j = j - 1, dir = Westwards)) else None
      val canGoRight = if (j + 1 < columns) Some(coord.copy(j = j + 1, dir = Eastwards)) else None

      List(canGoLeft, canGoRight).flatten
  }

  def determineNextStep(coord: Coord, traversedPaths: Set[Coord]): List[Coord] = coord match {
    case c @ Coord(i, j, dir) =>
      grid(i)(j) match {
        case '/' => reflect(c, '/')
        case '\\' => reflect(c, '\\')
        case '|' if dir == Eastwards | dir == Westwards => split(c, '|')
        case '-' if dir == Northwards | dir == Southwards => split(c, '-')
        case _ =>
          dir match {
            case Northwards if i - 1 >= 0 => List(c.copy(i = i - 1))
            case Southwards if i + 1 < rows => List(c.copy(i = i + 1))
            case Eastwards if j + 1 < columns => List(c.copy(j = j + 1))
            case Westwards if j - 1 >= 0 => List(c.copy(j = j - 1))
            case _ => List.empty
          }
      }
  }

  def iterateMovement(coords: List[Coord], traversedPaths: Set[Coord]): Set[Coord] = {
    if (coords.isEmpty) traversedPaths
    else {
      val newTraversedPaths = traversedPaths ++ coords

      val coordsToExpandOn = coords.filterNot(traversedPaths.contains)

      val nextSteps = coordsToExpandOn.flatMap(determineNextStep(_, newTraversedPaths)).distinct

      iterateMovement(nextSteps, newTraversedPaths)
    }
  }

  iterateMovement(List(start), Set.empty).map(c => ((c.i, c.j))).size
}

case class Coord(i: Int, j: Int, dir: Direction)

sealed trait Direction

case object Northwards extends Direction
case object Southwards extends Direction
case object Eastwards extends Direction
case object Westwards extends Direction
case object Stationary extends Direction
