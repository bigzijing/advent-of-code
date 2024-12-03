@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    "2413432311323",
    "3215453535623",
    "3255245654254",
    "3446585845452",
    "4546657867536",
    "1438598798454",
    "4457876987766",
    "3637877979653",
    "4654967986887",
    "4564679986453",
    "1224686865563",
    "2546548887735",
    "4322674655533"
  )

  val grid = sampleLines.map(_.toList)

  val rows = grid.length
  val columns = grid.map(_.length).max

  val (i, j) = (0, 0)

  def distanceFromEndpoint(i: Int, j: Int) =
    rows - i + (columns - j)

  def explodeThreeSteps(acc: List[List[Step]], steps: Int = 3): List[List[Step]] =
    if (steps == 0) acc
    else {
      val newAcc: List[List[Step]] = acc.map { ls => ls.flatMap {
          case Step(i, j, dir, count) =>
            val canGoUp = if (i - 1 >= 0) Some(Step(i - 1, j, Northwards, if (dir == Northwards) count + 1 else 1)) else None
            val canGoDown = if (i + 1 < rows) Some(Step(i + 1, j, Southwards, if (dir == Southwards) count + 1 else 1)) else None
            val canGoLeft = if (j - 1 >= 0) Some(Step(i, j - 1, Westwards, if (dir == Westwards) count + 1 else 1)) else None
            val canGoRight = if (j + 1 >= 0) Some(Step(i, j + 1, Eastwards, if (dir == Eastwards) count + 1 else 1)) else None

            (List(canGoUp) ++ List(canGoDown) ++ List(canGoLeft) ++ List(canGoRight)).flatten.filter(_.count <= 3)
        }
      }

      explodeThreeSteps(newAcc, steps - 1)
    }

  println(rows)
  println(columns)
}

sealed trait Direction
case object Northwards extends Direction
case object Southwards extends Direction
case object Eastwards extends Direction
case object Westwards extends Direction

case class Step(i: Int, j: Int, dir: Direction, count: Int)