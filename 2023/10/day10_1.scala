@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

//  val lines = List(".....", ".S-7.", ".|.|.", ".L-J.", ".....")

  implicit val grid: List[List[Char]] = lines.map(_.toList)

  val (jS, iS) = lines.zipWithIndex.find(_._1.contains("S"))
    .map {
      case (s, i) => (s.indexOf("S"), i)
    }
    .get

  val rows = grid.length
  val columns = grid.map(_.length).max
  val gridSize = rows * columns

  trait Symbol {
    def possibleSteps: List[Symbol]
    def i: Int
    def j: Int
  }

  case class S(i: Int, j: Int) extends Symbol {
    def possibleSteps =
      List(canGoUp(i, j), canGoDown(i, j), canGoLeft(i, j), canGoRight(i, j)).flatten
  }

  case class |(i: Int, j: Int, dir: Direction) extends Symbol {
    override def possibleSteps = (dir match {
      case ComingFromUp => List(canGoDown(i, j))
      case ComingFromDown => List(canGoUp(i, j))
    }).flatten
  }

  case class -(i: Int, j: Int, dir: Direction) extends Symbol {
    override def possibleSteps = (dir match {
      case ComingFromLeft => List(canGoRight(i, j))
      case ComingFromRight => List(canGoLeft(i, j))
    }).flatten
  }

  case class L(i: Int, j: Int, dir: Direction) extends Symbol {
    override def possibleSteps = (dir match {
      case ComingFromUp => List(canGoRight(i, j))
      case ComingFromRight => List(canGoUp(i, j))
    }).flatten
  }

  case class J(i: Int, j: Int, dir: Direction) extends Symbol {
    override def possibleSteps = (dir match {
      case ComingFromLeft => List(canGoUp(i, j))
      case ComingFromUp => List(canGoLeft(i, j))
    }).flatten
  }

  case class _7(i: Int, j: Int, dir: Direction) extends Symbol {
    override def possibleSteps = (dir match {
      case ComingFromLeft => List(canGoDown(i, j))
      case ComingFromDown => List(canGoLeft(i, j))
    }).flatten
  }

  case class F(i: Int, j: Int, dir: Direction) extends Symbol {
    override def possibleSteps = (dir match {
      case ComingFromDown => List(canGoRight(i, j))
      case ComingFromRight => List(canGoDown(i, j))
    }).flatten
  }

  case class Dot(i: Int, j: Int) extends Symbol {
    override def possibleSteps = List.empty
  }

  trait Direction

  case object ComingFromDown extends Direction
  case object ComingFromUp extends Direction
  case object ComingFromRight extends Direction
  case object ComingFromLeft extends Direction

  def charToSymbol(i: Int, j: Int, dir: Direction): Option[Symbol] =
    grid(i)(j) match {
      case 'S' => Some(S(i, j))
      case '|' if List(ComingFromDown, ComingFromUp).contains(dir) => Some(|(i, j, dir))
      case '-' if List(ComingFromLeft, ComingFromRight).contains(dir) => Some(-.apply(i, j, dir))
      case 'L' if List(ComingFromUp, ComingFromRight).contains(dir) => Some(L(i, j, dir))
      case 'J' if List(ComingFromUp, ComingFromLeft).contains(dir) => Some(J(i, j, dir))
      case '7' if List(ComingFromDown, ComingFromLeft).contains(dir) => Some(_7(i, j, dir))
      case 'F' if List(ComingFromRight, ComingFromDown).contains(dir) => Some(F(i, j, dir))
      case '.' => None
      case _ => None
    }

  def canGoDown(i: Int, j: Int) =
    if (i + 1 < rows) charToSymbol(i + 1, j, ComingFromUp) else None

  def canGoUp(i: Int, j: Int) =
    if (i - 1 >= 0) charToSymbol(i - 1, j, ComingFromDown) else None

  def canGoLeft(i: Int, j: Int) =
    if (j - 1 >= 0) charToSymbol(i, j - 1, ComingFromRight) else None

  def canGoRight(i: Int, j: Int) =
    if (i + 1 < columns) charToSymbol(i, j + 1, ComingFromLeft) else None

  def takeValidNexSteps(s: Symbol) = {
    val nextSteps = s.possibleSteps
  }

  val firstSteps = S(iS, jS).possibleSteps

  def takeSteps(current: List[Symbol], acc: Int): Int = {
    val nextSteps = current.flatMap(_.possibleSteps)
    if (nextSteps.isEmpty) throw new Exception("no steps left")
    println(s"Next steps: $nextSteps")

    nextSteps.find {
      case s @ S(_, _) => true
      case _ => false
    } match {
      case Some(_) => acc + 1
      case _ => takeSteps(nextSteps, acc + 1)
    }
  }

//  println(grid.map(_.mkString).mkString("\n"))
  println(rows)
  println(columns)
  println(s"$iS, $jS")
  println(s"${S(iS, jS).possibleSteps}")
  println(firstSteps)

  val totalSteps = takeSteps(List(S(iS, jS)), 0)
  println(s"Total steps: $totalSteps")
  println(s"Half of total: ${totalSteps / 2}")

//  println(grid(100)(52))
//  println(grid(101)(52))
//  println(grid(52)(100))
}


