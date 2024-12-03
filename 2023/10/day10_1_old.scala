@main
def main(file: String = "input.txt") = {
//  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val lines = List(".....", ".S-7.", ".|.|.", ".L-J.", ".....")

  val linesWithIndex = lines.zipWithIndex

  val sCoords = linesWithIndex.find(_._1.contains("S"))
    .map {
      case (s, i) => (s.indexOf("S"), i)
    }
    .get

  val rows = lines.size
  val columns = lines.map(_.size).max
  val gridSize = rows * columns

//  Range(0, gridSize).foldLeft(List(sCoords)) {
//    case (acc, _) =>
//
//  }


  trait Symbol {
//    def possiblePreviousCoords = List[(Int, Int)]
//    def possibleNextSteps(previousGrid: (Int, Int)): List[(Int, Int)]
  }

  case class S(coords: (Int, Int)) extends Symbol {
    override def possiblePreviousCoords = {
      val (i, j) = coords

      val canGoUp = if (i - 1 >= 0) Some((i - 1, j)) else None
      val canGoDown = if (i + 1 < rows) Some((i + 1, j)) else None
      val canGoLeft = if (j - 1 >= 0) Some((i, j - 1)) else None
      val canGoRight = if (i + 1 < columns) Some((i, j + 1)) else None

      List(canGoUp, canGoDown, canGoLeft, canGoRight).flatten
    }

    def possibleNextSteps(previousGrid: (Int, Int)) =
      List(
        if (coords._2 > 0) Some(North.nextStep(coords)) else None,
        if (coords._2 < rows) Some(South.nextStep(coords)) else None,
        if (coords._1 > 0) Some(West.nextStep(coords)) else None,
        if (coords._1 < columns) Some(East.nextStep(coords)) else None
      ).flatten
  }

  case class |(coords: (Int, Int)) extends Symbol {
    override def possiblePreviousCoords = {
      val (i, j) = coords

      val canGoUp = if (i - 1 >= 0) Some((i - 1, j)) else None
      val canGoDown = if (i + 1 < rows) Some((i + 1, j)) else None

      List(canGoUp, canGoDown).flatten
    }

    def possibleNextSteps(previousGrid: (Int, Int)) =
      if (previousStep == South && coords._2 < rows) List(South.nextStep(coords))
      else if (previousStep == North && coords._2 > 0) List(North.nextStep(coords))
      else List.empty
  }

  case class -(coords: (Int, Int)) extends Symbol {
    override def possiblePreviousCoords = {
      val (i, j) = coords

      val canGoLeft = if (j - 1 >= 0) Some((i, j - 1)) else None
      val canGoRight = if (i + 1 < columns) Some((i, j + 1)) else None

      List(canGoLeft, canGoRight).flatten
    }

    def possibleNextSteps(previousGrid: (Int, Int)) = {
      if (previousStep == West && coords._1 > 0) List(West.nextStep(coords))
      else if (previousStep == East && coords._1 < columns) List(East.nextStep(coords))
      else List.empty
    }
  }

  case class L(coords: (Int, Int)) extends Symbol {
    override def possiblePreviousCoords = {
      val (i, j) = coords

      val canGoUp = if (i - 1 >= 0) Some((i - 1, j)) else None
      val canGoRight = if (i + 1 < columns) Some((i, j + 1)) else None

      List(canGoUp, canGoRight).flatten
    }

    def possibleNextSteps(previousGrid: (Int, Int)) = {
      if (previousStep == West && coords._2 > 0) List(North.nextStep(coords))
      else if (previousStep == South && coords._1 < columns) List(East.nextStep(coords))
      else List.empty
    }
  }

  case class J(coords: (Int, Int)) extends Symbol {
    override def possibleNextSteps(previousGrid: (Int, Int)) = {
      if (previousStep == East && coords._2 > 0) List(North.nextStep(coords))
      else if (previousStep == South && coords._1 > 0) List(West.nextStep(coords))
      else List.empty
    }
  }

  case class _7(coords: (Int, Int)) extends Symbol {
    override def possibleNextSteps(previousGrid: (Int, Int)) = {
      if (previousStep == East && coords._2 < rows) List(South.nextStep(coords))
      if (previousStep == North && coords._1 > 0) List(West.nextStep(coords))
      else List.empty
    }
  }

  case class F(coords: (Int, Int)) extends Symbol {
    override def possibleNextSteps(previousGrid: (Int, Int)) = {
      if (previousStep == North && coords._1 < columns) List(East.nextStep(coords))
      else if (previousStep == West && coords._2 < rows) List(South.nextStep(coords))
      else List.empty
    }
  }

  case class Dot(coords: (Int, Int)) extends Symbol {
    def possibleNextSteps(previousGrid: (Int, Int)) = List.empty
  }

  sCoords
}

trait Direction {
  def nextStep(curr: (Int, Int)): (Int, Int)
}

case object North extends Direction {
  def nextStep(curr: (Int, Int)): (Int, Int) = (curr._1, curr._2 - 1)
}

case object South extends Direction {
  def nextStep(curr: (Int, Int)): (Int, Int) = (curr._1, curr._2 + 1)
}

case object East extends Direction {
  def nextStep(curr: (Int, Int)): (Int, Int) = (curr._1 + 1, curr._2)
}

case object West extends Direction {
  def nextStep(curr: (Int, Int)): (Int, Int) = (curr._1 - 1, curr._2)
}


def canGoUp(i: Int, j: Int)(implicit rows: Int, columns: Int) =
  if (i - 1 >= 0) Some((i - 1, j)) else None

def canGoDown(i: Int, j: Int)(implicit rows: Int, columns: Int) =
  if (i + 1 < rows) Some((i + 1, j)) else None

def canGoLeft(i: Int, j: Int)(implicit rows: Int, columns: Int) =
  if (j - 1 >= 0) Some((i, j - 1)) else None

def canGoRight(i: Int, j: Int)(implicit rows: Int, columns: Int) =
  if (i + 1 < columns) Some((i, j + 1)) else None

def checkUpDownLeftRight(s: (Int, Int))(rows: Int, columns: Int, grid: List[List[Char]]) = {
  val (i, j) = s
  implicit val rows_i = rows
  implicit val columns_i = columns

  val possibleMoves = List(canGoUp(i, j), canGoDown(i, j), canGoLeft(i, j), canGoRight(i, j)).flatten

  possibleMoves
    .collect {
      case (i, j) if grid(i)(j) != '.' => Some(grid(i)(j))
      case (i, j) => None
    }
    .flatten
}