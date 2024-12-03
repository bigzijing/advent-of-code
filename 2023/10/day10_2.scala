@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

//  val lines = List(
//    "...........",
//    ".S-------7.",
//    ".|F-----7|.",
//    ".||.....||.",
//    ".||.....||.",
//    ".|L-7.F-J|.",
//    ".|..|.|..|.",
//    ".L--J.L--J.",
//    "..........."
//  )

//  val lines = List(
//   //01234567890123456789
//    "OF----7F7F7F7F-7OOOO",//0
//    "O|F--7||||||||FJOOOO",//1
//    "O||OFJ||||||||L7OOOO",//2
//    "FJL7L7LJLJ||LJIL-7OO",//3
//    "L--JOL7IIILJS7F-7L7O",//4
//    "OOOOF-JIIF7FJ|L7L7L7",//5
//    "OOOOL7IF7||L7|IL7L7|",//6
//    "OOOOO|FJLJ|FJ|F7|OLJ",//7
//    "OOOOFJL-7O||O||||OOO",//8
//    "OOOOL---JOLJOLJLJOOO"//9
//  )

//  val lines = List(
//    "FF7FSF7F7F7F7F7F---7",
//    "L|LJ||||||||||||F--J",
//    "FL-7LJLJ||||||LJL-77",
//    "F--JF--7||LJLJIF7FJ-",
//    "L---JF-JLJIIIIFJLJJ7",
//    "|F|F-JF---7IIIL7L|7|",
//    "|FFJF7L7F-JF7IIL---7",
//    "7-L-JL7||F7|L7F-7F7|",
//    "L.L7LFJ|||||FJL7||LJ",
//    "L7JLJL-JLJLJL--JLJ.L"
//  )

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
    def dir: Direction
  }

  case class S(i: Int, j: Int, dir: Direction) extends Symbol {
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

  trait Direction

  case object ComingFromDown extends Direction
  case object ComingFromUp extends Direction
  case object ComingFromRight extends Direction
  case object ComingFromLeft extends Direction
  case object StartingPoint extends Direction

  def charToSymbol(i: Int, j: Int, dir: Direction): Option[Symbol] =
    grid(i)(j) match {
      case 'S' => Some(S(i, j, dir))
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

  val firstSteps = S(iS, jS, StartingPoint).possibleSteps

  def takeSteps(current: List[(Symbol, Int)]): List[(Symbol, Int)] = {

    current.last._1.possibleSteps.headOption match {
      case None => current
      case Some(nextSymbol) =>
        val nextStep = current.last._2 + 1

        val newList = current.appended((nextSymbol, nextStep))

        nextSymbol match {
          case S(_, _, _) => newList
          case _ => takeSteps(newList)
        }
    }
  }

  def transformS(directionToNext: Direction, directionFromLast: Direction)(i: Int, j: Int): Symbol = {
    (directionToNext, directionFromLast) match {
      case (ComingFromDown, ComingFromDown) | (ComingFromUp, ComingFromUp) => |.apply(i, j, directionFromLast)
      case (ComingFromLeft, ComingFromLeft) | (ComingFromRight, ComingFromRight) => -.apply(i, j, directionFromLast)
      case (ComingFromRight, ComingFromDown) | (ComingFromUp, ComingFromLeft) => _7(i, j, directionFromLast)
      case (ComingFromLeft, ComingFromDown) | (ComingFromUp, ComingFromRight) => F(i, j, directionFromLast)
      case (ComingFromRight, ComingFromUp) | (ComingFromDown, ComingFromLeft) => J(i, j, directionFromLast)
      case (ComingFromLeft, ComingFromUp) | (ComingFromDown, ComingFromRight) => L(i, j, directionFromLast)
    }
  }

  println(S(iS, jS, StartingPoint))
  val list = List(
    (S(iS, jS, StartingPoint), 0),
    (firstSteps.head, 1)
  )
  takeSteps(list)

  def sort(coords1: (Int, Int), coords2: (Int, Int)) = (coords1, coords2) match {
    case ((x1, _), (x2, _)) if x1 < x2 => true
    case ((x1, y1), (x2, y2)) if x2 == x1 => y1 < y2
    case _ => false
  }

  def recursiveIterateI(i: Int, j: Int)(f: Int => Int, stepList: List[Symbol], acc: List[(Int, Int)]): List[(Int, Int)] =
    stepList.find(symbol => symbol.i == i && symbol.j == j) match {
      case Some(_) => acc
      case None => recursiveIterateI(f(i), j)(f, stepList, (i, j) :: acc)
    }

  def recursiveIterateJ(i: Int, j: Int)(f: Int => Int, stepList: List[Symbol], acc: List[(Int, Int)]): List[(Int, Int)] =
    stepList.find(symbol => symbol.i == i && symbol.j == j) match {
      case Some(_) => acc
      case None => recursiveIterateJ(i, f(j))(f, stepList, (i, j) :: acc)
    }

  def walkTheWalk(step: Int, acc: List[(Int, Int)])(stepList: List[Symbol], clockwise: Boolean): List[(Int, Int)] =
    if (step + 1 >= stepList.length) acc
    else {
      val nextStep = stepList(step + 1)

      if (clockwise)
        (nextStep.dir, nextStep.i, nextStep.j) match {
          case (ComingFromLeft, i, j) =>
            nextStep match {
              case s: _7 => // + j
                walkTheWalk(step + 1, acc)(stepList, clockwise)
              case s: J => // + j, + i
                walkTheWalk(step + 1,
                  acc concat
                    recursiveIterateJ(i, j + 1)(j => j + 1, stepList, List.empty) concat
                    recursiveIterateI(i + 1, j)(i => i + 1, stepList, List.empty)
                )(stepList, clockwise)
              case s: - => // + i
                walkTheWalk(step + 1, acc concat recursiveIterateI(i + 1, j)(i => i + 1, stepList, List.empty))(stepList, clockwise)
            }
//          case (ComingFromRight, i, j) =>
//            nextStep match {
//              case s: L => // nothing
//                walkTheWalk(step + 1, acc)(stepList, clockwise)
//              case s: F => // nothing
//                walkTheWalk(step + 1, acc)(stepList, clockwise)
//              case s: - => // - i
//                walkTheWalk(step + 1, acc + recursiveIterateI(i - 1, j)(i => i - 1, stepList, 0))(stepList, clockwise)
//            }
          case (ComingFromUp, i, j) =>
            nextStep match {
              case s: J => // - i
                walkTheWalk(step + 1, acc)(stepList, clockwise)
              case s: L => // + i; - j
                walkTheWalk(
                  step + 1,
                  acc concat
                    recursiveIterateI(i + 1, j)(i => i + 1, stepList, List.empty) concat
                    recursiveIterateJ(i, j - 1)(j => j - 1, stepList, List.empty)
                )(stepList, clockwise)
              case s: | => // - j
                walkTheWalk(step + 1, acc concat recursiveIterateJ(i, j - 1)(j => j - 1, stepList, List.empty))(stepList, clockwise)
            }
//          case (ComingFromDown, i, j) =>
//            nextStep match {
//              case s: F => // nothing
//                walkTheWalk(step + 1, acc)(stepList, clockwise)
//              case s: _7 => // - i; + j
//                walkTheWalk(
//                  step + 1,
//                  acc +
//                    recursiveIterateI(i - 1, j)(i => i - 1, stepList, 0) +
//                    recursiveIterateJ(i, j + 1)(j => j + 1, stepList, 0)
//                )(stepList, clockwise)
//              case s: | => // - j
//                walkTheWalk(step + 1, acc + recursiveIterateJ(i, j)(j => j - 1, stepList, 0))(stepList, clockwise)
//            }
          case _ => walkTheWalk(step + 1, acc)(stepList, clockwise)
        }
      else
        (nextStep.dir, nextStep.i, nextStep.j) match {
          case (ComingFromUp, i, j) =>
            nextStep match {
              case s: J => // + i; + j
                walkTheWalk(
                  step + 1,
                  acc concat
                    recursiveIterateI(i + 1, j)(i => i + 1, stepList, List.empty) concat
                    recursiveIterateJ(i, j + 1)(j => j + 1, stepList, List.empty)
                )(stepList, clockwise)
              case s: L => // - i
                walkTheWalk(step + 1, acc)(stepList, clockwise)
              case s: | => // + j
                walkTheWalk(step + 1, acc concat recursiveIterateJ(i, j + 1)(j => j + 1, stepList, List.empty))(stepList, clockwise)
            }
          case (ComingFromLeft, i, j) =>
            nextStep match {
              case s: _7 => // - i; + j
                walkTheWalk(
                  step + 1,
                  acc concat
                    recursiveIterateI(i - 1, j)(i => i - 1, stepList, List.empty) concat
                    recursiveIterateJ(i, j + 1)(j => j + 1, stepList, List.empty)
                )(stepList, clockwise)
              case s: J => // - j
                walkTheWalk(step + 1, acc)(stepList, clockwise)
              case s: - => // - i
                walkTheWalk(step + 1, acc concat recursiveIterateI(i - 1, j)(i => i - 1, stepList, List.empty))(stepList, clockwise)
            }
          case _ => walkTheWalk(step + 1, acc)(stepList, clockwise)
        }

    }

  val loop = firstSteps.map { s =>
    List((S(iS, jS, StartingPoint), 0), (s, 1))
  }
    .map(takeSteps)
    .find { s => s.last match {
      case (s: S, _) => true
      case _ => false
    }}
    .get

  val firstC = loop.drop(1).head
  val lastS = loop.last
  val transformedS = transformS(firstC._1.dir, lastS._1.dir)(lastS._1.i, lastS._1.j)
  val loopTransformed = loop.drop(1).dropRight(1).map(_._1).prepended(transformedS)

  val minI = loop.map(_._1).map(_.i).min
  val newStart = loop.map(_._1).filter {
    case s => s.i == minI
  }.minBy(_.j)
  val indexOfNewStart = loopTransformed.indexOf(newStart)

  val (ls1, ls2) = loopTransformed.zipWithIndex.partition {
    case (_, b) => b < indexOfNewStart
  }

  val newLoop = ls2.concat(ls1).map(_._1)
  val clockwise = newLoop.drop(1).head.dir == ComingFromLeft

  println(newLoop.head)
  println(newLoop.drop(1).head)
  println(clockwise)

  walkTheWalk(0, List.empty)(newLoop, clockwise).distinct.size
}
