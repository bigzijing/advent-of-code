@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1"
  )

  val inputs = sampleLines.map { line =>
    line.split(" ") match {
      case Array(puzzleStr, matchesStr) =>
        val puzzle = dropFrontAndBackDots(puzzleStr).toList.map(charToSymbol)
        val matches = matchesStr.split(",").toList.map(_.toInt)

        Row(puzzle, matches)
    }
  }

  inputs.zipWithIndex.map {
    case (r @ Row(puzzle, matches), ind) =>
      val smallestMatchingPattern = matches.map(List.fill(_)("#").mkString).mkString(".")

      if (puzzle.size == smallestMatchingPattern.size) {
        println(s"Row $ind is easy, there's only 1\n")
        1
      } else {

        val (fixed, unfixed, matches2, patternToMatch) = r.redefineProblem(smallestMatchingPattern)

        println(s"Row $ind")
        println(s"Puzzle: ${puzzle.map(_.char).mkString}")
        println(s"Matches: $matches")
        println(s"Smallest matching pattern: $smallestMatchingPattern")
        println(s"\nRedefined problem:")
        println(s"Fixed <---> unfixed: ${fixed.map(_.char).mkString} <---> ${unfixed.map(_.char).mkString}")
        println(s"Pattern to match: $patternToMatch")
//        (puzzle.map(_.char).mkString, matches, smallestMatchingPattern, goR1.map(_.char).mkString, goR2.map(_.char).mkString, goL1.map(_.char).mkString, goL2.map(_.char).mkString)
        println("\n")

        val (a, b, c) = eatThroughProblem(fixed.map(_.char).mkString, unfixed.map(_.char).mkString, matches2)

        println(s"\nProblem left")
        println(s"Fixed <---> unfixed: $a <---> $b")
        println(s"To match: $c")
    }
  }
}

def dropFrontAndBackDots(string: String): String =
  if (string.startsWith(".")) dropFrontAndBackDots(string.drop(1))
  else if (string.endsWith(".")) dropFrontAndBackDots(string.dropRight(1))
  else string

def eatThroughProblem(fixed: String, unfixed: String, matches: List[Int]): (String, String, List[Int]) = {
  val dropped = fixed.dropWhile(_ == '#')
  val hashLen = fixed.length - dropped.length

  if (hashLen == matches.head) ("", unfixed, matches.drop(1))
  else {
    val leftover = dropFrontAndBackDots(unfixed)
    if (!leftover.startsWith("?")) throw new Exception(s"this is not supposed to happen: ${leftover.head}")
    eatThroughProblem(fixed.appended('#'), unfixed.drop(1), matches)
  }
}

def goRight(acc: (List[Symbol], List[Symbol])): (List[Symbol], List[Symbol]) = {
  val (fixed, unfixed) = acc

  unfixed.headOption match {
    case Some(?) => acc
    case c @ (Some(H) | Some(Dot)) => goRight(fixed.appended(c.get), unfixed.drop(1))
    case None => acc
  }
}

def goLeft(acc: (List[Symbol], List[Symbol])): (List[Symbol], List[Symbol]) = {
  val (fixed, unfixed) = acc

  unfixed.lastOption match {
    case Some(?) => acc
    case c @ (Some(H) | Some(Dot)) => goLeft(fixed.prepended(c.get), unfixed.dropRight(1))
    case None => acc
  }
}

sealed trait Symbol {
  val char: Char
}

case object ? extends Symbol {
  val char = '?'
}
case object H extends Symbol {
  val char = '#'
}
case object Dot extends Symbol {
  val char = '.'
}

def charToSymbol(char: Char): Symbol = char match {
  case '?' => ?
  case '#' => H
  case '.' => Dot
}

case class Row(puzzle: List[Symbol], matches: List[Int]) {
  def redefineProblem(pattern: String): (List[Symbol], List[Symbol], List[Int], String) = {
    val (fixed1, unfixed1) = goRight((List.empty[Symbol], puzzle))
    val (fixed2, unfixed2) = goRight((List.empty[Symbol], puzzle.reverse))

    if (unfixed2.size < unfixed1.size)
      (fixed2, unfixed2, matches.reverse, pattern.reverse)
    else
      (fixed1, unfixed1, matches, pattern)
  }

}