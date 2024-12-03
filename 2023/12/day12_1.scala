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
//        val puzzle = dropFrontAndBackDots(puzzleStr).toList.map(charToSymbol)
        val matches = matchesStr.split(",").toList.map(_.toInt)

        (puzzleStr, matches)
    }
  }

  inputs
    .zipWithIndex
    .map {
      case ((puzzle, toMatch), index) =>
        println(s"Puzzle row $index:")
        println(s"Original puzzle: $puzzle, to match: ${toMatch}")
        val (leftoverPuzzle, leftoverMatches) = reduceProblem(puzzle, toMatch)
        val minimumPattern = buildMinimumPattern(leftoverMatches)
        println(s"\nLeftover puzzle: $leftoverPuzzle, to match: ${leftoverMatches}")
        println(s"Minimum pattern = $minimumPattern")
        println(s"Result: ${fitPuzzle(leftoverMatches, minimumPattern, leftoverPuzzle)}")
        println("--------------------------------------------------\n\n")
    }
}

def findFixed(fixed: String, unfixed: String): (String, String) = {
  unfixed.headOption match {
    case Some(value) if value != '?' => findFixed(fixed.appended(value), unfixed.drop(1))
    case _ => (fixed, unfixed)
  }
}

def tryFindMatch(fixed: String, toMatch: List[Int]): (String, List[Int]) = {
  val dropped = fixed.dropWhile(_ == '#')
  val hashLen = fixed.length - dropped.length

  if (toMatch.head == hashLen) (dropped, toMatch.drop(1))
  else (fixed, toMatch)
}

def reduceProblem(puzzle: String, matches: List[Int]): (String, List[Int]) = {
  if (puzzle == "") ("", List.empty)
  else if (puzzle.startsWith(".")) reduceProblem(puzzle.drop(1), matches)
  else if (puzzle.endsWith(".")) reduceProblem(puzzle.dropRight(1), matches)
  else {
    val (fixedL, unfixedL) = findFixed("", puzzle.reverse)
    val (eatFromRightStr, toMatchL) = tryFindMatch(fixedL, matches.reverse)

    val (fixedR, unfixedR) = findFixed("", (eatFromRightStr + unfixedL).reverse)
    val (leftoverStr, toMatchR) = tryFindMatch(fixedR, toMatchL.reverse)

    if (toMatchR == matches) (leftoverStr + unfixedR, toMatchR)
    else reduceProblem(leftoverStr + unfixedR, toMatchR)
  }
}

def buildMinimumPattern(matches: List[Int]) =
  matches.map(List.fill(_)("#").mkString).mkString(".")

def fitPuzzle(matches: List[Int], minimumPattern: String, puzzle: String) =
  if (minimumPattern.size == puzzle.size) 1
  else {
    -1
  }

def pickAnchor(matches: List[Int], minimumPattern: String, puzzle: String) = {
  ???
}

??? 3 * 1 / 1
#
..#
.#.
#

???? 4 * 1 / 1
#
...#
..#.
.#..
#...

???
##
.##
##. 3 * 2 / 2

???? 4 * 2 / 2
##
..##
.##.
##..

???? 4 * 3 / 3 / 2
###
.###
###.

????? 5 * 4 * 3 * 2 / 2 / 3 / 4
##
...##
..##.
.##...
##...