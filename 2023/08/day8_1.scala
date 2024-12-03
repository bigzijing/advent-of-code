@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  )

  val directions = lines.take(1)
    .head

  val dMaps = lines.drop(2)
    .map { s =>
      parseRegex(s)
    }

  nextStep("AAA", 0)(dMaps, directions)
}

case class DMap(key: String, left: String, right: String)

def parseRegex(string: String): DMap = {
  val reg = "(\\w+) = \\((\\w+), (\\w+)\\)".r

  reg.findFirstMatchIn(string) match {
    case Some(m) =>
      DMap(m.group(1), m.group(2), m.group(3))
  }
}

def nextStep(curr: String, index: Int)(dMaps: List[DMap], directions: String): Int = {

  val nextDir = index % (directions.length)

  dMaps.find {
    case DMap(k, l, r) => k == curr
  }.get match {
    case DMap(k, l, r) =>
      if (directions(nextDir) == 'L') l
      else r
  } match {
    case "ZZZ" => index + 1
    case others: String => nextStep(others, index + 1)(dMaps, directions)
  }
}