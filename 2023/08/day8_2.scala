@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)"
  )

  val directions = sampleLines.take(1)
    .head

  val dMaps = sampleLines.drop(2)
    .map { s =>
      parseRegex(s)
    }

  val startingPoints = dMaps.filter {
    case DMap(key, _, _) => key.endsWith("A")
  }.map(_.key)

  nextStep(startingPoints, 0)(dMaps, directions)
}

case class DMap(key: String, left: String, right: String)

def parseRegex(string: String): DMap = {
  val reg = "(\\w+) = \\((\\w+), (\\w+)\\)".r

  reg.findFirstMatchIn(string) match {
    case Some(m) =>
      DMap(m.group(1), m.group(2), m.group(3))
  }
}

def nextStep(currents: List[String], index: Int)(dMaps: List[DMap], directions: String): Int = {

  val nextDir = directions(index % (directions.length))

  val nextSteps = currents.map { curr =>
    dMaps.find {
      case DMap(k, l, r) => k == curr
    }.get match {
      case DMap(k, l, r) =>
        if (nextDir == 'L') l
        else r
    }
  }

  if (nextSteps.forall(_.endsWith("Z"))) index + 1
  else nextStep(nextSteps, index + 1)(dMaps, directions)
}

def nextStep(curr: String, index: Int)(dMaps: List[DMap], directions: String): Int = {

  val nextDir = directions(index % (directions.length))

  dMaps.find {
    case DMap(k, l, r) => k == curr
  }.get match {
    case DMap(k, l, r) =>
      if (nextDir == 'L') l
      else r
  } match {
    case "ZZZ" => index + 1
    case others: String => nextStep(others, index + 1)(dMaps, directions)
  }
}