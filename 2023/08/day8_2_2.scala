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

  val directions = lines.take(1)
    .head

  val dMaps = lines.drop(2)
    .map { s =>
      parseRegex(s)
    }

  val steps = List.fill(85)(directions).mkString

  val startingPoints = dMaps.filter {
    case DMap(key, _, _) => key.endsWith("A")
  }.map(_.key)

  val rounds = startingPoints
    .map { sp =>
      steps.foldLeft(List(sp)) {
          case (acc, next) =>
            val last = acc.last
            val nextStep = next match {
              case 'L' => dMaps.find(_.key == last).get.left
              case 'R' => dMaps.find(_.key == last).get.right
            }
            acc.appended(nextStep)
        }.zipWithIndex
        .filter {
          case (acc, _) => acc.endsWith("Z")
        }
    }
    .map(_.head._2)
    .map(BigInt(_))

  rounds.foldLeft(BigInt(1)) {
    case (acc, next) =>
      lcm(acc, next)
  }
}

def gcf(n1: BigInt, n2: BigInt): BigInt = {
  val res = n1 - n2
  if (res == 0) n2
  else if (res > 0) gcf(res, n2)
  else gcf(n2, n1)
}

def lcm(number1: BigInt, number2: BigInt): BigInt = {
  val divisor = gcf(number1, number2)

  (number1 * number2) / divisor
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