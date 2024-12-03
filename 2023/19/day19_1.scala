@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

//  val lines = List(
//    "px{a<2006:qkq,m>2090:A,rfg}",
//    "pv{a>1716:R,A}",
//    "lnx{m>1548:A,A}",
//    "rfg{s<537:gd,x>2440:R,A}",
//    "qs{s>3448:A,lnx}",
//    "qkq{x<1416:A,crn}",
//    "crn{x>2662:A,R}",
//    "in{s<1351:px,qqz}",
//    "qqz{s>2770:qs,m<1801:hdj,R}",
//    "gd{a>3333:R,R}",
//    "hdj{m>838:A,pv}",
//    "",
//    "{x=787,m=2655,a=1222,s=2876}",
//    "{x=1679,m=44,a=2067,s=496}",
//    "{x=2036,m=264,a=79,s=2244}",
//    "{x=2461,m=1339,a=466,s=291}",
//    "{x=2127,m=1623,a=2188,s=1013}"
//  )

  val map = lines.takeWhile(_ != "")
    .map { raw =>
      raw.split('{') match {
        case Array(id, remaining) =>
          val mapping = remaining.dropRight(1).split(",").toList.map { s =>
            if (s.contains('>'))
              s.split(">") match {
                case Array(part, numberRaw) =>
                  numberRaw.split(":") match {
                    case Array(number, valueRaw) =>
                      (convertCondition(part, number.toInt, '>'), parseMap(valueRaw))
                  }
              }
            else if (s.contains('<'))
              s.split("<") match {
                case Array(part, numberRaw) =>
                  numberRaw.split(":") match {
                    case Array(number, valueRaw) =>
                      (convertCondition(part, number.toInt, '<'), parseMap(valueRaw))
                  }
              }
            else
              ((input: Input) => true, parseMap(s))
          }

          WorkflowMap(id, mapping)
      }
    }

  val inputs = lines.dropWhile(_ != "").drop(1).map { raw =>
    raw.drop(1).dropRight(1).split(",") match {
      case Array(x, m, a, s) =>
        Input(x.drop(2).toInt, m.drop(2).toInt, a.drop(2).toInt, s.drop(2).toInt)
    }
  }

  inputs.map {
    case input =>
      iterateMapping(input, "in", map)
  }
    .collect {
      case Some((ip, char)) if char == 'A' => ip.partSum
    }
    .sum
}

case class Input(x: Int, m: Int, a: Int, s: Int) {
  def partSum = x + m + a + s
}
case class WorkflowMap(id: String, mapping: List[(Input => Boolean, Either[String, Char])])

def parseMap(string: String): Either[String, Char] = string match {
  case "A" => Right('A')
  case "R" => Right('R')
  case key => Left(key)
}

def convertCondition(part: String, number: Int, sign: Char): Input => Boolean = (input: Input) =>
  part match {
    case "x" => if (sign == '>') input.x > number else input.x < number
    case "m" => if (sign == '>') input.m > number else input.m < number
    case "a" => if (sign == '>') input.a > number else input.a < number
    case "s" => if (sign == '>') input.s > number else input.s < number
  }

def iterateMapping(input: Input, key: String, mappings: List[WorkflowMap]): Option[(Input, Char)] =
  mappings
    .find(_.id == key)
    .head
    .mapping
    .foldLeft(Option.empty[Either[String, Char]]) {
      case (acc, next) =>
        if (acc.isDefined) acc
        else next match {
          case (f, result) =>
            if (f(input)) Some(result)
            else None
        }
    }.head match {
    case Right(char) => Some((input, char))
    case Left(nextKey) => iterateMapping(input, nextKey, mappings)
  }