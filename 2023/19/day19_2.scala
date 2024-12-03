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

  val maps = lines.takeWhile(_ != "")
    .map { raw =>
      raw.split('{') match {
        case Array(id, remaining) =>
          val (conditionsMinusOne, lastCondition) =  remaining.dropRight(1).split(',').toList.foldLeft((List.empty[List[Condition]], List.empty[Condition])) {
            case ((normalAcc, flippedAcc), next) =>
              if (next.contains('>'))
                next.split(">") match {
                  case Array(part, numberRaw) =>
                    numberRaw.split(":") match {
                      case Array(number, valueRaw) => {
                        val conditionToAdd = Condition(part.head, '>', number.toInt, parseMap(valueRaw))
                        val flippedCondition = Condition(part.head, '<', number.toInt + 1, parseMap(valueRaw))
                        val newCondition = flippedAcc.map(_.copy(key = parseMap(valueRaw))).appended(conditionToAdd)

                        (normalAcc.appended(newCondition), flippedAcc.appended(flippedCondition))
                      }
                    }
                }
              else if (next.contains('<'))
                next.split("<") match {
                  case Array(part, numberRaw) =>
                    numberRaw.split(":") match {
                      case Array(number, valueRaw) => {
                        val conditionToAdd = Condition(part.head, '<', number.toInt, parseMap(valueRaw))
                        val flippedCondition = Condition(part.head, '>', number.toInt - 1, parseMap(valueRaw))
                        val newCondition = flippedAcc.map(_.copy(key = parseMap(valueRaw))).appended(conditionToAdd)

                        (normalAcc.appended(newCondition), flippedAcc.appended(flippedCondition))
                      }
                    }
                }
              else (normalAcc, flippedAcc.map(_.copy(key = if (List("R", "A").contains(next)) Right(next.head) else Left(next))))
          }

          (id, conditionsMinusOne.appended(lastCondition))
      }
    }

  val in = maps.find(_._1 == "in").get._2

  val chainmap = chainMap(in, maps)
    .map { conditions =>
      conditions.filter {
        case Condition(_, _, _, Right('A')) => true
        case _ => false
      }
    }
    .filter(_.nonEmpty)

  val chainmapIterated = iterateChainmap(chainmap).sortWith(InputRanges.sortWith)

  maps.foreach(println)
  println("\n")
  chainmapIterated.foreach(println)

  chainmapIterated.map {
    case InputRanges((x1, x2), (m1, m2), (a1, a2), (s1, s2)) =>
      (x2 - x1 + 1).toLong *
        (m2 - m1 + 1).toLong *
        (a2 - a1 + 1).toLong *
        (s2 - s1 + 1).toLong
  }.sum

}

case class InputRanges(x: (Int, Int), m: (Int, Int), a: (Int, Int), s: (Int, Int)) {
  def readCondition(condition: Condition) = condition match {
    case Condition('x', operator, number, _) =>
      if (operator == '>') this.copy(x = (number + 1, x._2))
      else this.copy(x = (x._1, number - 1))
    case Condition('m', operator, number, _) =>
      if (operator == '>') this.copy(m = (number + 1, m._2))
      else this.copy(m = (m._1, number - 1))
    case Condition('a', operator, number, _) =>
      if (operator == '>') this.copy(a = (number + 1, a._2))
      else this.copy(a = (a._1, number - 1))
    case Condition('s', operator, number, _) =>
      if (operator == '>') this.copy(s = (number + 1, s._2))
      else this.copy(s = (s._1, number - 1))
  }
}

object InputRanges {
  val first = InputRanges((1, 4000), (1, 4000), (1, 4000), (1, 4000))

  def sortWith(ip1: InputRanges, ip2: InputRanges) =
    if (ip1.x._1 != ip2.x._1) ip1.x._1 < ip2.x._1
    else if (ip1.x._2 != ip2.x._2) ip1.x._2 < ip2.x._2
    else if (ip1.m._1 != ip2.m._1) ip1.m._1 < ip2.m._1
    else if (ip1.m._2 != ip2.m._2) ip1.m._2 < ip2.m._2
    else if (ip1.a._1 != ip2.a._1) ip1.a._1 < ip2.a._1
    else if (ip1.a._2 != ip2.a._2) ip1.a._2 < ip2.a._2
    else if (ip1.s._1 != ip2.s._1) ip1.s._1 < ip2.s._1
    else ip1.s._2 < ip2.s._2
}

case class Condition(part: Char, operator: Char, number: Int, key: Either[String, Char])

case class NewWorkflowMap(id: String, mapping: List[(Char, Int, Char, Either[String, Char])])

def parseMap(string: String): Either[String, Char] = string match {
  case "A" => Right('A')
  case "R" => Right('R')
  case key => Left(key)
}

def chainMap(start: List[List[Condition]], maps: List[(String, List[List[Condition]])]): List[List[Condition]] =
  if (start.flatten.map(_.key).forall(_.isRight)) start
  else {
    val (conditionToUpdate, indexToUpdate) = start.zipWithIndex.find {
      case ((Condition(_, _, _, Left(key)) :: nil), in) => true
      case (conditions, in) => conditions.head.key.isLeft
      case _ => false
    }.get

    val key = conditionToUpdate.head.key match {
      case Left(value) => value
    }

    val conditionToUpdateTo = maps.find(_._1 == key).get._2.map { conditions =>
      val pointer = conditions.head.key

      conditionToUpdate.map(_.copy(key = pointer)) ++ conditions
    }

    val newConditions = start.map(List(_)).updated(indexToUpdate, conditionToUpdateTo).flatten

    chainMap(newConditions, maps)
  }

def iterateChainmap(chainmap: List[List[Condition]]): List[InputRanges] =
  chainmap.map { conditions =>
    conditions.foldLeft(InputRanges.first) {
      case (acc, next) => acc.readCondition(next)
    }
  }
