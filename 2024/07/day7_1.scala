@main
def main(file: String = "input.txt") = {
  import scala.math.pow

  case class Equation(numbers: List[Long], equal: Long)

//  val equations = """190: 10 19
//                    |3267: 81 40 27
//                    |83: 17 5
//                    |156: 15 6
//                    |7290: 6 8 6 15
//                    |161011: 16 10 13
//                    |192: 17 8 14
//                    |21037: 9 7 18 13
//                    |292: 11 6 16 20""".stripMargin
//    .split("\n")
//    .map { r =>
//      val Array(eRaw, nRaw) = r.split(":")
//      val n = nRaw.split(" ").filterNot(_ == "").toList.map(_.toLong)
//
//      Equation(n, eRaw.toLong)
//    }

  val equations = scala.io.Source.fromFile(file).getLines
    .toList
    .map { r =>
      val Array(eRaw, nRaw) = r.split(":")
      val n = nRaw.split(" ").filterNot(_ == "").toList.map(_.toLong)

      Equation(n, eRaw.toLong)
    }

  def convertToBinary(num: Long, acc: List[Int] = List.empty): List[Int] =
    num match {
      case 0L | 1L => num.toInt :: acc
      case _ => convertToBinary(num / 2, (num % 2).toInt :: acc)
    }

  def zeroPad(currentBinary: List[Int], operandsNeeded: Int) =
    if currentBinary.size == operandsNeeded then currentBinary
    else List.fill(operandsNeeded - currentBinary.size)(0) ++ currentBinary

  def translateToMath(numbers: List[Long], operands: List[Int]) =
    numbers.tail.zipWithIndex.foldLeft(numbers.head) {
      case (acc, (nextNumber, nextIndex)) =>
        operands(nextIndex) match {
          case 0 => acc + nextNumber
          case 1 => acc * nextNumber
        }
    }

  def iterateLong(max: Long, numberOfOperandsNeeded: Int, equation: Equation, curr: Long = 0L, acc: Boolean = false): Boolean =
    if curr == max then acc
    else {
      val binary = zeroPad(convertToBinary(curr), numberOfOperandsNeeded)
      if translateToMath(equation.numbers, binary) == equation.equal then true
      else iterateLong(max, numberOfOperandsNeeded, equation, curr + 1, acc)
    }

  equations.foldLeft(0L) {
    case (acc, next) =>
      val numbers = next.numbers
      val totalNumbers = numbers.size
      val numberOfOperandsNeeded = totalNumbers - 1
      val maxToBinary = pow(2, numberOfOperandsNeeded).toLong

//      Range(0, maxToBinary.toInt).toList.foldLeft(false) {
//        case (true, decimal) => true
//        case (bool, decimal) =>
//          val binary = zeroPad(convertToBinary(decimal), numberOfOperandsNeeded)
//          if translateToMath(numbers, binary) == next.equal then true
//          else false
//      } match {
      iterateLong(maxToBinary, numberOfOperandsNeeded, next) match {
        case true =>
          acc + next.equal
        case false => acc
      }
  }
}