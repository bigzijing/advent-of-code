import scala.annotation.tailrec

@main
def main(file: String = "input.txt") = {

//    val str = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  val str = scala.io.Source.fromFile(file)
    .getLines
    .mkString

  val mulStart = "mul("

  @tailrec
  def tailRecurseBreakString(str: String, acc: Int): Int = {
    val indexOfMulStart = str.indexOf(mulStart)

    if (indexOfMulStart == -1) then acc
    else {
      val cutString = str.drop(indexOfMulStart)
      val indexOfClosingBracket = cutString.indexOf(")")
      val betweenBrackets = cutString.slice(mulStart.length, indexOfClosingBracket)

      val (mul, stringToReturn) = betweenBrackets.split(",").toList match {
        case List(f, s) if f.forall(_.isDigit) && s.forall(_.isDigit) =>
          (f.toInt * s.toInt, cutString.drop(indexOfClosingBracket))
        case _ => (0, cutString.drop(1))
      }

      tailRecurseBreakString(stringToReturn, acc + mul)
    }
  }

  val doStr = "do()"
  val dontStr = "don't()"

  @tailrec
  def tailRecurseWithDoUndo(str: String, acc: Int, doNotDont: Boolean): Int =
    if (doNotDont) then {
      val indexOfUndo = str.indexOf(dontStr)

      if (indexOfUndo == -1) then acc + tailRecurseBreakString(str, 0)
      else {
        val strToAssessHere = str.slice(0, indexOfUndo)
        val sumWithSubstring = tailRecurseBreakString(strToAssessHere, 0)

        println(strToAssessHere)
        tailRecurseWithDoUndo(str.drop(indexOfUndo + dontStr.length), acc + sumWithSubstring, !doNotDont)
      }
    } else {
      val indexOfDo = str.indexOf(doStr)

      if (indexOfDo == -1) then acc
      else {
        tailRecurseWithDoUndo(str.drop(indexOfDo + doStr.length), acc, !doNotDont)
      }
    }

  tailRecurseWithDoUndo(str, 0, true)

}