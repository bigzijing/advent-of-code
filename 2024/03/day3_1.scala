import scala.annotation.tailrec

@main
def main(file: String = "input.txt") = {

//  val str = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

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

  tailRecurseBreakString(str, 0)

}