import scala.math._

@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  lines.map { row =>
    val Array(cardNoRaw, cardRowRaw) = row.split(": ")

    val cardNo = cardNoRaw.drop(4).trim.toInt

    val Array(winningRaw, ticketRaw) = cardRowRaw.split('|')

    val winningNos = winningRaw.split(' ').filterNot(_ == "").toList.map(_.toInt)

    val ticketNos = ticketRaw.split(' ').filterNot(_ == "").toList.map(_.toInt)

    val numberOfMatches = ticketNos.filter(n => winningNos.contains(n)).size

    if (numberOfMatches == 0) 0
    else Math.pow(2, numberOfMatches - 1)
  }
    .sum
    .toInt
}