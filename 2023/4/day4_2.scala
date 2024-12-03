@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

//  val lines = List(
//    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
//    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
//    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
//    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
//    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
//    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
//  )

  val cards = lines.map { row =>
    val Array(cardNoRaw, cardRowRaw) = row.split(": ")

    val cardNo = cardNoRaw.drop(4).trim.toInt

    val Array(winningRaw, ticketRaw) = cardRowRaw.split('|')

    val winningNos = winningRaw.split(' ').filterNot(_ == "").toList.map(_.toInt)

    val ticketNos = ticketRaw.split(' ').filterNot(_ == "").toList.map(_.toInt)

    val numberOfMatches = ticketNos.filter(n => winningNos.contains(n)).size

    Card(cardNo, numberOfMatches)
  }

  cards.foldLeft(cards.map(c => (c.no -> 1)).toMap) {
    case (acc, next) =>
      Range(next.no + 1, next.no + 1 + next.matches)
        .foldLeft(acc) {
          case (acc, toWin) =>
            acc.updated(toWin, acc(toWin) + acc(next.no))
        }
  }
    .values
    .sum
}

case class Card(no: Int, matches: Int)