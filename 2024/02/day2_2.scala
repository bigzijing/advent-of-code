@main
def main(file: String = "input.txt") = {
  val file =
    """|7 6 4 2 1
       |1 2 7 8 9
       |9 7 6 2 1
       |1 3 2 4 5
       |8 6 4 4 1
       |1 3 6 7 9
       |""".stripMargin

  def increaseDampenerOrFlipSwitch(dampener: Int, n1: Int, n2: Int)(conditionToKeep: Boolean) = {
    if (conditionToKeep) then (n2, dampener, true)
    else if (dampener == 0) then (n1, dampener + 1, true)
    else (n1, dampener, false)
  }

//  scala.io.Source.fromFile(file).getLines
  file.split("\n")
    .map(row => row.split(" ").toList.map(_.toInt))
    .foldLeft(0) {
      case (acc, nextReport) =>
        if {
          val (_, _, c, d) = nextReport.tail.foldLeft((nextReport.head, Option.empty[Boolean], 0, true)) {
            case ((previousNumber, directionOpt, dampener, acc), next) =>
              val diff = next - previousNumber

              if ((1 <= diff && diff <= 3 && dampener <= 1) || (-3 <= diff && diff <= -1 && dampener <= 1) || !acc) then {
                directionOpt match {
                  case None => (next, if (diff < 0) then Some(false) else Some(true), dampener, true)
                  case Some(true) =>
                    val (n, d, s) = increaseDampenerOrFlipSwitch(dampener, previousNumber, next)(diff > 0)
                    (n, Some(true), d, s)
                  case Some(false) =>
                    val (n, d, s) = increaseDampenerOrFlipSwitch(dampener, previousNumber, next)(diff < 0)
                    (n, Some(false), d, s)
                }
              } else (next, directionOpt, dampener, false)
          }

          if (d) then println(s"Good: $nextReport") else println(s"Bad: $nextReport")
          d
        } then acc + 1
        else 0
    }
}