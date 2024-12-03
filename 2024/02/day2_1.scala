@main
def main(file: String = "input.txt") = {

  scala.io.Source.fromFile(file).getLines
    .map(row => row.split(" ").toList.map(_.toInt))
    .foldLeft(0) {
      case (acc, nextReport) =>
        if {
          nextReport.tail.foldLeft((nextReport.head, Option.empty[Boolean], true)) {
            case ((previousNumber, directionOpt, acc), next) =>
              val diff = next - previousNumber

              if ((1 <= diff && diff <= 3) || (-3 <= diff && diff <= -1) || !acc) then {

                directionOpt match {
                  case None => (next, if (diff < 0) then Some(false) else Some(true), true)
                  case Some(true) => (next, Some(true), if (diff < 0) then false else true)
                  case Some(false) => (next, Some(false), if (diff <0) then true else false)
                }
              } else (next, directionOpt, false)
          }._3
        } then acc + 1
        else 0
    }
}