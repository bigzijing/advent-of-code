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
      .split("\n")

//  val file = scala.io.Source.fromFile(file).getLines

  def badDifference(n: Int) = n == 0 || n > 3 || n < -3

  def isValidReport(report: List[Int]) = {
    val (_, diffList) = report.tail.foldLeft((report.head, List.empty[Int])) {
      case ((prev, acc), next) =>
        val diff = next - prev
        (next, diff :: acc)
    }

    val failedNegatives = diffList.filterNot(s => s <= -1 && s >= -3)
    val failedPositives = diffList.filterNot(s => s >= 1 && s <= 3)

    failedNegatives.size == 0 || failedPositives.size == 0
  }

  file
    .map(row => row.split(" ").toList.map(_.toInt))
    .map { report =>
      if isValidReport(report) then
        println(s"1: ${report.mkString}")
        1
      else {
        report.tail.foldLeft((report.head, List.empty[Int])) {
          case ((prev, acc), next) =>
            (next, if (badDifference(next - prev)) then report.indexOf(next) :: acc else acc)
        } match {
          case (_, ls) if ls.size > 0 =>
            (0 :: ls).foldLeft(false) {
              case (acc, next) =>
                if (isValidReport(report.patch(next, List.empty[Int], 1))) true else acc
            } match {
              case true =>
                println(s"1: ${report.mkString}")
                1
              case _ =>
                println(s"0: ${report.mkString}")
                0
            }
          case _ =>
            println(s"0: ${report.mkString}")
            0
        }
      }
//      else {
//        val firstDiff = diffList.find(d => (d > 3 || d < -3 || d == 0))
//        val indexOpt = firstDiff.map(i => diffList.indexOf(i) + 1)
//
//        indexOpt match {
//          case None => 0
//          case Some(index) =>
//            val list1 = report.patch(index, List.empty[Int], 1)
//            val list2 = report.patch(0, List.empty[Int], 1)
//
//            isValidReport(list1) match {
//              case (_, negs, pos) if negs.size <= 0 || pos.size <= 0 => 1
//              case _ => isValidReport(list2) match {
//                case (_, negs2, pos2) => if (negs2.size <= 0 || pos2.size <= 0) then 1 else 0
//              }
//            }
//        }
//      }
    }
    .sum
}