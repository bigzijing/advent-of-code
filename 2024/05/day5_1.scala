@main
def main(file: String = "input.txt") = {

  case class BeforeAfter(before: Int, after: Int)

//  val lines = """47|53
//              |97|13
//              |97|61
//              |97|47
//              |75|29
//              |61|13
//              |75|53
//              |29|13
//              |97|29
//              |53|29
//              |61|53
//              |97|53
//              |61|29
//              |47|13
//              |75|47
//              |97|75
//              |47|61
//              |75|61
//              |47|29
//              |75|13
//              |53|13
//              |
//              |75,47,61,53,29
//              |97,61,53,29,13
//              |75,29,13
//              |75,97,47,61,53
//              |61,13,29
//              |97,13,75,29,47""".stripMargin
//    .split("\n")

  val lines = scala.io.Source.fromFile(file).getLines
    .toList

  val map: List[BeforeAfter] = lines.filter(_.contains('|')).map { row =>
    row.split('|').toList match {
      case head :: tail :: Nil => BeforeAfter(head.toInt, tail.toInt)
    }
  }.toList

  val order: List[List[Int]] = lines.filter(_.contains(','))
    .map(row => row.split(',').toList.map(_.toInt))
    .toList

  def isBefore(before: Int, after: Int) =
    map.find(ba => ba.before == before && ba.after == after).isDefined

  order.foldLeft(0) {
    case (count, nextOrder) =>
      Range(0, nextOrder.size).toList.foldLeft(true) {
        case (isInRightOrder, nextIndex) =>
          if !isInRightOrder then isInRightOrder
          else if nextIndex == nextOrder.size then isInRightOrder
          else
            val curr = nextOrder(nextIndex)
            nextOrder.slice(nextIndex + 1, nextOrder.size).forall(after => (isBefore(curr, after)))
      } match {
        case true => count + (nextOrder(nextOrder.size / 2))
        case _ => count
      }
  }
}