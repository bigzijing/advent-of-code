@main
def main(file: String = "input.txt"): Unit = {

//  val input = "2333133121414131402"
  val input = scala.io.Source.fromFile(file).getLines.mkString

  case class Accumulator(current: Int, isDot: Boolean, ls: List[(Int, String)])

  val visualized = input.foldLeft(Accumulator(0, false, List.empty)) {
    case (Accumulator(current, false, ls), next) =>
      Accumulator(current + 1, true, ls ++ List((next.toString.toInt, current.toString)))
    case (Accumulator(current, true, ls), next) =>
      Accumulator(current, false, ls ++ List((next.toString.toInt, ".")))
  }

  val size = visualized.ls.size
  val maxIn = size - 1

  def rearrange(ls: List[(Int, String)], in: Int): List[(Int, String)] =
    if in <= 0 then ls
    else {
      ls(in) match {
        case (_, ".") => rearrange(ls, in - 1)
        case (n, str) =>
          ls
            .find {
              case (n2, ".") if n2 >= n => true
              case _ => false
            } match {
            case Some((n2, _)) if n2 == n =>
              val indexToMove = ls.indexOf((n2, "."))
              if in <= indexToMove then rearrange(ls, in - 1)
              else
                rearrange(ls.patch(in, List((n2, ".")), 1).patch(indexToMove, List((n, str)), 1), in - 1)
            case Some((n2, _)) =>
              val indexToMove = ls.indexOf((n2, "."))
              val leftOverDots = n2 - n
              if in <= indexToMove then rearrange(ls, in - 1)
              else
                rearrange(ls.patch(in, List((n, ".")), 1).patch(indexToMove, List((n, str), (leftOverDots, ".")), 1), in - 1)
            case None => rearrange(ls, in - 1)
          }
      }
    }

  val rearranged = rearrange(visualized.ls, maxIn)

  val result = rearranged.flatMap {
    case (n, str) => List.fill(n)(str)
  }
    .zipWithIndex
    .filterNot(_._1 == ".")
    .map {
      case (l, in) => l.toLong * in
    }
    .sum

  println(result)
}