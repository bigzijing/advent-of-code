@main
def main(file: String = "input.txt"): Unit = {

//  val input = "2333133121414131402"
  val input = scala.io.Source.fromFile(file).getLines.mkString

  case class Accumulator(current: Int, isDot: Boolean, ls: List[String])

  val visualized = input.foldLeft(Accumulator(0, false, List.empty)) {
    case (Accumulator(current, false, ls), next) =>
      Accumulator(current + 1, true, ls ++ List.fill(next.toString.toInt)(current.toString))
    case (Accumulator(current, true, ls), next) =>
      Accumulator(current, false, ls ++ List.fill(next.toString.toInt)("."))
  }

  val size = visualized.ls.size
  val nonDotCount = visualized.ls.filterNot(_ == ".").size

  def rearrange(ls: List[String]): List[String] =
    if ls.slice(nonDotCount, size + 1) == List.fill(size - nonDotCount)(".") then ls
    else {
      val dotIn = ls.indexOf(".")
      val nonDot = ls.reverse.filterNot(_ == ".").head
      val nonDotInReverse = ls.reverse.indexOf(nonDot)
      val nonDotIn = size - 1 - nonDotInReverse

      rearrange(ls.patch(dotIn, List(nonDot), 1).patch(nonDotIn, List("."), 1))
    }

  val remapped = rearrange(visualized.ls)

  val result = remapped
    .filterNot(_ == ".")
    .zipWithIndex
    .foldLeft(0L) {
      case (acc, (l, in)) => acc + (l.toLong * in)
    }

  println(result)
}