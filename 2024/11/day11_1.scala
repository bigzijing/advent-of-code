@main
def main(file: String = "input.txt"): Unit = {

//  val input = "125 17".split(" ").toList
  val input: List[String] = scala.io.Source.fromFile(file).getLines.toList.head.split(" ").toList

  def changeRock(rock: String): List[String] =
    if rock == "0" then List("1")
    else if rock.length % 2 == 0 then
      val len = rock.length

      List(rock.slice(0, len / 2), rock.slice(len / 2, len).toLong.toString)

    else List((rock.toLong * 2024).toString)

  def blink(ls: List[String], n: Int): List[String] =
    if n == 0 then ls
    else
      {
        val newLs = ls.foldLeft(List.empty[String]) {
          case (acc, next) =>
            acc ++ changeRock(next)
        }

        blink(newLs, n - 1)
      }

  val results = input.foldLeft(0L) {
    case (acc, next) =>
      blink(List(next), 75).size + acc
  }

  println(results)
}