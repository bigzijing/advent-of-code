@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines.mkString.split(",")

//  val sampleLines = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
//    .split(",")

  lines.map { string =>
    traverse(string)
  }
    .sum
}

def traverse(string: String): Int =
  string.foldLeft(0) {
    case (acc, next) =>
      ((next.toInt + acc) * 17) % 256
  }