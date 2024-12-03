@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines.mkString.split(",")

  val sampleLines = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
    .split(",")

  val instructions: List[Operation] = lines.toList
    .map { string =>
      val containsEq = string.contains('=')
      val containsD = string.contains('-')

        if (containsEq) {
          val (a, b) = string.split('=') match {
            case Array(a, b) => (a, b)
          }
          Equal(traverse(a), a, b.toInt)
        } else {
          val a = string.split('-').head

          Dash(traverse(a), string.dropRight(1))
        }
    }

  val boxes = List.fill(256)(List.empty[(String, Int)])

//  instructions

  instructions.foldLeft(boxes) {
    case (acc, next) =>
       next match {
         case Equal(boxNo, code, f) =>
           val box = acc(boxNo)
           val lensIn = box.map(_._1).indexOf(code)

           if (lensIn == -1) acc.updated(boxNo, box.appended((code -> f)))
           else {
             acc.updated(boxNo, box.updated(lensIn, (code -> f)))
           }
         case Dash(boxNo, code) =>
           val box = acc(boxNo)
           val lensIn = box.map(_._1).indexOf(code)

           if (lensIn == -1) acc
           else {
             val boxLen = box.size
             val newBox = box.take(lensIn) concat box.takeRight(boxLen - lensIn - 1)

             acc.updated(boxNo, newBox)
           }
       }
  }
    .zipWithIndex
    .filterNot {
      case (Nil, _) => true
      case _ => false
    }
    .map {
      case (ls, in) => ((ls.zipWithIndex).map {
        case (a, b) => (a, b + 1)
      }, in + 1)
    }
    .map {
      case (ls, outerInd) =>
        ls.map {
          case ((_, f), innerInd) => f * innerInd * outerInd
        }
    }
    .flatten
    .sum

}

def traverse(string: String): Int =
  string.foldLeft(0) {
    case (acc, next) =>
      ((next.toInt + acc) * 17) % 256
  }

sealed trait Operation

case class Equal(box: Int, code: String, f: Int) extends Operation
case class Dash(box: Int, code: String) extends Operation