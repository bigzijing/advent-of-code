@main
def main(file: String = "input.txt"): Unit = {

  import scala.math.pow

//  val inputRaw = """x00: 1
//                   |x01: 0
//                   |x02: 1
//                   |x03: 1
//                   |x04: 0
//                   |y00: 1
//                   |y01: 1
//                   |y02: 1
//                   |y03: 1
//                   |y04: 1
//                   |
//                   |ntg XOR fgs -> mjb
//                   |y02 OR x01 -> tnw
//                   |kwq OR kpj -> z05
//                   |x00 OR x03 -> fst
//                   |tgd XOR rvg -> z01
//                   |vdt OR tnw -> bfw
//                   |bfw AND frj -> z10
//                   |ffh OR nrd -> bqk
//                   |y00 AND y03 -> djm
//                   |y03 OR y00 -> psh
//                   |bqk OR frj -> z08
//                   |tnw OR fst -> frj
//                   |gnj AND tgd -> z11
//                   |bfw XOR mjb -> z00
//                   |x03 OR x00 -> vdt
//                   |gnj AND wpb -> z02
//                   |x04 AND y00 -> kjc
//                   |djm OR pbm -> qhw
//                   |nrd AND vdt -> hwm
//                   |kjc AND fst -> rvg
//                   |y04 OR y02 -> fgs
//                   |y01 AND x02 -> pbm
//                   |ntg OR kjc -> kwq
//                   |psh XOR fgs -> tgd
//                   |qhw XOR tgd -> z09
//                   |pbm OR djm -> kpj
//                   |x03 XOR y03 -> ffh
//                   |x00 XOR y04 -> ntg
//                   |bfw OR bqk -> z06
//                   |nrd XOR fgs -> wpb
//                   |frj XOR qhw -> z04
//                   |bqk OR frj -> z07
//                   |y03 OR x01 -> nrd
//                   |hwm AND bqk -> z03
//                   |tgd XOR rvg -> z12
//                   |tnw OR pbm -> gnj""".stripMargin
//    .split("\n")

  val inputRaw = scala.io.Source.fromFile(file).getLines.toList

  val (init, initRules) = inputRaw
    .toList
    .filterNot(_ == "")
    .partition(_.contains(":"))

  case class Wire(name: String, bool: Boolean)
  case class Rule(wire1: String, wire2: String, condition: String, wire3: String, res: Option[Boolean])

  val initialWires = init.map { l =>
    l.split(": ").toList match {
      case List(wire, "1") => Wire(wire, true)
      case List(wire, "0") => Wire(wire, false)
    }
  }

  val rules: List[Rule] = initRules.map { r =>
    r.split(" -> ").toList match {
      case List(wires, wire3) =>
        wires.split(" ").toList match {
          case List(wire1, condition, wire2) => Rule(wire1, wire2, condition, wire3, None)
        }
    }
  }

  def mapWires(rules: List[Rule], wires: List[Wire]): (List[Rule], List[Wire]) =
    if rules.isEmpty then (rules, wires)
    else {
      val head = rules.head
      (wires.find(_.name == head.wire1), wires.find(_.name == head.wire2)) match {
        case (Some(wire1 @ Wire(name1, cond1)), Some(wire2 @ Wire(name2, cond2))) =>
          val newCond = head.condition match {
            case "OR" => cond1 || cond2
            case "AND" => cond1 && cond2
            case "XOR" => cond1 != cond2
          }

          val newWire = Wire(head.wire3, newCond)

          mapWires(rules.drop(1), wires appended newWire)
        case _ => mapWires(rules.drop(1) appended head, wires)
      }
    }

  val (_, finalWires) = mapWires(rules, initialWires)

  val zWires = finalWires.filter(_.name.startsWith("z")).sortBy(_.name)

  val res = zWires.map {
    case Wire(_, true) => "1"
    case Wire(_, false) => "0"
  }.reverse
    .foldLeft(0L) {
      case (acc, bit) =>
        acc * 2 + (bit.toString.toInt * 1)
    }

  val forVis = zWires.map {
    case Wire(_, true) => "1"
    case _ => "0"
  }.reverse.mkString

  println(forVis)
  println("\n")
  println(res)
}