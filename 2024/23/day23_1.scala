@main
def main(file: String = "input.txt") = {

  case class Connection(one: String, other: String)
  case class LAN(conns: List[String]) {
    def contains(conn: Connection): Boolean =
      conns.exists(s => s == conn.one || s == conn.other)

    def add(conn: Connection): LAN = {
      val newComputer = if (conns.exists(_ == conn.one)) then conn.other else conn.one

      LAN(newComputer :: conns)
    }

    def merge(other: LAN): LAN =
      LAN((this.conns ++ other.conns).toSet.toList)
  }

//  val inputRaw = """kh-tc
//                |qp-kh
//                |de-cg
//                |ka-co
//                |yn-aq
//                |qp-ub
//                |cg-tb
//                |vc-aq
//                |tb-ka
//                |wh-tc
//                |yn-cg
//                |kh-ub
//                |ta-co
//                |de-co
//                |tc-td
//                |tb-wq
//                |wh-td
//                |ta-ka
//                |td-qp
//                |aq-cg
//                |wq-ub
//                |ub-vc
//                |de-ta
//                |wq-aq
//                |wq-vc
//                |wh-yn
//                |ka-de
//                |kh-ta
//                |co-tc
//                |wh-qp
//                |tb-vc
//                |td-yn""".stripMargin
//    .split("\n").toList

  val inputRaw = scala.io.Source.fromFile(file).getLines.toList

  val input = inputRaw
    .map(l => l.split("-").toList)
    .map {
      case List(one, other) => Connection(one, other)
    }


  val LANs = input.foldLeft(List.empty[LAN]) {
    case (acc, next @ Connection(one, other)) =>
      val containsOne: List[String] = input.filterNot(_ == next).filter(conn => conn.one == one || conn.other == one).map {
        case Connection(a, b) => if a == one then b else a
      }
      val containsOther: List[String] = input.filterNot(_ == next).filter(conn => conn.one == other || conn.other == other).map {
        case Connection(a, b) => if a == other then b else a
      }

      val intersect: List[String] = containsOne intersect containsOther

      val newConnections = intersect.map(o => LAN(List(o, one, other)))

      acc ++ newConnections
  }

  val ts = LANs.map(_.conns).filter(ls => ls.exists(c => c.startsWith("t"))).map(_.sorted).toSet

  println(ts.size)
}