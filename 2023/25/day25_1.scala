@main
def main(file: String = "input.txt") = {
  val lines: List[String] = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    "jqt: rhn xhk nvd",
    "rsh: frs pzl lsr",
    "xhk: hfx",
    "cmg: qnr nvd lhk bvb",
    "rhn: xhk bvb hfx",
    "bvb: xhk hfx",
    "pzl: lsr hfx nvd",
    "qnr: nvd",
    "ntq: jqt hfx bvb xhk",
    "nvd: lhk",
    "lsr: lhk",
    "rzs: qnr cmg lsr rsh",
    "frs: qnr lhk lsr"
  )

  val inputs = sampleLines
    .map { line =>
      line.split(": ") match {
        case Array(component, componentsRaw) =>
          Mapping(Component(component), componentsRaw.split(' ').toList.map(Component.apply))
      }
    }

  val inputsCleaned = inputs.foldLeft(Map.empty[String, List[String]]) {
    case (acc, Mapping(component, components)) =>
      val accUpdated1 = acc.get(component.name) match {
        case Some(values) => acc.updated(component.name, (components.map(_.name) ++ values).distinct)
        case _ => acc + (component.name -> components.map(_.name))
      }

      val accUpdated2 = components.map(_.name).foldLeft(accUpdated1) {
        case (acc, next) =>
          acc.get(next) match {
            case Some(values) => acc.updated(next, (component.name :: values).distinct)
            case _ => acc + (next -> List(component.name))
          }
      }

      accUpdated2
  }

  inputsCleaned
    .values.toList
    .flatten
    .groupBy(identity)
    .map {
      case (k, v) => (k, v.size)
    }
    .toList
    .sortBy(_._2)
}

case class Component(name: String)

case class Mapping(component: Component, components: List[Component])