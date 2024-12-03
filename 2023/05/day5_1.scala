@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines.toList
    .filterNot(_ == "")
    .mkString("|")

//  val lines = List(
//    "seeds: 79 14 55 13",
//    "",
//    "seed-to-soil map:",
//    "50 98 2",
//    "52 50 48",
//    "",
//    "soil-to-fertilizer map:",
//    "0 15 37",
//    "37 52 2",
//    "39 0 15",
//    "",
//    "fertilizer-to-water map:",
//    "49 53 8",
//    "0 11 42",
//    "42 0 7",
//    "57 7 4",
//    "",
//    "water-to-light map:",
//    "88 18 7",
//    "18 25 70",
//    "",
//    "light-to-temperature map:",
//    "45 77 23",
//    "81 45 19",
//    "68 64 13",
//    "",
//    "temperature-to-humidity map:",
//    "0 69 1",
//    "1 0 69",
//    "",
//    "humidity-to-location map:",
//    "60 56 37",
//    "56 93 4"
//  )
//    .filterNot(_ == "")
//    .mkString("|")

  val keys = List(
    "seed-to-soil",
    "soil-to-fertilizer",
    "fertilizer-to-water",
    "water-to-light",
    "light-to-temperature",
    "temperature-to-humidity",
    "humidity-to-location"
  ).reverse

  val (map, seedStrRaw) = keys.foldLeft((Map.empty[String, List[AMap]], lines)) {
    case ((accMap, accStr), next) =>
      val (mapRaw, remainingStr) = getLastMapAndDropFromString(accStr, next)
      val map = parseMapString(mapRaw, next)

      (accMap.updated(next, map), remainingStr)
  }

  val seeds = seedStrRaw.drop(seedStrRaw.indexOf(':') + 2)
    .split(' ').toList.map(_.toDouble)

  seeds.map { seed =>
    keys.reverse.foldLeft(seed) {
      case (acc, next) =>
        findNextDigit(acc, map(next))
    }
  }
  .min
  .toInt
}

case class AMap(destination: Double, source: Double, step: Double)


def getLastMapAndDropFromString(string: String, mapKey: String): (String, String) = {
  val strLen = string.size
  val indexToSlice = string.indexOf(mapKey)
  val map = string.slice(indexToSlice, strLen)
  val remainingStringRaw = string.dropRight(strLen - indexToSlice)
  val remainingStr = if (remainingStringRaw.last == '|') remainingStringRaw.dropRight(1) else remainingStringRaw

  (map, remainingStr)
}

def parseMapString(string: String, mapKey: String) =
  string.split('|').drop(1).map { triplets =>
      triplets.split(" ") match {
        case Array(a, b, c) => AMap(a.toDouble, b.toDouble, c.toDouble)
      }
    }
  .toList

def findNextDigit(init: Double, maps: List[AMap]) = {
  maps.find { amap =>
    init >= amap.source && amap.source + amap.step >= init
  }
  .map { aMap =>
    val diff = aMap.destination - aMap.source

    init + diff
  }
  .getOrElse(init)
}