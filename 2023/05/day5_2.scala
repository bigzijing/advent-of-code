@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines.toList
    .filterNot(_ == "")
    .mkString("|")

    val sampleLines = List(
      "seeds: 79 14 55 13",
      "",
      "seed-to-soil map:",
      "50 98 2",
      "52 50 48",
      "",
      "soil-to-fertilizer map:",
      "0 15 37",
      "37 52 2",
      "39 0 15",
      "",
      "fertilizer-to-water map:",
      "49 53 8",
      "0 11 42",
      "42 0 7",
      "57 7 4",
      "",
      "water-to-light map:",
      "88 18 7",
      "18 25 70",
      "",
      "light-to-temperature map:",
      "45 77 23",
      "81 45 19",
      "68 64 13",
      "",
      "temperature-to-humidity map:",
      "0 69 1",
      "1 0 69",
      "",
      "humidity-to-location map:",
      "60 56 37",
      "56 93 4"
    )
      .filterNot(_ == "")
      .mkString("|")

  val keys = List(
    "seed-to-soil",
    "soil-to-fertilizer",
    "fertilizer-to-water",
    "water-to-light",
    "light-to-temperature",
    "temperature-to-humidity",
    "humidity-to-location"
  ).reverse

  val max = lines.split(" ")
    .map(_.filter(_.isDigit))
    .filterNot(_ == "")
    .map(s => BigInt(s)).max

  val (map, seedStrRaw) = keys.foldLeft((Map.empty[String, List[SeedTransformer]], lines)) {
    case ((accMap, accStr), next) =>
      val (mapRaw, remainingStr) = getLastMapAndDropFromString(accStr, next)
      val map = parseMapTransformer(mapRaw, next, max)

      (accMap.updated(next, map), remainingStr)
  }

  val seeds = seedStrRaw.drop(seedStrRaw.indexOf(':') + 2)
    .split(' ').toList.map(s => BigInt(s))
    .grouped(2)
    .map {
      case init :: step :: Nil =>
        SeedRange(init, init + step - 1)
    }
    .toList

  keys.reverse.foldLeft(seeds) {
    case (acc, next) =>

      val transformers = map.get(next).get

//      println(s"f(x): ${transformers.map(t => (t.start, t.end, s" x + ${t.transformer(0)}"))}")

      val res = acc.flatMap(splitSeeds(transformers, _))

      res
  }
    .map(_.start).min
}

case class AMap(destination: BigInt, source: BigInt, step: BigInt) {
  def toSeedTransformer =
    SeedTransformer(source, source + step - 1, int => int + (destination - source))
}

case class SeedTransformer(start: BigInt, end: BigInt, transformer: BigInt => BigInt)

object SeedTransformer {
  def identity(start: BigInt, stop: BigInt) =
    SeedTransformer(start, stop, x => x)
}

case class SeedRange(start: BigInt, stop: BigInt)

def splitSeeds(transformers: List[SeedTransformer], seed: SeedRange): List[SeedRange] = {
  val transformer = transformers.sortBy(_.start).filter(t => seed.start >= t.start).lastOption

//  println(s"Seed: (${seed.start}, ${seed.stop})")
//  println(s"Transformer found: ${transformer.map(t => (t.start, t.end, s" x + ${t.transformer(0)}"))}")

  transformer match {
    case Some(SeedTransformer(start, end, f)) if end >= seed.stop =>
      List(SeedRange(start = f(seed.start), stop = f(seed.stop)))
    case Some(SeedTransformer(start, end, f)) =>
      SeedRange(start = f(seed.start), stop = f(end)) :: splitSeeds(transformers, SeedRange(end + 1, seed.stop))
    case _ => List(seed)
  }

}


def getLastMapAndDropFromString(string: String, mapKey: String): (String, String) = {
  val strLen = string.size
  val indexToSlice = string.indexOf(mapKey)
  val map = string.slice(indexToSlice, strLen)
  val remainingStringRaw = string.dropRight(strLen - indexToSlice)
  val remainingStr = if (remainingStringRaw.last == '|') remainingStringRaw.dropRight(1) else remainingStringRaw

  (map, remainingStr)
}

def parseMapTransformer(string: String, mapKey: String, max: BigInt) = {
  val rawMap = string.split('|').drop(1).map { triplets =>
      triplets.split(" ") match {
        case Array(a, b, c) => AMap(BigInt(a), BigInt(b), BigInt(c))
      }
    }
    .toList
    .sortBy(_.source)

  val seedTransformersRaw = rawMap.map(_.toSeedTransformer)

  val seedTransformers = seedTransformersRaw.sliding(2).toList.foldLeft(List(seedTransformersRaw.head)) {
    case (acc, head :: last :: Nil) => (head, last) match {
      case (SeedTransformer(_, end, _), b @ SeedTransformer(start, _, _)) if start - end > 1 =>
        SeedTransformer.identity(end + 1, start - 1) :: b :: acc
      case (a, b) => b :: acc
    }
  }.reverse
  match {
    case head :: tail if head.start > 0 => SeedTransformer.identity(0, head.start - 1) :: head :: tail
    case ls => ls
  } match {
    case ls: List[SeedTransformer] if ls.last.end < max => ls.appended(SeedTransformer.identity(ls.last.end + 1, max))
    case ls => ls
  }

  seedTransformers
}

def parseMapString(string: String, mapKey: String): List[AMap] =
  string.split('|').drop(1).map { triplets =>
      triplets.split(" ") match {
        case Array(a, b, c) => AMap(BigInt(a), BigInt(b), BigInt(c))
      }
    }
    .toList

def findNextDigit(init: BigInt, maps: List[AMap]) = {
  maps.find { amap =>
      init >= amap.source && amap.source + amap.step >= init
    }
    .map { aMap =>
      val diff = aMap.destination - aMap.source

      init + diff
    }
    .getOrElse(init)
}