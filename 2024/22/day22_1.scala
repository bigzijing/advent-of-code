@main
def main(file: String = "input.txt"): Unit = {

  val input: List[Long] = scala.io.Source.fromFile(file).getLines.toList.map(_.toLong)

  def longToBit(long: Long, acc: List[Int] = List.empty): List[Int] =
    long match {
      case 1L | 0L => long.toInt :: acc
      case num => longToBit(long / 2, (long % 2).toInt :: acc)
    }

  def bitToLong(bit: List[Int]) =
    bit.foldLeft(0L) {
      case (acc, next) =>
        (acc * 2L) + (next * 1L)
    }

  def mix(longAsBit: List[Int], multipliedAsBit: List[Int]): List[Int] = {
    val longBits = longAsBit.size
    val multipliedBits = multipliedAsBit.size

    val (zeroPaddedLong, zeroPaddedMultipled) = (longBits - multipliedBits) match {
      case 0 => (longAsBit, multipliedAsBit)
      case n if n > 0 => (longAsBit, List.fill(n)(0) ++ multipliedAsBit)
      case n => (List.fill(n * -1)(0) ++ longAsBit, multipliedAsBit)
    }

    zeroPaddedLong.zip(zeroPaddedMultipled).map {
      case (a, b) if a == b => 0
      case _ => 1
    }
  }

  def prune(long: Long) = long % 16777216

  def multiplyThenMix(long: Long): Long = {
    val multiplied = long * 64

    val longAsBit = longToBit(long)
    val multipliedAsBit = longToBit(multiplied)

    bitToLong(mix(longAsBit, multipliedAsBit))
  }

  def divideThenMixThenPrune(long: Long): Long = {
    val divided = long / 32
    val mixed = bitToLong(mix(longToBit(long), longToBit(divided)))

    prune(mixed)
  }

  def multiplyThenMixThenPrune(long: Long, multiplier: Long): Long = {
    val multiplied = long * multiplier
    val mixed = bitToLong(mix(longToBit(long), longToBit(multiplied)))

    prune(mixed)
  }

  def allInOneFellSwoop(long: Long) = multiplyThenMixThenPrune(divideThenMixThenPrune(multiplyThenMixThenPrune(long, 64L)), 2048L)

  def allInNFellSwoops(long: Long, n: Int): Long = n match {
    case 0 => long
    case _ => allInNFellSwoops(allInOneFellSwoop(long), n - 1)
  }

  val result = input.foldLeft(0L) {
    case (acc, next) =>
      acc + allInNFellSwoops(next, 2000)
  }

  println(result)

}