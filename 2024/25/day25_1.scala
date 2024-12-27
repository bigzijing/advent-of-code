@main
def main(file: String = "input.txt"): Unit = {

//  val input: List[String] =
//    """#####
//      |.####
//      |.####
//      |.####
//      |.#.#.
//      |.#...
//      |.....
//      |
//      |#####
//      |##.##
//      |.#.##
//      |...##
//      |...#.
//      |...#.
//      |.....
//      |
//      |.....
//      |#....
//      |#....
//      |#...#
//      |#.#.#
//      |#.###
//      |#####
//      |
//      |.....
//      |.....
//      |#.#..
//      |###..
//      |###.#
//      |###.#
//      |#####
//      |
//      |.....
//      |.....
//      |.....
//      |#....
//      |#.#..
//      |#.#.#
//      |#####""".stripMargin
//    .split("\n")
//    .toList

  val input = scala.io.Source.fromFile(file).getLines.toList

  val lockOrKeyLength = 7

  case class KeyOrLockHeights(zero: Int, one: Int, two: Int, three: Int, four: Int) {
    def prettyPrint = s"($zero, $one, $two, $three, $four)"

    def toKey = KeyOrLockHeights(5 - zero, 5 - one, 5 - two, 5 - three, 5 - four)

    def matchOther(other: KeyOrLockHeights) =
      this.zero + other.zero + 2 < 8 &&
        this.one + other.one + 2 < 8 &&
        this.two + other.two + 2 < 8 &&
        this.three + other.three + 2 < 8 &&
        this.four + other.four + 2 < 8

  }

  case object KeyOrLockHeights {
    def empty = KeyOrLockHeights(-1, -1, -1, -1 ,-1)
  }

  val (locks, keys, _) = input.foldLeft((List.empty[List[String]], List.empty[List[String]], Option.empty[Boolean])) {
    case ((lockAcc, keyAcc, bool), next) =>
      next match {
        case "" => (lockAcc, keyAcc, None)
        case _ =>
          if bool.isEmpty then
            if next.head == '#' then
              (List(next) :: lockAcc, keyAcc, Some(true))
            else
              (lockAcc, List(next) :: keyAcc, Some(false))
          else
            if bool == Some(true) then
              ((lockAcc.head appended next) :: lockAcc.drop(1), keyAcc, Some(true))
            else
              (lockAcc, (keyAcc.head appended next) :: keyAcc.drop(1), Some(false))
      }
  }

  def checkStop(height: Int, index: Int, next: String, isLock: Boolean) =
    (next(index), isLock) match {
      case ('.', true) => height
      case ('#', false) => height
      case _ => height + 1
    }


  def countHeights(lockOrKey: List[String], isLock: Boolean) =
    lockOrKey.foldLeft(KeyOrLockHeights.empty) {
      case (KeyOrLockHeights(zero, one, two, three, four), next) =>
        KeyOrLockHeights(
          checkStop(zero, 0, next, isLock),
          checkStop(one, 1, next, isLock),
          checkStop(two, 2, next, isLock),
          checkStop(three, 3, next, isLock),
          checkStop(four, 4, next, isLock),
        )
    }

  val lockHeights: List[KeyOrLockHeights] = locks.map(countHeights(_, true))
  val keyHeights: List[KeyOrLockHeights] = keys.map(countHeights(_, false).toKey)

  val results = keyHeights.foldLeft(0) {
    case (acc, next) =>
      lockHeights.filter { l =>
        l.matchOther(next)
      }.size + acc
  }

  println(results)
}