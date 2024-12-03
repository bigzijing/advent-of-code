@main
def main(file: String = "input.txt", red: Int = 12, green: Int = 13, blue: Int = 14) = {
  val lines = scala.io.Source.fromFile(file).getLines

  val processed = lines.map { row =>
    val Array(gameIdRaw, unparsed) = row.split(":")

    val Array(_, gameIdString) = gameIdRaw.split(" ")

    val gameId = gameIdString.toInt

    val gamesRaw = unparsed.drop(1).split("; ").toList

    val games = gamesRaw.map { raw =>
      raw.split(", ")
        .toList
        .foldLeft(Game(0, 0, 0)) {
          case (acc, next) =>
            next.split(" ") match {
              case Array(num, "red") => acc.copy(red = num.toInt)
              case Array(num, "green") => acc.copy(green = num.toInt)
              case Array(num, "blue") => acc.copy(blue = num.toInt)
            }
        }
    }

    GameRow(gameId, games)
  }

  processed.toList.map {
    case GameRow(_, games) =>
      val r = games.map(_.red).max
      val b = games.map(_.blue).max
      val g = games.map(_.green).max

      r * g * b
  }
    .sum
}

case class Game(red: Int, green: Int, blue: Int)
case class GameRow(id: Int, games: List[Game])