@main
def main(file: String = "input.txt") = {
  val lines = scala.io.Source.fromFile(file).getLines.toList

  val sampleLines = List(
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  )

  val hands = lines.map { l =>
      val splitStr = l.split(" ")

      val hand = splitStr(0)
      val bet = splitStr(1).toInt

      (assessHand(hand), hand.map(charToCardFace).toList, bet)
    }
    .groupBy(_._1)
    .map {
      case (k, v) =>
        (
          v.sortWith(compareGroup).map(g => (g._1, g._2.map(_.card).mkString, g._3))
          )
    }
    .toList
    .flatten
    .sortBy(_._1)
    .zipWithIndex
    .map {
      case ((_, _, bet), in) => (in + 1) * bet
    }
    .sum

  hands
}

trait Combo {
  val rank: Int
}

case object HighCard extends Combo {
  val rank = 1
}

case object OnePair extends Combo {
  val rank = 2
}

case object TwoPair extends Combo {
  val rank = 3
}

case object Trips extends Combo {
  val rank = 4
}

case object FullHouse extends Combo {
  val rank = 5
}

case object FourOfAKind extends Combo {
  val rank = 6
}

case object FiveOfAKind extends Combo {
  val rank = 7
}

def scoreToCombo(score: Int) = score match {
  case 1 => HighCard
  case 2 => OnePair
  case 3 => TwoPair
  case 4 => Trips
  case 5 => FullHouse
  case 6 => FourOfAKind
  case 7 => FiveOfAKind
}

def elevateCombo(combo: Combo, numberOfJs: Int) =
  if (numberOfJs == 0) combo
  else
    combo match {
      case HighCard => OnePair
      case OnePair => Trips
      case TwoPair =>
        if (numberOfJs == 1) FullHouse
        else FourOfAKind
      case Trips => FourOfAKind
      case FullHouse => FiveOfAKind
      case FourOfAKind => FiveOfAKind
      case FiveOfAKind => FiveOfAKind
    }

def assessHand(string: String): Int = {
  val valuesMap = string.foldLeft(Map.empty[Char, Int]) {
    case (acc, next) =>
      if (acc.keys.toList.contains(next))
        acc.updated(next, acc(next) + 1)
      else acc.updated(next, 1)
  }

  val values = valuesMap.values.toList

  val initCombo: Combo =
    if (values.size == 1) FiveOfAKind
    else if (values.contains(4)) FourOfAKind
    else if (values.contains(3))
      if (values.contains(2)) FullHouse
      else Trips
    else if (values.contains(2))
      if (values.size == 3) TwoPair
      else OnePair
    else HighCard

  val newComboScore = elevateCombo(initCombo, valuesMap.getOrElse('J', 0))

  newComboScore.rank
}

trait CardFace {
  val value: Int
  val card: Char
}

case object Two extends CardFace {
  val value = 2
  val card = '2'
}
case object Three extends CardFace {
  val value = 3
  val card = '3'
}
case object Four extends CardFace {
  val value = 4
  val card = '4'
}
case object Five extends CardFace {
  val value = 5
  val card = '5'
}
case object Six extends CardFace {
  val value = 6
  val card = '6'
}
case object Seven extends CardFace {
  val value = 7
  val card = '7'
}
case object Eight extends CardFace {
  val value = 8
  val card = '8'
}
case object Nine extends CardFace {
  val value = 9
  val card = '9'
}
case object Ten extends CardFace {
  val value = 10
  val card = 'T'
}
case object Jack extends CardFace {
  val value = 1
  val card = 'J'
}
case object Queen extends CardFace {
  val value = 12
  val card = 'Q'
}
case object King extends CardFace {
  val value = 13
  val card = 'K'
}
case object Ace extends CardFace {
  val value = 14
  val card = 'A'
}

def charToCardFace(c: Char): CardFace = c match {
  case '2' => Two
  case '3' => Three
  case '4' => Four
  case '5' => Five
  case '6' => Six
  case '7' => Seven
  case '8' => Eight
  case '9' => Nine
  case 'T' => Ten
  case 'J' => Jack
  case 'Q' => Queen
  case 'K' => King
  case 'A' => Ace
}

def compareHands(h1: List[CardFace], h2: List[CardFace]) = (h1.map(_.value), h2.map(_.value)) match {
  case (a1 :: b1 :: c1 :: d1 :: e1 :: Nil, a2 :: b2 :: c2 :: d2 :: e2 :: Nil) =>
    if (a1 == a2)
      if (b1 == b2)
        if (c1 == c2)
          if (d1 == d2)
            if (e1 == e2) true
            else e1 < e2
          else d1 < d2
        else c1 < c2
      else b1 < b2
    else a1 < a2
}

def compareGroup(h1: (Int, List[CardFace], Int), h2: (Int, List[CardFace], Int)) = compareHands(h1._2, h2._2)