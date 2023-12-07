package nl.njtromp.adventofcode

class Day07 extends Puzzle[Long] {
  type Hand = Map[Char, String]
  /* PART 1 */
  private val cardValues = "AKQJT98765432"

  def isFiveOfAKind(cards: Hand): Boolean = cards.size == 1
  def isFourOfAKind(cards: Hand): Boolean = cards.size == 2 && cards.values.count(_.length == 4) == 1
  def isFullHouse(cards: Hand): Boolean = cards.size == 2 && cards.values.count(_.length == 3) == 1
  def isThreeOfAKind(cards: Hand): Boolean = cards.size == 3 && cards.values.count(_.length == 3) == 1
  def isTwoPair(cards: Hand): Boolean = cards.size == 3 && cards.values.count(_.length == 2) == 2
  def isOnePair(cards: Hand): Boolean = cards.size == 4 && cards.values.count(_.length == 2) == 1
  def isHighCard(cards: Hand): Boolean = cards.size == 5
  private val hands: List[Hand => Boolean] = List(isFiveOfAKind, isFourOfAKind, isFullHouse, isThreeOfAKind, isTwoPair, isOnePair, isHighCard)
  private val handOrdering = hands.zipWithIndex

  def orderCards(aCards: String, bCards: String): Boolean =
    val aIndex = cardValues.indexOf(aCards.head)
    val bIndex = cardValues.indexOf(bCards.head)
    if aIndex < bIndex then
      false
    else if aIndex > bIndex then
      true
    else
      orderCards(aCards.tail, bCards.tail)

  private def compare(a: String, b: String): Boolean =
    val aCards = a.substring(0, 5)
    val bCards = b.substring(0, 5)
    val aHand = aCards.groupBy(c => c)
    val bHand = bCards.groupBy(c => c)
    val aScore = handOrdering.filter(_._1(aHand)).head._2
    val bScore = handOrdering.filter(_._1(bHand)).head._2
    if aScore < bScore then
      false
    else if aScore > bScore then
      true
    else
      orderCards(aCards, bCards)

  /* PART 2 */
  private val jCardValues = "AKQT98765432J"

  def countJs(cards: Hand): Int =
    if cards.contains('J') then cards('J').length else 0

  def countMaxNoneJs(cards: Hand): Int =
    cards.filter(_._1 != 'J').map(_._2.length).max

  def isJFiveOfAKind(cards: Hand): Boolean =
    cards.size == 1 ||
      (cards.size == 2 && countMaxNoneJs(cards) + countJs(cards) == 5)
  def isJFourOfAKind(cards: Hand): Boolean =
    (cards.size == 2 && cards.values.count(_.length == 4) == 1) ||
      (cards.size >= 2 && countMaxNoneJs(cards) + countJs(cards) == 4)
  def isJFullHouse(cards: Hand): Boolean =
    (cards.size == 2 && cards.values.count(_.length == 3) == 1) ||
      (cards.size == 3 && countMaxNoneJs(cards) == 2 && countJs(cards) >= 1)
  def isJThreeOfAKind(cards: Hand): Boolean =
    (cards.size == 3 && cards.values.count(_.length == 3) == 1) ||
      (cards.size >= 3 && ((countMaxNoneJs(cards) == 2 && countJs(cards) == 1) || (countMaxNoneJs(cards) == 1 && countJs(cards) == 2)))
  def isJTwoPair(cards: Hand): Boolean =
    (cards.size == 3 && cards.values.count(_.length == 2) == 2) ||
      (cards.size == 3 && countJs(cards) >= 1)
  def isJOnePair(cards: Hand): Boolean =
    (cards.size == 4 && cards.values.count(_.length == 2) == 1) ||
      (cards.size == 5 && countJs(cards) == 1)
  def isJHighCard(cards: Hand): Boolean = cards.size == 5
  private val jhands: List[Hand => Boolean] = List(isJFiveOfAKind, isJFourOfAKind, isJFullHouse, isJThreeOfAKind, isJTwoPair, isJOnePair, isJHighCard)
  private val jhandOrdering = jhands.zipWithIndex

  def jOrderCards(aCards: String, bCards: String): Boolean =
    val aIndex = jCardValues.indexOf(aCards.head)
    val bIndex = jCardValues.indexOf(bCards.head)
    if aIndex < bIndex then
      false
    else if aIndex > bIndex then
      true
    else
      jOrderCards(aCards.tail, bCards.tail)

  private def jCompare(a: String, b: String): Boolean =
    val aCards = a.substring(0, 5)
    val bCards = b.substring(0, 5)
    val aHand = aCards.groupBy(c => c)
    val bHand = bCards.groupBy(c => c)
    val aScore = jhandOrdering.filter(_._1(aHand)).head._2
    val bScore = jhandOrdering.filter(_._1(bHand)).head._2
    if aScore < bScore then
      false
    else if aScore > bScore then
      true
    else
      jOrderCards(aCards, bCards)

  override def exampleAnswerPart1: Long = 6440
  override def solvePart1(lines: List[String]): Long =
    lines.sortWith(compare).zipWithIndex.map(c => (c._2 + 1) * c._1.split(' ').last.toLong).sum

  override def exampleAnswerPart2: Long = 5905
  override def solvePart2(lines: List[String]): Long =
    lines.sortWith(jCompare).zipWithIndex.map(c => (c._2 + 1) * c._1.split(' ').last.toLong).sum

}

object Day07 extends App {
  new Day07().solvePuzzles()
}
