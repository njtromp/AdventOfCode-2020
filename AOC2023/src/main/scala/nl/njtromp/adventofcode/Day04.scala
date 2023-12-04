package nl.njtromp.adventofcode

import scala.util.parsing.combinator.RegexParsers

class Day04 extends Puzzle[Long] with RegexParsers {
  case class ScratchCard(id: Int, winningNumbers: Set[Int], numbers: Set[Int]) {
    lazy val numberOfMatchingCards: Int = (winningNumbers intersect numbers).size
    lazy val score: Int = Math.pow(2, numberOfMatchingCards - 1).toInt // 0.5 => 0, :-)
    def score(cards: Array[ScratchCard]): Int =
      val copies = cards.slice(id, id + numberOfMatchingCards)
      1 + copies.map(c => c.score(cards)).sum
  }

  def number: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def card: Parser[ScratchCard] = "Card" ~ number ~ ":" ~ rep(number) ~ "|" ~ rep(number) ^^ {
    case  "Card" ~ id ~ ":" ~ winningNumbers ~ "|" ~ numbers => ScratchCard(id, winningNumbers.toSet, numbers.toSet)
  }

  private def parseScratchCard(line: String): ScratchCard =
    parse(card, line) match { case Success(card, _) => card}

  override def exampleAnswerPart1: Long = 13
  override def solvePart1(lines: List[String]): Long =
    lines.map(parseScratchCard).map(_.score).sum

  override def exampleAnswerPart2: Long = 30
  override def solvePart2(lines: List[String]): Long =
    val cards = lines.map(parseScratchCard).toArray
    cards.map(c => c.score(cards)).sum
}

object Day04 extends App {
  new Day04().solvePuzzles()
}
