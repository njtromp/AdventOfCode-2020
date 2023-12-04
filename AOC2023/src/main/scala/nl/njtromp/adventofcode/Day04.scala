package nl.njtromp.adventofcode

class Day04 extends Puzzle[Long] {
  private case class ScratchCard(id: Int, winningNumbers: Set[Int], numbers: Set[Int]) {
    private def numberOfMatchingCards(): Int = (winningNumbers intersect numbers).size
    def score(): Int = Math.pow(2, numberOfMatchingCards() - 1).toInt // 0.5 => 0, :-)
    def score(tail: Array[ScratchCard]): Int =
      val copies = tail.take(numberOfMatchingCards())
      1 + copies.indices.map(i => tail(i).score(tail.drop(i + 1))).sum
  }

  private def parseScratchCard(line: String): ScratchCard =
    val parts = line.split('|')
    val id = parts.head.split(':').head.split(' ').last.toInt
    val winningNumbers = parts.head.split(':').last.split(' ').filter(_.nonEmpty).map(_.toInt).toSet
    val numbers = parts.last.split(' ').filter(_.nonEmpty).map(_.toInt).toSet
    ScratchCard(id, winningNumbers, numbers)

  override def exampleAnswerPart1: Long = 13
  override def solvePart1(lines: List[String]): Long =
    lines.map(parseScratchCard).map(_.score()).sum

  override def exampleAnswerPart2: Long = 30
  override def solvePart2(lines: List[String]): Long =
    val cards = lines.map(parseScratchCard).toArray
    cards.indices.map(i => cards(i).score(cards.drop(i + 1))).sum
}

object Day04 extends App {
  new Day04().solvePuzzles()
}
