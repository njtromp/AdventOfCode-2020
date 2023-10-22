package nl.njtromp.adventofcode

class Day02 extends Puzzle[Long] {

  private val ROCK = 1L
  private val PAPER = 2L
  private val SCISSORS = 3L
  private val LOOSE = 0L
  private val DRAW = 3L
  private val WIN = 6L
  private val elvenMapping: Map[Char, Long] = Map( 'A' -> ROCK, 'B' -> PAPER, 'C' -> SCISSORS)

  override def exampleAnswerPart1: Long = 15
  override def solvePart1(lines: List[String]): Long = {
    val ourMapping: Map[Char, Long] = Map('X' -> ROCK, 'Y' -> PAPER, 'Z' -> SCISSORS)
    def resolveMapping(strategy: String): (Long, Long) =
      (elvenMapping.getOrElse(strategy.charAt(0), -1L), ourMapping.getOrElse(strategy.charAt(2), -1L))
    def score(hands: (Long, Long)): Long =
      def winOrLoose(): Long = hands match {
        case (ROCK, PAPER) => WIN
        case (ROCK, SCISSORS) => LOOSE
        case (ROCK, ROCK) => DRAW
        case (PAPER, PAPER) => DRAW
        case (PAPER, SCISSORS) => WIN
        case (PAPER, ROCK) => LOOSE
        case (SCISSORS, PAPER) => LOOSE
        case (SCISSORS, SCISSORS) => DRAW
        case (SCISSORS, ROCK) => WIN
      }
      winOrLoose() + hands._2
    def playRound(strategy: String): Long =
      score(resolveMapping(strategy))
    lines.map(playRound).sum
  }

  override def exampleAnswerPart2: Long = 12
  override def solvePart2(lines: List[String]): Long = {
    val outcomeMapping: Map[Char, Long] = Map('X' -> LOOSE, 'Y' -> DRAW, 'Z' -> WIN)
    def resolveMapping(strategy: String): (Long, Long) =
      (elvenMapping.getOrElse(strategy.charAt(0), -1L), outcomeMapping.getOrElse(strategy.charAt(2), -1L))
    def score(hands: (Long, Long)): Long =
      def winOrLoose(): Long = hands match {
        case (ROCK, LOOSE) => SCISSORS
        case (ROCK, DRAW) => ROCK
        case (ROCK, WIN) => PAPER
        case (PAPER, LOOSE) => ROCK
        case (PAPER, DRAW) => PAPER
        case (PAPER, WIN) => SCISSORS
        case (SCISSORS, LOOSE) => PAPER
        case (SCISSORS, DRAW) => SCISSORS
        case (SCISSORS, WIN) => ROCK
      }
      winOrLoose() + hands._2
    def playRound(strategy: String): Long =
      score(resolveMapping(strategy))
    lines.map(playRound).sum
  }

}

object Day02 extends App{
  new Day02().solvePuzzles("/day02.txt")
}
