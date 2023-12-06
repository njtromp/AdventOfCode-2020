package nl.njtromp.adventofcode

import scala.util.parsing.combinator.RegexParsers

class Day07 extends Puzzle[Long] with RegexParsers {
  private def number: Parser[Long] = "\\d+".r ^^ { _.toLong }
  private def numbers: Parser[List[Long]] = rep(number) ^^ { numbers => numbers }

  override def exampleAnswerPart1: Long = 0
  override def solvePart1(lines: List[String]): Long =
    -1

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day07 extends App {
  new Day07().solvePuzzles()
}
