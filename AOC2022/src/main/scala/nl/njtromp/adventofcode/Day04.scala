package nl.njtromp.adventofcode

import scala.util.matching.Regex

class Day04 extends Puzzle[Long] {
  private val pattern: Regex = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r

  private def cleanRanges(line: String): (LongRange, LongRange) =
    line match {
      case pattern(lower1, upper1, lower2, upper2) => (LongRange(lower1.toLong, upper1.toLong), LongRange(lower2.toLong, upper2.toLong))
    }

  override def exampleAnswerPart1: Long = 2
  override def solvePart1(lines: List[String]): Long = {
    lines.map(cleanRanges).count(a => a._1.contains(a._2) || a._2.contains(a._1))
  }

  override def exampleAnswerPart2: Long = 4
  override def solvePart2(lines: List[String]): Long = {
    lines.map(cleanRanges).count(a => a._1.isOverlapping(a._2) || a._2.isOverlapping(a._1))
  }

}

object Day04 extends App{
  new Day04().solvePuzzles("/day04.txt")
}
