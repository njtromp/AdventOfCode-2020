package nl.njtromp.adventofcode

import scala.collection.mutable
import scala.util.matching.Regex

class Day19 extends Puzzle[Long] {

  private def createRegex(patterns: List[String]): Regex =
    def create(patterns: List[String]): String =
      if patterns.isEmpty then
        ""
      else
        s"(${patterns.head})*${create(patterns.tail)}"
    s"(${create(patterns)})*".r

  private val cache = mutable.Map.empty[String, Long]
  private def produce(towel: String, availablePatterns: List[String]): Long =
    if towel.isEmpty then
      1
    else
      availablePatterns.filter(towel.startsWith)
        .map(p =>
          val remaining = towel.drop(p.length)
          cache.getOrElseUpdate(remaining, produce(remaining, availablePatterns))
        ).sum

  override def exampleAnswerPart1: Long = 6
  override def solvePart1(lines: List[String]): Long =
    val regex = createRegex(lines.head.split(", ").toList)
    lines.drop(2).count(regex.matches)

  override def exampleAnswerPart2: Long = 16
  override def solvePart2(lines: List[String]): Long =
    cache.clear()
    val patterns = lines.head.split(", ").toList
    val regex = createRegex(patterns)
    val result = lines.drop(2).filter(regex.matches)
      .map(t => produce(t, patterns.filter(t.contains))).sum
    result

}

object Day19 extends App {
  new Day19().solvePuzzles()
}
