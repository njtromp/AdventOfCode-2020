package nl.njtromp.adventofcode

import nl.njtromp.adventofcode.Day01C.{isExample1, isExample2}

class Day01C extends Puzzle[Long] {
  private val digitMapping = Map(
    "one" ->  "1",
    "two" -> "2",
    "three" -> "3",
    "four" ->"4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9"
  )

  override def exampleAnswerPart1: Long = 142
  override def solvePart1(lines: List[String]): Long =
    val actualLines = if isExample1 then lines.take(4) else lines
    isExample1 = false
    actualLines.map(_.filter(_.isDigit)).map(l => l.head.asDigit * 10 + l.last.asDigit).sum

  override def exampleAnswerPart2: Long = 281
  override def solvePart2(lines: List[String]): Long =
    val actualLines = if isExample2 then lines.drop(4) else lines
    isExample2 = false
    solvePart1(actualLines.map(l => digitMapping.foldLeft(l)((l, d) => l.replaceAll(d._1, d._2))))

}

object Day01C extends App {
  var isExample1 = true
  var isExample2 = true
  new Day01C().solvePuzzles()
}
