package nl.njtromp.adventofcode

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
    val actualLines = groupByEmptyLine(lines).head
    actualLines.map(_.filter(_.isDigit)).map(l => l.head.asDigit * 10 + l.last.asDigit).sum

  override def exampleAnswerPart2: Long = 281
  override def solvePart2(lines: List[String]): Long =
    val actualLines = groupByEmptyLine(lines).last
    solvePart1(actualLines.map(l => {
      val doubleETs = l.replaceAll("e", "ee").replaceAll("t", "tt")
      digitMapping.foldLeft(doubleETs)((l, d) => l.replaceAll(d._1, d._2))
    }))

}

object Day01C extends App {
  new Day01C().solvePuzzles("/day01.txt")
}
