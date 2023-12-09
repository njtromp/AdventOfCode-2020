package nl.njtromp.adventofcode

class Day01B extends Puzzle[Long] {
  private val digits = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  override def exampleAnswerPart1: Long = 142
  override def solvePart1(lines: List[String]): Long =
    val actualLines = groupByEmptyLine(lines).head
    actualLines.map(_.filter(_.isDigit)).map(l => l.head.asDigit * 10 + l.last.asDigit).sum

  override def exampleAnswerPart2: Long = 281
  override def solvePart2(lines: List[String]): Long =
    val actualLines = groupByEmptyLine(lines).last
    def toDigits(line: String): String =
      if line.isBlank then
        line
      else if line.head.isDigit then
        line.head + toDigits(line.tail)
      else {
        val decodedDigits = digits.filter(line.indexOf(_) == 0).map(d => digits.indexOf(d))
        if decodedDigits.isEmpty then
          toDigits(line.tail)
        else
          decodedDigits.head.toString.head + toDigits(line.tail)
      }

    actualLines.map(toDigits).map(l => l.head.asDigit * 10 + l.last.asDigit).sum

}

object Day01B extends App{
  new Day01B().solvePuzzles("/day01.txt")
}
