package nl.njtromp.adventofcode

class Day01 extends Puzzle[Long] {
  private val digits = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  override def exampleAnswerPart1: Long = 142
  override def solvePart1(lines: List[String]): Long =
    val actualLines = groupByEmptyLine(lines).head
    actualLines.map(l => {
      l.dropWhile(_.isLetter).head.asDigit * 10 + l.reverse.dropWhile(_.isLetter).head.asDigit
    }).sum

  override def exampleAnswerPart2: Long = 281
  override def solvePart2(lines: List[String]): Long =
    def firstDigit(line: String): Int = {
      val letterDigitsWithIndex = digits.filter(d => line.contains(d)).map(d => (line.indexOf(d), d))
      val digitIndex = line.takeWhile(_.isLetter).length
      if letterDigitsWithIndex.isEmpty then
        line.charAt(digitIndex).asDigit
      else
        val letterDigit = letterDigitsWithIndex.minBy(_._1)
        if letterDigit._1 < digitIndex then digits.indexOf(letterDigit._2) else line.charAt(digitIndex).asDigit
    }
    def lastDigit(line: String): Int = {
      val letterDigitsWithIndex = digits.filter(d => line.contains(d)).map(d => (line.length - line.reverse.indexOf(d.reverse) - d.length, d))
      val digitIndex = line.reverse.dropWhile(_.isLetter).length - 1
      if letterDigitsWithIndex.isEmpty then
        line.charAt(digitIndex).asDigit
      else
        val letterDigit = letterDigitsWithIndex.maxBy(_._1)
        if letterDigit._1 > digitIndex then digits.indexOf(letterDigit._2) else line.charAt(digitIndex).asDigit
    }
    val actualLines = groupByEmptyLine(lines).last
    actualLines.map(l => firstDigit(l) * 10 + lastDigit(l)).sum

}

object Day01 extends App{
  new Day01().solvePuzzles()
}
