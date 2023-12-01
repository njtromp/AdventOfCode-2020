package nl.njtromp.adventofcode

import nl.njtromp.adventofcode.Day01.{isExample1, isExample2}

class Day01 extends Puzzle[Long] {
  private val digits = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  override def exampleAnswerPart1: Long = 142
  override def solvePart1(lines: List[String]): Long =
    val actualLines = if isExample1 then lines.take(4) else lines
    isExample1 = false
    actualLines.map(l => {
      l.dropWhile(_.isLetter).head.asDigit * 10 + l.reverse.dropWhile(_.isLetter).head.asDigit
    }).sum

  override def exampleAnswerPart2: Long = 281
  override def solvePart2(lines: List[String]): Long =
    def firstDigit(line: String): Int = {
      val foundDigits = digits.filter(d => line.contains(d)).map(d => (line.indexOf(d), d))
      val digitIndex = line.takeWhile(_.isLetter).length
      if foundDigits.isEmpty then
        line.charAt(digitIndex).asDigit
      else
        val spelledDigit = foundDigits.minBy(_._1)
        if spelledDigit._1 < digitIndex then digits.indexOf(spelledDigit._2) else line.charAt(digitIndex).asDigit
    }
    def lastDigit(line: String): Int = {
      val foundDigits = digits.filter(d => line.contains(d)).map(d => (line.length - line.reverse.indexOf(d.reverse) - d.length, d))
      val digitIndex = line.reverse.dropWhile(_.isLetter).length - 1
      if foundDigits.isEmpty then
        line.charAt(digitIndex).asDigit
      else
        val spelledDigit = foundDigits.maxBy(_._1)
        if spelledDigit._1 > digitIndex then digits.indexOf(spelledDigit._2) else line.charAt(digitIndex).asDigit
    }
    val actualLines = if isExample2 then lines.drop(4) else lines
    isExample2 = false
    actualLines.map(l => firstDigit(l) * 10 + lastDigit(l)).sum

}

object Day01 extends App{
  var isExample1 = true
  var isExample2 = true
  new Day01().solvePuzzles("/day01.txt")
}
