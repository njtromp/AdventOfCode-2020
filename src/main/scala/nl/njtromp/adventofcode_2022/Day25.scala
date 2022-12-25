package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.StringPuzzle

class Day25 extends StringPuzzle {

  private val mapping = Map('2' -> 2L, '1' -> 1L, '0' -> 0L, '-' -> -1L, '=' -> -2L)

  private def powOf5(power: Int): Long =
    if (power == 0)
      1
    else
      5 * powOf5(power - 1)

  private def deSNAFU(snafu: String): Long =
    snafu.reverse.zipWithIndex.map(s => powOf5(s._2) * mapping(s._1)).sum

  private def snafu(number: Long): String = {
    if (number == 0)
      ""
    else {
      number % 5 match {
        case 4 => snafu(number / 5 + 1) ++ "-"
        case 3 => snafu(number / 5 + 1) ++ "="
        case n => snafu(number / 5) ++ n.toString
      }
    }
  }

  override def exampleAnswerPart1: String = "2=-1=0"
  override def solvePart1(lines: List[String]): String = {
    snafu(lines.map(deSNAFU).sum)
  }

  override def exampleAnswerPart2: String = "'example'"
  override def solvePart2(lines: List[String]): String = {
    "'Not implemented'"
  }

}

object Day25 extends App{
  new Day25().solvePuzzles("/2022/day25.txt")
}
