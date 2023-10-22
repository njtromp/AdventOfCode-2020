package nl.njtromp.adventofcode

class Day01 extends Puzzle[Long] {

  override def exampleAnswerPart1: Long = 24000
  override def solvePart1(lines: List[String]): Long = {
    groupByEmptyLine(lines).map(_.map(_.toLong).sum).max
  }

  override def exampleAnswerPart2: Long = 45000
  override def solvePart2(lines: List[String]): Long = {
    groupByEmptyLine(lines).map(_.map(_.toLong).sum).sortWith((a, b) => a > b).take(3).sum
  }

}

object Day01 extends App{
  new Day01().solvePuzzles("/day01.txt")
}
