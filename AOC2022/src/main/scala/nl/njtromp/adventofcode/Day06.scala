package nl.njtromp.adventofcode

class Day06 extends Puzzle[Long] {

  override def exampleAnswerPart1: Long = 7
  override def solvePart1(lines: List[String]): Long = {
    val marker = lines.head.sliding(4).filter(_.toSet.size == 4).next()
    lines.head.indexOf(marker) + marker.length
  }

  override def exampleAnswerPart2: Long = 19
  override def solvePart2(lines: List[String]): Long = {
    val marker = lines.head.sliding(14).filter(_.toSet.size == 14).next()
    lines.head.indexOf(marker) + marker.length
  }

}

object Day06 extends App{
  new Day06().solvePuzzles("/day06.txt")
}
