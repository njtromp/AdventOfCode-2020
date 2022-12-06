package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

class Day06 extends Puzzle2 {

  override def exampleAnswerPart1: Long = 7

  def isMarker(s: String, size: Int): Boolean = s.toSet.size == size

  override def solvePart1(lines: List[String]): Long = {
    lines.head.sliding(4, 1).foldLeft(-4)((a, s) => if (a > 0) a else {
      if (isMarker(s, 4)) -a else a - 1
    })
  }

  override def exampleAnswerPart2: Long = 19

  override def solvePart2(lines: List[String]): Long = {
    lines.head.sliding(14, 1).foldLeft(-14)((a, s) => if (a > 0) a else {
      if (isMarker(s, 14)) -a else a - 1
    })
  }

}

object Day06 extends App {
  new Day06().solvePuzzles("/2022/day06.txt")
}
