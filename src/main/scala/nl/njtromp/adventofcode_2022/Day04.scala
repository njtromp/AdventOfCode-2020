package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

class Day04 extends Puzzle2 {

  override def exampleAnswerPart1: Long = 2

  override def solvePart1(lines: List[String]): Long = {
    lines.map(l => l.split(","))
      .map(ls => (ls.head.split("-"), ls.tail.head.split("-")))
      .map(s => (s._1.head.toInt to s._1.tail.head.toInt, s._2.head.toInt to s._2.tail.head.toInt))
      .count(s => s._1.intersect(s._2) == s._1 || s._1.intersect(s._2) == s._2)
  }

  override def exampleAnswerPart2: Long = 4

  override def solvePart2(lines: List[String]): Long =
    lines.map(l => l.split(","))
      .map(ls => (ls.head.split("-"), ls.tail.head.split("-")))
      .map(s => (s._1.head.toInt to s._1.tail.head.toInt, s._2.head.toInt to s._2.tail.head.toInt))
      .count(s => s._1.intersect(s._2).nonEmpty)

}

object Day04 extends App {
  new Day04().solvePuzzles("/2022/day04.txt")
}
