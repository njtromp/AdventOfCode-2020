package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

class Day08 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    val bla = lines.map(_.split("\\|")(1).split(" ").map(_.trim).filterNot(_.isBlank))
    bla.flatMap(_.map(_.length)).count(List(2, 4, 3, 7).contains(_))
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day08 extends App{
  new Day08().solvePuzzles("/2021/day08.txt")
}
