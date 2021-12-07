package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

class Day07 extends Puzzle {
  private def costs1(aligned: Int, squids: List[Int]): Long = {
    squids.map(s => Math.abs(aligned - s)).sum
  }

  override def solvePart1(lines: List[String]): Long = {
    val squids = lines.head.split(",").map(_.toInt).toList
    (squids.min to squids.max).map(costs1(_, squids)).min
  }

  private def costs2(aligned: Int, squids: List[Int]): Long = {
    squids.map(s => Math.abs((Math.min(aligned, s) to Math.max(aligned, s)).map(s => Math.abs(aligned -s )).sum)).sum
  }

  override def solvePart2(lines: List[String]): Long = {
    val squids = lines.head.split(",").map(_.toInt).toList
    (squids.min to squids.max).map(costs2(_, squids)).min
  }
}

object Day07 extends App {
  new Day07().solvePuzzles("/2021/day07.txt")
}
