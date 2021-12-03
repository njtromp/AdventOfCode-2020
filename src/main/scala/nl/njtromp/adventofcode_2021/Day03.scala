package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode_2020.Puzzle

class Day03 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    def toInt(c: Char): Int = if (c == '1') 1 else -1
    val decoded = lines.map(_.toCharArray.map(toInt))
    val combined = decoded.reduce((a, b) => a.zip(b).map((x) => x._1 + x._2))
    val gammaRate = combined.foldLeft(0)((v, b) => v * 2 + (if (b > 0) 1 else 0))
    val epsilonRate = combined.foldLeft(0)((v, b) => v * 2 + (if (b < 0) 1 else 0))
    gammaRate * epsilonRate
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day03 extends App {
  new Day03().solvePuzzles("/2021/day03.txt")
}

