package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode_2020.Puzzle

class Day02 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    val pos = lines.map(_.split(" ")).map(c => {
      c.head.toLowerCase() match {
        case "forward" => (c(1).toInt, 0)
        case "up" => (0, -c(1).toInt)
        case "down" => (0, c(1).toInt)
      }
    }).reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    pos._1 * pos._2
  }

  override def solvePart2(lines: List[String]): Long = ???

}

object Day02 extends App {
  new Day02().solvePuzzles("/2021/day02.txt")
}

