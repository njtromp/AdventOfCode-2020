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

  override def solvePart2(lines: List[String]): Long = {
    val pos = lines.map(_.split(" "))
      .map(p => (p.head, p(1).toInt))
      .foldLeft((0, 0, 0))((p, c) => c._1.toLowerCase() match {
        case "forward" => (p._1 + c._2, p._2 + c._2 * p._3, p._3)
        case "up" => (p._1, p._2, p._3 - c._2)
        case "down" => (p._1, p._2, p._3 + c._2)
      })
    pos._1 * pos._2
  }

}

object Day02 extends App {
  new Day02().solvePuzzles("/2021/day02.txt")
}

