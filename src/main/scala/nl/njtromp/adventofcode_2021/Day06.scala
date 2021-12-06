package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day06 extends Puzzle {

  @tailrec
  private def evolve(day: Int, fish: List[Int]): Long = {
    if (day == 0)
      fish.length
    else
      evolve(day - 1, fish.flatMap(f => if (f > 0) List(f - 1) else List(6, 8)))
  }

  override def solvePart1(lines: List[String]): Long = {
    evolve(80, lines.head.split(",").map(_.toInt).toList)
  }

  @tailrec
  private def evolve(day: Int, fish: Map[Int, Long]): Long = {
    if (day == 0)
      fish.values.sum
    else {
      val days = List(0, 1, 2, 3, 4, 5, 6, 8)
      val newFish: Map[Int, Long] = days.flatMap(d => {
        if (d == 0)
          Map(6 -> (fish.getOrElse(0, 0L) + fish.getOrElse(7, 0L)), 8 -> fish.getOrElse(0, 0L))
        else
          Map(d - 1 -> fish.getOrElse(d, 0L))
      }).toMap[Int, Long]
      evolve(day - 1, newFish)
    }
  }

  override def solvePart2(lines: List[String]): Long = {
    evolve(256, lines.head.split(",").map(_.toInt).toList
      .groupBy(f => f)
      .map(f => f._1 -> f._2.length.toLong))
  }
}

object Day06 extends App {
  new Day06().solvePuzzles("/2021/day06.txt")
}
