package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode_2020.Puzzle

class Day01 extends Puzzle {
  def zip(depths: List[Int]): List[(Int, Int)] = depths.zip(depths.tail)

  override def solvePart1(lines: List[String]): Long = {
    zip(lines.map(_.toInt)).count(d => d._1 < d._2)
  }

  override def solvePart2(lines: List[String]): Long = {
    def zipzip(depths: List[Int]): List[List[Int]] =
      depths.zip(depths.tail).zip(depths.tail.tail).map(d => List(d._1._1, d._1._2, d._2))
    zip(zipzip(lines.map(_.toInt)).map(_.sum)).count(d => d._1 < d._2)
  }

}

object Day01 extends App {
  new Day01().solvePuzzles("/2021/day01.txt")
}
