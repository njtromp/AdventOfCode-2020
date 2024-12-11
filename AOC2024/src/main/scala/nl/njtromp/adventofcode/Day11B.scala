package nl.njtromp.adventofcode

import scala.collection.mutable

class Day11B extends Puzzle[Long] {

  private def blink(stone: Long): List[Long] =
    if stone == 0 then
      List(1)
    else if stone.toString.length % 2 == 0 then
      val value = stone.toString
      List(value.take(value.length / 2).toLong, value.drop(value.length / 2).toLong)
    else
      List(stone * 2024)

  private val cache = mutable.Map.empty[(Long, Int), Long]

  private def blink(stones: List[Long], blinks: Int): Long =
    stones.map(stone =>
      cache.getOrElseUpdate((stone, blinks), if blinks == 1 then blink(stone).size.toLong else blink(blink(stone), blinks - 1))
    ).sum

  override def exampleAnswerPart1: Long = 55312
  override def solvePart1(lines: List[String]): Long =
    blink(lines.head.split(' ').map(_.toLong).toList, 25)

  // Example result based on running the example once and using the result as the expected value
  override def exampleAnswerPart2: Long = 65601038650482L
  override def solvePart2(lines: List[String]): Long =
    blink(lines.head.split(' ').map(_.toLong).toList, 75)

}

object Day11B extends App {
  new Day11B().solvePuzzles("/day11.txt")
}
