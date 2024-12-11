package nl.njtromp.adventofcode

import scala.collection.mutable

class Day11 extends Puzzle[Long] {

  private val cache = mutable.Map.empty[(Long, Int), Long]

  private def blinkOnce(stone: Long): List[Long] =
    if stone == 0 then
      List(1)
    else if stone.toString.length % 2 == 0 then
      val value = stone.toString
      List(value.take(value.length / 2).toLong, value.drop(value.length / 2).toLong)
    else
      List(stone * 2024)

  private def blink(stones: List[Long], blinks: Int): Long =
    stones.map(s =>
      if cache.contains((s, blinks)) then
        cache((s, blinks))
      else
        val result = if blinks == 1 then blinkOnce(s).size.toLong else blink(blinkOnce(s), blinks - 1)
        cache += ((s, blinks) -> result)
        result
    ).sum

  override def exampleAnswerPart1: Long = 55312
  override def solvePart1(lines: List[String]): Long =
    lines.map(l => blink(l.split(' ').map(_.toLong).toList, 25)).sum

  // Example result based on running the example once and using the result as the expected value
  override def exampleAnswerPart2: Long = 65601038650482L
  override def solvePart2(lines: List[String]): Long =
    lines.map(l => blink(l.split(' ').map(_.toLong).toList, 75)).sum

}

object Day11 extends App {
  new Day11().solvePuzzles()
}
