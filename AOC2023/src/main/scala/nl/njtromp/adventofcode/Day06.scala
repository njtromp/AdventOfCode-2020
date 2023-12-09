package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day06 extends ParserPuzzle[Long] {
  private def parseNumbers(line: String): List[Long] = parse(integers, line).get
  private def calculateWinningTimes(time: Long, distance: Long): Long =
    @tailrec
    def findLimit(t: Long, delta: Long): Long =
      if (time-t)*t > distance then
        t
      else
        findLimit(t + delta, delta)
    findLimit(time, -1) - findLimit(1, 1) + 1
  // I was my preferred solution but did not recognized in time that I needed a correction for the last example
  private def preferredCalculateWinningTimes(time: Long, distance: Long): Long =
    // This correction is needed in case of equals distances
    def correct(t: Long, correction: Long): Long =
      if (time - t) * t == distance then t + correction else t
    // distance < (time - t) * t
    // distance < t*time - t^2
    // 0 < -t^2 + time*t - distance
    // t^2 -time*t + distance = 0
    // a-b-c formula
    val a = 1
    val b = -time
    val c = distance
    val sqrt = Math.sqrt(b * b - 4 * a * c)
    val range = LongRange(correct(Math.ceil((-b - sqrt) / 2 * a).toLong, 1), correct(Math.floor((-b + sqrt) / 2 * a).toLong, -1))
    range.size

  override def exampleAnswerPart1: Long = 288
  override def solvePart1(lines: List[String]): Long =
    val times = parseNumbers(lines.head.substring(10))
    val distances = parseNumbers(lines.last.substring(10))
    times.zip(distances).map(calculateWinningTimes).product

  override def exampleAnswerPart2: Long = 71503
  override def solvePart2(lines: List[String]): Long =
    val time = lines.head.substring(10).replaceAll(" ", "").toLong
    val distance = lines.last.substring(10).replaceAll(" ", "").toLong
    calculateWinningTimes(time, distance)
}

object Day06 extends App {
  new Day06().solvePuzzles()
}
