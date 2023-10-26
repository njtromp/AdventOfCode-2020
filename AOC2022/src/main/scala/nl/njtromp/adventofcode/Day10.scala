package nl.njtromp.adventofcode

import scala.collection.mutable
import scala.util.matching.Regex

class Day10 extends Puzzle[Long] {
  private val noop: Regex = "noop".r
  private val addx: Regex = "addx (-?\\d+)".r

  private def executeProgram(instructions: List[String]): Array[Long] =
    var x = 1L
    val values = mutable.ListBuffer.empty[Long]
    values += 0
    instructions.foreach {
      case addx(delta) =>
        values += x
        values += x
        x += delta.toLong
      case noop =>
        values += x
    }
    values.toArray

  override def exampleAnswerPart1: Long = 13140
  override def solvePart1(lines: List[String]): Long = {
    val xs = executeProgram(lines)
    (20 to 220 by 40).map(t => t * xs(t)).sum
  }

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long = {
    println("=" * 40)
    val xs = executeProgram(lines)
    (1 to 201 by 40).foreach(offset =>
      (0 until 40).foreach(pixel => print(if (Math.abs(xs(offset + pixel) - pixel) <= 1) '#' else '.'))
      println
    )
    println("BRJLFULP")
    0
  }

}

object Day10 extends App{
  new Day10().solvePuzzles("/day10.txt")
}
