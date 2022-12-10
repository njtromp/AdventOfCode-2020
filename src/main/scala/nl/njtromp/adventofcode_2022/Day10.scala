package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

class Day10 extends Puzzle2 {

  def runSystem(instructions: List[String], x: Long): List[Long] =
    instructions match {
      case Nil => Nil
      case instruction:: remaining => if (instruction == "noop") {
        x :: runSystem(remaining, x)
      } else {
        val delta = instruction.substring(5).toInt
        x :: x :: runSystem(remaining, x + delta)
      }
    }

  override def exampleAnswerPart1: Long = 13140
  override def solvePart1(lines: List[String]): Long = {
    val xs = runSystem(lines.map(_.trim), 1)
    xs.foldLeft((1, 0L))((a, x) => (a._1 + 1, a._2 + (if (a._1 == 20 || (a._1 - 20) % 40 == 0) a._1 * x else 0)))._2
  }

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long = {
    val xs = runSystem(lines.map(_.trim), 1)
    xs.foldLeft(0)((a, p) => {
      print(if (Math.abs((a % 40) - p) <= 1) '#' else '.')
      if ((a + 1) % 40 == 0) println
      a + 1
    })
    println("="*40)
    println("BACEKLHF")
    0
  }

  def printInterestingCycles(l: List[Long]): Unit = {
    var xs = l
    xs = xs.drop(19)
    println(xs.head)

    xs = xs.drop(40)
    println(xs.head)
    xs = xs.drop(40)
    println(xs.head)
    xs = xs.drop(40)
    println(xs.head)
    xs = xs.drop(40)
    println(xs.head)
    xs = xs.drop(40)
    println(xs.head)
  }
}

object Day10 extends App{
  new Day10().solvePuzzles("/2022/day10.txt")
}
