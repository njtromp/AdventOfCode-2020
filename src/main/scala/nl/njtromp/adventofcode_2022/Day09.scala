package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

class Day09 extends Puzzle2 {
  type Pos = (Int, Int)
  val up: Pos = (0, 1)
  val down: Pos = (0, -1)
  val right: Pos = (1, 0)
  val left: Pos = (-1, 0)

  class Move(delta: Pos, var n: Int) {
    def moveHead(head: Pos): Pos =
      (head._1 + delta._1, head._2 + delta._2)

    def moveTail(head: Pos, tail: Pos): Pos = {
      if (Math.abs(tail._1 - head._1) > 1 || Math.abs(tail._2 - head._2) > 1) {
        (tail._1 + (head._1 - tail._1).signum, tail._2 + (head._2 - tail._2).signum)
      } else
        tail
    }

    def moveRope(rope: List[Pos]): (List[Pos], List[Pos]) = {
      var newRope: List[Pos] = rope
      var positions: List[Pos] = Nil
      while (n > 0) {
        val previousTail = newRope.last
        val newHead: Pos = moveHead(newRope.head)
        newRope = newHead :: newRope.tail.foldLeft((newHead, List[Pos]()))((a, k) => {
          val newKnot = moveTail(a._1, k)
          (newKnot, a._2 ++ List(newKnot))
        })._2
        val newTail = newRope.last
        if (newTail != previousTail) {
          positions = newTail :: positions
        }
        n -= 1
      }
      (newRope, positions)
    }
  }

  def decodeMove(line: String): Move =
    line.head match {
      case 'U' => new Move(up, line.substring(2).toInt)
      case 'D' => new Move(down, line.substring(2).toInt)
      case 'L' => new Move(left, line.substring(2).toInt)
      case 'R' => new Move(right, line.substring(2).toInt)
    }

  override def exampleAnswerPart1: Long = 88//13

  def moveRope(lines: List[String], rope: List[Pos], positions: List[Pos]): List[Pos] = {
    lines match {
      case line :: remaining =>
        val newRope = decodeMove(line).moveRope(rope)
        positions ++ newRope._2 ++ moveRope(remaining, newRope._1, positions)
      case Nil => Nil
    }
  }

  def printPositions(positions: List[(Int, Int)]): Unit = {
    if (positions != Nil) {
      val minX = positions.map(_._1).min
      val maxX = positions.map(_._1).max
      val minY = positions.map(_._2).min
      val maxY = positions.map(_._2).max
      for (y <- Range(maxY, minY, -1).inclusive) {
        for (x <- minX to maxX) {
          if ((x, y) == (0, 0))
            print('S')
          else
            print(if (positions.contains((x, y))) '#' else '.')
        }
        println
      }
      println
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val rope: List[Pos] = List((0, 0), (0, 0))
    val tailPositions = moveRope(lines, rope, List((0, 0)))
    tailPositions.distinct.size
  }

  override def exampleAnswerPart2: Long = 36

  override def solvePart2(lines: List[String]): Long = {
    val rope: List[Pos] = List((0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0))
    val tailPositions = moveRope(lines, rope, List((0, 0)))
    tailPositions.distinct.size
  }
}

object Day09 extends App{
  new Day09().solvePuzzles("/2022/day09.txt")
}
