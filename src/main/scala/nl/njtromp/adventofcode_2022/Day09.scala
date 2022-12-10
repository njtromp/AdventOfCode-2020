package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

class Day09 extends Puzzle2 {
  type Pos = (Int, Int)
  val up: Pos = (0, 1)
  val down: Pos = (0, -1)
  val right: Pos = (1, 0)
  val left: Pos = (-1, 0)

  class Move(delta: Pos, var moves: Int) {
    def moveHead(head: Pos): Pos =
      (head._1 + delta._1, head._2 + delta._2)

    def moveTail(head: Pos, tail: Pos): Pos = {
      if (Math.abs(tail._1 - head._1) > 1 || Math.abs(tail._2 - head._2) > 1) {
        (tail._1 + (head._1 - tail._1).signum, tail._2 + (head._2 - tail._2).signum)
      } else
        tail
    }

    def moveRope(rope: List[Pos], tailPositions: List[Pos], move: Int): (List[Pos], List[Pos]) = {
      if (move == moves)
        (rope, tailPositions)
      else {
        val newHead = moveHead(rope.head)
        val newRope = newHead :: rope.tail.foldLeft((newHead, List[Pos]()))((a, k) => {
          val newKnot = moveTail(a._1, k)
          (newKnot, a._2 ++ List(newKnot))
        })._2
        val newTail = newRope.last
        moveRope(newRope, if (newTail != rope.last) newTail :: tailPositions else tailPositions, move + 1)
      }
    }
  }

  def decodeMove(line: String): Move =
    line.head match {
      case 'U' => new Move(up, line.substring(2).toInt)
      case 'D' => new Move(down, line.substring(2).toInt)
      case 'L' => new Move(left, line.substring(2).toInt)
      case 'R' => new Move(right, line.substring(2).toInt)
    }

  override def exampleAnswerPart1: Long = 88 // 13 for example part 1

  def moveRope(lines: List[String], rope: List[Pos], tailPositions: List[Pos]): List[Pos] = {
    lines match {
      case line :: remaining =>
        val ropeAndTails = decodeMove(line).moveRope(rope, Nil, 0)
        tailPositions ++ ropeAndTails._2 ++ moveRope(remaining, ropeAndTails._1, tailPositions)
      case Nil => Nil
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

  def printPositions(tailPostitions: List[(Int, Int)]): Unit = {
    if (tailPostitions != Nil) {
      val minX = tailPostitions.map(_._1).min
      val maxX = tailPostitions.map(_._1).max
      val minY = tailPostitions.map(_._2).min
      val maxY = tailPostitions.map(_._2).max
      for (y <- Range(maxY, minY, -1).inclusive) {
        for (x <- minX to maxX) {
          if ((x, y) == (0, 0))
            print('S')
          else
            print(if (tailPostitions.contains((x, y))) '#' else '.')
        }
        println
      }
      println
    }
  }
}

object Day09 extends App{
  new Day09().solvePuzzles("/2022/day09.txt")
}
