package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.{Puzzle2, SimpleMapTypes}

class Day09 extends Puzzle2 with SimpleMapTypes{

  class Move(delta: Pos, var n: Int) {
    def doMove(head: Pos, tail: Pos): (Pos, Pos) =
      ((head._1 + delta._1, head._2 + delta._2), moveTail((head._1 +delta._1, head._2 + delta._2), tail))

    def moveTail(head: Pos, tail: Pos): Pos = {
      def correct(delta: Int, difference: Int): Int = if (delta == 0) difference else delta
      if (Math.abs(tail._1 - head._1) > 1 || Math.abs(tail._2 - head._2) > 1) {
        (tail._1 + correct(delta._1, head._1 - tail._1), tail._2 + correct(delta._2, head._2 - tail._2))
      } else
        tail
    }

    def moveRope(head: Pos, tail: Pos): (Pos, Pos, List[Pos]) = {
      var newHead = head
      var newTail = tail
      var positions: List[Pos] = Nil
      while (n > 0) {
        val p = doMove(newHead, newTail)
        if (p._2 != newTail) {
          positions = p._2 :: positions
        }
        newHead = p._1
        newTail = p._2
        n -= 1
      }
      (newHead, newTail, positions)
    }
  }

  def decodeMove(line: String): Move =
    line.head match {
      case 'U' => new Move(up, line.substring(2).toInt)
      case 'D' => new Move(down, line.substring(2).toInt)
      case 'L' => new Move(left, line.substring(2).toInt)
      case 'R' => new Move(right, line.substring(2).toInt)
    }

  override def exampleAnswerPart1: Long = 13

  def moveRope(lines: List[String], head: Pos, tail: Pos): List[Pos] = {
    lines match {
      case line :: remaining =>
        val newPos = decodeMove(line).moveRope(head, tail)
        newPos._3 ++ moveRope(remaining, newPos._1, newPos._2)
      case Nil => Nil
    }
  }

  def printPositions(positions: List[(Int, Int)]): Unit = {
    val minX = positions.map(_._1).min
    val maxX = positions.map(_._1).max
    val minY = positions.map(_._2).min
    val maxY = positions.map(_._2).max
    for (y <- Range(maxY, minY, -1).inclusive) {
      for (x <- minX to maxX) {
        print(if (positions.contains((x, y))) '#' else '.')
      }
      println
    }
    println
  }

  override def solvePart1(lines: List[String]): Long = {
    val tailPositions: List[Pos] = (0, 0) :: moveRope(lines, (0, 0), (0, 0))
//    printPositions(tailPositions)
    tailPositions.distinct.size
  }

  override def exampleAnswerPart2: Long = 36

  override def solvePart2(lines: List[String]): Long = {
    -1
  }
}

object Day09 extends App{
  new Day09().solvePuzzles("/2022/day09.txt")
}
