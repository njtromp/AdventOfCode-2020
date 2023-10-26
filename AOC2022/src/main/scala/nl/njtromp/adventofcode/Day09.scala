package nl.njtromp.adventofcode

import scala.collection.mutable
import scala.annotation.tailrec

class Day09 extends Puzzle[Long] {
  type Pos = (Int, Int)
  type Delta = (Int, Int)
  private val up = "U (\\d+)".r
  private val right = "R (\\d+)".r
  private val down = "D (\\d+)".r
  private val left = "L (\\d+)".r
  private val Up = (0, 1)
  private val Right = (1, 0)
  private val Down = (0, -1)
  private val Left = (-1, 0)

  private def tailNeedsToMove(head: Pos, tail: Pos): Boolean =
    Math.abs(head._1 - tail._1) > 1 || Math.abs(head._2 - tail._2) > 1

  private def moveRope(lines: List[String]): List[Pos] =
    def move(head: Pos, tail: Pos, move: Delta, steps: Int): (Pos, Pos, List[Pos]) =
      val tailPos = mutable.ListBuffer.empty[Pos]
      var h = head
      var t = tail
      (1 to steps).foreach(_ =>
        val newHead = (h._1 + move._1, h._2 + move._2)
        if (tailNeedsToMove(newHead, t))
          val newTail = (t._1 + Math.signum(newHead._1 - t._1).toInt, t._2 + Math.signum(newHead._2 - t._2).toInt)
          if (newTail != t)
            t = newTail
            tailPos += t
        h = newHead
      )
      (h, t, tailPos.toList)

    def moveHead(head: Pos, tail: Pos, line: String): (Pos, Pos, List[Pos]) = line match {
      case up(steps) => move(head, tail, Up, steps.toInt)
      case right(steps) => move(head, tail, Right, steps.toInt)
      case down(steps) => move(head, tail, Down, steps.toInt)
      case left(steps) => move(head, tail, Left, steps.toInt)
    }
    def moveRope(head: Pos, tail: Pos, lines: List[String]): List[Pos] =
      lines match {
        case Nil => Nil
        case move :: moves =>
          val (newHead, newTail, tailPos) = moveHead(head, tail, move)
          tailPos ++ moveRope(newHead, newTail, moves)
      }
    moveRope((0, 0), (0, 0), lines)

  override def exampleAnswerPart1: Long = 13
  override def solvePart1(lines: List[String]): Long = {
    val tailPositions = (0, 0) :: moveRope(lines)
    tailPositions.toSet.size
  }

  private def follow(head: Pos, tail: Pos): Pos =
    if (head == tail || !tailNeedsToMove(head, tail))
      tail
    else
      (tail._1 + Math.signum(head._1 - tail._1).toInt, tail._2 + Math.signum(head._2 - tail._2).toInt)

  private def follow(heads: List[Pos], tail: Pos): List[Pos] =
    heads match {
      case Nil => tail :: Nil
      case head :: rest =>
        val newTail = follow(head, tail)
        newTail :: follow(rest, newTail)
    }

  @tailrec
  private def follow(headPositions: List[Pos], knots: Int): List[Pos] =
    if (knots == 0)
      headPositions
    else
      follow(follow(headPositions, (0, 0)), knots - 1)

  override def exampleAnswerPart2: Long = 36
  override def solvePart2(lines: List[String]): Long = {
    val tailPositions = follow((0, 0) :: moveRope(lines), 9)
    tailPositions.toSet.size
  }

}

object Day09 extends App{
  new Day09().solvePuzzles("/day09.txt")
}
