package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec

class Day20 extends Puzzle2 {

  private class Number(val value: Long) {
    var previous: Number = null
    var next: Number = null
    @tailrec
    final def next(index: Long): Number =
      if (index == 0)
        this
      else
        next.next(index - 1)
    @tailrec
    final def previous(index: Long): Number =
      if (index == 0)
        this
      else
        previous.previous(index - 1)
    def disconnect(): Unit = {
      previous.next = next
      next.previous = previous
    }
    def placeAfter(predecessor: Number): Unit = {
      previous = predecessor
      next = predecessor.next
      next.previous = this
      previous.next = this
    }
  }

  private def linkNumbers(numbers: List[Number]): Unit = {
    val first = numbers.head
    val last = numbers.last
    @tailrec
    def link(current: Number, tail: List[Number]): Unit =
      if (tail.isEmpty) {
        first.previous = last
        last.next = first
      } else {
        val next = tail.head
        current.next = next
        next.previous = current
        link(next, tail.tail)
      }
    link(first, numbers.tail)
  }

  @tailrec
  private def encrypt(numbers: List[Number], length: Long): Unit = {
    if (numbers.nonEmpty) {
      val moving = numbers.head
      val moves = moving.value
      if (moves != 0) {
        moving.disconnect()
        val predecessor = if (moves > 0) moving.next(moves % (length - 1)) else moving.previous(((-moves) + 1L) % (length - 1))
        moving.placeAfter(predecessor)
      }
      encrypt(numbers.tail, length)
    }
  }

  override def exampleAnswerPart1: Long = 3
  override def solvePart1(lines: List[String]): Long = {
    val numbers = lines.map(n => new Number(n.toLong))
    val zero = numbers.find(_.value == 0).get
    linkNumbers(numbers)
    encrypt(numbers, numbers.size)
    zero.next(1000).value + zero.next(2000).value + zero.next(3000).value
  }

  override def exampleAnswerPart2: Long = 1623178306L
  override def solvePart2(lines: List[String]): Long = {
    val numbers = lines.map(n => new Number(811589153L * n.toLong))
    val zero = numbers.find(_.value == 0).get
    linkNumbers(numbers)
    (1 to 10).foreach(_ => encrypt(numbers, numbers.size))

    zero.next(1000).value + zero.next(2000).value + zero.next(3000).value
  }

}

object Day20 extends App{
  new Day20().solvePuzzles("/2022/day20.txt")
}
