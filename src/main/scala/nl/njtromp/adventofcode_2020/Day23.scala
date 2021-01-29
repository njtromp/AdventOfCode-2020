package nl.njtromp.adventofcode_2020

import scala.annotation.tailrec
import scala.collection.mutable

class Day23(limit1: Long, moves1: Long, limit2: Long, moves2: Long) extends Puzzle {
  class Cup(value: Long) {
    var next: Cup = null;
    @tailrec
    final def last: Cup = if (next == null) this else next.last
    @tailrec
    final def skip(nr: Int): Cup = if (nr == 0) this else next.skip(nr - 1)
    @tailrec
    final def cut(tail: Cup): Unit = if (next == tail) next = null else next.cut(tail)
    @tailrec
    final def exists(nr: Long): Boolean = if (nr == value) true else if (next == null) false else next.exists(nr)
    def label: Long = value
    override def toString: String = value.toString
  }

  private val cups: mutable.HashMap[Long, Cup] = mutable.HashMap.empty

  override def solvePart1(lines: List[String]): Long = {
    var current: Cup = createCups(lines.head.toList, limit1)
    current.last.next = current
    letCrabPlay(current, limit1, moves1)
    val one = cups(1)
    val result: StringBuilder = new StringBuilder()
    current = one.next
    while (current != one) {
      result.append(current.label)
      current = current.next
    }
    result.toString().toLong
  }

  override def solvePart2(lines: List[String]): Long = {
    val current: Cup = createCups(lines.head.toList, limit2)
    current.last.next = current
    letCrabPlay(current, limit2, moves2)
    val firstStar = cups(1).next
    firstStar.label * firstStar.next.label
  }

  def letCrabPlay(start: Cup, limit: Long, maxMoves: Long): Unit = {
    var current = start
    var moves = maxMoves
    while (moves > 0) {
      val removed = current.next
      val gap = removed.skip(3)
      removed.cut(gap)
      current.next = gap

      var value = current.label
      do {
        value -= 1
        if (value == 0) value = limit
      }
      while (removed.exists(value));

      removed.last.next = cups(value).next
      cups(value).next = removed

      current = current.next
      moves -= 1
    }

  }

  def createCups(labels: List[Char], limit: Long): Cup = {
    val head = new Cup(labels.head.toString.toLong)
    cups += (head.label -> head)
    var last = head
    var label: Long = 2
    for (l <- labels.tail) {
      last.next = new Cup(l.toString.toLong)
      last = last.next
      cups += (last.label -> last)
      label += 1
    }
    while (label <= limit) {
      last.next = new Cup(label)
      last = last.next
      cups += (last.label -> last)
      label += 1
    }
    head
  }

}

object Day23 extends App {
  new Day23(9, 100, 1000000L, 10000000L).solvePuzzles("/2020/input-puzzle23.txt")
}
