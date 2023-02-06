package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

import scala.annotation.tailrec

class Day09 extends Puzzle2 {
  private val SETTINGS = "(\\d+) players; last marble is worth (\\d+) points".r

  private class Marble(val value: Long) {
    var clockwise = this
    var counterClockwise = this
    @tailrec
    final def clockwise(n: Int): Marble =
      if (n == 0) this else clockwise.clockwise(n - 1)
    @tailrec
    final def counterClockwise(n: Int): Marble =
      if (n == 0) this else counterClockwise.counterClockwise(n - 1)
    def attach(other: Marble): Unit = {
      other.counterClockwise = this
      other.clockwise = clockwise
      clockwise = other
      other.clockwise.counterClockwise = other
    }
    def remove(): Unit = {
      clockwise.counterClockwise = counterClockwise
      counterClockwise.clockwise = clockwise
    }
    def printMarbles(): Unit = {
      val stop = this
      @tailrec
      def printMarble(m: Marble): Unit = {
        print(s"${m.value} ")
        if (m.clockwise == stop)
          println
        else {
          printMarble(m.clockwise)
        }
      }
      printMarble(this)
    }
  }

  private class Elf(id: Int) {
    var marbles = List.empty[Long]
    def score(): Long = marbles.sum
  }

  private def play(numberOfElves: Int, numberOfMarbles: Long): Array[Elf] = {
    @tailrec
    def play(elves: Array[Elf], activeElf: Int, marbleValue: Long, current: Marble): Array[Elf] = {
//      current.printMarbles()
      if (marbleValue == numberOfMarbles)
        elves
      else {
        if (marbleValue % 23 == 0) {
          val marble = current.counterClockwise(7)
          elves(activeElf).marbles = marbleValue :: marble.value :: elves(activeElf).marbles
          marble.remove()
          play(elves, (activeElf + 1) % numberOfElves, marbleValue + 1L, marble.clockwise)
        } else {
          val marble = current.clockwise(1)
          marble.attach(new Marble(marbleValue))
          play(elves, (activeElf + 1) % numberOfElves, marbleValue + 1L, marble.clockwise)
        }
      }
    }
    play((1 to numberOfElves).map(new Elf(_)).toArray, 0, 1L, new Marble(0L))
  }

  override def exampleAnswerPart1: Long = 32
  override def solvePart1(lines: List[String]): Long = {
    lines.head match {
      case SETTINGS(players, marbles) => play(players.toInt, marbles.toLong).maxBy(_.score()).score()
    }
  }

  override def exampleAnswerPart2: Long = 22563L
  override def solvePart2(lines: List[String]): Long = {
    lines.head match {
      case SETTINGS(players, marbles) => play(players.toInt, 100L * marbles.toLong).maxBy(_.score()).score()
    }
  }

}

object Day09 extends App {
  new Day09().solvePuzzles("/2018/day09.txt")
}
