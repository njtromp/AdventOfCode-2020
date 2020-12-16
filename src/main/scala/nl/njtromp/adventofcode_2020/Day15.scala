package nl.njtromp.adventofcode_2020

import scala.collection.mutable.HashMap

class Day15 extends Puzzle {

  def solvePart1(lines: List[String]): Long = {
    solve(lines, 2020L)
  }

  def solvePart2(lines: List[String]): Long = {
    solve(lines, 30000000L)
  }

  private def solve(lines: List[String], maxTurns: Long): Long = {
    val numbers = lines.head.split(",").map(_.toLong)
    var spokenInTurn: HashMap[Long, (Long, Long)] = HashMap.empty
    spokenInTurn = spokenInTurn ++ numbers.zipWithIndex.map({case (n, i) => (n, (i + 1L, 0L))}).toMap
    var turn: Long = numbers.length
    var lastSpoken: Long = numbers.reverse.head
    while (turn < maxTurns) {
      turn += 1L
      lastSpoken = spokenInTurn.get(lastSpoken) match {
        case Some((_, 0L)) => 0L
        case Some((t, p)) => t - p
        case None => 0L
      }
      spokenInTurn += (spokenInTurn.get(lastSpoken) match {
        case Some((t, _)) => (lastSpoken -> (turn, t))
        case None => (lastSpoken -> (turn, 0L))
      })
    }
    lastSpoken
  }

}

object Day15 extends App {
  new Day15().solvePuzzles("/input-puzzle15.txt")
}
