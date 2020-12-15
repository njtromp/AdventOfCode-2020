package nl.njtromp.adventofcode_2020

class Day15 extends Puzzle {

  def solvePart1(lines: List[String]): Long = {
    solve(lines, 2020L)
  }

  def solvePart2(lines: List[String]): Long = {
    solve(lines, 30000000L)
  }

  private def solve(lines: List[String], maxTurns: Long): Long = {
    val numbers = lines.head.split(",").map(_.toLong)
    var spokenInTurn: Map[Long, (Long, Long)] = numbers.zipWithIndex.map({case (n, i) => (n, (i + 1L, 0L))}).toMap
    var turn: Long = numbers.length
    var lastSpoken: Long = numbers.reverse.head
    while (turn < maxTurns) {
      turn += 1L
      spokenInTurn.get(lastSpoken) match {
        case Some((t, 0L)) => lastSpoken = 0L
        case Some((t, p)) => lastSpoken = t - p
        case None => lastSpoken = 0L
      }
      spokenInTurn.get(lastSpoken) match {
        case Some((t, _)) => spokenInTurn += (lastSpoken -> (turn, t))
        case None => spokenInTurn += (lastSpoken -> (turn, 0L))
      }
    }
    lastSpoken
  }

}

object Day15 extends App {
  new Day15().solvePuzzles("/input-puzzle15.txt")
}


