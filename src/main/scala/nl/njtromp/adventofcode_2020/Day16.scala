package nl.njtromp.adventofcode_2020

class Day16 extends Puzzle {
  private val rule = "(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r
  private val yourTicket = "your ticket:".r
  private val nearbyTickets = "nearby tickets:".r

  def solvePart1(lines: List[String]): Long = {

  }

  def solvePart2(lines: List[String]): Long = {
    -1
  }

}

object Day16 extends App {
  new Day16().solvePuzzles("/input-puzzle16.txt")
}
