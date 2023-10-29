package nl.njtromp.adventofcode

import nl.njtromp.adventofcode.{SimpleMap, SimpleMapTypes}

class Day12 extends Puzzle[Long] with SimpleMapTypes {

  private def canReach(s: Char, d: Char): Boolean = d - s <= 1

  override def exampleAnswerPart1: Long = 31
  override def solvePart1(lines: List[String]): Long = {
    val map = SimpleMap[Char](lines, _.toCharArray)
    val start = map.find('S').head
    val finish = map.find('E').head
    map(start) = 'a'
    map(finish) = 'z'
    val path = Dijkstra.findRoute(map, canReach, p => -map(p), start, finish)
    path.size - 1 // We need the number of steps, so that is one less then the positions
  }

  def listStartingPoints(map: SimpleMap[Char]): List[Pos] = {
    map.allPositions()
      .filter(map(_) == 'a')
      .filter(_._2 == 0) // Shortcut after looking at the map. Needs to be on the first column.
      .filter(p => map.neighbors(p, square).count(_ == 'b') > 0) // Ensure one og its neighbors is a 'b'.
  }

  override def exampleAnswerPart2: Long = 29
  override def solvePart2(lines: List[String]): Long = {
    val map = SimpleMap[Char](lines, _.toCharArray)
    val start = map.find('S').head
    val finish = map.find('E').head
    map(start) = 'a'
    map(finish) = 'z'
    // Find the best alternative starting point
    val bestStartPath = listStartingPoints(map)
      .map(s => Dijkstra.findRoute(map, canReach, p => -map(p), s, finish))
      .filter(_.nonEmpty)
      .minBy(_.size)
    bestStartPath.size - 1
  }

}

object Day12 extends App{
  new Day12().solvePuzzles("/day12.txt")
}
