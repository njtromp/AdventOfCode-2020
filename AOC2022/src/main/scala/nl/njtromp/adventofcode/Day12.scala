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
    val path = Dijkstra.findRoute(map, canReach, start, finish)
    map(start) = 'S'
    map(finish) = 'E'
//    Dijkstra.printPath(map, path, p => Math.abs(20 + (p - 'a') * 200 / ('z' -'a')))
    path.size - 1 // We need the number of steps, so that is one less then the positions
  }

  private def alternateStartingPoints(map: SimpleMap[Char]): List[Pos] = {
    map.allPositions()
      .filter(map(_) == 'a')
      .filter(_._2 == 0) // Shortcut after looking at the map. Needs to be on the first column.
      .filter(map.neighbors(_, square).count(_ == 'b') > 0) // Ensure one of its neighbors is a 'b'.
  }

  override def exampleAnswerPart2: Long = 29
  override def solvePart2(lines: List[String]): Long = {
    val map = SimpleMap[Char](lines, _.toCharArray)
    val start = map.find('S').head
    val finish = map.find('E').head
    map(start) = 'a'
    map(finish) = 'z'
    // Find the best alternative starting point
    val path = alternateStartingPoints(map)
      .map(Dijkstra.findRoute(map, canReach, _, finish))
      .minBy(_.size)
    map(start) = 'S'
    map(finish) = 'E'
//    Dijkstra.printPath(map, path, p => Math.abs(20 + (p - 'a') * 200 / ('z' - 'a')))
    path.size - 1
  }

}

object Day12 extends App{
  new Day12().solvePuzzles("/day12.txt")
}
