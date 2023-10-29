package nl.njtromp.adventofcode

import nl.njtromp.adventofcode.{SimpleMap, SimpleMapTypes}

class Day12 extends Puzzle[Long] with SimpleMapTypes {

  private def canReach(s: Char, d: Char): Boolean = d - s <= 1

  def listStartingPoints(map: SimpleMap[Char]): List[Pos] = {
    map.allPositions()
      .filter(map(_) == 'a')
      .filter(p => map.neighbors(p, square).count(canReach(map(p), _)) > 0)
  }

  override def exampleAnswerPart1: Long = 31
  override def solvePart1(lines: List[String]): Long = {
    val map = SimpleMap[Char](lines, _.toCharArray)
    val start = map.find('S').head
    val finish = map.find('E').head
    map(start) = 'a'
    map(finish) = ('z' + 1).toChar
    val path = Dijkstra.findRoute(map, canReach, p => -map(p), start, finish)
    Dijkstra.printPath(map, path)
    println("Solution for day 1 should be 462!!!!")
    path.size - 1 // We need the number of steps, so that is one less then the positions
  }

  override def exampleAnswerPart2: Long = 29
  override def solvePart2(lines: List[String]): Long = {
//    val map = SimpleMap[Char](lines, _.toCharArray)
//    val start = map.find('S').head
//    val finish = map.find('E').head
//    map(start) = 'a'
//    map(finish) = ('z' + 1).toChar
//
//    val startingPoints = listStartingPoints(map)
//    startingPoints.map(s => Dijkstra.findRoute(map, canReach, p => -map(p), s, finish)).min
    -1
  }

}

object Day12 extends App{
  new Day12().solvePuzzles("/day12.txt")
}
