package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.{Puzzle2, SimpleMap, SimpleMapTypes}

class Day12 extends Puzzle2 with SimpleMapTypes {

  private def find(map: SimpleMap[Char], c: Char): Pos = {
    val positions = (0 until map.height).flatMap(y =>
        (0 until map.width)
        .map(x => (y, x)))
    positions.filter(p => map(p) == c).head
  }

  private def findRoute(map: SimpleMap[Char], start: Pos, finish: Pos): List[Pos] = {
    var bestRoute: List[Pos] = List()
    var bestLength: Int = Int.MaxValue
    def canReach(s: Char, d: Char): Boolean = d - s == 0 || d - s == 1
    def dijkstra(current: Pos, route: List[Pos], visited: Set[Pos]): Unit =
      if (current == finish) {
        println("Found a route")
        if (route.length < bestLength) {
          println("Found a better route")
          bestLength = route.length
          bestRoute = route
        }
      } else if (route.length < bestLength) {
        val neighbors = map.neighborPositions(current, List(right, up, down, left))
          .filterNot(visited.contains)
          .filter(p => canReach(map.elems(current._1)(current._2), map.elems(p._1)(p._2)))
        neighbors.foreach(n => dijkstra(n, n :: route, visited + n))
      }
    dijkstra(start, List(start), Set(start))
    bestRoute
  }

  override def exampleAnswerPart1: Long = 31
  override def solvePart1(lines: List[String]): Long = {
    println("="*20)
    val map = SimpleMap[Char](lines, _.toCharArray)
    val start = find(map, 'S')
    val finish = find(map, 'E')
    map.set(start, 'a')
    map.set(finish, ('z' + 1).toChar)
    findRoute(map, start, finish).size - 1
  }

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long = {
    -1
  }

  private def hamilton(a: Pos, b: Pos): Int = Math.abs(a._1 - b._1) + Math.abs(a._2 - b._2)

  def printRoute(route: List[Pos], heigth: Int, width: Int): Unit =
    (0 until heigth).foreach(y => {
      (0 until width).foreach(x => print(if (route.contains((y, x))) '#' else '.'))
      println
    })
}

object Day12 extends App{
  new Day12().solvePuzzles("/2022/day12.txt")
}
