package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.{Puzzle2, SimpleMap, SimpleMapTypes}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Day12 extends Puzzle2 with SimpleMapTypes {

  private def find(map: SimpleMap[Char], c: Char): Pos = {
    val positions = (0 until map.height).flatMap(y =>
        (0 until map.width)
        .map(x => (y, x)))
    positions.filter(p => map(p) == c).head
  }

  private def findRoute(map: SimpleMap[Char], start: Pos, finish: Pos): Int = {
    var bestLength: Int = Int.MaxValue
    val numberOfSteps = Array.fill(map.height, map.width)(Int.MaxValue)
    val source = mutable.Map[Pos, Pos]()
    val visited = mutable.Set[Pos]()
    def priority(p: Pos): Int = -map.elems(p._1)(p._2)
    var toBeVisited = ArrayBuffer[Pos]()
    def canReach(s: Char, d: Char): Boolean = d - s <= 1
    @tailrec
    def dijkstra(): Unit =
      if (toBeVisited.nonEmpty) {
        val current = toBeVisited.head
        toBeVisited = toBeVisited.tail
        visited += current
        val length = numberOfSteps(current._1)(current._2)
        if (current == finish) {
          if (length < bestLength) {
            bestLength = length
          }
        } else {
          val neighbors = map.neighborPositions(current, square)
            .filter(p => canReach(map(current), map(p)))
          neighbors.foreach(n => {
            if (length + 1 < numberOfSteps(n._1)(n._2)) {
              numberOfSteps(n._1)(n._2) = length + 1
              source += n -> current
            }
            if (!visited.contains(n) && !toBeVisited.contains(n)) {
              toBeVisited += n
            }
          })
          toBeVisited = toBeVisited.sortBy(priority)
          dijkstra()
        }
      }
    numberOfSteps(start._1)(start._2) = 0
    toBeVisited += start
    dijkstra()
    numberOfSteps(finish._1)(finish._2)
  }

  override def exampleAnswerPart1: Long = 31

  def listStartingPoints(map: SimpleMap[Char]): List[Pos] = {
    (0 until map.height).flatMap(y => {
      (0 until map.width).map(x => (y, x))
    }).toList
      .filter(map(_) == 'a')
      .filter(p => map.neighbors(p, square).count(l => l - map(p) <= 1) > 0)
  }

  override def solvePart1(lines: List[String]): Long = {
    val map = SimpleMap[Char](lines, _.toCharArray)
    val start = find(map, 'S')
    val finish = find(map, 'E')
    map.set(start, 'a')
    map.set(finish, ('z' + 1).toChar)
    findRoute(map, start, finish)
  }

  override def exampleAnswerPart2: Long = 29
  override def solvePart2(lines: List[String]): Long = {
    val map = SimpleMap[Char](lines, _.toCharArray)
    val start = find(map, 'S')
    val finish = find(map, 'E')
    map.set(start, 'a')
    map.set(finish, ('z' + 1).toChar)

    val startingPoints = listStartingPoints(map)
    startingPoints.foldLeft(Int.MaxValue)((a, s) => Math.min(a, findRoute(map, s, finish)))
  }

  def printRoute(route: List[Pos], map: SimpleMap[Char]): Unit =
    (0 until map.height).foreach(y => {
      (0 until map.width).foreach(x => print(if (route.contains((y, x))) map((y, x)).toUpper else map((y, x))))
      println
    })
}

object Day12 extends App{
  new Day12().solvePuzzles("/2022/day12.txt")
}
