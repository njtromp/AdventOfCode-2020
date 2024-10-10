package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day09 extends Puzzle {

  def isOutsideMap(x: Int, y: Int, map: Array[Array[Int]]): Boolean = {
    x < 0 || x >= map.head.length || y < 0 || y >= map.length
  }

  def isLowPoint(x: Int, y: Int, map: Array[Array[Int]]): Boolean = {
    val neighbors = List((-1, 0), (1, 0), (0, -1), (0, 1))
    neighbors.count(d => isOutsideMap(x + d._1, y + d._2, map) || map(y)(x) < map(y + d._2)(x + d._1)) == 4
  }

  override def solvePart1(lines: List[String]): Long = {
    val map = lines.toArray.map(_.toArray.map(_.asDigit))
    map.indices.flatMap(y => map.head.indices.map(x => if (isLowPoint(x, y, map)) map(y)(x) + 1 else 0)).sum
  }

  type Pos = (Int, Int)
  override def solvePart2(lines: List[String]): Long = {
    val map = lines.toArray.map(_.toArray.map(_.asDigit))
    val lowPoints = map.indices.flatMap(y => map.head.indices.filter(x => isLowPoint(x, y, map)).map((_, y)))
    def basinSize(p: Pos): Long = {
      @tailrec
      def basinSize(knowns: Set[Pos], unknowns: Set[Pos]): Long = {
        if (unknowns.isEmpty)
          knowns.size
        else {
          val p = unknowns.head
          if (map(p._2)(p._1) == 9) {
            basinSize(knowns, unknowns.tail)
          } else {
            def newNeighbors(p: Pos): Set[Pos] = {
              List((-1, 0), (1, 0), (0, -1), (0, 1))
                .map(d => (p._1 + d._1, p._2 + d._2))
                .filterNot(p => isOutsideMap(p._1, p._2, map))
                .filterNot(knowns.contains)
                .toSet
            }
            basinSize(knowns + p, unknowns.tail ++ newNeighbors(p))
          }
        }
      }
      basinSize(Set.empty[Pos], Set(p))
    }
    lowPoints.map(basinSize).sorted.reverse.take(3).product
  }
}

object Day09 extends App {
  new Day09().solvePuzzles("/2021/day09.txt")
}
