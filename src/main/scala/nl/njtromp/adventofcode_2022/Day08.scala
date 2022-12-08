package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.{Puzzle2, SimpleMap, SimpleMapTypes}

class Day08 extends Puzzle2 with SimpleMapTypes {

  override def exampleAnswerPart1: Long = 21

  private def isVisible(treeMap: SimpleMap[Int], p: Pos): Boolean = {
    treeMap.allNeighbors(p, List(left)).forall(_ < treeMap(p)) ||
    treeMap.allNeighbors(p, List(right)).forall(_ < treeMap(p)) ||
    treeMap.allNeighbors(p, List(up)).forall(_ < treeMap(p)) ||
    treeMap.allNeighbors(p, List(down)).forall(_ < treeMap(p))
  }

  override def solvePart1(lines: List[String]): Long = {
    val treeMap = SimpleMap(lines, _.map(_.asDigit).toArray)
    (0 until treeMap.height).flatMap(y => (0 until treeMap.width).map(x => (y, x)))
      .count(isVisible(treeMap, _))
  }

  override def exampleAnswerPart2: Long = 8

  def lineOfSite(trees: List[Int], height: Int): Int = {
    val site = trees.span(_ < height)
    site._1.length + (if (site._2.isEmpty) 0 else 1)
  }

  private def scenicScore(treeMap: SimpleMap[Int], p: Pos): Long = {
    lineOfSite(treeMap.allNeighbors(p, List(left)), treeMap(p)) *
    lineOfSite(treeMap.allNeighbors(p, List(right)), treeMap(p)) *
    lineOfSite(treeMap.allNeighbors(p, List(up)), treeMap(p)) *
    lineOfSite(treeMap.allNeighbors(p, List(down)), treeMap(p))
  }

  override def solvePart2(lines: List[String]): Long = {
    val treeMap = SimpleMap(lines, _.map(_.asDigit).toArray)
    (0 until treeMap.height).flatMap(y => (0 until treeMap.width).map(x => (y, x)))
      .map(scenicScore(treeMap, _)).max
  }

}

object Day08 extends App {
  new Day08().solvePuzzles("/2022/day08.txt")
}
