package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

class Day15 extends Puzzle {

  type Pos = (Int, Int) // (X, Y)
  private def x(p: Pos) = p._1
  private def y(p: Pos) = p._2

  override def solvePart1(lines: List[String]): Long = {
    val riskMap = lines.map(_.toArray.map(_.asDigit)).toArray

    def findPath(bestRiskLevel: Long, bestPath: List[Pos], riskLevel: Long, path: List[Pos]): (Long, List[Pos]) = {
      val currentPos = path.head
      if (x(currentPos) == riskMap.head.length - 1 && y(currentPos) == riskMap.length - 1) {
        // Found the exit
        if (riskLevel < bestRiskLevel) (riskLevel, path) else (bestRiskLevel, bestPath)
      } else {
        // try right and down
        val pos = path.head
        List((1, 0), (0, 1)).foldLeft((bestRiskLevel, bestPath))((acc, d) => {
          val newPos = (x(pos) + x(d), y(pos) + y(d))
          if (x(newPos) < riskMap.head.length && y(newPos) < riskMap.length && riskLevel <= acc._1)
            findPath(acc._1, acc._2, riskLevel + riskMap(y(newPos))(x(newPos)), newPos :: path)
          else
            acc
        })
      }
    }

    val maxRiskLevel = 9L * riskMap.length * riskMap.head.length
    val (lowestRiskLevel, bestPath) = findPath(maxRiskLevel, List.empty[(Int, Int)], 0L, List((0, 0)))
    lowestRiskLevel
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day15 extends App {
  new Day15().solvePuzzles("/2021/day15.txt")
}
