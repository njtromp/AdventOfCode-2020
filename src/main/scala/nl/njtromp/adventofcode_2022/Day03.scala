package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle

class Day03 extends Puzzle {
  override def solvePart1(lines: List[String]): Long = {
    lines
      .map(l => l.trim)
      .map(l => (l.substring(0, l.length / 2).toSet, l.substring(l.length / 2).toSet))
      .map(c => c._1.intersect(c._2))
      .map(_.toList.head)
      .map(c => if (c.isLower) c - 'a' + 1 else c - 'A' + 27)
      .sum
  }

  override def solvePart2(lines: List[String]): Long = {
    def makeGroups(lines: List[String]): List[List[String]] = {
      lines match {
        case Nil => List()
        case _ => lines.take(3) :: makeGroups(lines.drop(3))
      }
    }
    val allChars = ('a' to 'z').toSet.union(('A' to  'Z').toSet)
    makeGroups(lines)
      .map(g => g.map(_.toSet).foldLeft(allChars)((a, r) => a.intersect(r)))
      .map(_.toList.head)
      .map(c => if (c.isLower) c - 'a' + 1 else c - 'A' + 27)
      .sum
  }
}

object Day03 extends App{
  new Day03().solvePuzzles("/2022/day03.txt")
}
