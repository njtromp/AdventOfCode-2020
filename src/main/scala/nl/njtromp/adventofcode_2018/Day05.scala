package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle2

class Day05 extends Puzzle2 {

  private def reduce(polymer: List[Char]): List[Char] = {
    def reduceMatching(polymer: List[Char]): List[Char] = {
      polymer match {
        case Nil => Nil
        case x :: Nil => List(x)
        case x :: y :: tail => if (x != y && x.toLower == y.toLower) reduce(tail) else x :: reduce(y :: tail)
      }
    }
    val reduced = reduceMatching(polymer)
    if (reduced == polymer) polymer else reduce(reduced)
  }

  override def exampleAnswerPart1: Long = 10
  override def solvePart1(lines: List[String]): Long = {
    reduce(lines.head.toList).size
  }

  override def exampleAnswerPart2: Long = 4
  override def solvePart2(lines: List[String]): Long = {
    val polymer = lines.head.toList
    ('a' to 'z').foldLeft(polymer.size)((smallest, c) => {
      Math.min(smallest, reduce(polymer.filterNot(_.toLower == c)).size)
    })
  }

}

object Day05 extends App {
  new Day05().solvePuzzles("/2018/day05.txt")
}
