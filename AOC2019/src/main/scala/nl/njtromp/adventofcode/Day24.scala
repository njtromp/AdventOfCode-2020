package nl.njtromp.adventofcode

import scala.collection.mutable

class Day24 extends Puzzle[Long] {
  private type Pos = (Int, Int)
  private val neighbors = List((0, 1), (0, -1), (1, 0), (-1, 0))
  private val all = (0 until 5).flatMap(y => (0 until 5).map((_, y))).toSet

  private def printMap(bugs: Set[Pos]): Unit =
    (0 until 5).foreach(y =>
      (0 until 5).foreach(x => print(if bugs.contains(x, y) then '#' else '.'))
      println
    )

  private def parseLife(lines: List[String]): Set[Pos] =
    lines
      .zipWithIndex
      .flatMap(l => l._1
        .zipWithIndex
        .filter(_._1 == '#')
        .map(p => (p._2, l._2))
      ).toSet

  private def nextGeneration(bugs: Set[Pos]): Set[Pos] =
    val survivors = bugs
      .filter(b => neighbors
        .map(n => (b._1 + n._1, b._2 + n._2))
        .count(bugs.contains) == 1
      )
    val babies = all
      .removedAll(bugs)
      .filter(b =>
        val ns = neighbors
          .map(n => (b._1 + n._1, b._2 + n._2))
          .count(bugs.contains)
        ns == 1 || ns == 2
      )
    survivors ++ babies

  private def biodiversityRating(bugs: Set[Pos]): Long =
    bugs.map(b => 1 << (b._2 * 5) + b._1).sum

  override def exampleAnswerPart1: Long = 2129920
  override def solvePart1(lines: List[String]): Long =
    val bugs = parseLife(lines)
    val states = mutable.Set.empty[Set[Pos]]
    var next = bugs
    while !states.contains(next) do
      states += next
      next = nextGeneration(next)
    biodiversityRating(next)

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day24 extends App {
  new Day24().solvePuzzles()
}
