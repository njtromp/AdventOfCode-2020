package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day03 extends Puzzle[Long] with SimpleMapTypes {

  @tailrec
  private def lastMatching(line: String, index: Int, direction: Int, matching: Char => Boolean): Int =
    if index > 0 && index < line.length - 1 && matching(line.charAt(index + direction)) then
      lastMatching(line, index + direction, direction, matching)
    else
      index

  private def findNumber(ls: String, p: Pos): (Pos, Int) =
    val first = lastMatching(ls, p._2, -1, _.isDigit)
    val last = lastMatching(ls, p._2, 1, _.isDigit)
    ((p._1, first), ls.substring(first, last + 1).toInt)

  override def exampleAnswerPart1: Long = 4361
  override def solvePart1(lines: List[String]): Long =
    val schematic = SimpleMap[Char](lines, l => l.toCharArray)
    val allPartPositions = schematic.allPositions().filter(p => !schematic(p).isDigit && schematic(p) != '.')
    val numberPositions = allPartPositions
      .flatMap(p => schematic.neighborPositions(p, all).filter(n => schematic(n).isDigit))
    val numbers = numberPositions.map(p => findNumber(lines(p._1), p))
    numbers.groupBy(_._1).map(_._2.head._2).sum

  override def exampleAnswerPart2: Long = 467835
  override def solvePart2(lines: List[String]): Long =
    val schematic = SimpleMap[Char](lines, l => l.toCharArray)
    val allMultiplyPositions = schematic.find('*')
    allMultiplyPositions.map(p => {
      val numberPositions = schematic.neighborPositions(p, all).filter(n => schematic(n).isDigit)
      val numbers = numberPositions.map(p => findNumber(lines(p._1), p)).groupBy(_._1).map(_._2.head._2)
      if numbers.size == 2 then
        numbers.product
      else
        0
    }).sum

}

object Day03 extends App {
  new Day03().solvePuzzles()
}
