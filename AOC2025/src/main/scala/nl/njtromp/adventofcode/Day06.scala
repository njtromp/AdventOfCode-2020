package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day06 extends Puzzle[Long] {
  private val MUL = '*'
  private val ADD = '+'

  private def solveColumn(op: Char, numbers: List[Long]): Long =
    op match {
      case MUL => numbers.product
      case ADD => numbers.sum
    }

  private def grandTotal(lines: List[String], transform: List[String] => List[Long]): Long =
    @tailrec
    def solve(index: Int, total: Long): Long =
      if index >= lines.head.length then
        total
      else
        val op = lines.head.charAt(index)
        val columnWidth = lines.head.substring(index + 1).takeWhile(_ == ' ').length
        val values = lines.tail.map(l => l.substring(index, if columnWidth == 0 then l.length else index + columnWidth))
        solve(index + columnWidth + 1, total + solveColumn(op, transform(values)))
    solve(0, 0L)

  override def exampleAnswerPart1: Long = 4277556
  override def solvePart1(lines: List[String]): Long =
    def transform(values: List[String]): List[Long] =
      values.map(_.trim.toLong)
    grandTotal(lines.reverse, transform)

  override def exampleAnswerPart2: Long = 3263827
  override def solvePart2(lines: List[String]): Long =
    def transform(values: List[String]): List[Long] =
      def extractColumn(digits: List[Char]): Long =
        digits.mkString.trim.toLong
      if values.head.isEmpty then
        Nil
      else
        extractColumn(values.map(v => if v.isEmpty then ' ' else v.charAt(0)).reverse) :: transform(values.map(v => if v.isEmpty then " " else v.substring(1)))
    grandTotal(lines.reverse, transform)

}

object Day06 extends App {
  new Day06().solvePuzzles(false)
}
