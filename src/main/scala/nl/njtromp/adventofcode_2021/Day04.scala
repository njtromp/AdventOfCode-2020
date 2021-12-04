package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode_2020.Puzzle

import scala.annotation.tailrec

class Day04 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    type Board = Array[Array[Int]]
    def createBoards(lines: List[Array[Int]]): List[Board] = {
      if (lines.isEmpty)
        List.empty
      else {
        lines.splitAt(5)._1.toArray :: createBoards(lines.splitAt(5)._2)
      }
    }
    @tailrec
    def winningBoard(drown: List[Int], numbers: Array[Int], boards: List[Board]): Long = {
      def isWinner(board: Board): Boolean = {
        def matchingRow(row: Array[Int]): Boolean = {
          row.count(drown.contains(_)) == 5
        }
        def matchingColumns(): Int = {
          val columns: Seq[Array[Int]] = (0 to 4).map(c => board.map(_(c)))
          columns.count(matchingRow)
        }
        board.count(matchingRow) > 0 || matchingColumns() > 0
      }
      val winningBoards = boards.filter(isWinner)
      if (winningBoards.length == 1) {
        val winningBoard: Board = winningBoards.head
        winningBoard.flatMap(_.toList).filter(n => !drown.contains(n)).sum * drown.head
      } else {
        winningBoard(numbers.head :: drown, numbers.tail, boards)
      }
    }
    val numbers = lines.head.split(",").map(_.toInt)
    val validLines = lines.tail.filterNot(_.isEmpty)
    val boards = createBoards(validLines.map(_.split(" ").filterNot(_.isEmpty).map(_.toInt)))
    winningBoard(List.empty[Int], numbers, boards)
  }

  override def solvePart2(lines: List[String]): Long = ???
}

object Day04 extends App {
  new Day04().solvePuzzles("/2021/day04.txt")
}
