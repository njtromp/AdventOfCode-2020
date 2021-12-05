package nl.njtromp.adventofcode_2021

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day04 extends Puzzle {
  private type Board = Array[Array[Int]]
  private val BOARD_SIZE = 5

  private def createBoards(lines: List[Array[Int]]): List[Board] = {
    if (lines.isEmpty)
      List.empty
    else {
      lines.splitAt(BOARD_SIZE)._1.toArray :: createBoards(lines.splitAt(BOARD_SIZE)._2)
    }
  }

  private def isWinner(drown: List[Int], board: Board): Boolean = {
    def matchingRow(row: Array[Int]): Boolean = {
      row.count(drown.contains(_)) == BOARD_SIZE
    }
    def matchingColumns(): Int = {
      val columns: Seq[Array[Int]] = (0 until BOARD_SIZE).map(c => board.map(_(c)))
      columns.count(matchingRow)
    }
    board.count(matchingRow) > 0 || matchingColumns() > 0
  }

  @tailrec
  private def winningBoardPart1(drown: List[Int], numbers: Array[Int], boards: List[Board]): Long = {
    val winningBoards = boards.filter(isWinner(drown, _))
    if (winningBoards.length == 1) {
      val winningBoard: Board = winningBoards.head
      winningBoard.flatMap(_.toList).filter(n => !drown.contains(n)).sum * drown.head
    } else {
      winningBoardPart1(numbers.head :: drown, numbers.tail, boards)
    }
  }

  override def solvePart1(lines: List[String]): Long = {
    val numbers = lines.head.split(",").map(_.toInt)
    val boards = createBoards(lines.tail.filterNot(_.isEmpty).map(_.split(" ").filterNot(_.isEmpty).map(_.toInt)))
    winningBoardPart1(List.empty[Int], numbers, boards)
  }

  override def solvePart2(lines: List[String]): Long = {
    @tailrec
    def winningBoard(drown: List[Int], numbers: Array[Int], boards: List[Board]): Long = {
      val winningBoards = boards.filter(isWinner(drown, _))
      if (winningBoards.length == boards.length - 1) {
        winningBoardPart1(drown, numbers, boards.filterNot(winningBoards.contains(_)))
      } else {
        winningBoard(numbers.head :: drown, numbers.tail, boards)
      }
    }
    val numbers = lines.head.split(",").map(_.toInt)
    val boards = createBoards(lines.tail.filterNot(_.isEmpty).map(_.split(" ").filterNot(_.isEmpty).map(_.toInt)))
    winningBoard(List.empty[Int], numbers, boards)

  }
}

object Day04 extends App {
  new Day04().solvePuzzles("/2021/day04.txt")
}
