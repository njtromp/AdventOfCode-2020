package nl.njtromp.adventofcode_2018

import nl.njtromp.adventofcode.Puzzle

import scala.annotation.tailrec

class Day01 extends Puzzle {

  override def solvePart1(lines: List[String]): Long = {
    lines.filterNot(_.isBlank).map(_.toLong).sum
  }


  override def solvePart2(lines: List[String]): Long = {
    val pattern = lines.filterNot(_.isBlank).map(_.toLong)
    @tailrec
    def findDuplicateFreq(freq :Long, freqs: List[Long], changes: List[Long]): Long = {
      if (freqs.contains(freq))
        freq
      else if (changes.isEmpty)
        findDuplicateFreq(freq, freqs, pattern)
      else
        findDuplicateFreq(freq + changes.head, freq :: freqs, changes.tail)
    }
    findDuplicateFreq(0, List.empty[Long], pattern)
  }
}

object Day01 extends App {
  new Day01().solvePuzzles("/2018/day01.txt")
}
