package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle

class Day02 extends Puzzle {
  override def solvePart1(lines: List[String]): Long = {
    def score(elf: Char, me: Char): Int = {
      (elf, me) match {
        // Rock - Rock
        case ('A', 'X') => 1 + 3
        // Rock - Paper
        case ('A', 'Y') => 2 + 6
        // Rock - Scissors
        case ('A', 'Z') => 3 + 0
        // Paper - Rock
        case ('B', 'X') => 1 + 0
        // Paper - Paper
        case ('B', 'Y') => 2 + 3
        // Paper - Scissors
        case ('B', 'Z') => 3 + 6
        // Scissors - Rock
        case ('C', 'X') => 1 + 6
        // Scissors - Paper
        case ('C', 'Y') => 2 + 0
        // Scissors - Scissors
        case ('C', 'Z') => 3 + 3
      }
    }

    lines.map(l => score(l.charAt(0), l.charAt(2)))
      .sum
  }

  override def solvePart2(lines: List[String]): Long = {
    def score(elf: Char, me: Char): Int = {
      (elf, me) match {
        // Lose -> Rock - Scissors
        case ('A', 'X') => 3 + 0
        // Draw -> Rock - Rock
        case ('A', 'Y') => 1 + 3
        // Win -> Rock - Paper
        case ('A', 'Z') => 2 + 6
        // Lose -> Paper - Rock
        case ('B', 'X') => 1 + 0
        // Draw -> Paper - Paper
        case ('B', 'Y') => 2 + 3
        // Paper - Scissors
        case ('B', 'Z') => 3 + 6
        // Lose -> Scissors - Paper
        case ('C', 'X') => 2 + 0
        // Draw -> Scissors - Scissors
        case ('C', 'Y') => 3 + 3
        // Win -> Scissors - Rock
        case ('C', 'Z') => 1 + 6
      }
    }

    lines.map(l => score(l.charAt(0), l.charAt(2)))
      .sum
  }
}

object Day02 extends App{
  new Day02().solvePuzzles("/2022/day02.txt")
}
