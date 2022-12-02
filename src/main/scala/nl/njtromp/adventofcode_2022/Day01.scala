package nl.njtromp.adventofcode_2022

import nl.njtromp.adventofcode.Puzzle

class Day01 extends Puzzle {
  override def solvePart1(lines: List[String]): Long =
    lines.map((line: String) => if (line.isBlank) 0 else line.toInt)
      .foldLeft((0, 0))((a, snack) => if (snack == 0) (Math.max(a._1, a._2), 0) else (a._1, a._2 + snack))._1

  override def solvePart2(lines: List[String]): Long =
    lines.map((line: String) => if (line.isBlank) 0 else line.toInt)
      .foldLeft((List[Int](), 0))((a, snack) =>
        if (snack == 0) (a._2 :: a._1, 0) else (a._1, a._2 + snack))
      ._1.sorted.reverse
      .take(3).sum
}

object Day01 extends App{
  new Day01().solvePuzzles("/2022/day01.txt")
}
