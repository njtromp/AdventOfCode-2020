package nl.njtromp.adventofcode

class Day02 extends Puzzle[Long] {

  private def levelDiffs(levels: List[Long]) =
    levels.zip(levels.tail).map((f, l) => f - l)

  private def isIncreasing(diffs: List[Long]): Boolean =
    diffs.forall(d => d > 0 && d <= 3)

  private def isDecreasing(diffs: List[Long]): Boolean =
    diffs.forall(d => d < 0 && d >= -3)

  private def isSafe(levels: List[Long]): Boolean =
    val diffs = levelDiffs(levels)
    isIncreasing(diffs) || isDecreasing(diffs)

  private def isSafeWithLevelDampener(levels: List[Long]): Boolean =
    def levelDampener(levels: List[Long]): List[List[Long]] =
      List(levels) ++ levels.indices.map(i => levels.take(i) ++ levels.drop(i + 1))
    levelDampener(levels).map(levelDiffs).exists(ds => isIncreasing(ds) || isDecreasing(ds))

  override def exampleAnswerPart1: Long = 2
  override def solvePart1(lines: List[String]): Long =
    val levels = lines.map(_.split(" +").map(_.toLong).toList)
    levels.count(isSafe)

  override def exampleAnswerPart2: Long = 4
  override def solvePart2(lines: List[String]): Long =
    val levels = lines.map(_.split(" +").map(_.toLong).toList)
    levels.count(isSafeWithLevelDampener)

}

object Day02 extends App {
  new Day02().solvePuzzles()
}
