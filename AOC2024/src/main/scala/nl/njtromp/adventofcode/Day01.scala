package nl.njtromp.adventofcode

class Day01 extends Puzzle[Long] {

  override def exampleAnswerPart1: Long = 11
  override def solvePart1(lines: List[String]): Long =
    val pairs = lines.map(_.split(" {3}"))
    val first = pairs.map(_.head.toLong).sorted
    val last = pairs.map(_.last.toLong).sorted
    first.zip(last).map((f, l) => Math.abs(f - l)).sum

  override def exampleAnswerPart2: Long = 31
  override def solvePart2(lines: List[String]): Long =
    val pairs = lines.map(_.split(" {3}"))
    val first = pairs.map(_.head.toLong).sorted
    val last: Map[Long, List[Long]] = pairs.map(_.last.toLong).groupBy(v => v).withDefaultValue(List.empty[Long])
    first.map(f => f * last(f).size).sum

}

object Day01 extends App {
  new Day01().solvePuzzles()
}
