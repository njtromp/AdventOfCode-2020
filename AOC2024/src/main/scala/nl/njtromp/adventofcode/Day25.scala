package nl.njtromp.adventofcode

class Day25 extends Puzzle[Long] {

  private val LOCK = 'L'
  private val KEY = 'K'

  private def parse(lines: List[String]): (Char, List[Int]) =
    val lockOrKey = SimpleMap(lines)
    (
      if lines.head.count(_ == '#') == lines.head.length then LOCK else KEY,
      (0 until lockOrKey.width).map(c => lockOrKey.column(c).count(_ == '#') - 1).toList
    )

  override def exampleAnswerPart1: Long = 3
  override def solvePart1(lines: List[String]): Long =
    val lockAndKeys = groupByEmptyLine(lines).map(parse)
    val locks = lockAndKeys.filter(_._1 == LOCK).map(_._2)
    val keys = lockAndKeys.filter(_._1 == KEY).map(_._2)
    locks.map(l => keys.count(_.zip(l).map(_ + _).forall(_ <= 5))).sum

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day25 extends App {
  new Day25().solvePuzzles()
}
