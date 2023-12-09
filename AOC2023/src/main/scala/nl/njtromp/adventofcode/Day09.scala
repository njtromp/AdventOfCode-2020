package nl.njtromp.adventofcode

class Day09 extends Puzzle[Long] {

  private def predictNext(data: List[Long]): Long =
    if data.forall(_ == 0) then
      0
    else
      val next = data.tail.zip(data).map(d => d._1 - d._2)
      data.last + predictNext(next)

  private def predictPrevious(data: List[Long]): Long =
    if data.forall(_ == 0) then
      0
    else
      val previous = data.tail.zip(data).map(d => d._1 - d._2)
      data.head - predictPrevious(previous)

  override def exampleAnswerPart1: Long = 114
  override def solvePart1(lines: List[String]): Long =
    lines.map(_.split(' ').map(_.toLong).toList).map(predictNext).sum

  override def exampleAnswerPart2: Long = 2
  override def solvePart2(lines: List[String]): Long =
    lines.map(_.split(' ').map(_.toLong).toList).map(predictPrevious).sum

}

object Day09 extends App {
  new Day09().solvePuzzles()
}
