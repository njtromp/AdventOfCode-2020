package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

class Day16 extends Puzzle[Long] {
  private def fft(repeats: Int, digits: List[Int], basePattern: List[Int]): List[Int] =
    def createPattern(multiplier: Int): mutable.Queue[Int] =
      val pattern = mutable.Queue.from(basePattern.flatMap(List.fill(multiplier)(_)))
      // Rotate once
      pattern.enqueue(pattern.dequeue())
      pattern
    def phase(digits: List[Int]): List[Int] =
      (1 to digits.length).toList.map(n =>
        val pattern = createPattern(n)
        val result = digits.foldLeft(0)((a, d) =>
          val f = pattern.dequeue()
          pattern.enqueue(f)
          a + d * f
        )
        Math.abs(result) % 10
      )
    @tailrec
    def fft(repeats: Int, digits: List[Int]): List[Int] =
      if repeats == 0 then
        digits
      else
        fft(repeats - 1, phase(digits))

    fft(repeats, digits)

  override def exampleAnswerPart1: Long = 24176176L + 73745418L + 52432133L
  override def solvePart1(lines: List[String]): Long =
    (if lines.length == 1 then lines else lines.take(3)).map(l => {
      fft(100, l.toList.map(_.asDigit), List(0, 1, 0, -1))
        .take(8)
        .foldLeft(0L)((a, d) => a * 10L + d)
    }
    ).sum

  override def exampleAnswerPart2: Long = 0
  override def solvePart2(lines: List[String]): Long =
    -1

}

object Day16 extends App {
  new Day16().solvePuzzles()
}
