package nl.njtromp.adventofcode

import scala.annotation.tailrec

class Day16 extends Puzzle[Long] {
  private def fft(repeats: Int, digits: List[Int], basePattern: Array[Int]): List[Int] =
    def createPattern(multiplier: Int): Array[Int] =
      basePattern.flatMap(Array.fill(multiplier)(_))
    def phase(digits: List[Int]): List[Int] =
      (1 to digits.length).toList.map(n =>
        val pattern = createPattern(n)
        var i = 1
        val result = digits.foldLeft(0)((a, d) =>
          val f = pattern(i)
          i = (i + 1) % pattern.length
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
      fft(100, l.toList.map(_.asDigit), Array(0, 1, 0, -1))
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
