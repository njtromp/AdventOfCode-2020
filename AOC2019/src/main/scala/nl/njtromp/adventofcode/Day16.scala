package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

class Day16 extends Puzzle[Long] {
  private def fft(repeats: Int, digits: List[Int], basePattern: Array[Int]): List[Int] =
    def createPattern(multiplier: Int): Array[Int] =
      basePattern.flatMap(Array.fill(multiplier)(_))
    def phase(digits: List[Int]): List[Int] =
      (1 to digits.length).par.map(n =>
        val pattern = createPattern(n)
        var i = 1
        val result = digits.foldLeft(0)((a, d) =>
          val f = pattern(i)
          i = (i + 1) % pattern.length
          a + d * f
        )
        Math.abs(result) % 10
      ).toList
    @tailrec
    def fft(repeats: Int, digits: List[Int]): List[Int] =
      if repeats == 0 then
        println
        digits
      else
        print('.')
        fft(repeats - 1, phase(digits))

    fft(repeats, digits)
    
  private def asNumber(digits: List[Int]): Long =
    digits.foldLeft(0L)((a, d) => a * 10L + d)  

  override def exampleAnswerPart1: Long = 24176176L + 73745418L + 52432133L
  override def solvePart1(lines: List[String]): Long =
    (if lines.length == 1 then lines else lines.take(3)).map(l =>
      asNumber(fft(100, l.toList.map(_.asDigit), Array(0, 1, 0, -1)).take(8))
    ).sum

  override def exampleAnswerPart2: Long = 84462026L + 78725270L + 53553731L
  override def solvePart2(lines: List[String]): Long =
    println("Part 2 is not funny :-(")
    (if lines.length == 1 then lines else lines.drop(3)).map(l =>
      val input = l.toList.map(_.asDigit)
      val offset = l.take(7).toInt
      asNumber(fft(100, (0 until 10000).toList.flatMap(_ => input), Array(0, 1, 0, -1)).slice(offset, offset + 8))
    ).sum

}

object Day16 extends App {
  new Day16().solvePuzzles()
}
