package nl.njtromp.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

class Day16 extends Puzzle[Long] {
  private val basePattern = Array(0, 1, 0, -1)
  private def createPattern(multiplier: Int): Array[Int] =
    basePattern.flatMap(Array.fill(multiplier)(_))
  private val patterns = mutable.Map.empty[Int, Array[Int]].withDefault(createPattern)

  private def fft(repeats: Int, digits: List[Int]): List[Int] =
    def phase(digits: List[Int]): List[Int] =
      (1 to digits.length).par.map(n =>
        val pattern = patterns(n)
        var i = 1
        val result = digits.foldLeft(0)((a, d) =>
          val f = pattern(i)
          i = (i + 1) % pattern.length
          a + d * f
        )
        Math.abs(result) % 10
      ).toList
//    val offsets = mutable.Map.empty[Long, Long].withDefaultValue(0L)
    @tailrec
    def fft(repeats: Int, digits: List[Int]): List[Int] =
//      offsets(asNumber(digits.take(7))) += 1L
      if repeats == 0 then
        println
        digits
      else
        print('.')
        fft(repeats - 1, phase(digits))
    fft(repeats, digits)
//    val result = fft(repeats, digits)
//    offsets.filter(_._2 > 1).foreach(println)
//    result
    
  private def asNumber(digits: List[Int]): Long =
    digits.foldLeft(0L)((a, d) => a * 10L + d)  

  override def exampleAnswerPart1: Long = 24176176L + 73745418L + 52432133L
  override def solvePart1(lines: List[String]): Long =
//    (if lines.length == 1 then lines else lines.take(3)).map(l =>
//      asNumber(fft(100, l.toList.map(_.asDigit)).take(8))
//    ).sum
    -1

  override def exampleAnswerPart2: Long = 0 //84462026L + 78725270L + 53553731L
  override def solvePart2(lines: List[String]): Long =
    if lines.length != 1 then return 0
    (if lines.length == 1 then lines else lines.drop(3)).map(l =>
      val input = l.toList.map(_.asDigit)
      val offset = l.take(7).toInt
      asNumber(fft(100, (0 until 1000).toList.flatMap(_ => input)).slice(offset, offset + 8))
    ).sum

}

object Day16 extends App {
  new Day16().solvePuzzles()
}
