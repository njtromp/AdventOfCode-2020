package nl.njtromp.adventofcode

import java.util.concurrent.TimeUnit
import scala.collection.generic.IsIterable
import scala.collection.mutable.ListBuffer
import scala.io.Source

trait Puzzle[T] {
  def exampleAnswerPart1: T
  def solvePart1(lines: List[String]): T

  def exampleAnswerPart2: T
  def solvePart2(lines: List[String]): T

  def solvePuzzles(): Unit =
    solvePuzzles(true)

  def solvePuzzles(trim: Boolean): Unit =
    solvePuzzles(s"/${getClass.getSimpleName.toLowerCase}.txt", trim)

  def solvePuzzles(inputName: String): Unit =
    solvePuzzles(inputName: String, true)

  def solvePuzzles(inputName: String, trim: Boolean): Unit =
    val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream(inputName.replaceAll("\\.", "-example\\.")))
      .getLines()
      .toList
      .map(l => if trim then l.trim else l)
    val lines: List[String] = Source.fromInputStream(getClass.getResourceAsStream(inputName))
      .getLines()
      .toList
      .map(l => if trim then l.trim else l)
    solvePuzzles(example, lines)

  private def solvePuzzles(example: List[String], lines: List[String]): Unit =
    val answer1 = solvePart1(example)
    if (answer1 == exampleAnswerPart1)
      println(s"Answer ${getClass.getSimpleName} part 1: ${time{solvePart1(lines)}}")
    else
      Console.err.println(s"Part 1 failed, expecting $exampleAnswerPart1 but got $answer1")

    val answer2 = solvePart2(example)
    if (answer2 == exampleAnswerPart2)
      println(s"Answer ${getClass.getSimpleName} part 2: ${time{solvePart2(lines)}}")
    else
      Console.err.println(s"Part 2 failed, expecting $exampleAnswerPart2 but got $answer2")

  def gcd(a: Long, b: Long): Long = if a % b == 0 then b else gcd(b, a % b)

  def groupByEmptyLine(lines: List[String]): List[List[String]] =
    if (lines.isEmpty)
      Nil
    else
      lines.takeWhile(_.nonEmpty) :: groupByEmptyLine(lines.dropWhile(_.nonEmpty).dropWhile(_.isEmpty))

  extension[Repr] (repr: Repr)(using iterable: IsIterable[Repr])
    def takeUntil(p: iterable.A => Boolean): List[iterable.A] =
      val iter = iterable(repr).iterator
      val b = new ListBuffer[iterable.A]
      var continue = true
      while (iter.hasNext && continue) {
        val value = iter.next
        b += value
        continue = !p(value)
      }
      b.toList

  def time[R](block: => R): R =
    val start = System.nanoTime()
    val result = block    // call-by-name
    val finish = System.nanoTime()
    println(f"Elapsed time: ${TimeUnit.NANOSECONDS.toMillis(finish - start)}%,d ms")
    result
}
