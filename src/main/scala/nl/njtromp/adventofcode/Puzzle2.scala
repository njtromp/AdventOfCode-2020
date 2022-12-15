package nl.njtromp.adventofcode

import java.util.concurrent.TimeUnit
import scala.io.Source

trait Puzzle2 extends IntegerRangeUtils {

  def solvePuzzles(inputName: String): Unit = {
    val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream(inputName.replaceAll("\\.", "-example\\."))).getLines().toList
    val lines: List[String] = Source.fromInputStream(getClass.getResourceAsStream(inputName)).getLines().toList
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
  }

  def exampleAnswerPart1: Long

  def solvePart1(lines: List[String]): Long

  def exampleAnswerPart2: Long

  def solvePart2(lines: List[String]): Long

  def time[R](block: => R): R = {
    val start = System.nanoTime()
    val result = block    // call-by-name
    val finish = System.nanoTime()
    println(f"Elapsed time: ${TimeUnit.NANOSECONDS.toMillis(finish - start)}%,d ms")
    result
  }
}
