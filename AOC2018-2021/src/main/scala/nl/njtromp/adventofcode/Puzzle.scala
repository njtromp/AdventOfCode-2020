package nl.njtromp.adventofcode

import java.util.concurrent.TimeUnit
import scala.io.Source

trait Puzzle {

  def solvePuzzles(inputName: String): Unit = {
    val lines: List[String] = Source.fromInputStream(getClass.getResourceAsStream(inputName)).getLines().toList
    println(s"Answer ${getClass.getSimpleName} part 1: ${time{solvePart1(lines)}}")
    println(s"Answer ${getClass.getSimpleName} part 2: ${time{solvePart2(lines)}}")
  }

  def solvePart1(lines: List[String]): Long

  def solvePart2(lines: List[String]): Long

  def time[R](block: => R): R = {
    val start = System.nanoTime()
    val result = block    // call-by-name
    val finish = System.nanoTime()
    println(f"Elapsed time: ${TimeUnit.NANOSECONDS.toMillis(finish - start)}%,d ms")
    result
  }
}
