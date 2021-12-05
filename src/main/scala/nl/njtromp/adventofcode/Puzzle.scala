package nl.njtromp.adventofcode

import scala.io.Source

trait Puzzle {

  def solvePuzzles(inputName: String): Unit = {
    val lines: List[String] = Source.fromInputStream(getClass.getResourceAsStream(inputName)).getLines().toList
    println(s"Answer ${getClass.getSimpleName} part 1: ${solvePart1(lines)}")
    println(s"Answer ${getClass.getSimpleName} part 2: ${solvePart2(lines)}")
  }

  def solvePart1(lines: List[String]): Long;

  def solvePart2(lines: List[String]): Long;
}
