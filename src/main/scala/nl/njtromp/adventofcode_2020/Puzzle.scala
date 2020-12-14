package nl.njtromp.adventofcode_2020

import scala.io.Source

trait Puzzle {

  def solvePuzzles(inputName: String): Unit = {
    val lines: List[String] = Source.fromInputStream(getClass.getResourceAsStream(inputName)).getLines().toList
    println(s"Answer part 1: ${solvePart1(lines)}")
    println(s"Answer part 2: ${solvePart2(lines)}")
  }

  def solvePart1(lines: List[String]): Long;
  def solvePart2(lines: List[String]): Long;
}
