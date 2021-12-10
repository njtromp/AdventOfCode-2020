package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day10Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day10-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day10().solvePart1(example) === 26397)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day10().solvePart2(example) === 288957)
  }

}
