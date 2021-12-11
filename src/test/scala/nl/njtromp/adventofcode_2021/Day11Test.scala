package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day11Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day11-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day11().solvePart1(example) === 1656)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day11().solvePart2(example) === 195)
  }

}
