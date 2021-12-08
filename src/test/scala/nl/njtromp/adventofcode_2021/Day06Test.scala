package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day06Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day06-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day06().solvePart1(example) === 5934)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day06().solvePart2(example) === 26984457539L)
  }

}
