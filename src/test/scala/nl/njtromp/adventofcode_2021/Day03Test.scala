package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day03Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day03-test.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day03().solvePart1(example) === 198)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day03().solvePart2(example) === 230)
  }


}
