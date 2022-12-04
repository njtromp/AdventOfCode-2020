package nl.njtromp.adventofcode_2022

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day05Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2022/day05-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day05().solvePart1(example) === 2)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day05().solvePart2(example) === 4)
  }

}
