package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day19Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day19-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day19().solvePart1(example) === 79)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day19().solvePart2(example) === 3621)
  }

}
