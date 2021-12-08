package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day08Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day08-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day08().solvePart1(example) === 26)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day08().solvePart2(example) === 61229)
  }


}
