package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day15Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day15-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day15().solvePart1(example) === 40)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day15().solvePart2(example) === 315)
  }

}
