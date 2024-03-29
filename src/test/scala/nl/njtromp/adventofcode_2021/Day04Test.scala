package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day04Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day04-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day04().solvePart1(example) === 4512)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day04().solvePart2(example) === 1924)
  }

}
