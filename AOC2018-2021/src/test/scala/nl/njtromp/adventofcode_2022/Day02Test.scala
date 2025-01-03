package nl.njtromp.adventofcode_2022

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day02Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2022/day02-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day02().solvePart1(example) === 0)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day02().solvePart2(example) === 0)
  }

}
