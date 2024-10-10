package nl.njtromp.adventofcode_2018

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day01Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2018/day01-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day01().solvePart1(example) === 3)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day01().solvePart2(example) === 2)
  }

}
