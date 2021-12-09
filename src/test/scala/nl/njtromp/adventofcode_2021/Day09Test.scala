package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day09Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day09-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day09().solvePart1(example) === 15)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day09().solvePart2(example) === 1134)
  }

}
