package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day25Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day25-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day25().solvePart1(example) === 58)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day25().solvePart2(example) === 0)
  }



}
