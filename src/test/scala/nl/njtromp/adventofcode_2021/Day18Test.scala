package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day18Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day18-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day18().solvePart1(example) === 4140)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day18().solvePart2(example) === 0)
  }

}
