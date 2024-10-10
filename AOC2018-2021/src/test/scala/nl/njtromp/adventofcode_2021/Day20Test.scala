package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day20Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day20-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day20().solvePart1(example) === 35)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day20().solvePart2(example) === 3351)
  }

}
