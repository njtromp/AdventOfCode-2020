package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day07Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day07-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day07().solvePart1(example) === 0)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day07().solvePart2(example) === 0)
  }


}
