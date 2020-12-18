package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day17Test extends AnyFlatSpec {
  val example = List(
  ".#.",
  "..#",
  "###"
  )

  "Part 1" should "solve example correct" in {
    assert(new Day17().solvePart1(example) === 112)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day17().solvePart2(example) === 848)
  }

}
