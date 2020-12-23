package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day23Test extends AnyFlatSpec {
  val example = List("389125467")

  "Part 1" should "solve example with 10 moves correct" in {
    assert(new Day23(10).solvePart1(example) === 92658374L)
  }

  "Part 1" should "solve example with 100 moves correct" in {
    assert(new Day23(100).solvePart1(example) === 67384529L)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day23(0).solvePart2(example) === 0)
  }

}
