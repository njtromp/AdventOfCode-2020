package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day23Test extends AnyFlatSpec {
  val example = List("389125467")

  "Part 1" should "solve example with 10 moves correct" in {
    assert(new Day23(9, 10, 0, 0).solvePart1(example) === 92658374L)
  }

  "Part 1" should "solve example with 100 moves correct" in {
    assert(new Day23(9, 100, 0, 0).solvePart1(example) === 67384529L)
  }

  "Part 2" should "solve example correct" in {
    val example2 = List("389125467")
    assert(new Day23(0, 0, 1000000L,10000000L).solvePart2(example2) === 149245887792L)
  }

}
