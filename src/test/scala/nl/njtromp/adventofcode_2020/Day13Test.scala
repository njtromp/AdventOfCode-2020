package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day13Test extends AnyFlatSpec {
  val example = List("939", "7,13,x,x,59,x,31,19")

  "Part 1 " should " solve example correct" in {
    assert(new Day13().solvePart1(example) == 295)
  }

  "Part 2 " should " solve example correct" in {
    assert(new Day13().solvePart2(example) == 1068781)
  }

  "Part 2 " should "solve also extra example 1 correct" in {
    val example = List("", "17,x,13,19")
    assert(new Day13().solvePart2(example) == 3417)
  }

  "Part 2 " should "solve also extra example 2 correct" in {
    val example = List("", "67,7,59,61")
    assert(new Day13().solvePart2(example) == 754018)
  }

  "Part 2 " should "solve also extra example 3 correct" in {
    val example = List("", "67,x,7,59,61")
    assert(new Day13().solvePart2(example) == 779210)
  }

  "Part 2 " should "solve also extra example 4 correct" in {
    val example = List("", "67,7,x,59,61")
    assert(new Day13().solvePart2(example) == 1261476)
  }

  "Part 2 " should "solve also extra example 5 correct" in {
    val example = List("", "1789,37,47,1889")
    assert(new Day13().solvePart2(example) == 1202161486)
  }

}
