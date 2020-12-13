package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day13Test extends AnyFlatSpec {
  val example = Iterator("939", "7,13,x,x,59,x,31,19")

  "Part 1 " should " solve example correct " in {
    assert(Day13.solvePart1(example) == 295)
  }

  "Part 2 " should " solve example correct " in {
    assert(Day13.solvePart2(example) == 1068781)
  }
}
