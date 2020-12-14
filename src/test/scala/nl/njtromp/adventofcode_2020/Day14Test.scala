package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day14Test extends AnyFlatSpec {
  val example = List(
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
  )

  "Part 1 " should " solve example correct" in {
    assert(Day14.solvePart1(example) === 165L)
  }

  "Part 2 " should " solve example correct" in {
    assert(Day14.solvePart2(example) === 208)
  }

}
