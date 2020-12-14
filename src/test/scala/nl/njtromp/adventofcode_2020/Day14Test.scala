package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day14Test extends AnyFlatSpec {
  val examplePart1 = List(
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
  )

  "Part 1 " should " solve example correct" in {
    assert(Day14.solvePart1(examplePart1) === 165L)
  }

  val examplePart2 = List(
      "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"
  )

  "Part 2 " should " solve example correct" in {
    assert(Day14.solvePart2(examplePart2) === 208)
  }

}
