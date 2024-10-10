package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day14Test extends AnyFlatSpec {

  "Part 1 " should " solve example correct" in {
    val examplePart = List(
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
    )
    assert(new Day14().solvePart1(examplePart) === 165L)
  }

  "Part 2 " should " solve example correct" in {
    val examplePart = List(
      "mask = 000000000000000000000000000000X1001X",
      "mem[42] = 100",
      "mask = 00000000000000000000000000000000X0XX",
      "mem[26] = 1"
    )
    assert(new Day14().solvePart2(examplePart) === 208)
  }

}
