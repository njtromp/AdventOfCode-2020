package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day19Test extends AnyFlatSpec {
  val example = List(
  "0: 4 1 5",
  "1: 2 3 | 3 2",
  "2: 4 4 | 5 5",
  "3: 4 5 | 5 4",
  "4: a",
  "5: b",
  "",
  "ababbb",
  "bababa",
  "abbbab",
  "aaabbb",
  "aaaabbb"
  )

  "Part 1" should "solve example correct" in {
    assert(new Day19().solvePart1(example) === 2)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day19().solvePart2(example) === 0)
  }

}
