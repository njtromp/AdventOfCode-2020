package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day16Test extends AnyFlatSpec {
  val example = List(
  "class: 1-3 or 5-7",
  "row: 6-11 or 33-44",
  "seat: 13-40 or 45-50",
  "",
  "your ticket:",
  "7,1,14",
  "",
  "nearby tickets:",
  "7,3,47",
  "40,4,50",
  "55,2,20",
  "38,6,12"
  )

  "Part 1 " should " solve example correct" in {
    assert(new Day16().solvePart1(example) === 71)
  }

  "Part 2 " should " solve example correct" in {
    assert(new Day16().solvePart2(example) === 0)
  }

}
