package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day22Test extends AnyFlatSpec {
  val example = List(
  "Player 1:",
  "9",
  "2",
  "6",
  "3",
  "1",
  "",
  "Player 2:",
  "5",
  "8",
  "4",
  "7",
  "10"
  )

  "Part 1" should "solve example correct" in {
    assert(new Day22().solvePart1(example) === 306)
  }



  "Part 2" should "solve example correct" in {
    assert(new Day22().solvePart2(example) === 291)
  }

}
