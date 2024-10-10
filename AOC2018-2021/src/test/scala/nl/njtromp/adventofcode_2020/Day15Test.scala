package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day15Test extends AnyFlatSpec {

  "Part 1 " should " solve example correct" in {
    val example = List("0,3,6")
    assert(new Day15().solvePart1(example) === 436)
  }

  "Part 1 " should " solve example 1 correct" in {
    val example = List("1,3,2")
    assert(new Day15().solvePart1(example) === 1)
  }

  "Part 1 " should " solve example 2 correct" in {
    val example = List("2,1,3")
    assert(new Day15().solvePart1(example) === 10L)
  }

  "Part 1 " should " solve example 3 correct" in {
    val example = List("1,2,3")
    assert(new Day15().solvePart1(example) === 27L)
  }

  "Part 1 " should " solve example 4 correct" in {
    val example = List("2,3,1")
    assert(new Day15().solvePart1(example) === 78L)
  }

  "Part 1 " should " solve example 5 correct" in {
    val example = List("3,2,1")
    assert(new Day15().solvePart1(example) === 438L)
  }

  "Part 1 " should " solve example 6 correct" in {
    val example = List("3,1,2")
    assert(new Day15().solvePart1(example) === 1836L)
  }



  "Part 2 " should " solve example correct" in {
    val example = List("0,3,6")
    assert(new Day15().solvePart2(example) === 175594)
  }

  "Part 2 " should " solve example 1 correct" in {
    val example = List("1,3,2")
    assert(new Day15().solvePart2(example) === 2578)
  }

  "Part 2 " should " solve example 2 correct" in {
    val example = List("2,1,3")
    assert(new Day15().solvePart2(example) === 3544142)
  }

  "Part 2 " should " solve example 3 correct" in {
    val example = List("1,2,3")
    assert(new Day15().solvePart2(example) === 261214)
  }

  "Part 2 " should " solve example 4 correct" in {
    val example = List("2,3,1")
    assert(new Day15().solvePart2(example) === 6895259)
  }

  "Part 2 " should " solve example 5 correct" in {
    val example = List("3,2,1")
    assert(new Day15().solvePart2(example) === 18)
  }

  "Part 2 " should " solve example 6 correct" in {
    val example = List("3,1,2")
    assert(new Day15().solvePart2(example) === 362)
  }


}
