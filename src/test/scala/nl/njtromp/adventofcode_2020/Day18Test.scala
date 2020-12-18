package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day18Test extends AnyFlatSpec {

  "Part 1" should "solve example 1 correct" in {
    val example = List("1 + 2 * 3 + 4 * 5 + 6")
    assert(new Day18().solvePart1(example) === 71)
  }

  "Part 1" should "solve example 2 correct" in {
    val example = List("1 + (2 * 3) + (4 * (5 + 6))")
    assert(new Day18().solvePart1(example) === 51)
  }

  "Part 1" should "solve example 3 correct" in {
    val example = List("2 * 3 + (4 * 5)")
    assert(new Day18().solvePart1(example) === 26)
  }

  "Part 1" should "solve example 4 correct" in {
    val example = List("5 + (8 * 3 + 9 + 3 * 4 * 3)")
    assert(new Day18().solvePart1(example) === 437)
  }

  "Part 1" should "solve example 5 correct" in {
    val example = List("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
    assert(new Day18().solvePart1(example) === 12240)
  }

  "Part 1" should "solve example 6 correct" in {
    val example = List("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    assert(new Day18().solvePart1(example) === 13632)
  }


  "Part 2" should "solve example 1 correct" in {
    val example = List("1 + 2 * 3 + 4 * 5 + 6")
    assert(new Day18().solvePart1(example) === 231)
  }

  "Part 2" should "solve example 2 correct" in {
    val example = List("1 + (2 * 3) + (4 * (5 + 6))")
    assert(new Day18().solvePart1(example) === 51)
  }

  "Part 2" should "solve example 3 correct" in {
    val example = List("2 * 3 + (4 * 5)")
    assert(new Day18().solvePart1(example) === 46)
  }

  "Part 2" should "solve example 4 correct" in {
    val example = List("5 + (8 * 3 + 9 + 3 * 4 * 3)")
    assert(new Day18().solvePart1(example) === 1445)
  }

  "Part 2" should "solve example 5 correct" in {
    val example = List("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
    assert(new Day18().solvePart1(example) === 669060)
  }

  "Part 2" should "solve example 6 correct" in {
    val example = List("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
    assert(new Day18().solvePart1(example) === 23340)
  }

}
