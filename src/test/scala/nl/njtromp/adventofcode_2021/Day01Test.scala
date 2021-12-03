package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source


class Day01Test extends AnyFlatSpec {
  val example = List("199", "200", "208", "210", "200", "207", "240", "269", "260", "263")

  "Part 1" should "solve example correct" in {
    assert(new Day01().solvePart1(example) === 7)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day01().solvePart2(example) === 5)
  }


}
