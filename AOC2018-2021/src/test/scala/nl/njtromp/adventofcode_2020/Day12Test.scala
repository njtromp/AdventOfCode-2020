package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day12Test extends AnyFlatSpec {

  val example = List("F10", "N3", "F7", "R90", "F11")

  "Part 1" should "solve example correct" in {
        assert(new Day12().solvePart1(example) === 25)
    }

  "Part 2" should "solve example correct" in {
        assert(new Day12().solvePart2(example) === 286)
    }

}
