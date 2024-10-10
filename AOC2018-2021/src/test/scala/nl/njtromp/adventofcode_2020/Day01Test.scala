package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day01Test extends AnyFlatSpec {
  val example: Set[Int] =  Set(1721, 979, 366, 299, 675, 1456)

  "Part 1 " should " solve example correct " in {
    assert(Day01.solvePart1(example) == 514579)
  }

  "Part 2 " should " solve example correct " in {
    assert(Day01.solvePart2(example) == 241861950)
  }
}
