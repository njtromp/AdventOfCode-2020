package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day25Test extends AnyFlatSpec {
  val example = List(
    "5764801",
    "17807724"
  )

  "Part 1" should "solve example correct" in {
    assert(new Day25().encrypt(7, 8) === 5764801)
    assert(new Day25().encrypt(7, 11) === 17807724)
    assert(new Day25().findLoopSize(5764801) === 8)
    assert(new Day25().findLoopSize(17807724) === 11)
    assert(new Day25().solvePart1(example) === 14897079)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day25().solvePart2(example) === 0)
  }

}
