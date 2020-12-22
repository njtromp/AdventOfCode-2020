package nl.njtromp.adventofcode_2020

import org.scalatest.flatspec.AnyFlatSpec

class Day21Test extends AnyFlatSpec {
  val example = List(
      "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
      "trh fvjkl sbzzf mxmxvkd (contains dairy)",
      "sqjhc fvjkl (contains soy)",
      "sqjhc mxmxvkd sbzzf (contains fish)"
  )

  "Part 1" should "solve example correct" in {
    assert(new Day21().solvePart1(example) === 5)
  }



  "Part 2" should "solve example correct" in {
    assert(new Day21().solvePart2(example) === 0)
  }

}
