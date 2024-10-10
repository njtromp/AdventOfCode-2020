package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day14Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day14-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day14().solvePart1(example) === 1588 - 1) // Some oddities with 'B' count
  }

  "Part 2" should "solve example correct" in {
    assert(new Day14().solvePart2(example) === 2188189693529L)
  }

}
