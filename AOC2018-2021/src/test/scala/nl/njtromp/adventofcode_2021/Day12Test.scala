package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day12Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day12-example.txt")).getLines().toList
  val example2: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day12-example-2.txt")).getLines().toList
  val example3: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day12-example-3.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day12().solvePart1(example) === 10)
  }

  "Part 1" should "solve example 2 correct" in {
    assert(new Day12().solvePart1(example2) === 19)
  }

  "Part 1" should "solve example 3 correct" in {
    assert(new Day12().solvePart1(example3) === 226)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day12().solvePart2(example) === 36)
  }

  "Part 2" should "solve example 2 correct" in {
    assert(new Day12().solvePart2(example2) === 103)
  }

  "Part 2" should "solve example 3 correct" in {
    assert(new Day12().solvePart2(example3) === 3509)
  }

}
