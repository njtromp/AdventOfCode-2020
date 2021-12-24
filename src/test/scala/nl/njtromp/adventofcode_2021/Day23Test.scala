package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day23Test extends AnyFlatSpec {
  val example1: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day23-example-1.txt")).getLines().toList
  val example2: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day23-example-2.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day23().solvePart1(example1) === 12521)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day23().solvePart2(example2) === 44169)
  }



}
