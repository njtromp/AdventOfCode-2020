package nl.njtromp.adventofcode_2018

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day02Test extends AnyFlatSpec {
  val example1: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2018/day02-example-1.txt")).getLines().toList
  val example2: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2018/day02-example-2.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day02().solvePart1(example1) === 12)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day02().solvePart2(example2) === 0)
  }

}
