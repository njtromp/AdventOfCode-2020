package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day22Test extends AnyFlatSpec {
  val example_1: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day22-example-1.txt")).getLines().toList
  val example_2: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day22-example-2.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day22().solvePart1(example_1) === 590784)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day22().solvePart2(example_2) === 2758514936282235L)
  }



}
