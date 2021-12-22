package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day22Test extends AnyFlatSpec {
  val example: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day22-example.txt")).getLines().toList

  "Part 1" should "solve example correct" in {
    assert(new Day22().solvePart1(example) === 590784)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day22().solvePart2(example) === 2758514936282235L)
  }



}
