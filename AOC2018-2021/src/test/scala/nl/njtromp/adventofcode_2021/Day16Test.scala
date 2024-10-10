package nl.njtromp.adventofcode_2021

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class Day16Test extends AnyFlatSpec {
  val example_1_0: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day16-example-1-0.txt")).getLines().toList
  val example_1_1: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day16-example-1-1.txt")).getLines().toList
  val example_1_2: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day16-example-1-2.txt")).getLines().toList
  val example_1_3: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day16-example-1-3.txt")).getLines().toList
  val example_2: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/2021/day16-example-2.txt")).getLines().toList

  "Part 1" should "solve example 0 correct" in {
    assert(new Day16().solvePart1(example_1_0) === 6)
  }

  "Part 1" should "solve example 1 correct" in {
    assert(new Day16().solvePart1(example_1_1) === 16)
  }

  "Part 1" should "solve example 2 correct" in {
    assert(new Day16().solvePart1(example_1_2) === 23)
  }

  "Part 1" should "solve example 3 correct" in {
    assert(new Day16().solvePart1(example_1_3) === 31)
  }

  "Part 2" should "solve example correct" in {
    example_2.map(_.split(" ")).foreach(ms =>
      assert(new Day16().solvePart2(List(ms(0))) === ms(1).toLong)
    )

  }

}
