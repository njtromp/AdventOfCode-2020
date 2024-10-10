package nl.njtromp.adventofcode_2018

import org.scalatest.flatspec.AnyFlatSpec

class Day12Test extends AnyFlatSpec {
  val example = List(
    "initial state: #..#.#..##......###...###",
//    "",
    "...## => #",
    "..#.. => #",
    ".#... => #",
    ".#.#. => #",
    ".#.## => #",
    ".##.. => #",
    ".#### => #",
    "#.#.# => #",
    "#.### => #",
    "##.#. => #",
    "##.## => #",
    "###.. => #",
    "###.# => #",
    "####. => #"
  )

  "Part 1" should "solve example correct" in {
    assert(new Day12(20, 0).solvePart1(example) === 325)
  }

  "Part 2" should "solve example correct" in {
    assert(new Day12(0, 0).solvePart2(example) === 145)
  }

}
