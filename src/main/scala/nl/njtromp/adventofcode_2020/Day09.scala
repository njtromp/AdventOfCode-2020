package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day09 extends App {
  var numbers: Array[Long] = Array.empty
  for (line <- Source.fromInputStream(getClass.getResourceAsStream("/2020/input-puzzle09.txt")).getLines) {
    numbers = line.toLong +: numbers
  }
  numbers = numbers.reverse
  val preamble = 25

  println(s"Answer part 1: ${solvePart1}")
  println(s"Answer part 2: ${solvePart2(solvePart1)}")

  def solvePart1: Long = {
    for (i <- preamble to numbers.length) {
      val partial = numbers.slice(i - preamble, i)
      if (!partial.flatMap(n => partial.filter(m => n != m).map(m => n + m)).contains(numbers(i))) {
        return numbers(i)
      }
    }
    -1
  }

  def solvePart2(valueToConstruct: Long): Long = {
    for (i <- numbers.indices) {
      var j = i
      while (numbers.slice(i, j).sum < valueToConstruct) j += 1
      if (numbers.slice(i, j).sum == valueToConstruct)
        return numbers.slice(i, j).min + numbers.slice(i, j).max
    }
    -1
  }
}
