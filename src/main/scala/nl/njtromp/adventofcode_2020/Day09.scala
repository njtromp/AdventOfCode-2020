package nl.njtromp.adventofcode_2020

import scala.io.Source

object Day09 extends App {
  var numbers: Array[Long] = Array.empty
  for (line <- Source.fromInputStream(Day01.getClass.getResourceAsStream("/input-puzzle09.txt")).getLines) {
    numbers = line.toLong +: numbers
  }
  numbers = numbers.reverse

//  numbers = Array(35, 20 ,15 ,25 ,47 ,40 ,62 ,55 ,65 ,95 ,102 ,117 ,150 ,182 ,127 ,219 ,299 ,277 ,309 ,576)
  val preamble = 25

  println(s"Answer part 1: ${solvePart1}")
  val answerPart2 = -1
  println(s"Answer part 2: $answerPart2")

  def solvePart1: Long = {
    for (i <- preamble to numbers.length) {
      val partial = numbers.slice(i - preamble, i)
      if (!partial.flatMap(n => partial.filter(m => n != m).map(m => n + m)).contains(numbers(i))) {
        return numbers(i)
      }
    }
    -1
  }

}
